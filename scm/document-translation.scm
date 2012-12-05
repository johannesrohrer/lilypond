;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;; Main sources of information processed in this file:
;;
;; * output of (ly:output-description OUTPUT-DEF)
;; * output of (ly:get-all-translators)
;; * output of (ly:translator-description TRANSLATOR)
;; * all-grob-descriptions
;; * all-user-translation-properties
;; * all-internal-translation-properties
;;
;; together with the variables output-def-descs, translator-types and
;; output-object-types defined here below.


;;; Helper functions

(define (predicate-alist-lookup element lst)
  "Return the first element of LST that is an alist with a
'predicate entry that returns not #f when applied to ELEMENT. Return
#f if none is found."
  (if (null? lst)
      #f
      (let* ((first-alist (car lst))
             (pred (assoc-get 'predicate first-alist)))
        (if (and pred (pred element))
            first-alist
            (predicate-alist-lookup element (cdr lst))))))

(define (group-by-function-result func lst)
  "Partition LST into sublists of elements that yield the same value
when FUNC is applied to them, with that value prepended to each
sublist."
  (let ((plist '()))
    (map (lambda (x)
           (let ((key (func x)))
             (set! plist
                   (assoc-set! plist
                               key
                               (append (assoc-get key plist '())
                                       (list x))))))
         lst)
    plist))

;; The context description alists returned by ly:output-description
;; contain name symbols for translators rather than translator objects,
;; so we need a lookup table.
(define name->translator-table (make-hash-table 61))
(map
 (lambda (x)
   (hash-set! name->translator-table (ly:translator-name x) x))
 (ly:get-all-translators))

(define (find-translator-by-name-sym name-sym)
  (hash-ref name->translator-table name-sym #f))


;;; Context classification (layout, MIDI)

(define output-def-descs
  (list
   (list
    $defaultlayout
    '(context-type-name . "layout context")
    '(context-category-name . "Layout contexts")
    '(context-category-desc . "Contexts for graphical output")
    '(context-category-text . "Layout contexts are built from
@ref{Engravers} to produce graphical output. Their default versions
are defined in @code{ly/engraver-init.ly}.")
    ;; The context-node-suffix is appended to the texinfo node name
    ;; of each context node that belongs to this output definition.
    ;; For backwards compatibility of links, use no suffix for layout
    ;; contexts.
    '(context-node-suffix . ""))

   (list
    $defaultmidi
    '(context-type-name . "MIDI context")
    '(context-category-name . "MIDI contexts")
    '(context-category-desc . "Contexts for MIDI output")
    '(context-category-text . "MIDI contexts are built from
@ref{Performers} to produce MIDI output. Their default versions are
defined in @code{ly/performer-init.ly}")
    '(context-node-suffix . " (MIDI)"))))


;;; Retrieving context information

;; (ly.output-description OUTPUT-DEF) returns a list of context
;; descriptions of the form (name-sym . info-alist), where info-alist
;; contains most of the interesting data.
;;
;; Collect these descriptions from all output definitions in a
;; variable, and enrich each info-alist with an 'output-def entry,
;; a 'type-name entry and a 'node-name entry.
;;
;; Within this file, use these context-desc elements to unambigously
;; identify contexts.
(define all-context-descs-list
  (apply
   append
   (map
    (lambda (output-def-desc)
      (let* ((output-def (car output-def-desc))
             (output-desc (sort (ly:output-description output-def)
                                (lambda (x y)
                                 (ly:symbol-ci<? (car x) (car y)))))
             (category-data (cdr output-def-desc))
             (type-name (assoc-get 'context-type-name category-data))
             (suffix (assoc-get 'context-node-suffix category-data)))
        (map
          (lambda (context-desc)
            (let* ((name-sym (car context-desc))
                   (info-alist (cdr context-desc))
                   (node-name (string-append (symbol->string name-sym)
                                             suffix)))
              (cons name-sym
                    (acons 'output-def output-def
                     (acons 'type-name type-name
                      (acons 'node-name node-name info-alist))))))
          output-desc)))
    output-def-descs)))

(define* (context-attr attr context-desc #:optional (not-found-result #f))
  (assoc-get attr (cdr context-desc) not-found-result))

(define (context-name-sym context-desc)
  (car context-desc))

(define (context-ref context-desc)
  (ref-ify (context-attr 'node-name context-desc)))

(define (contexts-with-name-sym name-sym)
  (filter (lambda (cd) (eq? name-sym (context-name-sym cd)))
          all-context-descs-list))
          
(define (contexts-from-output-def output-def)
  (filter (lambda (cd) (equal? output-def (context-attr 'output-def cd)))
          all-context-descs-list))
          
(define (find-context-desc name-sym output-def)
  (let ((candidates
         (filter (lambda (cd) (equal? output-def
                                      (context-attr 'output-def cd)))
                 (contexts-with-name-sym name-sym))))
    (if (null? candidates)
        #f
        (car candidates))))

(define (corresponding-context-strings name-sym exclude-output-def)
  (let* ((all-with-name-sym (contexts-with-name-sym name-sym))
         (all-relevant
           (if exclude-output-def
               (filter (lambda (cd)
                         (not (equal? exclude-output-def
                                      (context-attr 'output-def cd))))
                       all-with-name-sym)
               all-with-name-sym)))
    (map (lambda (cd)
           (format "Corresponding ~a: ~a.\n\n"
                   (context-attr 'type-name cd)
                   (context-ref cd)))
         all-relevant)))


;;; Translator classification (engravers, performers, ...)

;; Identify specific translator types (currently: engravers and
;; performers) by their names. An alternative would be to implement
;; proper type predicates.
;;
;; This separation is purely cosmetic; specifically, associating a
;; translator with the correct context types does not depend on it.

(define (engraver? translator)
  (string-suffix-ci? "Engraver"
                     (symbol->string (ly:translator-name translator))))

(define (performer? translator)
  (string-suffix-ci? "Performer"
                     (symbol->string (ly:translator-name translator))))

(define specific-translator-types
  `(((type-name . "engraver")
     (predicate . ,engraver?)
     (category-node-name . "Engravers")
     (category-node-desc . "All separate engravers")
     (category-node-text . "Engravers produce @emph{layout objects},
also known as @emph{grobs}, the building blocks for graphical
output. They are part of @ref{Layout contexts}.

See also @ruser{Modifying context plug-ins}."))

    ((type-name . "performer")
     (predicate . ,performer?)
     (category-node-name . "Performers")
     (category-node-desc . "All separate performers")
     (category-node-text . "Performers produce @emph{audio items} used
for assembling MIDI output. They are part of @ref{MIDI contexts}."))))

;; Any translator that does not fit one of these specific types
;; shall wind up in a final default category.

(define (no-specific-translator? translator)
  (not (predicate-alist-lookup translator specific-translator-types)))

(define translator-types
  (append
   specific-translator-types
   `(((type-name . "translator")
      (predicate . ,no-specific-translator?)
      (category-node-name . "Other translators")
      (category-node-desc . "Translators that are neither engravers nor
performers.")
      (category-node-text . "")))))

(define (translator-type-alist translator)
  (predicate-alist-lookup translator translator-types))

(define (translator-type-name translator)
  (assoc-get 'type-name (translator-type-alist translator)))

(define all-translators-list
  (sort (ly:get-all-translators)
        (lambda (a b)
          (ly:symbol-ci<? (ly:translator-name a)
                          (ly:translator-name b)))))


;;; Output object classification (layout objects, audio items...)

(define specific-output-object-types
  `(((type-name . "layout object")
     (predicate . ,(lambda (name-sym)
                     (memq name-sym
                           (map car all-grob-descriptions)))))

    ;; Audio item descriptions don't exist yet.
    ;;((type-name . "audio item")
    ;; (predicate . ,(lambda (name-sym)
    ;;                (memq name-sym
    ;;                      (map car all-audio-item-descriptions)))))
   ))

 (define (unknown-output-object? name-sym)
    (not (predicate-alist-lookup name-sym
                                 specific-output-object-types)))

(define output-object-types
  (append
   specific-output-object-types
   `((type-name . "undocumented output object")
     (predicate . ,unknown-output-object?))))

(define (output-object-type-alist oo-name-sym)
  (predicate-alist-lookup oo-name-sym output-object-types))

(define (output-object-type-name oo-name-sym)
  (assoc-get 'type-name (output-object-type-alist oo-name-sym)))

(define (output-object-ref oo-name-sym)
  (if (unknown-output-object? oo-name-sym)
      (symbol->string oo-name-sym)
      (ref-ify (symbol->string oo-name-sym))))

(define (output-objects-creation-strings creator oo-list)
  "Assemble a list of strings
'This CREATOR creates the following [output object](s): ...',
one for each output object type present in OO-LIST."
  (let* ((oos-by-type (group-by-function-result output-object-type-name
                                                oo-list)))
    (if (null? oo-list)
        (list (format "This ~a does not create any output objects.\n\n"
                      creator))
        (map (lambda (oo-type-entry)
               (let ((oo-type (car oo-type-entry)) ; e.g. "layout object"
                     (oo-sublist (cdr oo-type-entry)))
                 (format "This ~a creates the following ~a(s):\n\n~a\n\n"
                         creator
                         oo-type
                         (human-listify (map output-object-ref oo-sublist)))))
             oos-by-type))))


;;; Translator - output object relation

(define (translator-output-objects translator)
  "Return a list of name-symbols."
  ;; For historical reasons, the relevant key in the translator
  ;; description is named grobs-created, no matter whether the
  ;; translator actually is an engraver or some other type.
  (delete-duplicates
   (sort
    (assoc-get 'grobs-created (ly:translator-description translator) '())
    ly:symbol-ci<?)))

(define (translators-making-output-object name-sym)
  "Return a list of translator objects that create output object
NAME-SYM."
  (filter
   (lambda (trans)
     (memq name-sym (translator-output-objects trans)))
   (ly:get-all-translators)))

(define (translator-makes-objects-string translator)
  "Assemble 'This [translator] creates the following [object](s): ...' lines."
  (let* ((output-objects (translator-output-objects translator))
         (ttype (translator-type-name translator)) ; e.g. "engraver"
         (desc-lines (output-objects-creation-strings ttype output-objects)))
    (string-join desc-lines)))


;;; Translator - music expression relation

(define (translator-accepts-music-type? event-name-symbol translator)
  (memq event-name-symbol
        (assoc 'events-accepted (ly:translator-description translator))))

(define (translator-accepts-music-types? types translator)
  (if (null? types)
      #f
      (or
       (translator-accepts-music-type? (car types) translator)
       (translator-accepts-music-types? (cdr types) translator))))

(define (translator-accepts-music-types-string translator)
  (let ((accepted (assoc-get 'events-accepted
                             (ly:translator-description translator))))
    (if (null? accepted)
        ""
        (format "Music types accepted:\n\n~a\n\n"
                (human-listify
                 (map ref-ify (sort (map symbol->string accepted)
                                    ly:string-ci<?)))))))


;;; Translator - Context relation

(define (contexts-with-translator translator)
  "Find contexts that consist TRANSLATOR. Return a list of context
descriptions."
  (let ((tr-name-sym (ly:translator-name translator)))
    (filter
     (lambda (cd) 
       (let ((group (context-attr 'group-type cd))
             (consists-name-syms (sort (context-attr 'consists cd)
                                 ly:symbol-ci<?)))
         (or (member tr-name-sym consists-name-syms)
             (and group (eq? tr-name-sym group)))))
     all-context-descs-list)))

(define (contexts-with-translator-string translator)
  (let* ((translator-name-str (symbol->string (ly:translator-name translator)))
         (context-descs (contexts-with-translator translator))
         (context-list (human-listify
                        (map context-ref context-descs))))
    (if (null? context-descs)
        (format "@code{~a} is not part of any context.\n\n"
                translator-name-str)
        (format "@code{~a} is part of the following context(s):\n\n~a\n\n"
                translator-name-str
                context-list))))


;;; Translator - property relation

(define (translator-properties-list-string props)
  (let ((description-list (map (lambda (x)
                                 (property->texi 'translation x '()))
                               (sort props ly:symbol-ci<?))))
    (if (null? props)
        ""
        (description-list->texi description-list #t))))

(define (translator-properties-read-string translator)
  (let* ((props (assoc-get 'properties-read
                           (ly:translator-description translator)
                           '()))
         (str (translator-properties-list-string props)))
    (if (string-null? str)
        ""
        (format "Properties (read)\n~a\n\n" str))))

(define (translator-properties-written-string translator)
  (let* ((props (assoc-get 'properties-written
                           (ly:translator-description translator)
                           '()))
         (str (translator-properties-list-string props)))
    (if (string-null? str)
        ""
        (format "Properties (written)\n~a\n\n" str))))


;;; Context - output object relation

(define (context-output-objects context-desc)
  (let* ((group (assq-ref (cdr context-desc) 'group-type))
         (consists-syms (assoc-get 'consists (cdr context-desc) '()))
         (consists (map find-translator-by-name-sym consists-syms)))
    (delete-duplicates
     (sort
      (apply append (map translator-output-objects consists))
      ly:symbol-ci<?))))

(define (context-creates-output-objects-string context-desc)
  (let* ((oos (context-output-objects context-desc))
         (desc-lines (output-objects-creation-strings "context" oos)))
    (string-join desc-lines)))


;;; Context - property relation

(define (document-property-operation prop-op context-desc)
  (let* ((tag (car prop-op))
         (context-sym (cadr prop-op))
         (args (cddr prop-op)))
    (cond
     ((equal?  tag 'push)
      (let ((value (car args))
            (path (cdr args)))
        (format "@item Set grob-property @code{~a} in ~a to ~a.\n"
                (string-join (map symbol->string path) " ")
                (context-ref context-desc)
                (scm->texi value))))
     ((equal? (object-property context-sym 'is-grob?) #t) "")
     ((equal? tag 'assign)
      (format #f "@item Set translator property @code{~a} to ~a.\n"
              context-sym
              (scm->texi (car args)))))))

(define (context-property-operations-string context-desc)
  (let* ((output-def (context-attr 'output-def context-desc))
         (prop-ops (context-attr 'property-ops context-desc '()))
         (prop-op-items (map (lambda (prop-op)
                               (document-property-operation prop-op
                                                            context-desc))
                             prop-ops))
         (prop-ops-string (string-join
                           (sort prop-op-items ly:string-ci<?))))
    (if (string-null? prop-ops-string)
        ""
        (format
         "This context sets the following properties\n\n@itemize @bullet\n~a@end itemize\n\n"
         prop-ops-string))))


;;; Assembled single translator documentation

(define (translator-doc-string translator show-contexts?)
  (let ((desc (assoc-get 'description (ly:translator-description translator))))
    (string-append
     desc
     "\n\n"

     ;; "Music types accepted: ..."
     (translator-accepts-music-types-string translator)

     ;; "Properties (read) ..." / "Properties (written) ..."
     (translator-properties-read-string translator)
     (translator-properties-written-string translator)

     ;; "This (translator) creates the following (object)(s): ..."
     (translator-makes-objects-string translator)

     ;; Optional "(translator) is part of the following context(s): ..."
     (if show-contexts?
         (contexts-with-translator-string translator)
         ""))))

(define (translator-doc translator)
  "Standalone translator documentation node"
  (make <texi-node>
    #:name (symbol->string (ly:translator-name translator))
    #:text (translator-doc-string translator #t)))

(define (translator-doc-embedded translator)
  "Shortened translator description for embedding into context description."
  (cons (format "@code{~a}"
                (ref-ify (symbol->string (ly:translator-name translator))))
        (translator-doc-string translator #f)))


;;; Assembled single context documentation

(define (context-doc context-desc)
  (let* ((name-sym (context-name-sym context-desc))
         (name (symbol->string name-sym))
         (node-name (context-attr 'node-name context-desc))
         (desc (context-attr 'description context-desc))
         (output-def (context-attr 'output-def context-desc))
         (aliases-name-syms (sort (context-attr 'aliases context-desc)
                                  ly:symbol-ci<?))
         (accepts-name-syms (sort (context-attr 'accepts context-desc)
                                  ly:symbol-ci<?))
         (consists-name-syms (sort (context-attr 'consists context-desc)
                                   ly:symbol-ci<?))
         (prop-ops (context-attr 'property-ops context-desc))
         ;; With 'Timing', there exists at least one "context" that
         ;; appears as an alias, but is not a true context documented
         ;; in the output description. That also means there is no
         ;; documentation node to cross-reference. Isolate such cases
         ;; so we can ref-ify the others.
         (aliases-strings
          (map (lambda (ns)
                 (let ((cd (find-context-desc ns output-def)))
                   (if cd
                       (context-ref cd)
                       (format "~a (alias only)" (symbol->string ns)))))
               aliases-name-syms))
         (accepts-strings
          (map (lambda (ns)
                 (context-ref (find-context-desc ns output-def)))
               accepts-name-syms))
         (consists (map find-translator-by-name-sym consists-name-syms)))
    (if (or (not desc) (null? desc))
        (set! desc "(not documented)"))
    (make <texi-node>
      #:name
      node-name
      #:text
      (string-append
       desc
       "\n\n"

       (if (null? aliases-strings)
           ""
           (format "This context also accepts commands for the following context(s): ~a.\n\n"
                   (human-listify aliases-strings)))

       ;; "Corresponding [layout/midi] context: ..."
       (string-join (corresponding-context-strings name-sym output-def))

       ;; "This context creates the following [output object](s): ..."
       (context-creates-output-objects-string context-desc)

       ;; "This context sets the following properties: ..."
       (context-property-operations-string context-desc)

       (if (null? accepts-strings)
           "This context is a `bottom' context; it cannot contain other contexts.\n\n"
           (format "Context @code{~a} can contain\n~a.\n\n"
                   name
                   (human-listify accepts-strings)))

       (if (null? consists)
           ""
           (format "This context is built from the following translator(s):\n\n~a\n\n"
                   (description-list->texi
                    (map translator-doc-embedded consists)
                    #t)))))))


;;; High-level documentation nodes

(define (context-type-node output-def-desc)
  (let* ((output-def (car output-def-desc))
         (output-desc (sort (ly:output-description output-def)
                            (lambda (x y) (ly:symbol-ci<? (car x) (car y)))))
         (category-data (cdr output-def-desc)))
    (make <texi-node>
      #:name (assoc-get 'context-category-name category-data)
      #:desc (assoc-get 'context-category-desc category-data)
      #:text (assoc-get 'context-category-text category-data)
      #:children
      (map context-doc (contexts-from-output-def output-def)))))

(define (all-contexts-doc)
  (make <texi-node>
    #:name "Contexts"
    #:desc "Complete descriptions of all contexts."
    #:children
    (map context-type-node output-def-descs)))

(define (translator-type-node ttype-desc)
  (let ((pred (assoc-get 'predicate ttype-desc)))
    (make <texi-node>
      #:name (assoc-get 'category-node-name ttype-desc)
      #:desc (assoc-get 'category-node-desc ttype-desc)
      #:text (assoc-get 'category-node-text ttype-desc)
      #:children
      (map translator-doc
           (filter pred all-translators-list)))))

(define (all-translators-doc)
  (make <texi-node>
    #:name "Translators"
    #:desc "Engravers, performers and other translators."
    #:text "Translators are part of contexts. They @emph{accept} (or
@emph{listen to}) some types of music expressions delivered by
iterators as @emph{stream events}, process them and create various
types of output objects. They may also @emph{acknowledge} output
objects produced by other translators.

@emph{Context properties} control the behaviour of translators."
    #:children
    (map translator-type-node translator-types)))

(define (translation-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
         (sortedsyms (map string->symbol ps))
         (propdescs
          (map
           (lambda (x) (property->texi 'translation  x '()))
           sortedsyms))
         (texi (description-list->texi propdescs #f)))
    texi))

(define (translation-doc-node)
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout or audio."
    #:children
    (list
     (all-contexts-doc)
     (all-translators-doc)
     (make <texi-node>
       #:name "Tunable context properties"
       #:desc "All tunable context properties."
       #:text (translation-properties-doc-string
               all-user-translation-properties))

     (make <texi-node>
       #:name "Internal context properties"
       #:desc "All internal context properties."
       #:text (translation-properties-doc-string
               all-internal-translation-properties)))))
