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

;;;; Class infrastructure

;;; Contexts

(define-class <context-doc> (<texi-node>)
  ;; "parent" <context-type-doc> instance
  (context-type-doc
   #:init-keyword #:context-type-doc
   #:getter context-type-doc)
  ;; context record in the form (name-sym . info-alist) as returned as
  ;; part of (ly.output-description OUTPUT-DEF). info-alist contains
  ;; most of the interesting data; use the 'attr' method defined below
  ;; to access it.
  (context-desc #:init-keyword #:context-desc #:getter context-desc))

(define-method (attr attribute (context-doc <context-doc>) not-found-result)
  (assoc-get attribute
             (cdr (slot-ref context-doc 'context-desc))
             not-found-result))

(define-method (attr attribute obj) (attr attribute obj #f))

(define-method (name-sym (cd <context-doc>))
  (car (slot-ref cd 'context-desc)))

(define-method (type-string (cd <context-doc>))
  (type-string (context-type-doc cd)))

(define-method (initialize (cd <context-doc>) initargs)
  (next-method)
  (set!
   (node-name cd)
   (string-append (symbol->string (name-sym cd))
                  (node-suffix (context-type-doc cd)))))


;;; Context types (layout, MIDI)

(define-class <context-type-doc> (<texi-node>)
  (output-def #:init-keyword #:output-def)
  (type-string #:init-keyword #:type-string #:getter type-string)
  ;; The node-suffix is appended to the texinfo node name of each
  ;; context that belongs to this output definition.
  (node-suffix #:init-keyword #:node-suffix #:getter node-suffix))

(define-method (initialize (ctd <context-type-doc>) initargs)
  (next-method)
  (set!
   (node-children ctd)
   (let ((context-descs
          (sort (ly:output-description (slot-ref ctd 'output-def))
                (lambda (x y) (ly:symbol-ci<? (car x) (car y))))))
     (map (lambda (context-desc)
            (make <context-doc>
              #:context-type-doc ctd
              #:context-desc context-desc))
          context-descs))))


;;; Translators

(define-class <translator-doc> (<texi-node>)
  ;; <translator> object
  (translator #:init-keyword #:translator #:getter translator)
  (translator-type-doc   ; "parent" <translator-type-doc> object
   #:init-keyword #:translator-type-doc
   #:getter translator-type-doc))

(define-method (attr attribute (td <translator-doc>) not-found-result)
  (assoc-get attribute 
             (ly:translator-description (translator td))
             not-found-result))

(define-method (name-sym (td <translator-doc>))
  (ly:translator-name (translator td)))

(define-method (type-string (td <translator-doc>))
  (type-string (translator-type-doc td)))

(define-method (initialize (td <translator-doc>) initargs)
  (next-method)
  (set! (node-name td) (symbol->string (name-sym td))))


;;; Translator types (engravers, performers, ...)
;;
;; In contrast to context types, this classification is purely
;; cosmetic. Specifically, associating a translator with contexts of
;; the right type does not depend on it.

(define-class <translator-type-doc> (<texi-node>)
  (type-string #:init-keyword #:type-string #:getter type-string)
  (translators #:init-keyword #:translators #:getter translators))

(define-method (initialize (ttd <translator-type-doc>) initargs)
  (next-method)
  (set! (node-children ttd)
        (map (lambda (translator)
               (make <translator-doc>
                 #:translator translator
                 #:translator-type-doc ttd))
             (translators ttd))))


;;; Output objects, e.g. grobs

(define-class <output-object-doc> (<texi-node>)
  ;; "parent" <output-object-type-doc> instance
  (type #:init-keyword #:type #:getter type)
  ;; related alist from all-grob-description etc.
  (record #:init-keyword #:object-record #:getter object-record))

(define-method (name-sym (ood <output-object-doc>))
  (car (object-record ood)))

(define-method (type-string (ood <output-object-doc>))
  (type-string (type ood)))

(define-method (initialize (ood <output-object-doc>) initargs)
  (next-method)
  (set! (node-name ood) (symbol->string (car (object-record ood)))))

(define-class <undocumented-output-object-doc> (<output-object-doc>)
  (type #:init-value #f)
  (record #:init-value #f)
  (name-sym #:init-keyword #:name-sym #:getter name-sym)
  (type-string #:init-value "undocumented output object" #:getter type-string))

(define-method (ref-ify (uoo <undocumented-output-object-doc>))
  (node-name uoo)) ; no cross-reference for undocumented objects


;;; Output object types (layout objects, audio items...)

(define-class <output-object-type-doc> (<texi-node>)
  (type-string #:init-keyword #:type-string #:getter type-string)
  (output-object-records
   #:init-keyword #:object-records #:getter object-records))

(define-method (initialize (oot <output-object-type-doc>) initargs)
  (next-method)
  (set!
   (node-children oot)
   (map (lambda (rec)
          (make <output-object-doc> #:type oot #:object-record rec))
        (object-records oot))))



;;;; Assemble documentation structure

;;; Contexts

(define all-context-types-doc
  (make <texi-node>
    #:name "Contexts"
    #:desc "Complete descriptions of all contexts."
    #:children
    (list
     (make <context-type-doc>
       #:output-def $defaultlayout
       #:type-string "layout-context"
       #:name "Layout contexts"
       #:desc "Contexts for graphical output"
       #:text "Layout contexts are built from
@ref{Engravers} to produce graphical output. Their default versions
are defined in @code{ly/engraver-init.ly}."
       ;; For backwards compatibility of links, use no suffix for layout
       ;; contexts.
       #:node-suffix "")

     (make <context-type-doc>
       #:output-def $defaultmidi
       #:type-string "MIDI context"
       #:name "MIDI contexts"
       #:desc "Contexts for MIDI output"
       #:text "MIDI contexts are built from
@ref{Performers} to produce MIDI output. Their default versions are
defined in @code{ly/performer-init.ly}"
       #:node-suffix " (MIDI)"))))


;;; Translators

(define all-translators-list
  (sort (ly:get-all-translators)
        (lambda (a b)
          (ly:symbol-ci<? (ly:translator-name a)
                          (ly:translator-name b)))))

;; Identify specific translator types (currently: engravers and
;; performers) by their names. An alternative would be to implement
;; proper type predicates.

(define (engraver? translator)
  (string-suffix-ci? "Engraver"
                     (symbol->string (ly:translator-name translator))))

(define (performer? translator)
  (string-suffix-ci? "Performer"
                     (symbol->string (ly:translator-name translator))))

(define all-engravers-list (filter engraver? all-translators-list))
(define all-performers-list (filter performer? all-translators-list))
(define all-other-translators-list
  (lset-difference eq?
                   all-translators-list
                   all-engravers-list
                   all-performers-list))

(define all-translator-types-doc
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
    (list
     (make <translator-type-doc>
       #:type-string "engraver"
       #:translators all-engravers-list
       #:name "Engravers"
       #:desc "All separate engravers"
       #:text "Engravers produce @emph{layout objects},
also known as @emph{grobs}, the building blocks for graphical
output. They are part of @ref{Layout contexts}.

See also @ruser{Modifying context plug-ins}.")

     (make <translator-type-doc>
       #:type-string "performer"
       #:translators all-performers-list
       #:name "Performers"
       #:desc "All separate performers"
       #:text "Performers produce @emph{audio items} used
for assembling MIDI output. They are part of @ref{MIDI contexts}.")

     (make <translator-type-doc>
       #:type-string "translator"
       #:translators all-other-translators-list
       #:name "Other translators"
       #:desc "Translators that are neither engravers nor performers."
       #:text ""))))


;;; Properties

;; This could be expanded...

(define (translation-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
         (sortedsyms (map string->symbol ps))
         (propdescs
          (map
           (lambda (x) (property->texi 'translation  x '()))
           sortedsyms))
         (texi (description-list->texi propdescs #f)))
    texi))

(define all-user-props-doc
  (make <texi-node>
    #:name "Tunable context properties"
    #:desc "All tunable context properties."
    #:text (translation-properties-doc-string
            all-user-translation-properties)))

(define all-internal-props-doc
  (make <texi-node>
    #:name "Internal context properties"
    #:desc "All internal context properties."
    #:text (translation-properties-doc-string
            all-internal-translation-properties)))


;;; Translation top level

(define (translation-doc-node)
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout or audio."
    #:children
    (list
     all-context-types-doc
     all-translator-types-doc
     all-user-props-doc
     all-internal-props-doc)))


;;; Output objects

;; Currently not really documented here, but in document-backend.scm.
;; Use these structures to assemble sentences like "This [translator]
;; creates the following [layout objects/audio items]", and to help
;; with cross-referencing.

(define documented-output-object-type-docs
  (list
   (make <output-object-type-doc>
     #:type-string "layout object"
     #:name "All layout objects"
     #:object-records all-grob-descriptions)

   ;; Audio item descriptions don't exist yet.
   ;;(make <output-object-type>
   ;;  #:type-string "audio item"
   ;;  #:name "All audio items"
   ;;  #:object-records all-audio-item-descriptions)
   ))



;;;; Collect information for cross-referencing

;; Mainly, we need to be able to look up <*-doc> instances by name
;; symbols, because that is how related entities are referenced in the
;; output of ly:output-description, ly:translator-description etc.


;;; Contexts

(define all-context-docs-list
  (apply append
         (map node-children (node-children all-context-types-doc))))

(define* (name-sym->context-docs
          nsym
          #:optional (from-cds all-context-docs-list))
  (filter (lambda (cd) (eq? nsym (name-sym cd))) from-cds))

(define-method (name-sym->context-doc ns (ctd <context-type-doc>))
  (let ((candidates
         (name-sym->context-docs ns (node-children ctd))))
    (if (null? candidates)
        #f
        (car candidates))))


;;; Translators

(define all-translator-docs-list
  (apply append
         (map node-children (node-children all-translator-types-doc))))

(define name-sym->translator-doc-table (make-hash-table 120))
(map (lambda (td)
       (hash-set! name-sym->translator-doc-table (name-sym td) td))
     all-translator-docs-list)

(define (name-sym->translator-doc name-sym)
  (hash-ref name-sym->translator-doc-table name-sym #f))


;;; Output objects

(define all-output-object-docs-list
  (apply append
         (map node-children documented-output-object-type-docs)))

(define name-sym->output-object-doc-table (make-hash-table 120))
(map (lambda (ood)
       (hash-set! name-sym->output-object-doc-table (name-sym ood) ood))
     all-output-object-docs-list)

(define (name-sym->output-object-doc name-sym)
  (or (hash-ref name-sym->output-object-doc-table name-sym #f)
      (make <undocumented-output-object> #:name-sym name-sym)))



;;;; Assemble documentation node texts

;;; Context - context relation

(define-method (corresponding-context-strings (this-cd <context-doc>))
  "Return 'Corresponding [layout context/MIDI context]: ...' string list."
  (let* ((all-with-name-sym (name-sym->context-docs (name-sym this-cd)))
         (all-relevant
          (filter (lambda (cd)
                    (not (equal? (context-type-doc this-cd)
                                 (context-type-doc cd))))
                  all-with-name-sym)))
    (map (lambda (cd)
           (format "Corresponding ~a: ~a.\n\n"
                   (type-string cd)
                   (ref-ify cd)))
         all-relevant)))

(define-method (aliases-string (cd <context-doc>))
  (let* ((name-syms (sort (attr 'aliases cd) ly:symbol-ci<?))
         ;; With 'Timing', there exists at least one "context" that
         ;; appears as an alias, but is not a true context documented
         ;; in the output description. That also means there is no
         ;; <context-doc> instance to cross-reference.
         (strings
          (map (lambda (ns)
                 (let ((cd (name-sym->context-doc ns (context-type-doc cd))))
                   (if cd
                       (ref-ify cd)
                       (format "~a (alias only)" (symbol->string ns)))))
               name-syms)))
    (if (null? strings)
        ""
        (format
         "This context also accepts commands for the following context(s): ~a.\n\n"
         (human-listify strings)))))

(define-method (accepts-string (cd <context-doc>))
  (let* ((name-syms (sort (attr 'accepts cd) ly:symbol-ci<?))
         (strings
          (map (lambda (ns)
                 (ref-ify (name-sym->context-doc ns (context-type-doc cd))))
               name-syms)))
    (if (null? strings)
        "This context is a `bottom' context; it cannot contain other contexts."
        (format "Context @code{~a} can contain\n~a.\n\n"
                (symbol->string (name-sym cd))
                (human-listify strings)))))


;;; Output object creation (general)

(define (group-by-function-result func lst)
  "Partition LST into sublists of elements that yield the same value
under FUNC, with that value prepended to each sublist."
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

(define (output-objects-creation-strings creator ood-list)
  "Assemble a list of strings
'This CREATOR creates the following [output object](s): ...',
one for each output object type present in OOD-LIST."
  (if (null? ood-list)
      (list
       (format "This ~a does not create any output objects.\n\n" creator))
      (let* ((oods-by-type (group-by-function-result type-string
                                                     ood-list)))
        (map (lambda (ood-type-entry)
               (format "This ~a creates the following ~a(s): ~a\n\n"
                       creator
                       (car ood-type-entry)
                       (human-listify (map ref-ify (cdr ood-type-entry)))))
             oods-by-type))))


;;; Translator - output object relation

(define-method (creations (td <translator-doc>))
  "Return a list of <output-object-doc> instances."
  ;; For historical reasons, the relevant key in the translator
  ;; description is named grobs-created, no matter whether the
  ;; translator actually is an engraver or something else.
  (map name-sym->output-object-doc
       (delete-duplicates
        (sort (attr 'grobs-created td) ly:symbol-ci<?))))

(define-method (creators (ood <output-object-doc>))
  "Return a list of <translator-doc> instances."
  (filter (lambda (td) (member ood (creations td)))
          all-translator-docs-list))

(define-method (creations-string (td <translator-doc>))
  "Assemble 'This [translator] creates the following [object](s): ...' lines."
  (string-join
   (output-objects-creation-strings (type-string td) (creations td))))


;;; Translator - music expression relation

(define-method (accepts? music-types (td <translator-doc>))
  (lset<= music-types (attr 'events-accepted td)))

;; variant currently needed for music-doc-str in document-backend.scm
;; and music-type-doc in document-music.scm
(define (translator-accepts? music-types trans)
  (accepts? music-types
            (name-sym->translator-doc (ly:translator-name trans))))

(define-method (accepts-string (td <translator-doc>))
  (let ((accepted (attr 'events-accepted td)))
    (if (null? accepted)
        ""
        (format "Music types accepted: ~a\n\n"
                (human-listify
                 (map ref-ify (sort (map symbol->string accepted)
                                    ly:string-ci<?)))))))


;;; Translator - Context relation

(define-method (consists (cd <context-doc>))
  "Return a list of <translator-doc> instances."
  (map name-sym->translator-doc (sort (attr 'consists cd) ly:symbol-ci<?)))

(define-method (consisting (td <translator-doc>))
  "Return a list of <context-doc> instances."
  (filter (lambda (cd) (member td (consists cd)))
          all-context-docs-list))

(define-method (consists-string (cd <context-doc>))
  (let ((tds (consists cd)))
    (if (null? tds)
        ""
        (format
         "This context is built from the following translator(s):\n\n~a\n\n"
         (description-list->texi (map translator-doc-embedded tds)
                                 #t)))))

(define-method (consisting-string (td <translator-doc>))
  (let* ((context-docs (consisting td)))
    (if (null? context-docs)
        (format "@code{~a} is not part of any context.\n\n"
                (node-name td))
        (format "@code{~a} is part of the following context(s):\n\n~a\n\n"
                (node-name td)
                (human-listify (map ref-ify context-docs))))))


;;; Translator - property relation

(define (format-properties-list props)
  (let ((description-list (map (lambda (x)
                                 (property->texi 'translation x '()))
                               (sort props ly:symbol-ci<?))))
    (if (null? props)
        ""
        (description-list->texi description-list #t))))

(define-method (read-properties-string (td <translator-doc>))
  (let* ((props (attr 'properties-read td '())))
    (if (null? props)
        ""
        (format "Properties (read):\n~a\n\n"
                (format-properties-list props)))))

(define-method (written-properties-string (td <translator-doc>))
  (let* ((props (attr 'properties-written td '())))
    (if (null? props)
        ""
        (format "Properties (written):\n~a\n\n" 
                (format-properties-list props)))))


;;; Context - output object relation

(define-method (creations (cd <context-doc>))
  "Return a list of <output-object-doc> instances."
  (delete-duplicates
   (sort (apply append (map creations (consists cd)))
         (lambda (a b) (ly:string-ci<? (node-name a) (node-name b))))))

(define-method (creations-string (cd <context-doc>))
  "Assemble 'This context creates the following [object](s): ...' lines."
  (string-join
   (output-objects-creation-strings "context" (creations cd))))


;;; Context - property relation

(define-method (property-operation-string prop-op (ctd <context-type-doc>))
  (let* ((tag (car prop-op))
         (sym (cadr prop-op))  ; meaning of sym depends on tag
         (args (cddr prop-op)))
    (cond
     ((equal? tag 'push)       ; sym is name symbol for related grob
      (let ((oo-name-sym (cadr prop-op))
            (value (car args))
            (path (cdr args)))
        (format "@item Set grob-property @code{~a} in ~a to ~a.\n"
                (string-join (map symbol->string path) " ")
                (ref-ify (name-sym->output-object-doc oo-name-sym))
                (scm->texi value))))
     ((equal? (object-property (cadr prop-op) 'is-grob?) #t) "")
     ((equal? tag 'assign)
      (let ((prop-name-sym (cadr prop-op)))
        (format #f "@item Set translator property @code{~a} to ~a.\n"
                (symbol->string prop-name-sym)
                (scm->texi (car args))))))))

(define-method (properties-set-string (cd <context-doc>))
  (let* ((ctd (context-type-doc cd))
         (prop-ops (attr 'property-ops cd '()))
         (prop-op-items (map (lambda (prop-op)
                               (property-operation-string prop-op ctd))
                             prop-ops))
         (prop-ops-strings (sort prop-op-items ly:string-ci<?)))
    (if (null? prop-ops-strings)
        ""
        (format
         (string-append "This context sets the following properties:\n\n"
                        "@itemize @bullet\n~a@end itemize\n\n")
         (string-join prop-ops-strings)))))


;;; Assemble single translator documentation

(define-method (short-doc-string (td <translator-doc>))
  (string-append
   (string-or (attr 'description td) "(not documented)")
   "\n\n"
   (accepts-string td)             ; "Music types accepted: ..."
   (read-properties-string td)     ; "Properties (read) ..."
   (written-properties-string td)  ; " Properties (written) ..."
   ;; "This (translator) creates the following (object)(s): ..."
   (creations-string td)))

(define-method (doc-string (td <translator-doc>))
  (string-append
   (short-doc-string td)
   ;; "(translator) is part of the following context(s): ..."
   (consisting-string td)))

(define-method (translator-doc-embedded (td <translator-doc>))
  "Shortened translator description for embedding into context description."
  (cons (format "@code{~a}" (ref-ify td))
        (short-doc-string td)))


;;; Assemble single context documentation

(define-method (doc-string (cd <context-doc>))
  (string-append
   (string-or (attr 'description cd) "(not documented)")
   "\n\n"
   ;; "This context also accepts commands for the following context(s): ..."
   (aliases-string cd)
   ;; "Corresponding [layout/midi] context: ..."
   (string-join (corresponding-context-strings cd))
   ;; "This context creates the following [output object](s): ..."
   (creations-string cd)
   ;; "This context sets the following properties: ..."
   (properties-set-string cd)
   ;; "Context CD can contain ..."
   (accepts-string cd)
   ;; "This context is built from the following translators: ..."
   (consists-string cd)))


;;; Create documentation node texts

(map (lambda (x) (set! (node-text x) (doc-string x)))
     (append all-context-docs-list
             all-translator-docs-list))