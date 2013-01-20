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
;; * Scheme object-properties associated with (LilyPond) property name
;;   symbols

;;;; Class infrastructure

;;; Contexts

(define-class <context-doc> (<texi-node>)
  ;; "parent" <context-type-doc> instance
  (context-type-doc
   #:init-keyword #:context-type-doc
   #:getter context-type-doc)
  ;; context record in the form (name-sym . info-alist) as returned as
  ;; part of (ly:output-description OUTPUT-DEF). info-alist contains
  ;; most of the interesting data; use the 'attr' method defined below
  ;; to access it.
  (context-desc #:init-keyword #:context-desc))

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

;; With 'Timing', there exists at least one 'context' that appears as
;; an alias, but is not a true context documented in the output
;; description. In such cases, create placeholder documentation
;; objects.

(define-class <undocumented-context-doc> (<context-doc>)
  (context-desc #:init-value #f)
  (node-suffix #:init-value "" #:init-keyword #:node-suffix #:getter node-suffix)
  (name-sym #:init-keyword #:name-sym #:getter name-sym))

(define-method (initialize (ucd <undocumented-context-doc>) initargs)
  (next-method)
  (set! (node-name ucd) (string-append (node-name ucd) (node-suffix ucd))))

(define-method (node-ref (ucd <undocumented-context-doc>))
  (node-name ucd)) ; no cross-reference


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
              #:context-desc context-desc
              #:numbered #f))
          context-descs))))


;;; Translators

;; Besides their standalone texinfo nodes, we also embed part of the
;; documentation of translators into context documentation nodes as
;; part of a table. Therefore make <translator-doc> support both the
;; <texi-node> and the <texi-item> interface.

(define-class <translator-doc> (<texi-node> <texi-item>)
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

(define-method (item-key (td <translator-doc>))
  (format "@code{~a}" (node-ref td)))


;;; Translator types (engravers, performers, ...)
;;
;; In contrast to context types, this classification is purely
;; cosmetic, since all translators have unique names. Specifically,
;; associating a translator with contexts of the right type does not
;; depend on it.

(define-class <translator-type-doc> (<texi-node>)
  (type-string #:init-keyword #:type-string #:getter type-string)
  (translators #:init-keyword #:translators #:getter translators))

(define-method (initialize (ttd <translator-type-doc>) initargs)
  (next-method)
  (set! (node-children ttd)
        (map (lambda (translator)
               (make <translator-doc>
                 #:translator translator
                 #:translator-type-doc ttd
                 #:numbered #f))
             (translators ttd))))

(define-method (type-string (lst <list>) (fallback <string>))
  (let ((types (delete-duplicates (map type-string lst))))
    (if (equal? (length types) 1) (car types) fallback)))


;;; Music classes

(define-class <music-class-doc> (<texi-node>)
  (name-sym #:init-keyword #:name-sym #:getter name-sym))

(define-method (initialize (mcd <music-class-doc>) initargs)
  (next-method)
  (set! (node-name mcd) (symbol->string (name-sym mcd))))


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
  (set! (node-name ood) (symbol->string (name-sym ood))))

(define-class <undocumented-output-object-doc> (<output-object-doc>)
  (type #:init-value #f)
  (record #:init-value #f)
  (name-sym #:init-keyword #:name-sym #:getter name-sym)
  (type-string #:init-value "undocumented output object" #:getter type-string))

(define-method (node-ref (uoo <undocumented-output-object-doc>))
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


;;; Properties

;; We can retrieve a description text and a type predicate for each
;; property from the following guile object-properties stored for the
;; corresponding name symbol (see define-*-properties.scm):
;;
;; - for music properties: music-doc, music-type? (->document-music.scm)
;; - for context properties: translation-doc, translation-type?
;; - for backend (grob) properties: backend-doc, backend-type?

(define-class <property-doc> (<texi-item>)
  (name-sym #:init-keyword #:name-sym #:getter name-sym)
  (doc-object-property)
  (type-object-property))

(define-class <context-property-doc> (<property-doc>)
  (doc-object-property #:init-value 'translation-doc)
  (type-object-property #:init-value 'translation-type?))

(define-class <backend-property-doc> (<property-doc>)
  (doc-object-property #:init-value 'backend-doc)
  (type-object-property #:init-value 'backend-type?))


(define-method (item-name (pd <property-doc>))
  (format #f "@code{~a}" (symbol->string (name-sym pd))))

(define-method (type-string (pd <property-doc>))
  (let* ((type-op-key (slot-ref pd 'type-object-property))
         (type (object-property (name-sym pd) type-op-key)))
    (if (eq? type #f)
        (ly:error (_ "cannot determine type ('~S) for property `~S'")
                  type-op-key
                  (name-sym pd)))
    (type-name type)))

(define-method (description (pd <property-doc>))
  (let* ((doc-op-key (slot-ref pd 'doc-object-property))
         (desc (object-property (name-sym pd) doc-op-key)))
    (if (eq? desc #f)
        (ly:error (_ "cannot find description ('~S) for property `~S'")
                  doc-op-key
                  (name-sym pd)))
    desc))

;; some property name symbols are marked as internal with a guile
;; object property

(define-method (internal? (cpd <context-property-doc>))
  (object-property (name-sym cpd) 'internal-translation))

(define-method (internal? (bpd <backend-property-doc>))
  (object-property (name-sym bpd) 'backend-internal))

;; detailed documentation item for the main property tables

(define-method (item-key (pd <property-doc>))
  (format #f "~a (~a)" (item-name pd) (type-string pd)))

(define-method (item-text (pd <property-doc>))
  ;; overridden with more detailed, cross-referencing information
  ;; for specific types of properties later
  (description pd))

;; short documentation items for embedding elsewhere

(define-method (property-item-short (pd <property-doc>))
  (make <texi-item>
    #:key (item-key pd)
    #:text (description pd)))

(define-method (property-value-item (pd <property-doc>) value)
  (make <texi-item>
    #:key (string-append
           (item-key pd)
           (format "\n\n~a\n\n" (scm->texi value)))
    #:text (description pd)))

(define (short-prop-table property-doc-list)
  (make <texi-table>
    #:items (map property-item-short property-doc-list)))

(define (short-prop-table-string title property-doc-list)
  (if (null? property-doc-list)
      ""
      (format
       "~a\n~a\n\n"
       title
       (texi-quoted-table-string (short-prop-table property-doc-list)))))

(define (short-prop-value-table propdoc-val-list)
  "From a list of pairs (PROPERTY-DOC . VAL), create a texinfo table."
  (make <texi-table>
    #:items (map (lambda (pd-v)
                   (property-value-item (car pd-v) (cdr pd-v)))
                 propdoc-val-list)))

(define (short-prop-value-table-string title propdoc-val-list)
  (if (null? propdoc-val-list)
      ""
      (format
       "~a\n~a\n\n"
       title
       (texi-quoted-table-string
        (short-prop-value-table propdoc-val-list)))))


;;; Property types

(define-class <property-type-doc> (<texi-node> <texi-table>))

(define-method (node-text (ptd <property-type-doc>))
  (texi-table-string ptd))

(define-method (node-text-short (ptd <property-type-doc>))
  (texi-table-string (short-prop-table (table-items ptd))))


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
       #:type-string "layout context"
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
    #:text "Translators plug into a stream of music event objects
delivered by iterators. They selectively @emph{accept} (or
@emph{listen to}) some of them based on their assigned @ref{Music
classes}, process them and create various types of output objects.

They may also @emph{acknowledge} and modify output objects produced by
other translators.

Translators are part of contexts. @emph{Context properties} can
control their behaviour."
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

(define all-user-context-props-doc
  (make <property-type-doc>
    #:name "Tunable context properties"
    #:desc "All tunable context properties."
    #:items
    (map (lambda (sym) (make <context-property-doc> #:name-sym sym))
         (sort all-user-translation-properties ly:symbol-ci<?))))

(define all-internal-context-props-doc
  (make <property-type-doc>
    #:name "Internal context properties"
    #:desc "All internal context properties."
    #:items
    (map (lambda (sym) (make <context-property-doc> #:name-sym sym))
         (sort all-internal-translation-properties ly:symbol-ci<?))))


;;; Translation top level

(define translation-doc-node
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout or audio."
    #:children
    (list
     all-context-types-doc
     all-translator-types-doc
     all-user-context-props-doc
     all-internal-context-props-doc)))


;;; Music classes

;; Music types form a class hierarchy tree.
;;
;; Each music event EVENT is specifically and immediately of a type
;; (ly:camel-case->lisp-identifier EVENT); these are the leaves of the
;; tree.
;;
;; The position of any given music class CLASS within the tree can be
;; determined with (ly:make-event-class GLOBAL-CONTEXT CLASS), which
;; returns a list of class name symbols starting with CLASS, followed
;; by its ancestors up to StreamEvent.
;;
;; No ready-made authoritative list of all music classes seems to
;; exist, so we reconstruct this tree from the leaves, which we obtain
;; from the list of music events.

;; It seems that the structure of the class hierarchy could in
;; principle vary depending on the output definition. I don't think
;; this is currently used, so it should not matter whether we choose
;; $defaultlayout or $defaultmidi here:

(define doc-context (ly:make-global-context $defaultlayout))

(define all-music-classes-list
  (delete-duplicates
   (sort
    (apply
     append
     (map (lambda (m-ex-rec)
            (let* ((event-ns (car m-ex-rec))
                   (event-type-ns
                    (ly:camel-case->lisp-identifier event-ns)))
              (ly:make-event-class doc-context event-type-ns)))
          music-descriptions))
    ly:symbol-ci<?)))

(define all-music-class-docs-list
  (map (lambda (ns) (make <music-class-doc> #:name-sym ns))
       all-music-classes-list))

(define music-classes-doc
  (make <texi-node>
    #:name "Music classes"
    #:desc "Groups of related music events."
    #:text "Music classes group related music events.

They provide the interface to the translation stage: @ref{Translators}
accept or ignore events based on the classes assigned to them.

Music classes form a hierarchy tree. Each music event (bottom-level
music expression) @var{MyMusicEvent} is specifically of a type
@var{my-music-event}; these are the leaves of the tree."
    #:children
    all-music-class-docs-list))


;;; Output objects

;; Currently not really documented here, but in document-backend.scm.
;; Use these structures to assemble sentences like "This [translator]
;; creates the following [layout objects/audio items]", and to help
;; with cross-referencing.

(define all-grobs-doc
  (make <output-object-type-doc>
    #:type-string "layout object"
    #:name "All layout objects"
    #:desc "Description and defaults for all graphical objects (grobs)."
    #:object-records all-grob-descriptions))

;; Audio element descriptions don't exist yet.
;; (define all-audio-elements-doc
;;   (make <output-object-type>
;;     #:type-string "audio element"
;;     #:name "All audio elements"
;;     #:desc "Description and default for all audio element objects."
;;     #:object-records all-audio-element-descriptions))

(define documented-output-object-type-docs
  (list
   all-grobs-doc
   ;; all-audio-elements-doc
   ))



;;;; Collect information for cross-referencing

;; Mainly, we need to be able to look up <*-doc> instances by name
;; symbols, because that is how related entities are referenced in the
;; output of ly:output-description, ly:translator-description etc.


;;; Contexts

;; Since we document contexts from different output definitions
;; separately, name symbols alone are not sufficient to identify a
;; context uniquely. To disambiguate, providing the corresponding
;; <context-type-doc> instance.

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


;;; Music classes

(define name-sym->music-class-doc-table (make-hash-table 100))
(map (lambda (mcd)
       (hash-set! name-sym->music-class-doc-table (name-sym mcd) mcd))
     (node-children music-classes-doc))

(define (name-sym->music-class-doc name-sym)
  (hash-ref name-sym->music-class-doc-table name-sym #f))


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
      (make <undocumented-output-object-doc> #:name-sym name-sym)))


;;; Properties

(define all-context-prop-items-list
  (append (table-items all-user-context-props-doc)
          (table-items all-internal-context-props-doc)))

(define name-sym->context-property-doc-table (make-hash-table 180))
(map (lambda (cpd)
       (hash-set! name-sym->context-property-doc-table (name-sym cpd) cpd))
     all-context-prop-items-list)

(define (name-sym->context-property-doc name-sym)
  (hash-ref name-sym->context-property-doc-table name-sym #f))


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
                   (node-ref cd)))
         all-relevant)))

(define-method (aliases (cd <context-doc>))
  "Return a list of <context-doc> instances.

With 'Timing', there exists at least one 'context' that appears as an
alias, but is not a true context documented in the output
description. For such cases, create <undocumented-context-doc>
instances."
  (let* ((name-syms (sort (attr 'aliases cd) ly:symbol-ci<?)))
    (map (lambda (ns)
           (or (name-sym->context-doc ns (context-type-doc cd))
               (make <undocumented-context-doc>
                 #:name-sym ns
                 #:node-suffix " (alias only)"
                 #:context-type-doc (context-type-doc cd))))
         name-syms)))

(define-method (aliases-string (cd <context-doc>))
  (let* ((strings (map node-ref (aliases cd))))
    (describe-list
     ""
     "This context also accepts commands for the following context: %LIST.\n\n"
     "This context also accepts commands for the following contexts: %LIST.\n\n"
     strings)))

(define-method (accepts (cd <context-doc>))
  (let* ((name-syms (sort (attr 'accepts cd) ly:symbol-ci<?)))
    (map (lambda (ns)
           (name-sym->context-doc ns (context-type-doc cd)))
         name-syms)))

(define-method (accepts-string (cd <context-doc>))
  (describe-list
   "This context is a `bottom' context; it cannot contain other contexts.\n\n"
   (format "Context @code{~a} can contain\n%LIST.\n\n"
           (symbol->string (name-sym cd)))
   (map node-ref (accepts cd))))


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
'This CREATOR creates the following [output object(s)]: ...',
one for each output object type present in OOD-LIST."
  (if (null? ood-list)
      (list
       (format "This ~a does not create any output objects.\n\n" creator))
      (let* ((oods-by-type (group-by-function-result type-string
                                                     ood-list)))
        (map (lambda (ood-type-entry)
               (describe-list
                ""
                (format "This ~a creates the following ~a: %LIST.\n\n"
                        creator
                        (car ood-type-entry))
                (format "This ~a creates the following ~as:\n%LIST\n\n"
                        creator
                        (car ood-type-entry))
                (map node-ref (cdr ood-type-entry))))
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
  "Assemble 'This [translator] creates the following [object(s)]: ...' lines."
  (string-join
   (output-objects-creation-strings (type-string td) (creations td))))


;;; Translator - music class relation

(define-method (accepts (td <translator-doc>))
  "Return list of <music-class-doc> instances."
  (map name-sym->music-class-doc
       (sort (attr 'events-accepted td) ly:symbol-ci<?)))

(define-method (accepting (mcd <music-class-doc>))
  "Return a list <translator-doc> instances."
  (filter (lambda (td) (member mcd (accepts td)))
          all-translator-docs-list))

(define-method (accepts-string (td <translator-doc>))
  (describe-list
   ""
   "Music classes accepted: %LIST.\n\n"
   (map node-ref (accepts td))))

(define-method (accepting-string (mcd <music-class-doc>))
  (describe-list
   "Not accepted by any translator.\n\n"
   "Accepted by %LIST.\n\n"
   (map node-ref (accepting mcd))))


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
    (string-append
     (describe-list
      ""
      (format "This context is built from the following ~a:\n\n%DESC\n\n"
              (type-string tds "translator"))
      (format "This context is built from the following ~as:\n\n%DESC\n\n"
              (type-string tds "translator"))
      tds
      "%DESC"
      (lambda (lst)
        (texi-quoted-table-string (make <texi-table> #:items lst)))))))

(define-method (consisting-string (td <translator-doc>))
  (let* ((context-docs (consisting td)))
    (describe-list
     (format "@code{~a} is not part of any context.\n\n"
             (node-name td))
     (format "@code{~a} is part of the context %LIST.\n\n"
             (node-name td))
     (format "@code{~a} is part of the following contexts:\n%LIST\n\n"
             (node-name td))
     (map node-ref context-docs))))


;;; Translator - property relation

(define-method (reads (td <translator-doc>))
  "Return a list of <context-property-doc> instances."
  (map name-sym->context-property-doc
       (attr 'properties-read td '())))

(define-method (writes (td <translator-doc>))
  "Return a list of <context-property-doc> instances."
  (map name-sym->context-property-doc
       (attr 'properties-written td '())))

(define-method (readers (cpd <context-property-doc>))
  "Return a list of <translator-doc> instances."
  (filter (lambda (td) (member cpd (reads td)))
          all-translator-docs-list))

(define-method (writers (cpd <context-property-doc>))
  "Return a list of <translator-doc> instances."
  (filter (lambda (td) (member cpd (writes td)))
          all-translator-docs-list))

(define-method (read-properties-string (td <translator-doc>))
  (short-prop-table-string "Properties (read):" (reads td)))

(define-method (written-properties-string (td <translator-doc>))
  (short-prop-table-string "Properties (written):" (writes td)))

(define-method (readers/writers-strings (cpd <context-property-doc>))
  (let* ((reading (readers cpd))
         (writing (writers cpd))
         (reading+writing (lset-intersection eq? reading writing))
         (reading-only (lset-difference eq? reading reading+writing))
         (writing-only (lset-difference eq? writing reading+writing))
         (rw-string (describe-list ""
                                   "@item Read and written by %LIST.\n\n"
                                   (map node-ref reading+writing)))
         (r-string (describe-list ""
                                  "@item Read by %LIST.\n\n"
                                  (map node-ref reading-only)))
         (w-string (describe-list ""
                                  "@item Written by %LIST.\n\n"
                                  (map node-ref writing-only))))
    (filter (lambda (s) (not (string-null? s)))
            (list rw-string r-string w-string))))


;;; Context - output object relation

(define-method (creations (cd <context-doc>))
  "Return a list of <output-object-doc> instances."
  (delete-duplicates
   (sort (apply append (map creations (consists cd)))
         (lambda (a b) (ly:string-ci<? (node-name a) (node-name b))))))

(define-method (creations-string (cd <context-doc>))
  "Assemble 'This context creates the following [object(s)]: ...' lines."
  (string-join
   (output-objects-creation-strings "context" (creations cd))))


;;; Context - property relation

(define-method (pushes (cd <context-doc>))
  "Return a list of triples (OO PROPPATH VAL), which signify that in
context CD, output objects of type OO (<output-object-doc> instance)
receive a default value VAL for the nested output object property
described by PROPPATH, a list of symbols."
  (let* ((prop-ops (attr 'property-ops cd '()))
         (push-ops (filter (lambda (op) (eq? (car op) 'push))
                           prop-ops)))
    (sort
     (map
      (lambda (op) ; op = ('property-ops OO VALUE PROP [SUBPROP...])
        (list (name-sym->output-object-doc (cadr op))
              (cdddr op)
              (caddr op)))
      push-ops)
     prop-push<?)))

(define (prop-path->string path)
  (string-join (map symbol->string path) "."))

(define (prop-push<? a b) ; for sorting (OO PROPPATH VAL) triples
  (or (ly:string-ci<? (node-name (car a)) (node-name (car b)))
      (and (equal? (car a) (car b))
           (ly:string-ci<? (prop-path->string (cadr a))
                           (prop-path->string (cadr b))))))

(define (prop-push->string pp-triple)
  "Textual description of (OO PROPPATH VAL) triples returned by
   (pushes CD)."
  (format
   #f
   "For ~a objects, set @code{~a} to ~a"
   (node-ref (car pp-triple))
   (prop-path->string (cadr pp-triple))
   (scm->texi (caddr pp-triple))))

(define-method (pushing (bpd <backend-property-doc>))
  "Return a list of pairs (CD . OO), which signify that the context
CD (<context-doc> instance) pushes a default value for the property
BPD of output objects of type OO (<output-object-doc> instance)."
  (apply
   append
   (map
    (lambda (cd)
      (let*
          ((all-pushes (pushes cd))
           (pushes-for-bpd (filter (lambda (p)
                                     (eq? (caadr p) (name-sym bpd)))
                                   all-pushes)))
        (map (lambda (p) (cons cd (car p))) pushes-for-bpd)))
    all-context-docs-list)))

(define-method (assigns (cd <context-doc>))
  "Return a list of pairs (PROP . VAL), which signify that the context
CD sets a context property PROP (<context-property-doc> instance) to a
default value VAL."
  (let* ((prop-ops (attr 'property-ops cd '()))
         (assign-ops (filter
                       (lambda (op)
                         (and
                           (not (equal?
                                  (object-property (cadr op) 'is-grob?)
                                  #t))
                           (eq? (car op) 'assign)))
                       prop-ops)))
    (map
      (lambda (op) ; op = ('assign PROP-SYM VALUE)
        (cons (name-sym->context-property-doc (cadr op)) (caddr op)))
      (sort assign-ops (lambda (a b) (ly:symbol-ci<? (cadr a)
                                                     (cadr b)))))))

(define-method (assigning (cpd <context-property-doc>))
  "Return a list of <context-doc> instances."
  (filter (lambda (cd) (member cpd (map car (assigns cd))))
          all-context-docs-list))

(define-method (assigners-string (cpd <context-property-doc>))
  (describe-list ""
                 "@item Set in context %LIST.\n"
                 "@item Set in contexts %LIST.\n"
                 (map node-ref (assigning cpd))))

(define-method (oo-properties-set-strings (cd <context-doc>))
  (map (lambda (p) (format "@item ~a\n" (prop-push->string p)))
       (pushes cd)))

(define-method (context-properties-set-strings (cd <context-doc>))
  (map (lambda (ass) ; ass = (PROP-DOC . VAL)
         (format #f "@item Set context property ~a to ~a\n"
                 (item-name (car ass))
                 (scm->texi (cdr ass))))
         (assigns cd)))

(define-method (properties-set-string (cd <context-doc>))
  (let ((all-strings (append (context-properties-set-strings cd)
                             (oo-properties-set-strings cd))))
    (if (null? all-strings)
        ""
        (format
          #f
          (string-append
            "This context sets the following properties:\n\n"
            "@itemize @bullet\n~a@end itemize\n\n")
          (apply string-append all-strings)))))


;;; Assemble single translator documentation

(define-method (item-text (td <translator-doc>))
  (string-append
   (string-or (attr 'description td) "(not documented)")
   "\n\n"
   (accepts-string td)             ; "Music classes accepted: ..."
   (read-properties-string td)     ; "Properties (read) ..."
   (written-properties-string td)  ; "Properties (written) ..."
   ;; "This [translator] creates the following [object(s)]: ..."
   (creations-string td)))

(define-method (node-text (td <translator-doc>))
  (string-append
   (next-method)
   (item-text td)
   ;; "[translator] is part of the following context(s): ..."
   (consisting-string td)))


;;; Assemble single context documentation

(define-method (node-text (cd <context-doc>))
  (string-append
   (next-method)
   (string-or (attr 'description cd) "(not documented)")
   "\n\n"
   ;; "This context also accepts commands for the following context(s): ..."
   (aliases-string cd)
   ;; "Corresponding [layout/midi] context: ..."
   (string-join (corresponding-context-strings cd))
   ;; "This context creates the following [output object(s)]: ..."
   (creations-string cd)
   ;; "This context sets the following properties: ..."
   (properties-set-string cd)
   ;; "Context CD can contain ..."
   (accepts-string cd)
   ;; "This context is built from the following translators: ..."
   (consists-string cd)))


;;; Assemble single context property documentation

(define-method (item-text (cpd <context-property-doc>))
  (let ((rw-strings (readers/writers-strings cpd))
        (as-string (assigners-string cpd)))
    (if (not (string-null? as-string))
        (append! rw-strings (list as-string)))
    (string-append
     (next-method)
     "\n"
     (if (null? rw-strings)
         ""
         (format #f "@itemize @bullet\n~a@end itemize\n\n"
                 (string-join rw-strings "\n"))))))