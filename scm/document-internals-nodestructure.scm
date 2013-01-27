;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2013 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                          Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                          Johannes Rohrer <src@johannesrohrer.de>
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

;;; Commentary:

;; Define nodes, items and global structure for the Internals Reference

;; See document-internals.scm for an overview of the IR generation
;; process.

;;; Code:

(define-module (scm document-internals-nodestructure)
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (delete-duplicates
                                        lset-difference))
  #:use-module ((lily) #:select (all-grob-descriptions
                                 all-internal-grob-properties
                                 all-music-properties
                                 all-user-grob-properties
                                 all-user-translation-properties
                                 all-internal-translation-properties
                                 ly:all-grob-interfaces
                                 ly:camel-case->lisp-identifier
                                 ly:get-all-translators
                                 ly:make-event-class
                                 ly:make-global-context
                                 ly:parser-lookup
                                 ly:programming-error
                                 ly:translator-name
                                 music-descriptions))
  #:use-module (scm lily-sort)
  #:use-module (scm texinfo-generation)
  #:use-module (scm document-internals-docclasses)
  #:duplicates (merge-generics)
  #:export (all-music-expression-docs
            all-music-class-docs
            all-music-property-docs
            all-context-docs
            all-translator-docs
            all-context-property-docs
            all-output-object-docs
            all-interface-docs
            all-backend-property-docs

            get-music-doc-node
            get-translation-doc-node
            layout-backend-doc-node))


;;; Music expressions

(define all-music-expression-docs-list
  (map (lambda (rec)
         (make <music-expression-doc> #:low-level-record rec))
       music-descriptions))

(define (all-music-expression-docs) all-music-expression-docs-list)

(define music-expressions-doc
  (make <texi-node>
    #:name "Music expressions"
    #:desc "Objects that represent music."
    #:children
    all-music-expression-docs-list))


;;; Music classes (event classes)

;; Event classes form a hierarchy tree.
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
;; Reconstruct this tree from the leaves, which we obtain from the
;; list of music events.

;; It seems that the structure of the class hierarchy could in
;; principle vary depending on the output definition. I don't think
;; this is currently used, so it should not matter whether we choose
;; $defaultlayout or $defaultmidi here:

(define (doc-context parser)
  (ly:make-global-context (ly:parser-lookup parser '$defaultlayout)))

(define (get-all-music-classes-list parser)
  (delete-duplicates
   (sort
    (apply
     append
     (map (lambda (m-ex-rec)
            (let* ((event-ns (car m-ex-rec))
                   (event-type-ns
                    (ly:camel-case->lisp-identifier event-ns)))
              (ly:make-event-class (doc-context parser) event-type-ns)))
          music-descriptions))
    ly:symbol-ci<?)))

(define all-music-class-docs-list #f)

(define (all-music-class-docs)
  (if (not all-music-class-docs-list)
      (ly:programming-error
       (string-append
        "all-music-class-docs-list not initialized. Before using "
        "(all-music-class-docs), call (music-classes-doc parser)."))
      all-music-class-docs-list))

(define (get-all-music-class-docs parser)
  (if (not all-music-class-docs-list)
      (set! all-music-class-docs-list
            (map (lambda (ns) (make <music-class-doc>
                                #:name-sym ns
                                #:doc-context (doc-context parser)))
                 (get-all-music-classes-list parser))))
  all-music-class-docs-list)

(define (get-music-classes-doc parser)
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
    (get-all-music-class-docs parser)))


;;; Music properties

(define music-props-doc
  (make <property-type-doc>
    #:type-string "music property"
    #:name "Music properties"
    #:desc "All music properties, including descriptions."
    #:subnode-class <music-property-doc>
    #:low-level-records
    (sort all-music-properties ly:symbol-ci<?)))

(define all-music-property-docs-list (node-children music-props-doc))

(define (all-music-property-docs)
  all-music-property-docs-list)


;;; Music definitions top level

(define (get-music-doc-node parser)
  (make <texi-node>
    #:name "Music definitions"
    #:desc "Definition of the input data structures."
    #:children
    (list
     music-expressions-doc
     (get-music-classes-doc parser)
     music-props-doc)))


;;; Contexts

(define (get-layout-contexts-doc parser)
  (make <context-type-doc>
    #:output-def (ly:parser-lookup parser '$defaultlayout)
    #:type-string "layout context"
    #:name "Layout contexts"
    #:desc "Contexts for graphical output"
    #:text "Layout contexts are built from
@ref{Engravers} to produce graphical output. Their default versions
are defined in @code{ly/engraver-init.ly}."
    ;; For backwards compatibility of links, use no suffix for layout
    ;; contexts.
    #:node-suffix ""))

(define (get-midi-contexts-doc parser)
  (make <context-type-doc>
    #:output-def (ly:parser-lookup parser '$defaultmidi)
    #:type-string "MIDI context"
    #:name "MIDI contexts"
    #:desc "Contexts for MIDI output"
    #:text "MIDI contexts are built from
@ref{Performers} to produce MIDI output. Their default versions are
defined in @code{ly/performer-init.ly}"
    #:node-suffix " (MIDI)"))

(define all-context-type-docs-list #f)

(define (get-context-type-docs-list parser)
  (if (not all-context-type-docs-list)
      (set!
       all-context-type-docs-list
       (list
        (get-layout-contexts-doc parser)
        (get-midi-contexts-doc parser))))
  all-context-type-docs-list)

(define (all-context-docs)
  (if (not all-context-type-docs-list)
      (ly:programming-error
       (string-append
        "all-context-type-docs-list not initialized. Before using "
        "(all-context-docs), call (context-types-doc parser)."))
      (apply append
             (map node-children
                  all-context-type-docs-list))))

(define (get-context-types-doc parser)
  (make <texi-node>
    #:name "Contexts"
    #:desc "Complete descriptions of all contexts."
    #:children
    (get-context-type-docs-list parser)))


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

(define all-translator-type-docs-list
  (list
   (make <translator-type-doc>
     #:type-string "engraver"
     #:low-level-records all-engravers-list
     #:name "Engravers"
     #:desc "All separate engravers"
     #:text "Engravers produce @emph{layout objects},
also known as @emph{grobs}, the building blocks for graphical
output. They are part of @ref{Layout contexts}.

See also @ruser{Modifying context plug-ins}.")

   (make <translator-type-doc>
     #:type-string "performer"
     #:low-level-records all-performers-list
     #:name "Performers"
     #:desc "All separate performers"
     #:text "Performers produce @emph{audio items} used
for assembling MIDI output. They are part of @ref{MIDI contexts}.")

   (make <translator-type-doc>
     #:type-string "translator"
     #:low-level-records all-other-translators-list
     #:name "Other translators"
     #:desc "Translators that are neither engravers nor performers."
     #:text "")))

(define translator-types-doc
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
    all-translator-type-docs-list))

(define (all-translator-docs)
  (apply append
         (map node-children all-translator-type-docs-list)))


;;; Context properties

(define tunable-context-properties-doc
  (make <property-type-doc>
    #:name "Tunable context properties"
    #:type-string "tunable context property"
    #:desc "All tunable context properties."
    #:subnode-class <context-property-doc>
    #:low-level-records
    (sort all-user-translation-properties ly:symbol-ci<?)))

(define tunable-context-prop-docs-list
  (node-children tunable-context-properties-doc))

(define internal-context-properties-doc
  (make <property-type-doc>
    #:name "Internal context properties"
    #:type-string "internal context property"
    #:desc "All internal context properties."
    #:subnode-class <context-property-doc>
    #:low-level-records
    (sort all-internal-translation-properties ly:symbol-ci<?)))

(define internal-context-prop-doc-list
  (node-children internal-context-properties-doc))

(define all-context-property-docs-list
  (append tunable-context-prop-docs-list
          internal-context-prop-doc-list))

(define (all-context-property-docs) all-context-property-docs-list)


;;; Translation top level

(define (get-translation-doc-node parser)
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout or audio."
    #:children
    (list
     (get-context-types-doc parser)
     translator-types-doc
     tunable-context-properties-doc
     internal-context-properties-doc)))


;;; Output objects

(define grobs-doc
  (make <output-object-type-doc>
    #:type-string "layout object"
    #:name "All layout objects"
    #:desc "Description and defaults for all graphical objects (grobs)."
    #:low-level-records all-grob-descriptions))

;; Audio element descriptions don't exist yet.
;; (define audio-elements-doc
;;   (make <output-object-type>
;;     #:type-string "audio element"
;;     #:name "All audio elements"
;;     #:desc "Description and default for all audio element objects."
;;     #:low-level-records all-audio-element-descriptions))

(define documented-output-object-type-docs
  (list
   grobs-doc
   ;; audio-elements-doc
   ))

(define all-output-object-docs-list
  (apply append
         (map node-children documented-output-object-type-docs)))

(define (all-output-object-docs) all-output-object-docs-list)


;;; Interfaces

(define all-grob-interface-records
  ;; sorted list of all values from hash table (ly:all-grob-interfaces)
  (sort
   (hash-map->list (lambda (a b) b) (ly:all-grob-interfaces))
   ly:alist-ci<?))

(define grob-interfaces-doc
  (make <texi-node>
    #:name "Graphical Object Interfaces"
    #:desc "Building blocks of graphical objects."
    #:children
    (map (lambda (rec) (make <interface-doc> #:low-level-record rec))
         all-grob-interface-records)))

(define all-interface-docs-list
  (append
   ;; only grob interfaces for now
   (node-children grob-interfaces-doc)))

(define (all-interface-docs) all-interface-docs-list)


;;; Output object properties

(define tunable-grob-properties-doc
  (make <property-type-doc>
    #:name "Tunable layout properties"
    #:type-string "tunable layout property"
    #:desc "All tunable layout properties in a big list."
    #:subnode-class <backend-property-doc>
    #:low-level-records
    (sort all-user-grob-properties ly:symbol-ci<?)))

(define internal-grob-properties-doc
  (make <property-type-doc>
    #:name "Internal layout properties"
    #:type-string "internal layout property"
    #:desc "All internal layout properties in a big list."
    #:subnode-class <backend-property-doc>
    #:low-level-records
    (sort all-internal-grob-properties ly:symbol-ci<?)))

(define all-backend-property-docs-list
  (append (node-children tunable-grob-properties-doc)
          (node-children internal-grob-properties-doc)))

(define (all-backend-property-docs) all-backend-property-docs-list)


;;; Complete layout backend

(define layout-backend-doc-node
  (make <texi-node>
    #:name "Layout backend"
    #:desc "Reference for the layout engine."
    #:children
    (list
     grobs-doc
     grob-interfaces-doc
     tunable-grob-properties-doc
     internal-grob-properties-doc)))
