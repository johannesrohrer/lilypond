;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2012--2013 Johannes Rohrer <src@johannesrohrer.de>
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

;; Class infrastructure for generating the Internals Reference

;; See document-internals.scm for an overview of the IR generation
;; process.
;;
;; Here, define a <texi-node> (or <texi-item>) subclass for each
;; documentation-worthy object, which encapsulates the low-level data
;; records we can use to obtain interesting information.

;;; Code:

(define-module (scm document-internals-docclasses)
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (delete-duplicates))
  #:use-module ((lily) #:select (assoc-get
                                 ly:translator-description
                                 ly:translator-name
                                 ly:output-description
                                 type-name))
  #:use-module (scm texinfo-generation)
  #:use-module (scm lily-sort)
  #:use-module (scm documentation-lib)
  #:export (<music-expression-doc>
            <music-class-doc>
            <context-doc>
            <undocumented-context-doc>
            <context-type-doc>
            <translator-doc>
            <translator-type-doc>
            <output-object-doc>
            <undocumented-output-object-doc>
            <output-object-type-doc>
            <interface-doc>
            <music-property-doc>
            <context-property-doc>
            <backend-property-doc>
            <property-type-doc>
            attr
            description
            internal?
            item-name
            low-level-record
            name-sym
            node-text-short
            short-prop-table-string
            short-prop-value-table-string
            type-doc
            type-string))


;;; IR node base class

;; Low-level documentation for various items comes in different forms.
;; A typical one is a list of the form (NAME-SYM . INFO-ALIST), like
;; the elements of music-descriptions or (ly:output-description
;; OUTPUT-DEF).
;;
;; For these cases, provide convenient access to INFO-ALIST with the
;; method attr. (For some other cases, like translators, override the
;; method as appropriate later.)
;;
;; Also allow to keep a reference to a parent <internal-type-doc>
;; instance. For example, a translator documentation node can record
;; here whether it is an engraver, a performer or another type of
;; translator.

(define-class <internal-item-doc> (<texi-node>)
  (type-doc #:init-keyword #:type-doc #:getter type-doc)
  (low-level-record #:init-keyword #:low-level-record
                    #:getter low-level-record))

(define-method (attr attribute
                     (iid <internal-item-doc>)
                     not-found-result)
  (assoc-get attribute
             (cdr (low-level-record iid))
             not-found-result))

(define-method (attr attribute obj) (attr attribute obj #f))

(define-method (type-string (iid <internal-item-doc>))
  (let ((icd (type-doc iid)))
    (if icd (type-string icd) #f)))


;;; Base class for IR node groups

;; A node with instances of a particular <internal-item-doc> subclass
;; as subnodes. Used for example for translator types (engravers,
;; performers, others) and context types.

(define-class <internal-type-doc> (<texi-node>)
  (type-string #:init-keyword #:type-string #:getter type-string)
  (subnode-class) ; init-value set in derived classes
  (low-level-records #:init-value #f
                     #:init-keyword #:low-level-records
                     #:accessor low-level-records))

(define-method (create-children! (itd <internal-type-doc>))
  (set! (node-children itd)
        (map (lambda (low-level-record)
               (make (slot-ref itd 'subnode-class)
                 #:type-doc itd
                 #:low-level-record low-level-record
                 #:numbered #f))
             (low-level-records itd))))

(define-method (initialize (itd <internal-type-doc>) initargs)
  (next-method)
  (if (low-level-records itd) (create-children! itd)))


;;; Music expressions

;; A list of music expressions is available with music-descriptions
;; (from define-music-types.scm), which contains records of the form
;;
;;   (NAMESYM (name . NAMESYM)
;;            (types TYPENAMESYM1 TYPENAMESYM2 ...)
;;            ...)
;;
;; A descriptive text for each music expression can be retrieved from
;; the guile object property 'music-description assosciated with the
;; corresponding name symbol.

(define-class <music-expression-doc> (<internal-item-doc>))

(define-method (name-sym (mxd <music-expression-doc>))
  (car (low-level-record mxd)))

(define-method (description (mxd <music-expression-doc>))
  (object-property (name-sym mxd) 'music-description))

(define-method (initialize (mxd <music-expression-doc>) initargs)
  (next-method)
  (set! (node-name mxd) (symbol->string (name-sym mxd))))


;;; Music classes (event classes)

;; Event classes form a hierarchical tree. (ly:make-event-class
;; GLOBAL-CONTEXT CLASS) can be used to determine the position of any
;; given CLASS (name symbol) within this hierarchy, so make sure we
;; have a GLOBAL-CONTEXT object available. Otherwise, we do not
;; currently have much information on them a priori.

(define-class <music-class-doc> (<internal-item-doc>)
  (doc-context #:init-keyword #:doc-context)
  (name-sym #:init-keyword #:name-sym #:getter name-sym))

(define-method (initialize (mcd <music-class-doc>) initargs)
  (next-method)
  (set! (node-name mcd) (symbol->string (name-sym mcd))))


;;; Contexts

;; (ly:output-description OUTPUT-DEF) returns a list of description
;; records of the form (name-sym . info-alist) for each context in a
;; given output definition ($default-layout,
;; $default-midi). Encapsulate these as low-level-records.

(define-class <context-doc> (<internal-item-doc>))

(define-class <context-type-doc> (<internal-type-doc>)
  (subnode-class #:init-value <context-doc>)
  (output-def #:init-keyword #:output-def)
  ;; The node-suffix is appended to the texinfo node name of each
  ;; context that belongs to this output definition.
  (node-suffix #:init-keyword #:node-suffix #:getter node-suffix))

(define-method (initialize (ctd <context-type-doc>) initargs)
  (next-method)
  (set! (low-level-records ctd)
        (sort (ly:output-description (slot-ref ctd 'output-def))
              (lambda (x y) (ly:symbol-ci<? (car x) (car y)))))
  (create-children! ctd))

(define-method (name-sym (cd <context-doc>))
  (car (low-level-record cd)))

(define-method (initialize (cd <context-doc>) initargs)
  (next-method)
  (set!
   (node-name cd)
   (string-append (symbol->string (name-sym cd))
                  (node-suffix (type-doc cd)))))

;; With 'Timing', there exists at least one 'context' that appears as
;; an alias, but is not a true context documented in the output
;; description. Create placeholder documentation objects for such
;; cases to avoid case distinctions when cross-referencing. These
;; "nodes" do not actually show up in the documentation.

(define-class <undocumented-context-doc> (<context-doc>)
  (node-suffix #:init-value ""
               #:init-keyword #:node-suffix
               #:getter node-suffix)
  (name-sym #:init-keyword #:name-sym #:getter name-sym))

(define-method (initialize (ucd <undocumented-context-doc>) initargs)
  (next-method)
  (set! (node-name ucd)
        (string-append (node-name ucd) (node-suffix ucd))))

(define-method (node-ref (ucd <undocumented-context-doc>))
  (node-name ucd)) ; no cross-reference


;;; Translators

;; Besides standalone texinfo nodes, translator documentation also
;; gets embedded into context documentation nodes as part of a
;; table. Therefore inherit both the <texi-node> interface (through
;; <internal-item-doc>) and the <texi-item> interface.

(define-class <translator-doc> (<internal-item-doc> <texi-item>))

;; (ly:get-all-translators) provides a list of all translators as
;; <translator> objects. Use these for the low-level-record slots.

;; Since all translators have unique names, classifying them as
;; engravers, performers etc. is sort-of cosmetic. Specifically,
;; associating a translator with contexts of the right type does not
;; depend on it.

(define-class <translator-type-doc> (<internal-type-doc>)
  (subnode-class #:init-value <translator-doc>))

;; The following variant of the type-string method is useful for lists
;; of <translator-doc> instances of potentially mixed translator type.

(define-method (type-string (lst <list>) (fallback <string>))
  (let ((types (delete-duplicates (map type-string lst))))
    (if (equal? (length types) 1) (car types) fallback)))

;; (ly:translator-description TRANSLATOR) retrieves a documentation
;; alist for a <translator> object (our low-level-record), with keys
;; 'description, 'properties-written, 'properties-read,
;; 'events-accepted and 'grobs-created. Override the attr method to
;; provide convenient access to these.

(define-method (attr attribute (td <translator-doc>) not-found-result)
  (assoc-get attribute
             (ly:translator-description (low-level-record td))
             not-found-result))

(define-method (name-sym (td <translator-doc>))
  (ly:translator-name (low-level-record td)))

(define-method (initialize (td <translator-doc>) initargs)
  (next-method)
  (set! (node-name td) (symbol->string (name-sym td))))

(define-method (item-key (td <translator-doc>))
  (format "@code{~a}" (node-ref td)))


;;; Output objects, e.g. grobs

;; [In fact, grobs are currently the only output objects accessible
;; from Scheme. Assume that when this changes (audio elements!), the
;; new ones can be treated analogously.]

;; Low-level documentation for grobs is available from the list
;; all-grob-descriptions, which contains records of the form
;;
;;   (NAME-SYM . PROPERTY-ALIST).

(define-class <output-object-doc> (<internal-item-doc>))

(define-class <output-object-type-doc> (<internal-type-doc>)
  (subnode-class #:init-value <output-object-doc>))

(define-method (name-sym (ood <output-object-doc>))
  (car (low-level-record ood)))

(define-method (initialize (ood <output-object-doc>) initargs)
  (next-method)
  (set! (node-name ood) (symbol->string (name-sym ood))))

(define-class <undocumented-output-object-doc> (<output-object-doc>)
  (name-sym #:init-keyword #:name-sym #:getter name-sym)
  (type-string #:init-value "undocumented output object"
               #:getter type-string))

(define-method (node-ref (uoo <undocumented-output-object-doc>))
  (node-name uoo)) ; no cross-reference for undocumented objects


;;; Interfaces

;; Low-level interface documentation is available from the hash table
;; (ly:all-grob-interfaces). It is indexed with name symbols, and each
;; entry has the form (NAME-SYM DOCSTRING PROPERTY-NAME-SYM-LIST).

(define-class <interface-doc> (<internal-item-doc>))

(define-method (name-sym (ifd <interface-doc>))
  (car (low-level-record ifd)))

(define-method (description (ifd <interface-doc>))
  (cadr (low-level-record ifd)))

(define-method (initialize (ifd <interface-doc>) initargs)
  (next-method)
  (set! (node-name ifd) (symbol->string (name-sym ifd))))


;;; Properties

;; Available property name symbol lists [define-*-properties.scm]:
;;
;; * all-music-properties
;; * all-user-translation-properties and
;;   all-internal-translation-properties
;; * all-user-grob-properties and all-internal-grob-properties

(define-class <property-type-doc> (<texi-node> <texi-table>))

(define-method (node-text (ptd <property-type-doc>))
  (texi-table-string ptd))

(define-method (node-text-short (ptd <property-type-doc>))
  (texi-table-string (short-prop-table (table-items ptd))))

;; We can retrieve a description text and a type predicate for each
;; property from the following guile object-properties stored for the
;; corresponding name symbol [define-*-properties.scm]:
;;
;; - for music properties: music-doc, music-type?
;; - for context properties: translation-doc, translation-type?
;; - for backend (grob) properties: backend-doc, backend-type?

(define-class <property-doc> (<texi-item>)
  (name-sym #:init-keyword #:name-sym #:getter name-sym)
  (doc-object-property)
  (type-object-property))

(define-class <music-property-doc> (<property-doc>)
  (doc-object-property #:init-value 'music-doc)
  (type-object-property #:init-value 'music-type?))

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
