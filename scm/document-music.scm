;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

;;;; Additional class infrastructure

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

(define-class <music-expression-doc> (<texi-node>)
  ;; related entry from music-descriptions
  (record #:init-keyword #:record #:getter music-expression-record))

(define-method (name-sym (mxd <music-expression-doc>))
  (car (music-expression-record mxd)))

(define-method (description (mxd <music-expression-doc>))
  (object-property (name-sym mxd) 'music-description))

(define-method (initialize (mxd <music-expression-doc>) initargs)
  (next-method)
  (set! (node-name mxd) (symbol->string (name-sym mxd))))


;;; Music classes

;; see document-translation.scm


;;; Music properties

;; A list of all music properties is available with
;; all-music-properties (from define-music-properties.scm)

(define-class <music-property-doc> (<property-doc>)
  (doc-object-property #:init-value 'music-doc)
  (type-object-property #:init-value 'music-type?))


;;;; Assemble documentation structure

;;; Music expressions

(define all-music-expression-docs-list
  (map (lambda (rec) (make <music-expression-doc> #:record rec))
       music-descriptions))

(define music-expressions-doc
  (make <texi-node>
    #:name "Music expressions"
    #:desc "Objects that represent music."
    #:children
    all-music-expression-docs-list))


;;; Music classes

;; see document-translation.scm


;;; Music properties

(define all-music-props-doc
  (make <property-type-doc>
    #:name "Music properties"
    #:desc "All music properties, including descriptions."
    #:items
    (map (lambda (sym) (make <music-property-doc> #:name-sym sym))
         (sort all-music-properties ly:symbol-ci<?))))


;;; Music definitions top level

(define music-doc-node
  (make <texi-node>
    #:name "Music definitions"
    #:desc "Definition of the input data structures."
    #:children
    (list
     music-expressions-doc
     music-classes-doc
     all-music-props-doc)))


;;;; Collect information for cross-referencing

;;; Music expressions

(define name-sym->music-expression-doc-table (make-hash-table 100))
(map (lambda (mxd)
       (hash-set! name-sym->music-expression-doc-table (name-sym mxd) mxd))
     (node-children music-expressions-doc))

(define (name-sym->music-expression-doc name-sym)
  (hash-ref name-sym->music-expression-doc name-sym #f))


;;; Music classes

;; see document-translation.scm


;;; Music properties

(define name-sym->music-property-doc-table (make-hash-table 115))
(map (lambda (mpd)
       (hash-set! name-sym->music-property-doc-table (name-sym mpd) mpd))
     (table-items all-music-props-doc))

(define (name-sym->music-property-doc name-sym)
  (hash-ref name-sym->music-property-doc-table name-sym #f))


;;;; Assemble documentation node texts

;;; Music class - music class relation

(define-method (ancestors (mcd <music-class-doc>))
  "Return a hierarchy-sorted list of <music-class-doc> instances,
starting with the most specific superclass of MCD."
  (let ((ancestry-name-syms
         (cdr (ly:make-event-class doc-context (name-sym mcd)))))
    (map name-sym->music-class-doc ancestry-name-syms)))

(define-method (superclass (mcd <music-class-doc>))
  "Return a <music-class-doc> instance corresponding to the
immediate superclass of MCD, or #f if there is none."
  (let ((anc (ancestors mcd)))
    (if (null? anc)
        #f
        (car anc))))

(define-method (subclasses (mcd <music-class-doc>))
  "Return a list of <music-class-doc> instances corresponding to
immediate subclasses of MCD."
  (filter (lambda (cand) (equal? mcd (superclass cand)))
          all-music-class-docs-list))

(define-method (hierarchy-string (mcd <music-class-doc>))
  (string-append
   (string-join (map node-ref (reverse (ancestors mcd)))
                "@tie{}> "
                'suffix)
   (format #f "@strong{~S}\n\n" (name-sym mcd))
   (describe-list ""
                  "Immediate subclass: %LIST.\n\n"
                  "Immediate subclasses: %LIST.\n\n"
                  (map node-ref (subclasses mcd)))))


;;; Music expression - music class relation

(define-method (immediate-event-type (mxd <music-expression-doc>))
  "Return a <music-class-doc> instance, or #f if MXD is not an event."
  (name-sym->music-class-doc
   (ly:camel-case->lisp-identifier (name-sym mxd))))

(define-method (comprising (mxd <music-expression-doc>))
  "Return a list of <music-class-doc> instances."
  (let ((ev-type (immediate-event-type mxd)))
    (if ev-type
        (cons ev-type (ancestors ev-type))
        '())))

(define-method (comprises (mcd <music-class-doc>))
  "Return a list of <music-expression-doc> instances."
  (filter (lambda (mxd) (member mcd (comprising mxd)))
          all-music-expression-docs-list))

(define-method (comprising-string (mxd <music-expression-doc>))
  (let* ((classes (comprising mxd)))
    (if (null? classes)
        (format
         #f "@code{~a} is not an event (bottom-level music expression).\n\n"
         (node-name mxd))
        (format #f "Corresponding music event type: ~a.\n\n"
                (string-join (map node-ref (reverse classes))
                             "@tie{}> ")))))

(define-method (comprises-string (mcd <music-class-doc>))
  (describe-list
   "This event class is empty.\n\n" ; should not happen?
   "This event class specifically comprises %LIST expressions.\n\n"
   "This event class comprises the music expressions %LIST.\n\n"
   (map node-ref (comprises mcd))))


;;; Music class - translator relation

;; see document-translation.scm


;;; Music expression - translator relation

(define-method (accepting (mxd <music-expression-doc>))
  (let* ((ev-type (immediate-event-type mxd))
         (classes (if ev-type (cons ev-type (ancestors ev-type)) '())))
    (delete-duplicates
     (sort (apply append (map accepting classes))
           name-sym-ci<?))))

(define-method (accepting-string (mxd <music-expression-doc>))
  (describe-list
   ""
   "These event classes are accepted by %LIST.\n\n"
   (map node-ref (accepting mxd))))


;;; Music expression - music property relation

(define-method (assignments (mxd <music-expression-doc>))
  "Return a list of pairs (PROP . VAL), where PROP is a
<music-property-doc> instance."
  (let* ((namesym-val-list
          (sort (cdr (music-expression-record mxd))
                ly:alist-ci<?)))
    (map (lambda (ns-v)
           (let ((mpd (name-sym->music-property-doc (car ns-v)))
                 (value (cdr ns-v)))
             (cons mpd value)))
         namesym-val-list)))

(define-method (assigns (mxd <music-expression-doc>))
  "Return a list of <music-property-doc> instances."
  (map car (assignments mxd)))

(define-method (assigning (mpd <music-property-doc>))
  "Return a list of <music-expression-doc> instances."
  (filter (lambda (mxd) (member mpd (assigns mxd)))
          all-music-expression-docs-list))


;;; Assemble single music expression documentation

(define-method (node-text (mxd <music-expression-doc>))
  (string-append
   (next-method)
   (description mxd)
   "\n\n"
   (comprising-string mxd)
   (accepting-string mxd)

   (short-prop-value-table-string
    (format #f "Music properties with default settings for ~a:\n\n"
            (node-name mxd))
    (assignments mxd))))


;;; Assemble single music class documentation

(define-method (node-text (mcd <music-class-doc>))
  (string-append
   (next-method)
   (hierarchy-string mcd)
   (comprises-string mcd)
   (accepting-string mcd)))


;;; Assemble single music property documentation

(define-method (item-text (mpd <music-property-doc>))
  (string-append
   (next-method)
   "\n\n"
   (describe-list
    ""
    "Default values are defined for %LIST.\n\n"
    (map node-ref (assigning mpd)))))