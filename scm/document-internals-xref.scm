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

;; Collect information for Internals Reference cross-referencing

;; We need to be able to look up <*-doc> instances by name symbols,
;; because that is how related entities are referenced in low-level
;; documentation records, like for example the output of
;; ly:output-description or ly:translator-description.

;; See document-internals.scm for an overview of the IR generation
;; process.

;;; Code:

(define-module (scm document-internals-xref)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (scm texinfo-generation)
  #:use-module (scm document-internals-docclasses)
  #:use-module (scm document-internals-nodestructure)
  #:export (name-sym->music-expression-doc
            name-sym->music-class-doc
            name-sym->music-property-doc
            name-sym->context-docs
            name-sym->context-doc
            name-sym->translator-doc
            name-sym->context-property-doc
            name-sym->output-object-doc
            name-sym->interface-doc
            name-sym->backend-property-doc))


;;; Music expressions

(define name-sym->music-expression-doc-table (make-hash-table 100))
(map (lambda (mxd)
       (hash-set! name-sym->music-expression-doc-table (name-sym mxd) mxd))
     (all-music-expression-docs))

(define (name-sym->music-expression-doc name-sym)
  (hash-ref name-sym->music-expression-doc name-sym #f))


;;; Music classes

;; Delay generation of this hash table until we need it for the first
;; time. Necessary because the same is true for the list returned by
;; (all-music-class-docs), which needs a parser object to generate.

(define name-sym->music-class-doc-table #f)

(define (fill-name-sym->music-class-doc-table!)
  (set! name-sym->music-class-doc-table (make-hash-table 100))
  (map (lambda (mcd)
         (hash-set! name-sym->music-class-doc-table (name-sym mcd) mcd))
       (all-music-class-docs)))

(define (name-sym->music-class-doc name-sym)
  (if (not name-sym->music-class-doc-table)
      (fill-name-sym->music-class-doc-table!))
  (hash-ref name-sym->music-class-doc-table name-sym #f))


;;; Music properties

(define name-sym->music-property-doc-table (make-hash-table 115))
(map (lambda (mpd)
       (hash-set! name-sym->music-property-doc-table (name-sym mpd) mpd))
     (all-music-property-docs))

(define (name-sym->music-property-doc name-sym)
  (hash-ref name-sym->music-property-doc-table name-sym #f))


;;; Contexts

;; Since we document contexts from different output definitions
;; separately, name symbols alone are not sufficient to identify a
;; context uniquely. To disambiguate, providing the corresponding
;; <context-type-doc> instance.

(define* (name-sym->context-docs
          nsym
          #:optional (from-cds (all-context-docs)))
  (filter (lambda (cd) (eq? nsym (name-sym cd))) from-cds))

(define-method (name-sym->context-doc ns (ctd <context-type-doc>))
  (let ((candidates
         (name-sym->context-docs ns (node-children ctd))))
    (if (null? candidates)
        #f
        (car candidates))))


;;; Translators

(define name-sym->translator-doc-table (make-hash-table 120))
(map (lambda (td)
       (hash-set! name-sym->translator-doc-table (name-sym td) td))
     (all-translator-docs))

(define (name-sym->translator-doc name-sym)
  (hash-ref name-sym->translator-doc-table name-sym #f))


;;; Context properties

(define name-sym->context-property-doc-table (make-hash-table 180))
(map (lambda (cpd)
       (hash-set! name-sym->context-property-doc-table (name-sym cpd) cpd))
     (all-context-property-docs))

(define (name-sym->context-property-doc name-sym)
  (hash-ref name-sym->context-property-doc-table name-sym #f))


;;; Output objects

(define name-sym->output-object-doc-table (make-hash-table 120))
(map (lambda (ood)
       (hash-set! name-sym->output-object-doc-table (name-sym ood) ood))
     (all-output-object-docs))

(define (name-sym->output-object-doc name-sym)
  (or (hash-ref name-sym->output-object-doc-table name-sym #f)
      (make <undocumented-output-object-doc> #:name-sym name-sym)))


;;; Interfaces

(define name-sym->interface-doc-table (make-hash-table 150))
(map (lambda (ifd)
       (hash-set! name-sym->interface-doc-table (name-sym ifd) ifd))
     (all-interface-docs))

(define (name-sym->interface-doc name-sym)
  (or (hash-ref name-sym->interface-doc-table
                name-sym)
      (begin
        (ly:error (_ "unknown output object interface: ~S") name-sym)
        #f)))


;;; Output object properties

(define name-sym->backend-property-doc-table (make-hash-table 180))
(map (lambda (bpd)
       (hash-set! name-sym->backend-property-doc-table (name-sym bpd) bpd))
     (all-backend-property-docs))

(define (name-sym->backend-property-doc name-sym)
  (hash-ref name-sym->backend-property-doc-table name-sym #f))
