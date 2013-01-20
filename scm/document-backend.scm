;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;; Jan Nieuwenhuizen <janneke@gnu.org>
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

;;; Interfaces

;; Low-level interface documentation is available from the hash table
;; (ly:all-grob-interfaces). It is indexed with name symbols, and each
;; entry has the form (NAME-SYM DOCSTRING PROPERTY-NAME-SYM-LIST).

(define-class <interface-doc> (<texi-node>)
  ;; related entry from (ly:all-grob-interfaces)
  (record #:init-keyword #:iface-record #:getter iface-record))

(define-method (name-sym (ifd <interface-doc>))
  (car (iface-record ifd)))

(define-method (description (ifd <interface-doc>))
  (cadr (iface-record ifd)))

(define-method (initialize (ifd <interface-doc>) initargs)
  (next-method)
  (set! (node-name ifd) (symbol->string (name-sym ifd))))


;;;; Assemble documentation structure

;;; Output objects

;; see document-translation.scm


;;; Interfaces

(define all-grob-interface-records
  ;; sorted list of all values from hash table (ly:all-grob-interfaces)
  (sort
   (hash-map->list (lambda (a b) b) (ly:all-grob-interfaces))
   ly:alist-ci<?))

(define all-grob-interfaces-doc
  (make <texi-node>
    #:name "Graphical Object Interfaces"
    #:desc "Building blocks of graphical objects."
    #:children
    (map (lambda (rec) (make <interface-doc> #:iface-record rec))
         all-grob-interface-records)))


;;; Output object properties

(define all-user-grob-properties-doc
  (make <property-type-doc>
    #:name "Tunable layout properties"
    #:desc "All tunable layout properties in a big list."
    #:items
    (map (lambda (sym) (make <backend-property-doc> #:name-sym sym))
         (sort all-user-grob-properties ly:symbol-ci<?))))

(define all-internal-grob-properties-doc
  (make <property-type-doc>
    #:name "Internal layout properties"
    #:desc "All internal layout properties in a big list."
    #:items
    (map (lambda (sym) (make <backend-property-doc> #:name-sym sym))
         (sort all-internal-grob-properties ly:symbol-ci<?))))


;;; Complete layout backend

(define layout-backend-doc-node
  (make <texi-node>
    #:name "Layout backend"
    #:desc "Reference for the layout engine."
    #:children
    (list
     all-grobs-doc
     all-grob-interfaces-doc
     all-user-grob-properties-doc
     all-internal-grob-properties-doc)))


;;;; Collect information for cross-referencing

;;; Interfaces

(define all-interface-docs-list
  (append
   ;; only grob interfaces for now
   (node-children all-grob-interfaces-doc)))

(define name-sym->interface-doc-table (make-hash-table 150))
(map (lambda (ifd)
       (hash-set! name-sym->interface-doc-table (name-sym ifd) ifd))
     all-interface-docs-list)

(define (name-sym->interface-doc name-sym)
  (or (hash-ref name-sym->interface-doc-table
                name-sym)
      (begin
        (ly:error (_ "unknown output object interface: ~S") name-sym)
        #f)))


;;; Output object properties

(define all-backend-prop-items-list
  (append (table-items all-user-grob-properties-doc)
          (table-items all-internal-grob-properties-doc)))

(define name-sym->backend-property-doc-table (make-hash-table 180))
(map (lambda (bpd)
       (hash-set! name-sym->backend-property-doc-table (name-sym bpd) bpd))
     all-backend-prop-items-list)

(define (name-sym->backend-property-doc name-sym)
  (hash-ref name-sym->backend-property-doc-table name-sym #f))


;;;; Assemble documentation node texts

;;; Output-object - interface relation

(define-method (implements (ood <output-object-doc>))
  "Return a list of <interface-doc> instances."
  (let ((iface-name-syms
         (sort (assoc-get 'interfaces
                          (assoc-get 'meta (cdr (object-record ood))))
          ly:symbol-ci<?)))
    (map name-sym->interface-doc iface-name-syms)))

(define-method (implementing (ifd <interface-doc>))
  "Return a list of <output-object-doc> instances."
  (filter (lambda (ood) (member ifd (implements ood)))
          all-output-object-docs-list))


;;; Backend property - interface relation

(define-method (supports (ifd <interface-doc>))
  "Return a list of <backend-property-doc> instances."
  (map name-sym->backend-property-doc
       (sort (caddr (iface-record ifd)) ly:symbol-ci<?)))

(define-method (supporting (bpd <backend-property-doc>))
  "Return a list of <interface-doc> objects."
  (let ((iface-docs
         (filter (lambda (ifd) (member bpd (supports ifd)))
                 all-interface-docs-list)))
    ;; Since all backend properties must be supported by some
    ;; interface, warn if we did not find any.
    (if (null? iface-docs)
        (ly:error
         (string-append
          "define-grob-properties.scm: "
          (format #f (_ "cannot find interface for property: ~S")
                  (name-sym bpd)))))
    iface-docs))


;;; Output-object - backend property relation

(define-method (assignments (ood <output-object-doc>))
  "Return a list of pairs (PROP . VAL), where PROP is a
<backend-property-doc> instance."
  (let* ((namesym-val-list-unsorted (cdr (object-record ood)))
         (meta (assoc 'meta namesym-val-list-unsorted))
         ;; force 'meta property to the end of the list
         (namesym-val-list
          (append
           (sort (alist-delete 'meta namesym-val-list-unsorted)
                 ly:alist-ci<?)
           (list meta))))
    (map (lambda (ns-v)
           (let ((bpd (name-sym->backend-property-doc (car ns-v)))
                 (value (cdr ns-v)))
             (cons bpd value)))
         namesym-val-list)))

(define-method (assignments-tunable (ood <output-object-doc>))
  (filter (lambda (bpd-v) (not (internal? (car bpd-v))))
          (assignments ood)))

(define-method (assigns (ood <output-object-doc>))
  "Return a list of <backend-property-doc> instances."
  (map car (assignments ood)))

(define-method (assigning (bpd <backend-property-doc>))
  (filter (lambda (ood) (member bpd (assigns ood)))
          all-output-object-docs-list))


;;; Backend property - context - output object relation

(define-method (pushing-string (bpd <backend-property-doc>))
  (let* ((ctx-obj-list (pushing bpd))
         (setting-contexts-by-obj
          (sort
           (group-by-function-result cdr ctx-obj-list)
           (lambda (a b) (ly:symbol-ci<? (name-sym (car a))
                                         (name-sym (car b)))))))
    (cond
     ((null? setting-contexts-by-obj) "")
     ((equal? (length setting-contexts-by-obj) 1)
      (let ((obj (caar setting-contexts-by-obj))
            (contexts
             ;; Duplicates could occur if a context pushes several
             ;; subproperties of a nested property. For example,
             ;; TabVoice pushes both bound-details.left and
             ;; bound-details.right for Glissando grobs.
             (delete-duplicates
              (map car (cdar setting-contexts-by-obj)))))
        (describe-list
         "" ; this cannot happen
         (format
          #f
          "Context-specific defaults for ~a objects are set in context %LIST.\n\n"
          (node-ref obj))
         (format
          #f
          "Context-specific defaults for ~a objects are set in contexts %LIST.\n\n"
          (node-ref obj))
         (map node-ref contexts))))
     (else
      (string-append
       "Context-specific defaults are set\n"
       "@itemize @bullet\n"
       (string-join
        (map
         (lambda (lst)
           (let ((obj (car lst))
                 (contexts (delete-duplicates (map car (cdr lst)))))
             (describe-list
              (format #f "@item for ~a objects in context %LIST"
                      (node-ref obj))
              (format #f "@item for ~a objects in contexts %LIST"
                      (node-ref obj))
              (map node-ref contexts))))
         setting-contexts-by-obj)
        "\n\n")
       "@end itemize\n\n")))))


;;; Assemble single output object documentation

(define-method (node-text (ood <output-object-doc>))
  (let* ((namestr (node-name ood)))
    (string-append
     (describe-list
      (format #f "~a objects are not created by any translator.\n\n"
              namestr)
      (format #f "~a objects are created by %LIST.\n\n"
              namestr)
      (map node-ref (creators ood)))

     (short-prop-value-table-string
      (format #f "Tunable properties with default settings for ~a:\n\n"
              namestr)
      (assignments-tunable ood))
     "\n"

     (describe-list
      "This object does not support any interfaces.\n\n"
      "This object supports the interface %LIST.\n\n"
      "This object supports the following interfaces: %LIST.\n\n"
      (map node-ref (implements ood))))))


;;; Assemble single interface documentation

(define-method (node-text (ifd <interface-doc>))
  (let* ((props (supports ifd))
         (iprops (filter internal? props))
         (uprops (lset-difference equal? props iprops))
         (objs (implementing ifd)))
    (string-append
     (description ifd)
     "\n\n"
     (describe-list
      "This interface is not used for any output object."
      "This interface is used for %LIST objects."
      "This interface is used for the following objects: %LIST."
      (map node-ref objs))
     (short-prop-table-string
      "\n\n@subsubheading User-settable properties:"
      uprops)
     (short-prop-table-string
      "\n\n@subsubheading Internal properties:"
      iprops))))


;;; Assemble single backend property documentation

(define-method (item-text (bpd <backend-property-doc>))
  (string-append
   (next-method)
   "\n\n"
   (describe-list
    "No interface supports this property. THIS IS A BUG."
    "Supported by %LIST."
    (map node-ref (supporting bpd)))
   "\n\n"
   (describe-list
    ""
    "Global defaults are defined for %LIST.\n\n"
    (map node-ref (assigning bpd)))
   (pushing-string bpd)))
