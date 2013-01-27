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

;; Assemble Internals Reference documentation node/item texts

;; See document-internals.scm for an overview of the IR generation
;; process. Briefly:
;;
;; The global structure of the Internals Reference and all of its
;; documentation nodes and items were already defined in the module
;; (scm document-internals-nodestructure). Since the corresponding
;; descriptive texts shall liberally cross-reference back and forth,
;; actually defining them was deferred until here.
;;
;; To get the texts in now, we eventually override the methods
;; node-text and/or item-text of the relevant <*-doc> classes.

;;; Code:

(define-module (scm document-internals-doctexts)
  #:use-module ((srfi srfi-1) #:select (alist-delete
                                        delete-duplicates
                                        lset-difference
                                        lset-intersection))
  #:use-module (oop goops)
  #:use-module ((lily) #:select (assoc-get
                                 format
                                 ly:camel-case->lisp-identifier
                                 ly:make-event-class))
  #:use-module (scm lily-sort)
  #:use-module (scm texinfo-generation)
  #:use-module (scm document-internals-docclasses)
  #:use-module (scm document-internals-nodestructure)
  #:use-module (scm document-internals-xref)
  #:use-module (scm documentation-lib))


;;;; Cross-referencing relations

;;; Music class - music class relation

(define-method (ancestors (mcd <music-class-doc>))
  "Return a hierarchy-sorted list of <music-class-doc> instances,
starting with the most specific superclass of MCD."
  (let* ((doc-context (slot-ref mcd 'doc-context))
         (ancestry-name-syms
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
          (all-music-class-docs)))

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
          (all-music-expression-docs)))

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

(define-method (accepts (td <translator-doc>))
  "Return list of <music-class-doc> instances."
  (map name-sym->music-class-doc
       (sort (attr 'events-accepted td) ly:symbol-ci<?)))

(define-method (accepting (mcd <music-class-doc>))
  "Return a list <translator-doc> instances."
  (filter (lambda (td) (member mcd (accepts td)))
          (all-translator-docs)))

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


;;; Music expression - translator relation

(define-method (accepting (mxd <music-expression-doc>))
  (let* ((ev-type (immediate-event-type mxd))
         (classes (if ev-type (cons ev-type (ancestors ev-type)) '())))
    (delete-duplicates
     (sort (apply append (map accepting classes))
           (lambda (a b) (ly:symbol-ci<? (name-sym a) (name-sym b)))))))

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
          (sort (cdr (low-level-record mxd))
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
          (all-music-expression-docs)))


;;; Context - context relation

(define-method (corresponding-context-strings (this-cd <context-doc>))
  "Return 'Corresponding [layout context/MIDI context]: ...' string list."
  (let* ((all-with-name-sym (name-sym->context-docs (name-sym this-cd)))
         (all-relevant
          (filter (lambda (cd)
                    (not (equal? (type-doc this-cd)
                                 (type-doc cd))))
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
           (or (name-sym->context-doc ns (type-doc cd))
               (make <undocumented-context-doc>
                 #:name-sym ns
                 #:node-suffix " (alias only)"
                 #:type-doc (type-doc cd))))
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
           (name-sym->context-doc ns (type-doc cd)))
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
          (all-translator-docs)))

(define-method (creations-string (td <translator-doc>))
  "Assemble 'This [translator] creates the following [object(s)]: ...' lines."
  (string-join
   (output-objects-creation-strings (type-string td) (creations td))))


;;; Translator - Context relation

(define-method (consists (cd <context-doc>))
  "Return a list of <translator-doc> instances."
  (map name-sym->translator-doc (sort (attr 'consists cd) ly:symbol-ci<?)))

(define-method (consisting (td <translator-doc>))
  "Return a list of <context-doc> instances."
  (filter (lambda (cd) (member td (consists cd)))
          (all-context-docs)))

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
          (all-translator-docs)))

(define-method (writers (cpd <context-property-doc>))
  "Return a list of <translator-doc> instances."
  (filter (lambda (td) (member cpd (writes td)))
          (all-translator-docs)))

(define-method (read-properties-string (td <translator-doc>))
  (prop-table-string (reads td) #:title "Properties (read):"))

(define-method (written-properties-string (td <translator-doc>))
  (prop-table-string (writes td) #:title "Properties (written):"))

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
    (all-context-docs))))

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
          (all-context-docs)))

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
         (format #f "@item Set context property @code{~S} to ~a\n"
                 (name-sym (car ass))
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


;;; Output-object - interface relation

(define-method (implements (ood <output-object-doc>))
  "Return a list of <interface-doc> instances."
  (let ((iface-name-syms
         (sort (assoc-get 'interfaces
                          (assoc-get 'meta (cdr (low-level-record ood))))
          ly:symbol-ci<?)))
    (map name-sym->interface-doc iface-name-syms)))

(define-method (implementing (ifd <interface-doc>))
  "Return a list of <output-object-doc> instances."
  (filter (lambda (ood) (member ifd (implements ood)))
          (all-output-object-docs)))


;;; Backend property - interface relation

(define-method (supports (ifd <interface-doc>))
  "Return a list of <backend-property-doc> instances."
  (map name-sym->backend-property-doc
       (sort (caddr (low-level-record ifd)) ly:symbol-ci<?)))

(define-method (supporting (bpd <backend-property-doc>))
  "Return a list of <interface-doc> objects."
  (let ((iface-docs
         (filter (lambda (ifd) (member bpd (supports ifd)))
                 (all-interface-docs))))
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
  (let* ((namesym-val-list-unsorted (cdr (low-level-record ood)))
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
          (all-output-object-docs)))


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


;;;; Final text assembly

;;; Music expression

(define-method (node-text (mxd <music-expression-doc>))
  (string-append
   (next-method)
   (description mxd)
   "\n\n"
   (comprising-string mxd)
   (accepting-string mxd)

   (prop-value-table-string
    (assignments mxd)
    #:title
    (format #f "Music properties with default settings for ~a:\n\n"
            (node-name mxd)))))


;;; Music class

(define-method (node-text (mcd <music-class-doc>))
  (string-append
   (next-method)
   (hierarchy-string mcd)
   (comprises-string mcd)
   (accepting-string mcd)))


;;; Music property

(define-method (node-text (mpd <music-property-doc>))
  (string-append
   (next-method)
   "\n\n"
   (describe-list
    ""
    "Default values are defined for %LIST.\n\n"
    (map node-ref (assigning mpd)))))


;;; Context

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


;;; Translator

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


;;; Context property

(define-method (node-text (cpd <context-property-doc>))
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


;;; Output object

(define-method (node-text (ood <output-object-doc>))
  (let* ((namestr (node-name ood)))
    (string-append
     (describe-list
      (format #f "~a objects are not created by any translator.\n\n"
              namestr)
      (format #f "~a objects are created by %LIST.\n\n"
              namestr)
      (map node-ref (creators ood)))

     (prop-value-table-string
      (assignments-tunable ood)
      #:title
      (format #f "Tunable properties with default settings for ~a:\n\n"
              namestr))
     "\n"

     (describe-list
      "This object does not support any interfaces.\n\n"
      "This object supports the interface %LIST.\n\n"
      "This object supports the following interfaces: %LIST.\n\n"
      (map node-ref (implements ood))))))


;;; Interface

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
     (prop-table-string
      uprops
      #:title "\n\n@subsubheading User-settable properties:")
     (prop-table-string
      iprops
      #:title "\n\n@subsubheading Internal properties:"))))


;;; Output object (backend) property

(define-method (node-text (bpd <backend-property-doc>))
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
