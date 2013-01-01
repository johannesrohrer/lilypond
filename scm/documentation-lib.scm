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

(use-modules (oop goops)
	     (srfi srfi-13)
	     (srfi srfi-1)
             (ice-9 regex))

;;; Texinfo node interface

(define-class <texi-node> ()
  (appendix #:init-value #f #:accessor appendix? #:init-keyword #:appendix)
  (numbered #:init-value #t #:accessor numbered? #:init-keyword #:numbered)
  (text #:init-value "" #:accessor static-text #:init-keyword #:text)
  (name #:init-value "" #:accessor node-name #:init-keyword #:name)
  (description #:init-value "" #:accessor node-desc #:init-keyword #:desc)
  ;; Define an accessor node-children for the following member below.
  (children #:init-value '() #:init-keyword #:children)
  (parent #:init-value #f #:accessor node-parent #:init-keyword #:parent)
  ;; When calculating the sectioning level for this node (node-level
  ;; method), assign this level to the root of the present node tree.
  (root-level #:init-value 0 #:init-keyword #:root-level #:accessor root-level))

(define-method (children-update-parent! (tn <texi-node>))
  "Set parent slot of TN's children to TN."
  (map (lambda (ch) (set! (node-parent ch) tn))
       (node-children tn)))

(define-method (initialize (tn <texi-node>) initargs)
  (next-method)
  (children-update-parent! tn))

(define-method (node-text (tn <texi-node>)) (static-text tn))

(define-accessor node-children)

(define-method (node-children (tn <texi-node>))
  (slot-ref tn 'children))

(define-method ((setter node-children) (tn <texi-node>) chs)
  (slot-set! tn 'children chs)
  (children-update-parent! tn))

(define-method (node-level (tn <texi-node>))
  (let ((parent (node-parent tn)))
    (if (not parent)
        (root-level tn)
        (+ 1 (node-level parent)))))

(define-method (node-section-command (tn <texi-node>))
  (let* ((level (node-level tn))
         (command (cond
                   ((appendix? tn)
                    (texi-appendix-section-command level))
                   ((numbered? tn)
                    (texi-section-command level))
                   (else
                    (texi-unnumbered-section-command level)))))
    (format "~a ~a\n\n" command (node-name tn))))

(define-method (node-menu-item (tn <texi-node>))
  (make <texi-item>
    #:key (node-name tn)
    #:text (node-desc tn)))

(define-method (node-menu-table (tn <texi-node>))
  (make <texi-table>
    #:items (map node-menu-item (node-children tn))))

(define-method (node-menu (tn <texi-node>))
  (if (null? (node-children tn))
      ""
      (texi-menu-string (node-menu-table tn))))

(define-method (node-ref (tn <texi-node>))
  (format "@ref{~a}" (node-name tn)))

(define-method (headless-texi-string (tn <texi-node>))
  "Return a texinfo string for TN without @node and section command
headers. Include the node menu, but no subnodes."
  (string-append
   (node-text tn)
   "\n\n"
   (node-menu tn)))

(define-method (texi-string (tn <texi-node>))
  "Return a texinfo string for TN. Include the node menu, but no subnodes."
  (string-append
   (format "@node ~a\n"
           (if (= (node-level tn) 0) "Top" (node-name tn)))
   (node-section-command tn)
   (headless-texi-string tn)))

(define-method (texi-dump (tn <texi-node>) port)
  "Write the texinfo string for TN and, recursively, all subnodes, to port."
  (display (texi-string tn) port)
  (map (lambda (subnode) (texi-dump subnode port))
       (node-children tn)))

(define-method (headless-texi-dump (tn <texi-node>) port)
  "Write the texinfo string for TN and, recursively, all subnodes, to
port, but leave out the top-level @node and section command headers."
  (display (headless-texi-string tn) port)
  (map (lambda (subnode) (texi-dump subnode port))
       (node-children tn)))


;;; Complete texinfo document

(define-class <texi-document> (<texi-node>)
  (file-basename #:init-keyword #:file-name #:accessor file-name)
  (texi-extension #:init-value ".texi"
                  #:init-keyword #:extension
                  #:accessor texi-extension)
  (title #:init-keyword #:title #:accessor title)
  (dir-category #:init-value "LilyPond"
                #:init-keyword #:dir-category
                #:accessor dir-category)
  (dir-key #:init-keyword #:dir-key #:accessor dir-key)
  (dir-description #:init-keyword #:dir-desc #:getter dir-desc)
  (language #:init-value "en" #:init-keyword #:language #:getter language)
  (encoding #:init-value "utf-8" #:init-keyword #:encoding #:getter encoding)
  (preamble #:init-value "" #:init-keyword #:preamble #:getter preamble))

(define-method (output-file-name (tdoc <texi-document>))
  (string-append (file-name tdoc) (texi-extension tdoc)))

(define-method (dir-entry (tdoc <texi-document>))
  (format "* ~a: (~a).       ~a."
          (dir-key tdoc)
          (file-name tdoc)
          (dir-desc tdoc)))

(define-method (file-head (tdoc <texi-document>))
  (string-append
   "\\input texinfo @c -*-texinfo-*-\n"
   (format "@setfilename ~a.info\n" (file-name tdoc))
   (format "@settitle ~a\n" (title tdoc))
   (format "@dircategory ~a\n" (dir-category tdoc))
   (format "@direntry\n~a\n@end direntry\n" (dir-entry tdoc))
   (format "@documentlanguage ~a\n" (language tdoc))
   (format "@documentencoding ~a\n" (encoding tdoc))))

(define-method (texi-dump (tdoc <texi-document>) port)
  (display (file-head tdoc) port)
  (display (preamble tdoc) port)
  ;; In TeX output, skip @node and section heading for the top node
  (display (format "\n\n@ifnottex\n@node ~a\n~a\n@end ifnottex\n\n"
                   (if (= (node-level tdoc) 0) "Top" (node-name tdoc))
                   (node-section-command tdoc))
           port)
  (headless-texi-dump tdoc port)
  (display "@bye\n" port))

(define-method (write-texi-file (tdoc <texi-document>))
  (let* ((outname (output-file-name tdoc))
         (port (open-output-file outname)))
    (ly:progress (_ "Writing ~S... ") outname)
    (texi-dump tdoc port)
    (ly:progress "\n")))


;;; Smaller texinfo building blocks

(define-class <texi-item> ()
  (key #:init-keyword #:key)
  (text #:init-keyword #:text))

;; Implement getters for key and text as explicit methods to make them
;; overrideable (as used in <property-doc>).

(define-method (item-key (ti <texi-item>)) (slot-ref ti 'key))

(define-method (item-text (ti <texi-item>)) (slot-ref ti 'text))

(define-method (texi-item-string (ti <texi-item>))
  "Return empty string if ti has empty key."
  (if (string-null? (item-key ti))
      ""
      (format "@item ~a\n~a\n\n" (item-key ti) (item-text ti))))

(define-method (texi-ref-item-string (ti <texi-item>))
  "Return empty string if ti has empty key."
  (if (string-null? (item-key ti))
      ""
      (format "@item @ref{~a}\n~a\n\n" (item-key ti) (item-text ti))))

(define-method (texi-menu-item-string (ti <texi-item>) key-length)
  (let ((key-str (string-pad-right (format "* ~a:: " (item-key ti))
                                   key-length)))
    (string-append key-str (item-text ti) "\n")))


(define-class <texi-table> ()
  (items #:init-value '() #:init-keyword #:items #:accessor table-items))

(define-method (texi-table-string (tt <texi-table>))
  "Generate a texinfo table from TT's items."
  (string-append
   "@table @asis\n\n"
   (apply string-append (map texi-item-string (table-items tt)))
   "@end table\n"))

(define-method (texi-quoted-table-string (tt <texi-table>))
  "Generate a texinfo table wrapped in a @quotation environment from
TT's items."
  (string-append "@quotation\n" (texi-table-string tt) "@end quotation\n"))

(define-method (texi-menu-string (tt <texi-table>))
  "Generate a texinfo menu from TT's items."
  (let ((max-key-length
         (apply max
                (cons 0 ; avoid error for empty tables
                      (map (lambda (x) (string-length (item-key x)))
                           (table-items tt))))))
    (string-append
     "@menu\n"
     (apply string-append
            (map (lambda (item)
                   (texi-menu-item-string item (+ max-key-length 8)))
                 (table-items tt)))
     "@end menu\n\n"
     ;; Menus do not appear in HTML, so we make a list ourselves
     "@ignore\n\n"
     "@ifhtml\n"
     "@quotation\n"
     "@table @asis\n\n"
     (apply string-append (map texi-ref-item-string (table-items tt)))
     "@end table\n"
     "@end quotation\n"
     "@end ifhtml\n\n"
     "@end ignore\n\n")))


;;; Low-level stuff

(define (self-evaluating? x)
  (or (number? x) (string? x) (procedure? x) (boolean? x)))

(define (texify str)
  "Escape STR for use in a texinfo document."
  ;; This may not be exhaustive.
  (let ((rules '(("@" . "@@")
                 ("\\{" . "@{")
                 ("\\}" . "@}"))))
    (fold
     (lambda (rule s)
       (regexp-substitute/global #f (car rule) s 'pre (cdr rule) 'post))
     str
     rules)))

(define (scm->texi x)
  (string-append "@code{" (texify (scm->string x)) "}"))


(define (texi-section-command level)
  (cond ((equal? level 0) "@top")
        ;; Hmm, texinfo doesn't have ``part''
        ((equal? level 1) "@chapter")
        ((equal? level 2) "@section")
        ((equal? level 3) "@subsection")
        ((equal? level 4) "@subsubsec")
        (else "@subsubheading")))

(define (texi-unnumbered-section-command level)
  (cond ((equal? level 0) "@top")
        ((equal? level 1) "@unnumbered")
        ((equal? level 2) "@unnumberedsec")
        ((equal? level 3) "@unnumberedsubsec")
        ((equal? level 4) "@unnumberedsubsubsec")
        (else "@subsubheading")))

(define (texi-appendix-section-command level)
  (cond ((equal? level 0) "@top")
        ((equal? level 1) "@appendix")
        ((equal? level 2) "@appendixsec")
        ((equal? level 3) "@appendixsubsec")
        ((equal? level 4) "@appendixsubsubsec")
        (else "@subsubheading")))

(define (one-item->texi label-desc-pair)
  "Document one (LABEL . DESC); return empty string if LABEL is empty string."
  (if (eq? (car label-desc-pair) "")
      ""
      (string-append "\n@item " (car label-desc-pair) "\n" (cdr label-desc-pair))))


(define (description-list->texi items-alist quote?)
  "Document ITEMS-ALIST in a table; entries contain (item-label .
string-to-use).  If QUOTE? is #t, embed table in a @quotation environment."
  (string-append
   "\n"
   (if quote? "@quotation\n" "")
   "@table @asis\n"
   (apply string-append (map one-item->texi items-alist))
   "\n"
   "@end table\n"
   (if quote? "@end quotation\n" "")))

(define (string-or . args)
  "Return the first argument that is a non-empty string, or \"\" if
none is found."
  (if (null? args)
      ""
      (let ((str (car args)))
        (if (and (string? str) (not (string-null? str)))
            str
            (apply string-or (cdr args))))))

(define (describe-list . args)
  "describe-list FSTR0 [ FSTR1 ... FSTRn ] LST [ PLACEHOLDER [ LST->STR ]]

Return one FSTRm with the substring PLACEHOLDER (default \"%LIST\")
replaced with a textual enumeration of LST. m is chosen according to
the number of arguments of LST. The function LST->STR, which must take
a single list argument, is used to turn LST into a single string; it
defaults to human-listify."
  (let* ((fstrs (take-while string? args))
         (restargs (drop-while string? args))
         (lst (car restargs))
         (placeholder (if (null? (cdr restargs))
                          "%LIST"
                          (cadr restargs)))
         (lst->str (if (or (null? (cdr restargs))
                           (null? (cddr restargs)))
                       human-listify
                       (caddr restargs)))
         (m (min (- (length fstrs) 1) (length lst)))
         (fstr (list-ref fstrs m)))
    (regexp-substitute/global #f placeholder fstr
                              'pre (lst->str lst) 'post)))

(define (ref-ify x)
  "Return @ref{X}.  If mapping ref-ify to a list that needs to be sorted,
   sort the list first."
  (string-append "@ref{" x "}"))

(define (human-listify lst)
  "Produce a textual enumeration from LST, a list of strings"

  (cond
   ((null? lst) "none")
   ((null? (cdr lst)) (car lst))
   ((null? (cddr lst)) (string-append (car lst) " and " (cadr lst)))
   (else (string-append (car lst) ", " (human-listify (cdr lst))))))

(define (identifier<? a b)
  (ly:string-ci<?
   (symbol->string (car a))
   (symbol->string (car b))))

(define (name-sym-ci<? a b)
  (ly:symbol-ci<? (name-sym a) (name-sym b)))


;;; property  stuff

(define (verify-type-name where sym type)
  (if (eq? type #f)
      (ly:error (_ "cannot find description for property `~S' (~S)")
                sym
                where))
  (type-name type))

(define (property->texi where sym . rest)
  "Document SYM for WHERE (which can be translation, backend, music),
with init values from ALIST (1st optional argument)
"
  (let* ((name (symbol->string sym))
	 (alist (if (pair? rest) (car rest) '()))
	 (type?-name (string->symbol
		      (string-append (symbol->string where) "-type?")))
	 (doc-name (string->symbol
		    (string-append (symbol->string where) "-doc")))
	 (type (object-property sym type?-name))
	 (typename (verify-type-name where sym type))
	 (desc (object-property sym doc-name))
	 (init-value (assoc-get sym alist)))

    (if (eq? desc #f)
	(ly:error (_ "cannot find description for property ~S (~S)") sym where))

    (cons
     (string-append "@code{" name "} "
		    "(" typename ")"
		    (if init-value
			(string-append
			 ":\n\n"
			 (scm->texi init-value)
			 "\n\n")
			""))
     desc)))
