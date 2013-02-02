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

;; Utilities for generating texinfo input files. Used for
;; automatically-generated documentation, like the Internals
;; Reference.

;; Concept: Build a tree of <texi-node> instances, one for each
;; texinfo node, with a <texi-document> instance at the root. List
;; subnodes of a node, if any, in its 'children' slot.
;;
;; Store actual node texts statically in the 'text' slot, or, more
;; flexibly, sub-class <texi-node> and override the node-text method.
;;
;; Generate the whole document using the write-texi-file method of
;; <texi-document>, or parts of it with texi-dump or
;; headless-texi-dump. Sectioning commands are inserted automatically
;; based on nesting depth. Menus are generated automatically.

;;; Code:

(define-module (scm texinfo-generation)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module ((ice-9 regex) #:select (regexp-substitute/global))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((lily) #:select (format
                                 ly:message
                                 ly:progress
                                 _))
  #:use-module ((scm display-scm) #:select (scm->string))
  #:use-module ((scm documentation-lib)
                #:select (group-by-function-result
                          string-or))
  #:export (texify
            scm->texi
            <texi-node>
            disambiguate-node-names!
            disambig-suffix
            disambig-list
            headless-texi-dump
            node-children
            node-name
            node-ref
            node-text
            texi-dump
            <texi-document>
            write-texi-file
            <texi-item>
            item-key
            item-text
            <texi-table>
            table-items
            texi-table-string
            texi-quoted-table-string))


;;; Low-level stuff

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

(define* (scm->texi expr #:key (multiline? #t))
  "Return a texinfo string representation of the scheme value EXPR."
  (let ((tstr (texify (scm->string expr #:multiline? multiline?))))
    (if (and multiline? (string-index tstr #\newline))
        (format #f "\n@verbatim\n~a\n@end verbatim\n" tstr)
        (format #f "@code{~a}" tstr))))


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


;;; Texinfo nodes (general)

(define-class <texi-node> ()
  (name #:init-keyword #:name
        #:init-value ""
        #:accessor node-name)
  (description #:init-keyword #:desc
               #:init-value ""
               #:accessor node-desc)
  (text #:init-keyword #:text
        #:init-value ""
        #:accessor static-text)
  (appendix #:init-value #f
            #:init-keyword #:appendix
            #:accessor appendix?)
  (numbered #:init-keyword #:numbered
            #:init-value #t
            #:accessor numbered?)
  ;; Define an accessor node-children for the following member below.
  (children #:init-keyword #:children
            #:init-value '())
  (parent #:init-keyword #:parent
          #:init-value #f
          #:accessor node-parent)
  ;; If disambiguate-node-names! needs to change the name of this node
  ;; to avoid a collision with an eponymous one, first try to do this
  ;; by appending this suffix (if set) to the original node name.
  (disambig-suffix #:init-value #f
                   #:init-keyword #:disambig-suffix
                   #:accessor disambig-suffix)
  ;; If disambiguate-node-names! changes the name of this node to
  ;; avoid conflicts, it lists all nodes that previously conflicted
  ;; with this one here.
  (disambig-list #:init-value '() #:accessor disambig-list)
  ;; When calculating the sectioning level for this node (node-level
  ;; method), assign this level to the root of the present node tree.
  (root-level #:init-keyword #:root-level
              #:init-value 0
              #:accessor root-level))

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

(define-method (flat-node-list (tn <texi-node>))
  "Helper function: Return a flat list of all texi nodes in the
texinfo tree with root TN."
  (apply append
         (cons (list tn)
               (map flat-node-list (node-children tn)))))

(define-method (disambiguate-node-names! (tn <texi-node>))
  "Check for duplicate node names in the texinfo node tree with root
TN. Solve all conflicts by appending suffixes to node names as
necessary: first, add the one from the disambig-suffix slot, if
available; if that is not sufficient, add parenthesized numbers.

If node name conflicts were resolved for a node, list all previously
colliding nodes in its disambig-list slot."
  (let* ((all-nodes (flat-node-list tn))
         (all-node-names
          ;; prepend dummy because delete! cannot remove first elements
          (cons #f (map node-name all-nodes)))
         (conflict-groups   ; list of lists (NODENAME NODE1 NODE2 ...)
          (filter (lambda (lst) (> (length lst) 2))
                  (group-by-function-result node-name all-nodes))))
    (for-each
     (lambda (conflict-group)
       (delete! (car conflict-group) all-node-names)
       (ly:message (_ "Resolve node name conflict for `~a':")
                   (car conflict-group))
       (for-each
        (lambda (node)
          (set! (disambig-list node) (delete node (cdr conflict-group)))
          (let* ((initial-candidate-node-name
                  (string-append (node-name node)
                                 (string-or (disambig-suffix node))))
                 (candidate-node-name initial-candidate-node-name)
                 (n 1))
            (while (member candidate-node-name all-node-names)
                   (set! n (1+ n))
                   (set! candidate-node-name
                         (format #f "~a (~a)"
                                 initial-candidate-node-name
                                 n)))
            (ly:message (format #f "  --> `~a'" candidate-node-name))
            (set! (node-name node) candidate-node-name)
            (set! all-node-names (cons candidate-node-name all-node-names))))
        (cdr conflict-group)))
     conflict-groups)
    tn))


;;; Complete texinfo document

(define-class <texi-document> (<texi-node>)
  (file-basename #:init-keyword #:file-name
                 #:accessor file-name)
  (texi-extension #:init-keyword #:extension
                  #:init-value ".texi"
                  #:accessor texi-extension)
  (title #:init-keyword #:title #:accessor title)
  (dir-category #:init-keyword #:dir-category
                #:init-value "LilyPond"
                #:accessor dir-category)
  (dir-key #:init-keyword #:dir-key
           #:accessor dir-key)
  (dir-description #:init-keyword #:dir-desc
                   #:getter dir-desc)
  (language #:init-keyword #:language
            #:init-value "en"
            #:getter language)
  (encoding #:init-keyword #:encoding
            #:init-value "utf-8"
            #:getter encoding)
  (preamble #:init-keyword #:preamble
            #:init-value ""
            #:getter preamble))

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
  (items #:init-keyword #:items #:init-value '()))

;; Implement getter for items as explicit method to make it
;; overrideable

(define-method (table-items (tt <texi-table>)) (slot-ref tt 'items))

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
