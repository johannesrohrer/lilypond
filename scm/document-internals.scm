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

;; Assemble the complete Internals Reference texinfo document

;; Overview
;; ========
;;
;; Generate a texinfo document tree in the object-oriented framework
;; defined in the (scm texinfo-generation) module. Do most of the
;; actual work in various submodules:
;;
;; (scm document-internals-docclasses)
;;
;;     Define a <texi-node> (or <texi-item>) subclass for each
;;     documentation-worthy object, for example
;;     <music-expression-doc>, <translator-doc> or
;;     <context-property-doc>. These encapsulate the low-level data
;;     records used to obtain the information to document and provide
;;     an abstract interface to at least some of the data contained
;;     therein.
;;
;; (scm document-internals-nodestructure)
;;
;;     Generate all nodes and items and define the global structure of
;;     the IR. Specifically, define and export the top-level subnodes
;;     referenced here below.
;;
;;     Since we want to liberally cross-reference back and forth in
;;     the bottom-level node/item texts, do not specify these texts
;;     just yet; export lists of various types of <*-doc> objects used
;;     to generate these cross-references later.
;;
;; (scm document-internals-xref)
;;
;;     Index the lists mentioned previously in hash tables and export
;;     functions name-symbol->*-doc to easily look up those
;;     nodes/items.
;;
;; (scm document-internals-doctexts)
;;
;;     Put together the cross-referencing node and item texts.

;;; Code:

(define-module (scm document-internals)
  #:use-module (oop goops)
  #:use-module ((lily) #:select (lilypond-version))
  #:use-module (scm texinfo-generation)
  #:use-module (scm document-internals-docclasses)
  #:use-module (scm document-internals-nodestructure)
  #:use-module (scm document-internals-doctexts)
  #:use-module (scm document-functions)
  #:export (get-internals-reference-texidoc))


(define (get-internals-reference-texidoc parser)
  (make <texi-document>
    #:title "LilyPond Internals Reference"
    #:file-name "internals"
    #:dir-category "LilyPond"
    ;; prepend GNU for info directory entry; must be unique
    #:dir-key "GNU LilyPond Internals Reference"
    #:dir-desc "Definitions for tweaking"
    #:preamble "
@include macros.itexi

@ignore
@omftitle LilyPond internals
@omfcreator Han-Wen Nienhuys and Jan Nieuwenhuizen
@omfdescription Programmer's reference of the LilyPond music engraving system
@omftype user's guide
@omflanguage English
@omfcategory Applications|Publishing
@end ignore

@iftex
@afourpaper
@end iftex

@finalout

@titlepage
@title LilyPond
@subtitle The music typesetter
@titlefont{Internals Reference}
@author The LilyPond development team

@c `Internals Reference' was born 2000-10-21 with git commit 01e371f...
Copyright @copyright{} 2000--2012 by the authors

@vskip 20pt

For LilyPond version @version{}
@end titlepage

@contents"
    ;; top node data
    #:name "GNU LilyPond -- Internals Reference"
    #:text
    (string-append
     "@ifhtml
@ifclear bigpage
This document is also available as a
@uref{source/Documentation/internals.pdf,PDF} and as
@uref{source/Documentation/internals-big-page.html,one big page}.
@end ifclear
@ifset bigpage
This document is also available as a
@uref{source/Documentation/internals.pdf,PDF} and as a
@uref{source/Documentation/internals/index.html,HTML indexed multiple pages}.
@end ifset
@end ifhtml

This is the Internals Reference (IR) for version "
     (lilypond-version)
     " of LilyPond, the GNU music typesetter.")
    #:children
    (list
     (get-music-doc-node parser)
     (get-translation-doc-node parser)
     layout-backend-doc-node
     (all-scheme-functions-doc)
     (make <texi-node>
       #:appendix #t
       #:name "Indices"
       #:children
       (list
        (make <texi-node>
          #:appendix #t
          #:name "Concept index"
          #:text "@printindex cp")
        (make <texi-node>
          #:appendix #t
          #:name "Function index"
          #:text "@printindex fn"))))))
