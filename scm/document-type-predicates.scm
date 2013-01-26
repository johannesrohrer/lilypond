;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2012 Mark Polesky <markpolesky@yahoo.com>
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

(define-module (scm document-type-predicates)
  #:use-module ((lily) #:select (lilypond-exported-predicates
                                 lilypond-scheme-predicates
                                 guile-predicates
                                 r5rs-primary-predicates
                                 r5rs-secondary-predicates))
  #:use-module (scm lily-sort)
  #:export (type-predicates-doc-string))


(define (document-type-predicate entry)
  (let ((pred (procedure-name (car entry))))
    (string-append
     "@item @code{"
     (case pred
       ;; don't print "cheap-markup?"
       ((cheap-markup?) "markup?")
       (else (symbol->string pred)))
     "} @tab \n"
     (case pred
       ;; clarify `list?' vs. `cheap-list?'
       ((list?) "list @emph{(use} @code{cheap-list?}
                @emph{for faster processing)}")
       ((cheap-list?) "list @emph{(use this instead of}
                      @code{list?} @emph{for faster processing)}")
       (else (cdr entry)))
     "\n")))

(define (document-type-predicate-category alist nodename)
  (string-append
   "@node " nodename "\n"
   "@unnumberedsubsec " nodename "\n"
   "\n"
   "@multitable @columnfractions .33 .66\n"
   "@headitem Type predicate @tab Description\n"
   (apply string-append
          (sort (map document-type-predicate alist)
                ly:string-ci<?))
   "@end multitable\n"
   "\n"))

(define-public type-predicates-doc-string
  (string-append
   "@menu\n"
   "* R5RS primary predicates::\n"
   "* R5RS secondary predicates::\n"
   "* Guile predicates::\n"
   "* LilyPond scheme predicates::\n"
   "* LilyPond exported predicates::\n"
   "@end menu\n"
   "\n"
   (apply
    string-append
    (map
     (lambda (alist-nodename-list)
       (apply document-type-predicate-category
              alist-nodename-list))
     `((,r5rs-primary-predicates "R5RS primary predicates")
       (,r5rs-secondary-predicates "R5RS secondary predicates")
       (,guile-predicates "Guile predicates")
       (,lilypond-scheme-predicates "LilyPond scheme predicates")
       (,lilypond-exported-predicates "LilyPond exported predicates"))))))
