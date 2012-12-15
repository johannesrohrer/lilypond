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

(define doc-context (ly:make-global-context $defaultlayout))

(define (music-props-doc)
  (make <texi-node>
    #:name "Music properties"
    #:desc "All music properties, including descriptions."
    #:text
    (let* ((ps (sort (map symbol->string all-music-properties) ly:string-ci<?))
	   (descs (map (lambda (prop)
			 (property->texi 'music (string->symbol prop)))
		       ps))
	   (texi (description-list->texi descs #f)))
      texi)))

(define music-types->names (make-hash-table 61))
(filter-map (lambda (entry)
	      (let* ((class (ly:camel-case->lisp-identifier (car entry)))
		     (classes (ly:make-event-class doc-context class)))
		(if classes
		    (map
		     (lambda (cl)
		       (hashq-set! music-types->names cl
				   (cons (car entry)
					 (hashq-ref music-types->names cl '()))))
		     classes)
		    #f)))
	
	    music-descriptions)

(define (strip-description x)
  (cons (symbol->string (car x))
	""))

(define (music-type-doc entry)
  (let ((name-sym (car entry))
        (events (sort (cdr entry) ly:symbol-ci<?)))
    (make <texi-node>
      #:name (symbol->string name-sym)
      #:text
      (string-append
       (describe-list
        "\nThis event class is empty.\n\n" ; should not happen?
        "\nThis event class is assigned to music expressions of type %LIST.\n\n"
        (map (lambda (ev) (ref-ify (symbol->string ev))) events))
       (describe-list
        "Not accepted by any translator."
        "Accepted by: %LIST."
        (map ref-ify (accepting name-sym)))))))

(define (music-types-doc)
  (make <texi-node>
    #:name "Music classes"
    #:desc "Groups of related music events."
    #:text "Music classes group related music events.

They provide the interface to the translation stage: @ref{Translators}
accept or ignore events based on the classes assigned to them."
    #:children
    (map music-type-doc
         (sort
          (hash-table->alist music-types->names) ly:alist-ci<?))))

(define (music-doc-str obj)
  (let* ((namesym  (car obj))
         (props (cdr obj))
         (class (ly:camel-case->lisp-identifier namesym))
         (classes (ly:make-event-class doc-context class))
         (acceptors (delete-duplicates
                     (sort (apply append (map accepting classes))
                           name-sym-ci<?)))
         (event-texi (if (null? classes)
                         ""
                         (string-append
                          (format
                           "\n\nEvent classes:\n~a.\n\n"
                           (human-listify
                            (map ref-ify (sort (map symbol->string classes)
                                               ly:string-ci<?))))
                          (describe-list
                           "Not accepted by any translator."
                           "Hence accepted by %LIST."
                           (map ref-ify acceptors))))))
    (string-append
     (object-property namesym 'music-description)
     event-texi
     "\n\nProperties:\n"
     (description-list->texi
      (map
       (lambda (x) (property->texi 'music x props))
       (sort (map car props) ly:symbol-ci<?))
      #t))))

(define (music-object-doc obj)
  (make <texi-node>
    #:name (symbol->string (car obj))
    #:text (music-doc-str obj)))

(define (music-expressions-doc)
  (make <texi-node>
    #:name "Music expressions"
    #:desc "Objects that represent music."
    #:children
    (map music-object-doc music-descriptions)))

(define (music-doc-node)
  (make <texi-node>
    #:name "Music definitions"
    #:desc "Definition of the input data structures."
    #:children
    (list
     (music-expressions-doc)
     (music-types-doc)
     (music-props-doc))))
