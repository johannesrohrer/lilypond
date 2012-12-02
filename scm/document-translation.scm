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

;; Distinguish translator types (engravers, performers, other)
;; by their names. An alternative would be to implement proper
;; type predicates ly:engraver? and ly:performer?.

(define (engraver? trans)
  (string-suffix-ci? "Engraver" (symbol->string (ly:translator-name trans))))
 
(define (performer? trans)
  (string-suffix-ci? "Performer" (symbol->string (ly:translator-name trans))))

(define name->translator-table (make-hash-table 61))
(map
 (lambda (x)
   (hash-set! name->translator-table (ly:translator-name x) x))
 (ly:get-all-translators))

(define (find-translator-by-name name-sym)
  "NAME-SYM is a symbol."
  (hash-ref name->translator-table name-sym #f))

(define (engraver-makes-grob? name-symbol grav)
  (memq name-symbol (assoc 'grobs-created (ly:translator-description grav))))

(define (translator-accepts-music-type? name-symbol grav)
  (memq name-symbol (assoc 'events-accepted (ly:translator-description grav))))

(define (translator-accepts-music-types? types grav)
  (if (null? types)
      #f
      (or
       (translator-accepts-music-type? (car types) grav)
       (translator-accepts-music-types? (cdr types) grav))))

(define (contexts-with-translator name-sym)
  (let* ((output-def (if (performer? (find-translator-by-name name-sym))
                         $defaultmidi
                         $defaultlayout))
         (layout-alist (ly:output-description output-def))
         (context-description-alist (map cdr layout-alist)))
    (apply append
           (map
            (lambda (x)
              (let* ((context (assoc-get 'context-name x))
                     (group (assq-ref x 'group-type))
                     (consists (append
                                (if group
                                    (list group)
                                    '())
                                (assoc-get 'consists x))))
                (if (member name-sym consists)
                    (list context)
                    '())))
            context-description-alist))))

(define (translator-doc-string translator in-which-contexts)
  (let* ((propsr (assoc-get 'properties-read 
                            (ly:translator-description translator)))
         (propsw (assoc-get 'properties-written
                            (ly:translator-description translator)))
         (accepted  (assoc-get 'events-accepted
                               (ly:translator-description translator)))
         (name-sym  (ly:translator-name translator))
	 (name-str (symbol->string name-sym))
         (desc (assoc-get 'description (ly:translator-description translator)))
         (grobs (if (engraver? translator)
                    (engraver-grobs translator)
                    '())))
    (string-append
     desc
     "\n\n"
     (if (pair? accepted)
	 (string-append
	  "Music types accepted:\n\n"
	  (human-listify
	   (map ref-ify (sort (map symbol->string accepted) ly:string-ci<?))))
	 "")
     "\n\n"
     (if (pair? propsr)
	 (string-append
	  "Properties (read)"
	  (description-list->texi
	   (map (lambda (x) (property->texi 'translation x '()))
	        (sort propsr ly:symbol-ci<?))
	   #t))
	 "")

     (if (null? propsw)
	 ""
	 (string-append
	  "Properties (write)"
	  (description-list->texi
	   (map (lambda (x) (property->texi 'translation x '()))
	        (sort propsw ly:symbol-ci<?))
	   #t)))
     (if  (null? grobs)
	  ""
	  (string-append
	   "\n\nThis engraver creates the following layout object(s):\n\n"
	   (human-listify (map ref-ify (uniq-list (sort grobs ly:string-ci<?))))
	   "."))

     "\n\n"

     (if in-which-contexts
	 (let* ((contexts (contexts-with-translator name-sym))
		(context-list (human-listify (map ref-ify
						  (sort
						   (map symbol->string contexts)
						   ly:string-ci<?)))))
	   (string-append
	    "@code{" name-str "} "
	    (if (equal? context-list "none")
		"is not part of any context"
		(string-append
		 "is part of the following context(s): "
		 context-list))
	    "."))
	 ""))))

;; First level Translator description
(define (translator-doc grav)
  (make <texi-node>
    #:name (symbol->string (ly:translator-name grav))
    #:text (translator-doc-string grav #t)))

;; Second level, part of Context description
(define (document-translator-by-name name)
  "NAME is a symbol."

  (let* ((eg (find-translator-by-name name)))

    (cons (string-append "@code{" (ref-ify (symbol->string name)) "}")
	  (translator-doc-string eg #f))))

(define (document-property-operation op)
  (let ((tag (car op))
	(context-sym (cadr op))
	(args (cddr op))
	)

    (cond
     ((equal?  tag 'push)
      (let*
	  ((value (car args))
	   (path (cdr args)))

      (string-append
       "@item Set "
       (format #f "grob-property @code{~a} "
	       (string-join (map symbol->string path) " "))
       (format #f "in @ref{~a} to ~a."
	       context-sym (scm->texi value))
       "\n")))
     ((equal? (object-property context-sym 'is-grob?) #t) "")
     ((equal? tag 'assign)
      (format #f "@item Set translator property @code{~a} to ~a.\n"
	      context-sym
	      (scm->texi (car args))))
     )))

(define (context-doc context-descs)
  "CONTEXT-DESCS has the form
     (name-sym context-desc-layout context-desc-midi),
   where either of layout-content-desc and midi-content-desc
   may be '()."
  (let* ((name-sym (car context-descs))
         (name (symbol->string name-sym))
         (context-desc-layout (cadr context-descs))
         (context-desc-midi (caddr context-descs))
         (desc-layout (assoc-get 'description context-desc-layout ""))
         (desc-midi (assoc-get 'description context-desc-midi ""))
         (desc "")
         (lookup-all (lambda (key)
                       (delete-duplicates
                        (append
                         (assoc-get key context-desc-layout '())
                         (assoc-get key context-desc-midi '())))))
         (aliases (map symbol->string (lookup-all 'aliases)))
         (accepts (lookup-all 'accepts))
         (consists (lookup-all 'consists))
         (props (lookup-all 'property-ops))
         (grobs  (context-grobs context-desc-layout))
         (grob-refs (map ref-ify (sort grobs ly:string-ci<?))))

    (if (null? desc-layout) (set! desc-layout ""))
    (if (null? desc-midi) (set! desc-midi ""))
    (set! desc (string-append
                desc-layout
                (if (not (or (equal? desc-layout "")
                             (equal? desc-midi "")))
                    "\n"
                    "")
                desc-midi))
    (if (equal? desc "") (set! desc "(not documented)"))

    (make <texi-node>
      #:name name
      #:text
      (string-append
       desc
       (if (pair? aliases)
	   (string-append
	    "\n\nThis context also accepts commands for the following context(s):\n\n"
	    (human-listify (sort aliases ly:string-ci<?))
	    ".")
	   "")

       "\n\nThis context creates the following layout object(s):\n\n"
       (human-listify (uniq-list grob-refs))
       "."

       (if (and (pair? props) (not (null? props)))
	   (let ((str (apply string-append
		             (sort (map document-property-operation props)
			           ly:string-ci<?))))
	     (if (string-null? str)
		 ""
		 (string-append
		  "\n\nThis context sets the following properties:\n\n"
		  "@itemize @bullet\n"
		  str
		  "@end itemize\n")))
	   "")

       (if (null? accepts)
	   "\n\nThis context is a `bottom' context; it cannot contain other contexts."
	   (string-append
	    "\n\nContext "
	    name
	    " can contain\n"
	    (human-listify (map ref-ify (sort (map symbol->string accepts)
					      ly:string-ci<?)))
	    "."))

       (if (null? consists)
	   ""
	   (string-append
            "\n\nThis context is built from the following translator(s):"
	    (description-list->texi
             (map document-translator-by-name (sort consists ly:symbol-ci<?))
	     #t)))))))

(define (engraver-grobs grav)
  (let* ((eg (if (symbol? grav)
		 (find-translator-by-name grav)
		 grav)))
    (if (eq? eg #f)
	'()
	(map symbol->string (assoc-get 'grobs-created (ly:translator-description eg))))))

(define (context-grobs context-desc)
  (let* ((group (assq-ref context-desc 'group-type))
	 (consists (append
		    (if group
			(list group)
			'())
                    (assoc-get 'consists context-desc '())))
	 (grobs  (apply append
			(map engraver-grobs consists))))
    grobs))

(define (all-contexts-doc)
  (let* ((layout-alist (ly:output-description $defaultlayout))
         (midi-alist (ly:output-description $defaultmidi))
         (all-name-syms (delete-duplicates
                         (sort (append (map car layout-alist)
                                       (map car midi-alist))
                               ly:symbol-ci<?)))
         (context-descs-list
          (map (lambda (name-sym)
                 (list
                  name-sym
                  (assoc-get name-sym layout-alist '())
                  (assoc-get name-sym midi-alist '())))
               all-name-syms)))
    (make <texi-node>
      #:name "Contexts"
      #:desc "Complete descriptions of all contexts."
      #:children
      (map context-doc context-descs-list))))

(define all-translators-list 
  (sort (ly:get-all-translators)
        (lambda (a b)
          (ly:symbol-ci<? (ly:translator-name a)
                          (ly:translator-name b)))))

(define all-engravers-list
  (filter engraver? all-translators-list))

(define all-performers-list
  (filter performer? all-translators-list))

(define all-other-translators-list
  (filter (lambda (x) (not (or (engraver? x) (performer? x))))
          all-translators-list))

(define (all-engravers-doc)
  (make <texi-node>
    #:name "Engravers"
    #:desc "All separate engravers."
    #:text "See @ruser{Modifying context plug-ins}."
    #:children
    (map translator-doc all-engravers-list)))

(define (all-performers-doc)
  (make <texi-node>
    #:name "Performers"
    #:desc "All separate performers."
    #:children
    (map translator-doc all-performers-list)))

(define (all-other-translators-doc)
  (make <texi-node>
    #:name "Other translators"
    #:desc "Translators that are neither engravers nor performers."
    #:children
    (map translator-doc all-other-translators-list)))


(define (all-translators-doc)
  (make <texi-node>
    #:name "Translators"
    #:desc "Engravers, performers and other translators."
    #:text "Translators are part of contexts. They @emph{accept} (or
@emph{listen to}) music expressions of certain types delivered by
iterators as @emph{stream events} and process them. Additionally, they
may @emph{acknowledge} output objects produced by other translators.

There are two important translator subclasses:

@itemize @bullet
@item
@strong{Engravers} produce @emph{layout objects}, also known as @emph{grobs},
the building blocks for graphical output.

@item
@strong{Performers} produce @emph{audio items} used for assembling
MIDI output.
@end itemize

@emph{Context properties} control the behaviour of translators."
    #:children
    (list
     (all-engravers-doc)
     (all-performers-doc)
     (all-other-translators-doc))))

(define (translation-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
	 (sortedsyms (map string->symbol ps))
	 (propdescs
	  (map
	   (lambda (x) (property->texi 'translation  x '()))
	   sortedsyms))
	 (texi (description-list->texi propdescs #f)))
    texi))

(define (translation-doc-node)
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout."
    #:children
    (list
     (all-contexts-doc)
     (all-translators-doc)
     (make <texi-node>
       #:name "Tunable context properties"
       #:desc "All tunable context properties."
       #:text (translation-properties-doc-string
	       all-user-translation-properties))

     (make <texi-node>
       #:name "Internal context properties"
       #:desc "All internal context properties."
       #:text (translation-properties-doc-string
	       all-internal-translation-properties)))))
