;;;; c++.scm -- implement Scheme frontends to C++ functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;; Note: this file can't be used without LilyPond executable

(define (number-pair?  x)
  (and (pair? x)
       (number? (car x)) (number? (cdr x))))
(define (number-or-grob? x)
  (or (ly-grob? x) (number? x))
  )

(define (grob-list? x)
  (list? x))

(define (moment-pair?  x)
  (and (pair? x)
       (moment? (car x)) (moment? (cdr x))))

(define (boolean-or-symbol? x)
  (or (boolean? x) (symbol? x)))

(define (number-or-string? x)
  (or (number? x) (string? x)))

(define (markup? x)
  (or (string? x) (list? x)))

(define (scheme? x) #t)

(define type-p-name-alist
  `(
   (,dir? . "direction")
   (,scheme? . "any type")
   (,number-pair? . "pair of numbers")
   (,ly-input-location? . "input location")   
   (,ly-grob? . "grob (GRaphical OBject)")
   (,grob-list? . "list of grobs")
   (,duration? . "duration")
   (,pair? . "pair")
   (,integer? . "integer")
   (,list? . "list")
   (,symbol? . "symbol")
   (,string? . "string")
   (,boolean? . "boolean")
   (,moment? . "moment")
   (,ly-input-location? . "input location")
   (,music-list? . "list of music")
   (,music? . "music")
   (,number? . "number")
   (,char? . "char")
   (,input-port? . "input port")
   (,output-port? . "output port")   
   (,vector? . "vector")
   (,procedure? . "procedure") 
   (,boolean-or-symbol? . "boolean or symbol")
   (,number-or-string? . "number or string")
   (,markup? . "markup (list or string)")
   (,number-or-grob? . "number or grob")
   ))


(define (match-predicate obj alist)
  (if (null? alist)
      "Unknown type"
      (if (apply (caar alist) obj)
	  (cdar alist)
	  (match-predicate obj (cdr alist))
	  )
      ))

(define (object-type obj)
  (match-predicate obj type-p-name-alist))

(define (type-name  predicate)
  (let ((entry (assoc predicate type-p-name-alist)))
    (if (pair? entry) (cdr entry)
	"unknown"
	)))

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))


;; used in denneboom.ly
(define (cons-map f x)
  (cons (f (car x)) (f (cdr x))))

;; used where?
(define (reduce operator list)
  "reduce OP [A, B, C, D, ... ] =
   A op (B op (C ... ))
"
      (if (null? (cdr list)) (car list)
	  (operator (car list) (reduce operator (cdr list)))))



(define (take-from-list-until todo gathered crit?)
  "return (G, T), where (reverse G) + T = GATHERED + TODO, and the last of G
is the  first to satisfy CRIT "
  (if (null? todo)
      (cons gathered todo)
      (if (crit? (car todo))
	  (cons (cons (car todo) gathered) (cdr todo))
	  (take-from-list-until (cdr todo) (cons (car todo) gathered) crit?)
      )
  ))
; test:
; (take-from-list-until '(1 2 3  4 5) '() (lambda (x) (eq? x 3)))
; ((3 2 1) 4 5)



; Make a function that checks score element for being of a specific type. 
(define (make-type-checker symbol)
  (lambda (elt)
    ;;(display  symbol)
    ;;(eq? #t (ly-get-grob-property elt symbol))
    (not (eq? #f (memq symbol (ly-get-grob-property elt 'interfaces))))))


(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define (repeat-name-to-ctor name)
  (let*
      ((supported-reps
	`(("volta" . ((iterator-ctor . ,Volta_repeat_iterator::constructor)
		      (start-moment-function .  ,Repeated_music::first_start)
		      (length . ,Repeated_music::volta_music_length)))
	  
	    ("unfold" . ((iterator-ctor . ,Unfolded_repeat_iterator::constructor)
			 (start-moment-function .  ,Repeated_music::first_start)			 
			 (length . ,Repeated_music::unfolded_music_length)))
	    ("fold" . ((iterator-ctor  . ,Folded_repeat_iterator::constructor)
		       (start-moment-function .  ,Repeated_music::minimum_start)			 
		       (length . ,Repeated_music::folded_music_length)))
	    ("percent" . ((iterator-ctor . ,Percent_repeat_iterator::constructor)
			  (start-moment-function .  ,Repeated_music::first_start)
			  (length . ,Repeated_music::unfolded_music_length)))
	    ("tremolo" . ((iterator-ctor . ,Chord_tremolo_iterator::constructor)
			  (start-moment-function .  ,Repeated_music::first_start)

			  ;; the length of the repeat is handled by shifting the note logs
			  (length . ,Repeated_music::folded_music_length)))))
	  
       (handle (assoc name supported-reps)))

    (if (pair? handle)
	(cdr handle)
	(begin
	  (ly-warn
	   (string-append "Unknown repeat type `" name "'\nSee scm/c++.scm for supported repeats"))
	  '(type . 'repeated-music)))))
