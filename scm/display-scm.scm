;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2013 Johannes Rohrer <src@johannesrohrer.de>
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

;; Provide simple scheme expression pretty-printing for automatically
;; generated documentation.
;;
;; The main reason for defining a scm->string function on our own,
;; instead of using plain write, display or (ice-9 pretty-print), is
;; to avoid confusing users with the #<procedure .. > syntax.

(define-module (scm display-scm)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:export (scm->string))


(define (nested-map func expr)
  "Apply FUNC to the innermost elements of the nested list EXPR."
  (if (not (list? expr))
      (func expr)
      (map (lambda (el) (nested-map func el)) expr)))

(define (nest-level nlst)
  (cond ((not (list? nlst)) 0)
        ((null? nlst) 1)
        (else (1+ (apply max (map nest-level nlst))))))

(define (nested-string-list->string lst multiline? width)
  ;; Helper function, to be applied to a nested list AFTER
  ;; its innermost elements have been turned into strings.
  ;;
  ;; Intended functionality, by examples:
  ;;
  ;; ("s1" "s2" "s3")
  ;; --> single line or multiline with width>=11:
  ;;     "'(s1 s2 s3)"
  ;; --> multiline, width<11:
  ;;     "'(s1\ns2\ns3)", i.e.
  ;;     |'(s1
  ;;     |  s2
  ;;     |  s3)
  ;;
  ;; (("a" ("bcd" ("\"foo\"" "bar" "baz") ("1" "2"))) ("e" ("f" "g" "h")))
  ;; --> multiline, width=25:
  ;;     |'((a
  ;;     |   (bcd
  ;;     |    ("foo" bar baz)
  ;;     |    (1 2)))
  ;;     |  (e
  ;;     |   (f
  ;;     |    g
  ;;     |    h)))

  (define (contract-innermost-level nsl)
    (let ((max-level (nest-level nsl)))
      (let recurse ((sublist nsl)
                    (level 0))
        (if (not (list? sublist))
            sublist
            (if (>= level (- max-level 1))
                (string-append "(" (string-join sublist " ") ")")
                (map (lambda (sl) (recurse sl (1+ level))) sublist))))))

  (define (nsl->single-line-string nsl)
    (string-append
     "'"
     (let contract ((less-nested nsl))
       (if (string? less-nested)
           less-nested
           (contract (contract-innermost-level less-nested))))))

  (define (nsl->multiline-string nsl level)
    (string-append
     (if (equal? level 0) "'" "")
     (if (string? nsl)
         ;; this may happen in the call below if contract-to-width
         ;; has put everything into one line
         nsl
         (let* ((line-prefix (make-string (+ 2 level) #\space))
                (el-strings
                 (map (lambda (el)
                        (string-append
                         line-prefix
                         (if (list? el)
                             (nsl->multiline-string el (1+ level))
                             el)))
                      nsl)))
           (string-append
            "("
            (if (null? el-strings)
                ""
                (string-append
                 (string-trim (car el-strings))
                 (if (null? (cdr el-strings))
                     ""
                     (string-append
                      "\n"
                      (string-join (cdr el-strings) "\n")))))
            ")")))))

  (define (total-width nsl)
    (if (string? nsl)
        (string-length nsl)
        (+ 2 (apply max (append (map total-width nsl) (list 0))))))

  (define (contract-to-width nsl width)
    (do ((contracted (contract-innermost-level nsl)
                     (contract-innermost-level contracted)))
        ((or (string? nsl)
             (< width (total-width contracted)))
         nsl)
      (set! nsl contracted)))

  (if (not multiline?)
      (nsl->single-line-string lst)
      (nsl->multiline-string (contract-to-width lst width) 0)))

(define* (scm->string expr
                      #:key
                      (multiline? #t)
                      (width 72)
                      (quote-symbols? #t))
  "Return a string representation of the scheme value EXPR.
In contrast to plain display, pretty-print or similar, this function
avoids confusing users with #<procedure .. > syntax."
  (if (not (list? expr))
      (cond
       ((and (procedure? expr)
             (symbol? (procedure-name expr)))
        (symbol->string (procedure-name expr)))
       ((symbol? expr)
        (string-append
         (if quote-symbols? "'" "")
         (symbol->string expr)))
       ((string? expr)
        (string-append "\"" expr "\""))
       ((pair? expr)
        (string-append "("
                       (scm->string (car expr) #:quote-symbols? #f)
                       " . "
                       (scm->string (cdr expr) #:quote-symbols? #f)
                       ")"))
       (else
        (call-with-output-string (lambda (port) (display expr port)))))
      (nested-string-list->string
       (nested-map (lambda (el) (scm->string el #:quote-symbols? #f))
                   expr)
       multiline?
       width)))