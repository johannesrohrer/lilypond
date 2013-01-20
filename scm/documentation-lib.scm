;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2013 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Johannes Rohrer <src@johannesrohrer.de>
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

(use-modules
 (oop goops)
 (srfi srfi-13)
 (srfi srfi-1)
 ((ice-9 regex) #:select (regexp-substitute/global))
 (scm texinfo-generation))


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
