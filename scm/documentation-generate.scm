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

;; Scheme entry point for generated documentation. Running LilyPond on
;; ly/generate-documentation.ly, which loads this file, generates the
;; Internals Reference, as well as some texinfo include files for
;; use in the Notation Manual.

;;; Code:

(use-modules
 (oop goops)
 (scm texinfo-generation)
 (scm document-context-mods)
 (scm document-functions)
 (scm document-identifiers)
 (scm document-markup)
 (scm document-type-predicates)
 ((scm document-internals-docclasses) #:select (set-node-ref-type))
 ((scm document-internals-nodestructure)
  #:select (tunable-grob-properties-doc
            tunable-context-properties-doc))
 (scm document-internals))


;;; Generate the Internals Reference

(write-texi-file (get-internals-reference-texidoc parser))


;;; Automatically generated material for Notation Manual appendices

;; Do not include the initial @node and @appendixsec commands into
;; these .tely files. Not explicitly having these headers in
;; notation-appendices.itely has proven inconvenient for documentation
;; writers and translators in the past.

(set-node-ref-type 'external)

(display
 (slot-ref (all-scheme-functions-doc) 'text)
 (open-output-file "scheme-functions.tely"))

(headless-texi-dump
 (markup-doc-node)
 (open-output-file "markup-commands.tely"))

(display
 (markup-list-doc-string)
 (open-output-file "markup-list-commands.tely"))

(display
 type-predicates-doc-string
 (open-output-file "type-predicates.tely"))

(display
 (identifiers-doc-string)
 (open-output-file "identifiers.tely"))

(display
 (context-mods-doc-string)
 (open-output-file "context-mod-identifiers.tely"))

(display
 (texi-table-string tunable-grob-properties-doc)
 (open-output-file "layout-properties.tely"))

(display
 (texi-table-string tunable-context-properties-doc)
 (open-output-file "context-properties.tely"))
