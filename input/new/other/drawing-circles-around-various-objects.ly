\version "2.11.23"

\header { texidoc = "
The \circle command allows you to draw circles around various objects
(for example fingering indications). However, some objects require
specific tweaks: rehearsal marks depend on the Score.markFormatter
context, bar numbers on the Score.BarNumber context, and so on.

You can tweak the printing of your circles by setting some properties
such as #'thickness, #'circle-padding or #'font-size.
" }

\relative c'{
c1
\set Score.markFormatter
  = #(lambda (mark context)
             (make-circle-markup (format-mark-numbers mark context)))
\mark \default
c2 d^\markup{\circle \finger "2"}
\override Score.BarNumber #'break-visibility = #all-visible
\override Score.BarNumber  #'stencil
  = #(make-stencil-circler 0.1 0.25 ly:text-interface::print)
}
