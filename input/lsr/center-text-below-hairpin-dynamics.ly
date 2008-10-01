%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.62"

\header {
  lsrtags = "expressive-marks, text"

  texidoces = "
Este ejemplo proporciona una función para tipografiar un regulador
con texto por debajo, como \"molto\" o \"poco\". El ejemplo
ilustra también cómo modificar la manera en que se imprime
normalmente un objeto, utilizando código de Scheme.

"
  doctitlees = "Centrar texto debajo de un regulador"

  texidoc = "
This example provides a function to typeset a hairpin (de)crescendo
with some additional text below it, such as \"molto\" or \"poco\". The
example also illustrates how to modify the way an object is normally
printed, using some Scheme code.  

"
  doctitle = "Center text below hairpin dynamics"
} % begin verbatim
hairpinWithCenteredText = #(define-music-function (parser location text) (markup?)
#{
  \override Voice.Hairpin #'stencil = #(lambda (grob)
  (ly:stencil-aligned-to
   (ly:stencil-combine-at-edge
    (ly:stencil-aligned-to (ly:hairpin::print grob) X CENTER)
    Y DOWN
    (ly:stencil-aligned-to (ly:text-interface::print grob) X CENTER))
   X LEFT))
  \override Voice.Hairpin #'text = $text
#})

hairpinMolto = \hairpinWithCenteredText \markup { \italic molto }
hairpinMore = \hairpinWithCenteredText \markup { \bigger moltissimo }

\layout { ragged-right = ##f }

{
  \hairpinMolto c'2\< c'\f
  \hairpinMore  c'2\< c'\f
}
