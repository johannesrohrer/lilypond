%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.62"

\header {
  lsrtags = "expressive-marks, text, tweaks-and-overrides"

  texidoces = "
Las instrucciones @code{\\startTextSpan} y @code{\\stopTextSpan}
permiten la creación de elementos de extensión textuales tan
fácilmente como indicaciones de pedal u
octavaciones. Sobreescribimos ciertas propiedades del objeto
@code{TextSpanner} para modificar su salida.

"
  doctitlees = "Crear elementos de extensión textuales"

  texidoc = "
The @code{\\startTextSpan} and @code{\\stopTextSpan} commands allow the
creation of text spanners as easily as pedal indications or
octavations. Override some properties of the @code{TextSpanner} object
to modify its output.

"
  doctitle = "Creating text spanners"
} % begin verbatim
\relative c'' {
  \override TextSpanner  #'edge-text = #'("bla" . "blu")
  a \startTextSpan
  b c
  a \stopTextSpan
  
  \override TextSpanner  #'dash-period = #2
  \override TextSpanner  #'dash-fraction = #0.0
  a \startTextSpan
  b c
  a \stopTextSpan
  
  \revert TextSpanner #'style
  \override TextSpanner  #'style = #'dashed-line \override TextSpanner #'bound-details #'left #'text = \markup { \draw-line #'(0 . 1) }
 \override TextSpanner #'bound-details #'right #'text = \markup { \draw-line #'(0 . -2) }

  a \startTextSpan
  b c
  a \stopTextSpan
  
  \set Staff.middleCPosition = #-13
  \override TextSpanner  #'dash-period = #10
  \override TextSpanner  #'dash-fraction = #0.5
  \override TextSpanner  #'thickness = #10
  a \startTextSpan
  b c
  a \stopTextSpan
  \set Staff.middleCPosition = #-6
}
