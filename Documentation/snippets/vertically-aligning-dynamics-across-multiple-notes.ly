%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.14.0"

\header {
  lsrtags = "expressive-marks"

%% Translation of GIT committish: 615cbf212fdaf0b220b3330da417d0c3602494f2
  texidoces = "
Las expresiones dinámicas que se comienzan, terminan o se producen
en la misma nota se alinean verticalmente.  Para asegurar que las
expresiones dinámicas se alinean cuando no se producen sobre la
misma nota, incremente la propiedad @code{staff-padding} del
objeto @code{DynamicLineSpanner}.

"
  doctitlees = "Alinear verticalmente expresiones dinámicas que abarcan varias notas"

  texidoc = "
Dynamics that occur at, begin on, or end on the same note will be
vertically aligned.  To ensure that dynamics are aligned when they do
not occur on the same note, increase the @code{staff-padding} property
of the @code{DynamicLineSpanner} object.

"
  doctitle = "Vertically aligning dynamics across multiple notes"
} % begin verbatim

\relative c' {
  \override DynamicLineSpanner #'staff-padding = #4
  c2\p f\mf
  g2\< b4\> c\!
}

