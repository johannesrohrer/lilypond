%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.13.1"

\header {
  lsrtags = "expressive-marks, text"

%% Translation of GIT committish: dff50e8e1d3134657a6b6203b9c93826dc4cef65
  texidoces = "
Ciertas indicaciones dinámicas pueden llevar textos (como \"più
forte\" o \"piano subito\"). Se pueden producir usando un
bloque @code{\\markup}.

"
  doctitlees = "Combinar indicaciones dinámicas con marcados textuales"

  texidoc = "
Some dynamics may involve text indications (such as \"più forte\" or
\"piano subito\"). They can be produced using a @code{\\markup} block.

"
  doctitle = "Combining dynamics with markup texts"
} % begin verbatim

piuF = \markup { \italic più \dynamic f }
\layout { ragged-right = ##f }
\relative c'' {
  c2\f c-\piuF
}

