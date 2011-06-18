%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.14.0"

\header {
  lsrtags = "simultaneous-notes, chords, keyboards"

%% Translation of GIT committish: 615cbf212fdaf0b220b3330da417d0c3602494f2
  texidoces = "
Los «clusters» o racimos son un mecanismo para indicar la
interpretación de un ámbito de notas al mismo tiempo.

"
  doctitlees = "Clusters («racimos»)"

  texidoc = "
Clusters are a device to denote that a complete range of notes is to be
played.

"
  doctitle = "Clusters"
} % begin verbatim

fragment = \relative c' {
  c4 f <e d'>4
  <g a>8 <e a> a4 c2 <d b>4
  e2 c
}

<<
  \new Staff \fragment
  \new Staff \makeClusters \fragment
>>

