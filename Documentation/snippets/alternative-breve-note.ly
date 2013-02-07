%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.17.11"

\header {
  lsrtags = "expressive-marks, rhythms"

  texidoc = "
This code demonstrates how to use the alternative breve note with two
vertical lines on each side of the notehead instead of one line.

"
  doctitle = "Alternative breve note"
} % begin verbatim

\relative c'' {
  \time 4/2
  c\breve |
  \override Staff.NoteHead.style = #'altdefault
  b\breve
  \revert Staff.NoteHead.style
  a\breve
}
