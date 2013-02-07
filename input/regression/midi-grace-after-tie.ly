\header {

  texidoc = "Tied notes sound as one note in MIDI.  Grace notes
  following a tied note shorten the resulting single note in MIDI."

  % https://code.google.com/p/lilypond/issues/detail?id=3091
}
\version "2.16.0"
\score {
  \relative c' {
    % This first b~ tie should be honoured in the MIDI output:
    a2. b4~
    % i.e. this b8 must not be sounded:
    | b8 \grace c16 d4
  }
  \midi { }
}
