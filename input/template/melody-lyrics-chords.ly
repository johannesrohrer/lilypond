\version "1.9.8"

\header {
  texidoc = "Popsong format: chords, melody and lyrics."
}

melody = \notes \relative c' {
  a b c d
}

text = \lyrics {
  Aaa Bee Cee Dee
}

accompaniment = \chords {
  a2 c2
}

\score {
  <<
    \context ChordNames \accompaniment
    \addlyrics
      \context Staff = one {
        \autoBeamOff
        \melody
      }
      \context Lyrics \text
  >>
  \paper { }
  \midi  { }
}
