\header {

  texidoc = "Stem directions influence positioning of whole note
  tremolo beams."

}

\version "2.16.0"
\paper{
  ragged-right = ##t
}

\relative c {
  \stemDown
  \repeat tremolo 16 { d32 a'32 }
}
