
\header {

  texidoc = "Show tuplet numbers also on single-note tuplets (otherwise the timing would look messed up!), but don't show a bracket. Make sure that tuplets without any notes don't show any number, either."

}
\version "2.17.11"

\paper { ragged-right = ##t
indent = 0.0 }

\new Staff <<
  \new Voice \relative c'' {
    \tuplet 6/4 { c2.:8 } \tuplet 3/2 { g4.:8 } \tuplet 3/2 { a,4.:8 } \tuplet 6/4 {} \bar"|."
  }
>>
  
