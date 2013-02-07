\version "2.17.11"

\header {

  texidoc = "Grobs whose parents have @code{outside-staff-priority} set
should figure into the vertical skyline of the @code{VerticalAxisGroup}
such that grobs with a higher @code{outside-staff-priority} are correctly
positioned above them.
"

}

\relative c'' {
   \override TupletBracket.outside-staff-priority = #1
   \override TupletNumber.font-size = #5
   \tuplet 3/2 { a4\trill a\trill^"foo" a\trill }
}