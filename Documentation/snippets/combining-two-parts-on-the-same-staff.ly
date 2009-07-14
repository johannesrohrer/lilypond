%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.13.1"

\header {
  lsrtags = "simultaneous-notes, text"

%% Translation of GIT committish: 04ab6c269b4067b6ed334d460339818d90513396
  texidoces = "
La herramienta de combinación de partes ( instrucción
@code{\\partcombine}) permite la combinación de varias partes
diferentes sobre el mismo pentagrama.  Las indicaciones textuales
tales como \"solo\" o \"a2\" se añaden de forma predeterminada;
para quitarlas, sencillamente establezca la propiedad
@code{printPartCombineTexts} al valor \"falso\".  Para partituras
vocales (como himnos), no hay necesidad de añadir los textos
\"solo\" o \"a2\", por lo que se deben desactivar.  Sin embargo,
podría ser mejor no usarlo si hay solos, porque éstos no se
indicarán.  En tales casos podría ser preferible la notación
polifónica estándar.

Este fragmento de código presenta las tres formas en que se pueden
imprimir dos partes sobre un solo pentagrama: polifonía estándar,
@code{\\partcombine} sin textos, y @code{\\partcombine} con
textos.

"
  doctitlees = "Combinar dos partes sobre el mismo pentagrama"

%% Translation of GIT committish: 0364058d18eb91836302a567c18289209d6e9706
  texidocde = "
Die Funktion, die Stimmen kombiniert (also der @code{\\partcombine}-Befehl)
ermöglicht die Kombination unterschiedlicher Stimmen auf einem
System.  Textanweisungen wie \"solo\" or \"a2\" werden automatisch
hinzugefügt.  Wenn man sie entfernen will, muss man die Eigenschaft
@code{printPartCombineTexts} auf flasch setzen.  Für Klavierauszüge
muss natürlich kein \"solo\"/\"a2\" usw. hinzugefügt werdne, man
sollte sie also ausschalten.  Wenn aber Solo-Stellen in einem
Klavierauszug oder einer Chorpartitur angezeigt werden, ist es besser,
normale Polyphonie zu verwenden, weil so die Solostellen angezeigt
werden, auch wenn der Text des Stimmenkombinierers ausgeschaltet ist.

Der Schnipsel zeigt drei Möglichkeiten, Stimmen auf einem System zu
kombinieren: Standardpolyphonie, @code{\\partcombine} ohne Text und
@code{\\partcombine} mit Text.

"
  doctitlede = "Zwei Stimmen auf einem System kombinieren"

  texidoc = "
The part combiner tool ( @code{\\partcombine} command ) allows the
combination of several different parts on the same staff.  Text
directions such as \"solo\" or \"a2\" are added by default; to remove
them, simply set the property @code{printPartCombineTexts} to
\"false\". For vocal scores (hymns), there is no need to add
\"solo\"/\"a2\" texts, so they should be switched off.  However, it
might be better not to use it if there are any solos, as they won't be
indicated.  In such cases, standard polyphonic notation may be
preferable.

This snippet presents the three ways two parts can be printed on a same
staff: standard polyphony, @code{\\partcombine} without texts, and
@code{\\partcombine} with texts.




"
  doctitle = "Combining two parts on the same staff"
} % begin verbatim

musicUp = \relative c'' {
  \time 4/4
  a4 c4.( g8) a4 |
  g4 e' g,( a8 b) |
  c b a2.
}

musicDown = \relative c'' {
  g4 e4.( d8) c4 |
  r2 g'4( f8 e) |
  d2 \stemDown a
}

\score {
  <<
    <<
    \new Staff {
      \set Staff.instrumentName = "Standard polyphony  "
      << \musicUp \\ \musicDown >>
    }
    \new Staff \with { printPartCombineTexts = ##f } {
      \set Staff.instrumentName = "PartCombine without texts  "
      \partcombine \musicUp \musicDown
    }
    \new Staff {
      \set Staff.instrumentName = "PartCombine with texts  "
      \partcombine \musicUp \musicDown
    }
    >>
  >>
  \layout {
    indent = 6.0\cm
    \context {
      \Score
      \override SystemStartBar #'collapse-height = #30
    }
  }
}
