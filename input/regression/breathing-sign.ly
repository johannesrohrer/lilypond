\version "1.7.16"
\header{
texidoc="
Breathing signs, also used for phrasing, do normally not influence
global spacing -- only if space gets tight, notes are shifted to make
room for the breathing sign. Breathing signs break beams running
through their voice. In the following example, the notes in the first
two measures all have the same distance from each other.

Breathing signs are available in different tastes: commas (default),
ticks, vees and `railroad tracks' (caesura).

Gregorian chant notation sometimes also uses commas and ticks, but in
smaller font size (we call it 'virgula' and 'caesura').  However, the
most common breathing signs are divisio minima/maior/maxima and
finalis, the latter three looking similar to bar glyphs.

" }

\include "gregorian-init.ly"

\score {
  \notes \relative c' {
    \key es \major \time 3/4

% this bar contains no \breathe
    < \context Voice = two { \stemDown es4 bes es }
      \context Voice = one { \stemUp g4 as g }
    > |

% by default, \breathe uses the rcomma, just as if saying:
% \property Voice.BreathingSign \set #'text = #"scripts-rcomma"
    < \context Voice = two { \stemDown es4 \breathe bes es }
      \context Voice = one { \stemUp g4 as g }
    > |

% rvarcomma and lvarcomma are variations of the default rcomma and lcomma
    % N.B.: must use Staff context here, since we start a Voice below
    \property Staff.BreathingSign \override #'text = #"scripts-rvarcomma"
    < \context Voice = two { \stemDown es4 \breathe bes es }
      \context Voice = one { \stemUp g4 as g }
    > |

% wedge
    \property Voice.BreathingSign \override #'text = #"scripts-upbow"
    es8 d es f g8 \breathe f |

% caesura
    \property Voice.BreathingSign \set #'text = #"scripts-caesura"
    [es8 d] \breathe [es f g f] |
    es2 r4 \bar "||" \break

%
% Gregorian stuff:
%

% we turn bars off for Gregorian stuff
    \property Staff.BarLine \override #'transparent = ##t

% this bar contains no \breathe
    < \context Voice = two { \stemDown es4 bes es }
      \context Voice = one { \stemUp g4 as g }
    > |

% \virgula applies rcomma, but in a smaller font
    < \context Voice = two { \stemDown es4 \virgula bes es }
      \context Voice = one { \stemUp g4 as g }
    > |

% \caesura applies rvarcomma, but in a smaller font
    < \context Voice = two { \stemDown es4 \caesura bes es }
      \context Voice = one { \stemUp g4 as g }
    > |

% \divisiominima is a simple vertical stroke through the uppermost
% staffline, just like the original implementation of breathing signs.
    < \context Voice = two { \stemDown es4 \divisiominima bes es }
      \context Voice = one { \stemUp g4 as g }
    > |
% \divisio{maior,maxima} and \finalis look like bars and are vertically
% centered on the staff; the direction property has no effect
    < \context Voice = two { \stemDown es4 \divisiomaior bes es }
      \context Voice = one { \stemUp g4 as g }
    > |
    < \context Voice = two { \stemDown es4 \divisiomaxima bes es }
      \context Voice = one { \stemUp g4 as g }
    > |

% this one looks almost like a "||" type bar
    \finalis
  }
}
