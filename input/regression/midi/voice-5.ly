% Lily was here -- automatically converted by ../../../scripts/midi2ly.py from out-www/voice-4.midi
\version "2.16.0"

\layout {
  \context {
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
    \remove "Rest_engraver"
    \consists "Completion_rest_engraver"
  }
}
\midi {
  \context {
    \Score
    midiChannelMapping = #'instrument
  }
}

% included from ./out-www/voice-5.header
\header {
texidoc="midi2ly still produces output for a staff with five voices.  However, in such cases, most probably the the correct \voiceOne, \voiceX... mapping is lost."
options=""
}
% end

trackAchannelA = {
  
  % [SEQUENCE_TRACK_NAME] control track
  
  % [TEXT_EVENT] creator: 
  
  % [TEXT_EVENT] GNU LilyPond 2.13.54          
  
  \time 4/4 
  
  \tempo 4 = 60 
  
}

trackA = <<
  \context Voice = voiceA \trackAchannelA
>>


trackBchannelA = \relative c {
  \voiceOne
  
  \set Staff.instrumentName = ":1"
  <c''' a >2 b 
  | % 2
  
}

trackBchannelB = \relative c {
  \voiceThree
  c''4. d8 e4 f 
  | % 2
  
}

trackBchannelC = \relative c {
  \voiceFour
  d'1 
  | % 2
  
}

trackBchannelD = \relative c {
  \voiceTwo
  c'4 c2 c4 
  | % 2
  
}

trackBchannelE = \relative c {
  s1 d1
  | % 2
  
}

trackB = <<
  \context Voice = voiceA \trackBchannelA
  \context Voice = voiceB \trackBchannelB
  \context Voice = voiceC \trackBchannelC
  \context Voice = voiceD \trackBchannelD
  \context Voice = voiceE \trackBchannelE
>>


\score {
  <<
    \context Staff=trackB \trackA
    \context Staff=trackB \trackB
  >>
  \layout {}
  \midi {}
}
