\version "1.7.16"

\header {
texidoc  = " Ez-notation prints names in note heads.
You also get ledger lines, of course."
}

\include "paper26.ly"
\paper { \paperTwentysix }

\score {
        \notes { c'2 e'4 f' | g'1 b8 }
        \paper { \translator { \EasyNotation }
		 raggedright = ##t
	     } 
}
%% new-chords-done %%
