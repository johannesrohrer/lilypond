\header{
filename =	"title.ly";
title =		"Title";
subtitle =	"Subtitle";
composer=	"Composer (xxxx-yyyy)";
arranger =	"Arranger";
copyright = 	"public domain";
enteredby = 	"jcn";
source = 	"urtext";
instrument=	"Instrument";
}

\version "1.0.0";

\score{
	\melodic
	  \relative c'{
		c' d e f f e d c \break
		c d e f f e d c
	}
	\header{
	opus = 		"Opus 0";
	piece =		"Piece I";
	override="Overdriven";
	}
}


\score{
	\melodic
	\relative c' {
	    f' e d c c d e f \break
	    f e d c c d e f
	}
	\header{ 
	piece =	"Piece II"; 
	opus = 		"Opus 1";
	}
}

