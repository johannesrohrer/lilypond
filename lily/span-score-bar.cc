/*
  span-score-bar.cc -- implement Span_score_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "span-score-bar.hh"
#include "atom.hh"
#include "paper-def.hh"
#include "lookup.hh"

Span_score_bar::Span_score_bar()
{
  type_str_ = "|";
}

void
Span_score_bar::do_pre_processing()
{
  Span_bar::do_pre_processing();
  
  if (break_status_i() != 1) 
    {
      set_empty (true);
      transparent_b_ = true;
    }
}


Atom
Piano_brace::get_bar_sym (Real dy) const
{
  return paper()->lookup_l ()->vbrace (dy);
}
Interval
Piano_brace::do_width() const
{
  return Interval (0,0);
}


IMPLEMENT_IS_TYPE_B1(Span_score_bar, Span_bar);
IMPLEMENT_IS_TYPE_B1(Piano_brace, Span_score_bar);

  
