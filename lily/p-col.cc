/*
  p-col.cc -- implement Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "p-col.hh"
#include "p-score.hh"
#include "debug.hh"

void
Paper_column::add_rod (Paper_column * p, Real d)
{
  Direction dir =  Direction (sign (p->rank_i ()  - rank_i ()));
  
  if (!dir)
    {
      warning ("Must set minimum distance between differing columns. [PROGRAMMING ERROR]");
      return;
    }
  
  for (int i=0; i < minimal_dists_arr_drul_[dir].size (); i++)
    {
      Column_rod &rod = minimal_dists_arr_drul_[dir][i];
      if (rod.other_l_ == p)
	{
	  rod.distance_f_ = rod.distance_f_ >? d;
	  return ;
	}
    }

  Column_rod cr;
  cr.distance_f_ = d;
  cr.other_l_ = p;

  minimal_dists_arr_drul_[dir].push (cr);
}

int
Paper_column::rank_i() const
{
  return rank_i_;
}

void
Paper_column::set_rank (int i)
{
  rank_i_ = i;
}

void
Paper_column::do_print() const
{
#ifndef NPRINT
  DOUT << "rank: " << rank_i_ << '\n';
  for (int i=0; i < minimal_dists_arr_drul_[LEFT].size (); i++)
    {
      minimal_dists_arr_drul_[LEFT][i].print ();
    }
  DOUT << "Right: ";
  for (int i=0; i < minimal_dists_arr_drul_[RIGHT].size (); i++)
    {
      minimal_dists_arr_drul_[RIGHT][i].print ();
    }
  Item::do_print ();
#endif 
}

bool
Paper_column::breakpoint_b() const
{
  return !line_l_;
}

Paper_column::Paper_column()
{
  set_axes (X_AXIS, X_AXIS);

  line_l_=0;
  rank_i_ = -1;
}

Line_of_score*
Paper_column::line_l() const
{
  return line_l_;
}




Paper_column*
Paper_column::column_l () const
{
  return (Paper_column*)(this);
}


void
Paper_column::preprocess ()
{
  minimal_dists_arr_drul_[LEFT].sort (Column_rod::compare);
  minimal_dists_arr_drul_[RIGHT].sort (Column_rod::compare);  
}
