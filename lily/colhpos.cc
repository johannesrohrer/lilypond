/*
  colhpos.cc -- implement Column_x_positions

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "colhpos.hh"
#include "real.hh"
#include "debug.hh"
#include "vector.hh"
#include "line-spacer.hh"

Column_x_positions::Column_x_positions()
{
  energy_f_ = infinity_f;
  ugh_b_ = false;
  satisfies_constraints_b_ = false;
  spacer_l_ =0;
}

Column_x_positions::~Column_x_positions()
{

}

void
Column_x_positions::add_paper_column (Paper_column*c)
{
  cols.push (c);
}

void
Column_x_positions::print() const
{
#ifndef NPRINT
  DOUT << "energy : " << energy_f_ << '\n';
  DOUT << "line of " << config.size() << " cols\n";
  Vector v (config);
  DOUT << v;
#endif
}

void
Column_x_positions::OK() const
{
#ifndef NDEBUG
  assert (config.size() == cols.size ());
#endif
}

void
Column_x_positions::set_stupid_solution(Vector v)
{
  energy_f_ = infinity_f;
  ugh_b_ = true;
  config = v;
}

void
Column_x_positions::stupid_solution()
{
  set_stupid_solution (spacer_l_->default_solution());
}

void
Column_x_positions::solve_line() 
{
  spacer_l_->solve (this);
}


void
Column_x_positions::approximate_solve_line() 
{
  spacer_l_->lower_bound_solution (this);
}


