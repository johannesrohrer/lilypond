/*
  hara-kiri-vertical-group-spanner.hh -- declare Har_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
#define HARA_KIRI_VERTICAL_GROUP_SPANNER_HH

#include "axis-group-spanner.hh"

/** 
  As Vertical_group_spanner, but keep track of interesting items.  If
  we don't contain any interesting items after linebreaking, then
  gracefully commit suicide.  Objective: don't disgrace Lily by
  typesetting empty lines in orchestral scores.  */
class Hara_kiri_group_spanner : public Axis_group_spanner
{
public:
  Hara_kiri_group_spanner ();
  virtual void after_line_breaking ();
  void add_interesting_item (Item* n);
protected:
  VIRTUAL_COPY_CONS(Score_element);
};


#endif // HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
