/*   
  align-note-column-engraver.cc --  implement Align_note_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grace-align-item.hh"
#include "note-column.hh"
#include "local-key-item.hh"
#include "warn.hh"
#include "directional-element-interface.hh"
#include "side-position-interface.hh"

/**
   Catch notes, and put them in a row. Used for aligning grace notes.
 */
class Align_note_column_engraver: public Engraver
{
  Grace_align_item * align_item_p_;
  Note_column * now_column_l_;
  Local_key_item * accidental_l_;

  virtual void process_acknowledged ();
  virtual void do_post_move_processing ();
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
public:
  VIRTUAL_COPY_CONS(Translator);
  Align_note_column_engraver ();
};

Align_note_column_engraver::Align_note_column_engraver()
{
  align_item_p_ =0;
  now_column_l_ =0;
  accidental_l_ =0;
}

void
Align_note_column_engraver::do_creation_processing ()
{
  align_item_p_ = new Grace_align_item;
  side_position (align_item_p_).set_axis (X_AXIS);
  side_position (align_item_p_).set_direction (LEFT);  
  // needed  for setting font size.
  announce_element (Score_element_info (align_item_p_, 0));
}

void
Align_note_column_engraver::do_removal_processing ()
{
  SCM al = get_property ("graceAlignPosition");
  if (isdir_b (al))
    {
      Direction d = to_dir (al);
      directional_element (align_item_p_).set (d);
      align_item_p_->set_elt_property ("align-dir", to_dir (-d));
    }
  
  typeset_element (align_item_p_);
  align_item_p_ =0;
}

void
Align_note_column_engraver::acknowledge_element (Score_element_info inf)
{
  if (Note_column * n = dynamic_cast<Note_column*> (inf.elem_l_))
    {
      now_column_l_ =n;
    }
  else if (Local_key_item * it = dynamic_cast<Local_key_item*> (inf.elem_l_))
    {
      accidental_l_ = it;
    }
}
void
Align_note_column_engraver::process_acknowledged ()
{
  if (now_column_l_ && accidental_l_)
    {
      
      /* Can't inspect  width of Local_key_item, since

	 A. it may not be fully built

	 B. it has no pscore_l_ field.

      */
      SCM grsp = get_property ("graceAccidentalSpace");
      if (gh_number_p(grsp))
	{
	  /*
	    ugh.
	  */
	  Real extra_space = gh_scm2double(grsp);
	  SCM e = gh_cons (gh_double2scm (-extra_space), gh_double2scm (0.0));
	  now_column_l_->set_elt_property ("extra-space", e);
	}
    }

  if (now_column_l_)
    {
      align_item_p_->add_element (now_column_l_);
      now_column_l_ =0;
    }
}

void
Align_note_column_engraver::do_post_move_processing ()
{
  now_column_l_ =0;
  accidental_l_ =0;
}

ADD_THIS_TRANSLATOR(Align_note_column_engraver);

