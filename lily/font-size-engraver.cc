/*   
  font-size-grav.cc --  implement Font_size_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "font-size-engraver.hh"
#include "score-element.hh"

Font_size_engraver::Font_size_engraver ()
{
  size_i_ = 0;
}

void
Font_size_engraver::do_process_requests ()
{
  Scalar s (get_property ("fontsize"));
  
  if (s.length_i ()  && s.isnum_b ())
    {
      size_i_ = int (s);
    }
}

void
Font_size_engraver::acknowledge_element (Score_element_info e)
{
  e.elem_l_->size_i_ = size_i_;
}

ADD_THIS_TRANSLATOR (Font_size_engraver);
IMPLEMENT_IS_TYPE_B1(Font_size_engraver,Engraver);
