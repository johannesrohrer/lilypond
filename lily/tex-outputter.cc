/*
  tex-outputter.cc -- implement Tex_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "tex-outputter.hh"
#include "tex-stream.hh"
#include "molecule.hh"
#include "atom.hh"
#include "array.hh"
#include "dimension.hh"
#include "tex.hh"
#include "string-convert.hh"
#include "debug.hh"

Tex_outputter::Tex_outputter (Tex_stream *s)
{
  outstream_l_ = s;
}

/*
   26 fonts ought to be enough for anyone.
*/
static String
tex_font_command(int i)
{
  return "\\font"  +String_convert::form_str ("%c",  'A' + i) ;
}

void
Tex_outputter::switch_to_font (String fontname)
{
  if (!fontname.length_i () || fontname == current_font_)
    return;

  current_font_ = fontname;
  int i=0;
  for (; i< font_arr_.size (); i++)
    if (font_arr_[i] == fontname)
      {
	*outstream_l_ <<tex_font_command (i) << "\n";
	return ;
      }

  
  font_arr_.push (fontname);
  *outstream_l_ << "\\font"  + tex_font_command (i) << "=" + fontname << "\n";
  *outstream_l_<< tex_font_command (i);
}

void
Tex_outputter::output_molecule (Molecule const*m, Offset o, char const *nm)
{
  if (check_debug)
    *outstream_l_ << String ("\n%start: ") << nm << "\n";

  for (PCursor <Atom*> i (m->atoms_); i.ok (); i++)
    {
      Offset a_off = i->offset ();
      a_off += o;

      switch_to_font (i->font_);

      Array<String> a;
      String r;
  
    
      String s ("\\placebox{%}{%}{%}");
      a.push (print_dimen (a_off.y()));
      a.push (print_dimen (a_off.x()));
      a.push (i->tex_);
      r += substitute_args (s, a);
      *outstream_l_ << r;
    }
}


void
Tex_outputter::start_line ()
{
  *outstream_l_ << "\\hbox{%\n";
}

void
Tex_outputter::stop_line ()
{
  *outstream_l_ << "}";
  *outstream_l_ << "\\interscoreline";
  current_font_ = "";
  font_arr_.clear ();
}
