/*
  template8.cc -- instantiate audio List classes

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "midi-item.hh"
#include "cursor.tcc"
#include "pcursor.tcc"
#include "plist.tcc"

POINTERLIST_INSTANTIATE(Audio_element);
POINTERLIST_INSTANTIATE(Audio_column);
POINTERLIST_INSTANTIATE(Midi_event);
