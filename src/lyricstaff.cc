#include "musicalrequest.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "lyricstaff.hh"
#include "lyriccolumn.hh"
#include "lyricwalker.hh"
#include "pscore.hh"

Lyric_staff::Lyric_staff()
{
    pstaff_l_=0;
}

Staff_column*
Lyric_staff::create_col()
{
    return new Lyric_column(this);
}

void
Lyric_staff::set_output(PScore*pscore_l)
{
    pstaff_l_ = new PStaff(pscore_l);
    pscore_l_ = pscore_l;
    pscore_l_->add(pstaff_l_);
}

Staff_walker*
Lyric_staff::get_walker_p()
{
    return new Lyric_walker(this);
}
