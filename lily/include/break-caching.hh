/*
  break-caching.hh -- declare Break_caching

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_CACHING_HH
#define BREAK_CACHING_HH

/**
  TODO : store breakpoints on the disk.
 */
struct Break_caching : Break_algorithm
{
    void do_set_pscore();
    Array<Column_x_positions> do_solve() const;
};

    
#endif // BREAK_CACHING_HH
