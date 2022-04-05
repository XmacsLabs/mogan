
/******************************************************************************
*
* Copyright (C) 2022 PikachuHy <pikachuhy@163.com>
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/
#include <QApplication>
tmscm
tmg_show_about_qt () {

  // TMSCM_DEFER_INTS;
  qApp->aboutQt ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm ("");
}


void
initialize_glue_qt () {
  tmscm_install_procedure ("show-about-qt",  tmg_show_about_qt, 0, 0, 0);
}
