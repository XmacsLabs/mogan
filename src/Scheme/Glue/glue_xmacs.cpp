
/******************************************************************************
 *
 * Copyright (C) 2022 PikachuHy <pikachuhy@163.com>
 *
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 *
 ******************************************************************************/

#include "../S7/s7.h"
#include "Qt/qt_utilities.hpp"

#include <QApplication>
#include <QLibraryInfo>

tmscm
tmg_show_about_qt () {
  qApp->aboutQt ();
  return string_to_tmscm ("");
}

tmscm
tmg_qt_version () {
  string out= from_qstring (QLibraryInfo::version ().toString ());
  return string_to_tmscm (out);
}

tmscm
tmg_xmacs_version () {
  string out= XMACS_VERSION;
  return string_to_tmscm (out);
}

tmscm
tmg_s7_version () {
  string out= S7_VERSION;
  return string_to_tmscm (out);
}

void
initialize_glue_xmacs () {
  tmscm_install_procedure ("show-about-qt", tmg_show_about_qt, 0, 0, 0);
  tmscm_install_procedure ("xmacs-version", tmg_xmacs_version, 0, 0, 0);
  tmscm_install_procedure ("s7-version", tmg_s7_version, 0, 0, 0);
  tmscm_install_procedure ("qt-version", tmg_qt_version, 0, 0, 0);
}
