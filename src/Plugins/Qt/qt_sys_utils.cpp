
/******************************************************************************
 * MODULE     : qt_sys_utils.cpp
 * DESCRIPTION: external command launcher
 * COPYRIGHT  : (C) 2009, 2016  David MICHEL, Denis Raux
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_sys_utils.hpp"
#include "basic.hpp"
#include "file.hpp"
#include "qt_utilities.hpp"
#include "string.hpp"
#include "tm_debug.hpp"

#include <QDesktopServices>
#include <QProcess>
#include <QString>
#include <QSysInfo>
#include <QUrl>

string
qt_get_current_cpu_arch () {
  return from_qstring (QSysInfo::currentCpuArchitecture ());
}

string
qt_get_pretty_os_name () {
  return from_qstring (QSysInfo::prettyProductName ());
}

void
qt_open_url (url u) {
  debug_io << "open-url\t" << u << LF;
  if (is_local_and_single (u)) {
    QString link= to_qstring ("file:///" * as_string (u));
    QDesktopServices::openUrl (QUrl (link));
  }
  else {
    QString link= to_qstring (as_string (u));
    QDesktopServices::openUrl (QUrl (link));
  }
}
