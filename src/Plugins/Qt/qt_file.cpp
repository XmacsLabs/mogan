
/******************************************************************************
* MODULE     : qt_file.cpp
* DESCRIPTION: File handling using Qt
* COPYRIGHT  : (C) 2022  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "string.hpp"
#include "Qt/qt_utilities.hpp"
#include "Qt/qt_file.hpp"

#include <QDir>
#include <QFileInfoList>
#include <QFileInfo>

array<string>
qt_read_directory (string name, bool& error_flag) {
  QDir dir= QDir (to_qstring (name));
  if (!dir.exists ()) {
    error_flag= true;
    return array<string>();
  }

  QFileInfoList list = dir.entryInfoList();
  array<string> dirs;
  for (int i = 0; i < list.size(); ++i) {
    QFileInfo fileInfo = list.at(i);
    dirs << from_qstring(fileInfo.fileName());
  }
  return dirs;
}
