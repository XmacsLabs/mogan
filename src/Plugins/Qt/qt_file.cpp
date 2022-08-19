
/******************************************************************************
 * MODULE     : qt_file.cpp
 * DESCRIPTION: File handling using Qt
 * COPYRIGHT  : (C) 2022  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Qt/qt_file.hpp"
#include "Qt/qt_utilities.hpp"
#include "data_cache.hpp"
#include "merge_sort.hpp"
#include "string.hpp"

#include <QCache>
#include <QDir>
#include <QFileInfo>
#include <QFileInfoList>

static QCache<QString, QList<QString>> cache (500);

static QList<QString> *
read_directory (QDir dir, bool &error_flag) {
  bench_start ("read directory");
  QList<QString> *dirs= new QList<QString>;
  QFileInfoList   list= dir.entryInfoList ();
  for (int i= 0; i < list.size (); ++i) {
    QFileInfo fileInfo= list.at (i);
    dirs->append (fileInfo.fileName ());
  }
  dirs->sort ();
  bench_cumul ("read directory");
  return dirs;
}

array<string>
qt_read_directory (string name, bool &error_flag) {
  array<string> arr_dirs;

  // If it does not exist, just return an empty array
  QString qname= to_qstring (name);
  QDir    dir  = QDir (qname);
  if (!dir.exists ()) {
    error_flag= true;
    return arr_dirs;
  }
  else {
    QList<QString> *dirs;
    if (do_cache_dir (name)) {
      if (cache.contains (qname)) {
        dirs= cache.object (qname);
      }
      else {
        dirs= read_directory (dir, error_flag);
        cache.insert (qname, dirs);
      }
    }
    else {
      dirs= read_directory (dir, error_flag);
    }
    for (int i= 0; i < dirs->size (); ++i) {
      arr_dirs << from_qstring (dirs->at (i));
    }
    return arr_dirs;
  }
}
