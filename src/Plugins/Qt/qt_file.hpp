
/******************************************************************************
* MODULE     : qt_file.hpp
* DESCRIPTION: File handling using Qt
* COPYRIGHT  : (C) 2022  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_FILE_H
#define QT_FILE_H
#include "array.hpp"

array<string>
qt_read_directory (string name, bool& error_flag);

#endif
