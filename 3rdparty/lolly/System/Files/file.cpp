
/******************************************************************************
* MODULE     : file.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
                   2023  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "string.hpp"
#include "tbox/tbox.h"

bool
is_directory (string path) {
  tb_file_info_t info;
  if (tb_file_info (as_charp (path), &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_DIRECTORY:
      return true;
    default:
      return false;
    }
  }
  else {
    TM_FAILED ("is_directory failed")
  }
}

bool
is_directory (file_url name) {
  string path= name.concretize ();
  return is_directory (path);
}
