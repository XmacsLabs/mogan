
/******************************************************************************
 * MODULE     : app_type.hpp
 * DESCRIPTION: Application type for Mogan applications
 * COPYRIGHT  : (C) 2023 Oyyko
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef APP_TYPE_H
#define APP_TYPE_H

#include "string.hpp"

enum class app_type { BEAMER, DRAW, CODE, RESEARCH, TM2HTML };

string mogan_app_id () {
#ifdef APP_MOGAN_RESEARCH
  return "research";
#endif

#ifdef APP_MOGAN_BEAMER
  return "beamer";
#endif

#ifdef APP_MOGAN_CODE
  return "code";
#endif

#ifdef APP_MOGAN_DRAW
  return "draw";
#endif

#ifdef APP_MOGAN_TM2HTML
  return "tm2html";
#endif
}

#endif