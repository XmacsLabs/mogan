
/******************************************************************************
 * MODULE     : curl.hpp
 * DESCRIPTION: interface with CURL
 * COPYRIGHT  : (C) 2022  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TM_CURL_H
#define TM_CURL_H

#include "string.hpp"

namespace lolly {
string curl_version ();
string curl_get (string source, string user_agent);
} // namespace lolly

#endif
