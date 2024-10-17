
/******************************************************************************
 * MODULE     : http.hpp
 * DESCRIPTION: HTTP related routines
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#pragma once

#include "lolly/io/http_request.hpp"
#include "lolly/io/http_response.hpp"
#include "tree.hpp"
#include "url.hpp"

namespace lolly {
namespace io {
tree http_get (url u, http_headers headers= http_headers ());
tree http_head (url u, http_headers headers= http_headers ());
tree download (url from, url to, http_headers headers= http_headers ());

} // namespace io
} // namespace lolly
