
/******************************************************************************
 * MODULE     : subprocess.hpp
 * DESCRIPTION: subprocess related routines
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#pragma once

#include "string.hpp"

namespace lolly {
namespace system {
int call (string cmd);
int check_output (string cmd, string& result, int64_t timeout= 20);
} // namespace system
} // namespace lolly
