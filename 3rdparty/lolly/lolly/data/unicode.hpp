
/******************************************************************************
 * MODULE     : unicode.hpp
 * DESCRIPTION: Unicode support
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#pragma once

#include "string.hpp"

namespace lolly {
namespace data {
string unicode_get_range (int code);

/**
 * @brief Checks if a string contains only CJK Unified Ideographs.
 *
 * @param s The string to check.
 * @return True if all characters in the string are CJK Unified Ideographs,
 * otherwise false.
 * @note This function expects the CJK Unified Ideographs to be in a specific
 * encoded format.
 */
bool is_cjk_unified_ideographs (string s);

/**
 * @brief Checks if a string contains any CJK Unified Ideographs.
 *
 * @param s The string to check.
 * @return True if the string contains at least one CJK Unified Ideograph,
 * otherwise false.
 * @note This function expects the CJK Unified Ideographs to be in a specific
 * encoded format.
 */
bool has_cjk_unified_ideographs (string s);
} // namespace data
} // namespace lolly
