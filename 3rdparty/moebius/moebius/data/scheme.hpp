
/******************************************************************************
 * MODULE     : scheme.hpp
 * DESCRIPTION: scheme data
 * COPYRIGHT  : (C) 2024    Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tree.hpp"

#pragma once

namespace moebius {
namespace data {

/**
 * @brief Convert a string to its R5RS-compliant external representation.
 *
 * This function escapes special characters like double quotes and backslashes.
 *
 * @param s The input string.
 * @return A new string that represents the R5RS-compliant external
 * representation.
 */
string scm_quote (string s);

/**
 * @brief Unquote a R5RS-compliant external string representation.
 *
 * This function reverses the operation performed by scm_quote.
 *
 * @param s The quoted string.
 * @return The unquoted string.
 */
string scm_unquote (string s);

/******************************************************************************
 * Scheme format deserialization
 ******************************************************************************/

/**
 * @brief parse string as scheme tree
 *
 * @param s
 * @return scheme_tree
 */
scheme_tree string_to_scheme_tree (string s);

/**
 * @brief parse block as scheme tree
 *
 * @param s
 * @return scheme_tree
 */
scheme_tree block_to_scheme_tree (string s);

tree scheme_tree_to_tree (scheme_tree t, hashmap<string, int> codes, bool flag);

tree scheme_tree_to_tree (scheme_tree t);

/**
 * @brief convert scheme tree to TeXmacs tree
 *
 * @param t
 * @param version
 * @return tree
 */
tree scheme_tree_to_tree (scheme_tree t, string version);

/**
 * @brief parse string as TeXmacs tree
 *
 * @param s
 * @return tree
 */
tree scheme_to_tree (string s);

/**
 * @brief Convert scheme document (.stm) to TeXmacs tree
 *
 * @param s
 * @return tree
 */
tree scheme_document_to_tree (string s);

/******************************************************************************
 * Scheme format serialization
 ******************************************************************************/

/**
 * @brief Serialize scheme tree to string
 *
 * @param t
 * @return string
 */
string scheme_tree_to_string (scheme_tree t);

/**
 * @brief Serialize scheme tree to block
 *
 * @param t
 * @return string
 */
string scheme_tree_to_block (scheme_tree t);

/**
 * @brief Convert TeXmacs tree to scheme tree
 *
 * @param t
 * @return scheme_tree
 */
scheme_tree tree_to_scheme_tree (tree t);

/**
 * @brief Convert TeXmacs tree to scheme string
 *
 * @param t
 * @return string
 */
string tree_to_scheme (tree t);

} // namespace data
} // namespace moebius
