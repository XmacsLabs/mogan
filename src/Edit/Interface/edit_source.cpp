
/******************************************************************************
 * MODULE     : edit_source.cpp
 * DESCRIPTION: source edit routines
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "analyze.hpp"
#include "connect.hpp"
#include "dictionary.hpp"
#include "edit_interface.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "observers.hpp"
#include "preferences.hpp"
#include "tree_observer.hpp"

#include <moebius/data/scheme.hpp>

using namespace moebius;
using moebius::data::scm_quote;
using moebius::data::scm_unquote;

void
edit_interface_rep::source_complete_try () {
  completion_style = get_preference ("completion style");
  tree st          = subtree (et, path_up (tp));
  completion_prefix= st->label;
  if (completion_prefix != "") {
    array<string> completions;
    array<object> tmp= as_array_object (
        eval ("(match-macro-prefix " * scm_quote (completion_prefix) * ")"));
    int tmp_N= N (tmp);
    for (int i= 0; i < tmp_N; i++) {
      completions << scm_unquote (object_to_string (tmp[i]));
    }
    path tp1     = tp;
    int  prefix_N= N (completion_prefix);
    for (int i= 0; i < prefix_N; ++i) {
      tp1= previous_valid (et, tp1); // 向前移动到前缀的起始位置
    }
    cursor cu= eb->find_check_cursor (tp1);
    if (completion_style == string ("popup")) {
      show_completion_popup (tp, completions, cu, magf, get_scroll_x (),
                             get_scroll_y (), get_canvas_x ());
    }
  }
  else {
    hide_completion_popup ();
  }
}
