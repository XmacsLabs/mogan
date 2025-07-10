
/******************************************************************************
 * MODULE     : view_history.cpp
 * DESCRIPTION: view_history管理
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "view_history.hpp"
#include "new_view.hpp" // 用于 view_to_window_of_tabpage

view_history::view_history () : current_number (0), views_N (0) {}

int
view_history::size () {
  return views_N;
}

void
view_history::add_view (url u) {
  int i;
  for (i= 0; i < views_N; i++) {
    if (views[i] == u) break;
  }

  if (i >= views_N) {
    views  = append (u, views);
    numbers= append (current_number, numbers);
    current_number++;
    views_N++;
  }
  else {
    int tmp= numbers[i];
    for (int j= i; j > 0; j--) {
      views[j]  = views[j - 1];
      numbers[j]= numbers[j - 1];
    }
    numbers[0]= tmp;
    views[0]  = u;
  }
}

void
view_history::remove_view (url u) {
  for (int i= 0; i < views_N; i++) {
    if (views[i] == u) {
      views  = append (range (views, 0, i), range (views, i + 1, views_N));
      numbers= append (range (numbers, 0, i), range (numbers, i + 1, views_N));
      views_N--;
      return;
    }
  }
}

array<url>
view_history::get_all_views () {
  return views;
}

url
view_history::get_view_at_index (int i) {
  if (i >= 0 && i < views_N) return views[i];
  return url_none ();
}

void
view_history::set_number_at_index (int i, int value) {
  if (i >= 0 && i < views_N) {
    numbers[i]= value;
  }
}

void
view_history::inc_number_at_index (int i) {
  if (i >= 0 && i < views_N) {
    numbers[i]++;
  }
}

void
view_history::dec_number_at_index (int i) {
  if (i >= 0 && i < views_N) {
    numbers[i]--;
  }
}

view_history::FilteredView
view_history::get_views_for_window (url window, bool sorted) {
  FilteredView result;
  bool         filter_window= !is_none (window);

  for (int i= 0; i < views_N; i++) {
    if (!filter_window || view_to_window_of_tabpage (views[i]) == window) {
      result.views << views[i];
      result.numbers << numbers[i];
      result.original_indices << i;
    }
  }

  if (sorted) {
    sort_filtered_views (result);
  }

  return result;
}

void
view_history::sort_filtered_views (FilteredView& filtered) {
  array<std::pair<int, int>> number_to_index; // (编号, 在过滤数组中的索引)
  for (int i= 0; i < N (filtered.numbers); i++) {
    number_to_index << std::make_pair (filtered.numbers[i], i);
  }

  // 按编号排序
  std::sort (number_to_index.begin (), number_to_index.end (),
             [] (const std::pair<int, int>& a, const std::pair<int, int>& b) {
               return a.first < b.first;
             });

  // 创建新的排序数组
  array<url> sorted_views;
  array<int> sorted_numbers;
  array<int> sorted_indices;

  for (int i= 0; i < N (number_to_index); i++) {
    int idx= number_to_index[i].second;
    sorted_views << filtered.views[idx];
    sorted_numbers << filtered.numbers[idx];
    sorted_indices << filtered.original_indices[idx];
  }

  filtered.views           = sorted_views;
  filtered.numbers         = sorted_numbers;
  filtered.original_indices= sorted_indices;
}
