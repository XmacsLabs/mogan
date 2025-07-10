
/******************************************************************************
 * MODULE     : view_history.hpp
 * DESCRIPTION: view_history管理
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef VIEW_HISTORY_HPP
#define VIEW_HISTORY_HPP

#include "array.hpp"
#include "tm_url.hpp"
#include <algorithm>

// 前向声明
url get_current_window ();
url view_to_window_of_tabpage (url u);

class view_history {
private:
  array<url> views;          ///< 按历史顺序排列的view的url的数组
  array<int> numbers;        ///< 对应view的排序依据编号数组
  int        views_N;        ///< views和numbers数组的大小
  int        current_number; ///< 当前最高的排序依据编号

public:
  /**
   * @brief 用于保存过滤视图数据的结构体
   */
  struct FilteredView {
    array<url> views;
    array<int> numbers;          ///< 对应的排序依据编号数组
    array<int> original_indices; ///< 在主数组中的原始索引数组
  };

  view_history ();

  int  size ();
  void add_view (url u);
  void remove_view (url u);

  /**
   * @brief 按历史顺序获取所有view
   * @return 所有view的url的数组
   */
  array<url> get_all_views ();

  /**
   * @brief 获取指定索引处的view（带边界检查）
   * @param i 要访问的索引
   * @return 该索引处的view的url，如果越界则返回url_none()
   */
  url get_view_at_index (int i);

  /**
   * @brief 设置指定索引处的view的排序依据编号（带边界检查）
   * @param i 要修改的索引
   * @param value 要设置的新值
   */
  void set_number_at_index (int i, int value);

  /**
   * @brief 递增指定索引处的view的排序依据编号（带边界检查）
   * @param i 要修改的索引
   */
  void inc_number_at_index (int i);

  /**
   * @brief 递减指定索引处的view的排序依据编号（带边界检查）
   * @param i 要修改的索引
   */
  void dec_number_at_index (int i);

  /**
   * @brief 获取按窗口过滤的视图
   * @param window 用于过滤的窗口URL（url_none()表示所有窗口）
   * @param sorted 如果为true，则按编号排序视图（默认值：false）
   * @return 包含过滤后数据的FilteredView结构体
   */
  FilteredView get_views_for_window (url window, bool sorted= false);

  /**
   * @brief 按排序依据编号对FilteredView进行排序并更新索引
   * @param filtered 要排序的FilteredView结构体（直接修改）
   */
  void sort_filtered_views (FilteredView& filtered);
};

#endif // VIEW_HISTORY_HPP
