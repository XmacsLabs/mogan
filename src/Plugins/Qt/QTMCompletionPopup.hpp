
/******************************************************************************
 * MODULE     : QTMCompletionPopup.hpp
 * DESCRIPTION: Completion PopUp widget for auto-completion
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_COMPLETION_POPUP_HPP
#define QT_COMPLETION_POPUP_HPP

#include "qt_simple_widget.hpp"
#include "qt_utilities.hpp"

#include "boxes.hpp"
#include "tree_observer.hpp"
#include "tree_traverse.hpp"

#include "QTMScrollView.hpp"

#include <QListWidget>
#include <QPoint>
#include <QStringList>

class QTMCompletionPopup : public QListWidget {
  Q_OBJECT

protected:
  string                mode;
  int                   cached_cursor_x;
  int                   cached_cursor_y;
  int                   cached_scroll_x;
  int                   cached_scroll_y;
  int                   cached_canvas_x;
  double                cached_magf;
  path                  cached_tp;
  qt_simple_widget_rep* owner;
  QTMWidget*            parent;

public:
  QTMCompletionPopup (QTMWidget* parent, qt_simple_widget_rep* owner);

  void scrollBy (int x, int y);
  void selectNextItem ();
  void selectPreviousItem ();
  void selectItemIndex (int index);
  void setScrollOrigin (QPoint origin);
  void updatePosition ();
  void updateCache (tree& et, box& eb, path tp, double magf, int scroll_x,
                    int scroll_y, int canvas_x, int index);
  void showCompletions (string md, path tp, array<string>& completions,
                        struct cursor cu, double magf, int scroll_x,
                        int scroll_y, int canvas_x);

  // Getter methods for cached variables
  inline int    getCachedCursorX () const { return cached_cursor_x; }
  inline int    getCachedCursorY () const { return cached_cursor_y; }
  inline int    getCachedScrollX () const { return cached_scroll_x; }
  inline int    getCachedScrollY () const { return cached_scroll_y; }
  inline int    getCachedCanvasX () const { return cached_canvas_x; }
  inline double getCachedMagf () const { return cached_magf; }

  // Setter methods for cached variables
  inline void setCachedCursorX (int x) { cached_cursor_x= x; }
  inline void setCachedCursorY (int y) { cached_cursor_y= y; }
  inline void setCachedScrollX (int x) { cached_scroll_x= x; }
  inline void setCachedScrollY (int y) { cached_scroll_y= y; }
  inline void setCachedCanvasX (int x) { cached_canvas_x= x; }
  inline void setCachedMagf (double magf) { cached_magf= magf; }

protected:
  void keyPressEvent (QKeyEvent* event) override;
  void focusOutEvent (QFocusEvent* event) override;
  void wheelEvent (QWheelEvent* event) override;
  void showCompletions (array<string>& completions, int x, int y);
  void cachePosition (struct cursor cu, double magf, int scroll_x, int scroll_y,
                      int canvas_x);
  void getCachedPosition (int& x, int& y);
  void resizeHeight ();
  string getTextFromItem (int idx);
  string getTextFromSelectedItem ();

private slots:
  void onItemPressed (QListWidgetItem* item);
  void onCurrentItemChanged (QListWidgetItem* prevousItem,
                             QListWidgetItem* currentItem);
};

#endif // QT_COMPLETION_POPUP_HPP
