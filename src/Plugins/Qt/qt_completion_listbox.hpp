
/******************************************************************************
 * MODULE     : qt_completion_listbox.hpp
 * DESCRIPTION: Completion ListBox widget for auto-completion
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_COMPLETION_LISTBOX_HPP
#define QT_COMPLETION_LISTBOX_HPP

#include "QTMScrollView.hpp"
#include "array.hpp"
#include "typesetter.hpp"
#include "observer.hpp"
#include "tree_observer.hpp"
#include "qt_simple_widget.hpp"

#include <QListWidget>
#include <QPoint>
#include <QStringList>

class QtCompletionListBox : public QListWidget {
  Q_OBJECT
private:
  qt_simple_widget_rep* owner_widget; // for emiting messages
  string lastSelectedText;
  path cached_tp;
protected:
  SI cached_cursor_x;
  SI cached_cursor_y;
  SI cached_scroll_x;
  SI cached_scroll_y;
  SI cached_canvas_x;
  double cached_magf;

public:
  QtCompletionListBox (QWidget* parent, qt_simple_widget_rep* owner_widget);
  
  // Getter methods for cached variables
  inline SI getCachedCursorX() const { return cached_cursor_x; }
  inline SI getCachedCursorY() const { return cached_cursor_y; }
  inline SI getCachedScrollX() const { return cached_scroll_x; }
  inline SI getCachedScrollY() const { return cached_scroll_y; }
  inline SI getCachedCanvasX() const { return cached_canvas_x; }
  inline double getCachedMagf() const { return cached_magf; }

  // Setter methods for cached variables
  inline void setCachedCursorX(SI x) { cached_cursor_x = x; }
  inline void setCachedCursorY(SI y) { cached_cursor_y = y; }
  inline void setCachedScrollX(SI x) { cached_scroll_x = x; }
  inline void setCachedScrollY(SI y) { cached_scroll_y = y; }
  inline void setCachedCanvasX(SI x) { cached_canvas_x = x; }
  inline void setCachedMagf(double magf) { cached_magf = magf; }

  void showCompletions (array<string>& completions, SI x, SI y);
  void showCompletions (path tp, array<string>& completions, struct cursor cu, double magf, SI scroll_x, SI scroll_y, SI canvas_x);
  void selectNextItem ();
  void selectPreviousItem ();
  void selectItemIndex (int index);
  void showComponent ();
  void setScrollOrigin (QPoint origin);
  void updatePosition ();
  void scrollBy(SI x, SI y);
  void updateCache (tree& et, box eb, path tp, double magf, SI scroll_x, SI scroll_y, SI canvas_x, SI index);
  string getSelectedText ();

signals:
  void completionSelected (const QString& text);

protected:
  void keyPressEvent (QKeyEvent* event) override;
  void focusOutEvent (QFocusEvent* event) override;
  void wheelEvent (QWheelEvent* event) override;
  void cachePosition (struct cursor cu, double magf, SI scroll_x, SI scroll_y, SI canvas_x);

private:
  void getCachedPosition (SI& x, SI& y);
  string getText(SI idx);

private slots:
  void onItemClicked (QListWidgetItem* item);
  void onCurrentItemChanged(QListWidgetItem* current, QListWidgetItem* previous);
};

#endif // QT_COMPLETION_LISTBOX_HPP
