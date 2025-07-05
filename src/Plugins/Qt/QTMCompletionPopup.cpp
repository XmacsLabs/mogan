
/******************************************************************************
 * MODULE     : QTMCompletionPopup.cpp
 * DESCRIPTION: Implementation of Completion ListBox widget for auto-completion
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

// TODO: delete some headers
#include "QTMCompletionPopup.hpp"
#include "QTMScrollView.hpp"
#include "gui.hpp"
#include "message.hpp"
#include "observer.hpp"
#include "qt_gui.hpp"
#include "qt_simple_widget.hpp"
#include "qt_utilities.hpp"
#include "tm_window.hpp"
#include "tree_observer.hpp"
#include "tree_traverse.hpp"
#include "typesetter.hpp"
#include "widget.hpp"

#include <QApplication>
#include <QKeyEvent>
#include <QScrollBar>

QTMCompletionPopup::QTMCompletionPopup (QWidget*              parent,
                                        qt_simple_widget_rep* owner_widget)
    : QListWidget (parent), owner_widget (owner_widget), cached_cursor_x (0),
      cached_cursor_y (0), cached_scroll_x (0), cached_scroll_y (0),
      cached_canvas_x (0), cached_magf (0.0) {
  setObjectName ("completion_popup");
  setWindowFlags (Qt::FramelessWindowHint |
                  Qt::WindowStaysOnTopHint); // Qt::Window, Qt::Popup
  setFocusPolicy (Qt::NoFocus);              // Qt::ClickFocus, Qt::StrongFocus
  setSelectionMode (QAbstractItemView::SingleSelection);
  setMouseTracking (true);

  connect (this, &QListWidget::itemClicked, this,
           &QTMCompletionPopup::onItemClicked);
  connect (this, &QListWidget::currentItemChanged, this,
           &QTMCompletionPopup::onCurrentItemChanged);
}

void
QTMCompletionPopup::showCompletions (array<string>& completions, SI x, SI y) {
  clear ();
  for (int i= 0; i < N (completions); ++i) {
    addItem (QString::fromUtf8 (as_charp (completions[i])));
  }
  if (N (completions) > 0) setCurrentRow (0);

  QSize  size (200, qMin (100, 20 + N (completions) * 22));
  QPoint topLeft (x, y);
  // qDebug () << "QTMCompletionPopup::showCompletions: "
  //           << "x=" << x << ", y=" << y << ", size=" << size.width () << "x"
  //           << size.height () << Qt::endl;
  move (topLeft);
  resize (size);
  showComponent ();
}

void
QTMCompletionPopup::showCompletions (path tp, array<string>& completions,
                                     struct cursor cu, double magf, SI scroll_x,
                                     SI scroll_y, SI canvas_x) {
  // TODO: do not cache tp
  cached_tp= tp;
  cachePosition (cu, magf, scroll_x, scroll_y, canvas_x);
  SI pos_x, pos_y;
  getCachedPosition (pos_x, pos_y);
  showCompletions (completions, pos_x, pos_y);
}

void
QTMCompletionPopup::cachePosition (struct cursor cu, double magf, SI scroll_x,
                                   SI scroll_y, SI canvas_x) {
  cached_cursor_x= cu->ox;
  cached_cursor_y= cu->oy;
  cached_scroll_x= scroll_x;
  cached_scroll_y= scroll_y;
  cached_canvas_x= canvas_x;
  cached_magf    = magf;
}

void
QTMCompletionPopup::updateCache (tree& et, box eb, path tp, double magf,
                                 SI scroll_x, SI scroll_y, SI canvas_x,
                                 SI index) {
  // MUST called when cache is already set
  // now that the cursor position has been updated to a completed position
  // we need to get the new cursor based on that position
  cached_tp= tp;
  path tp1 = tp;
  for (int i= 0; i < N (getText (index)); ++i) {
    tp1= previous_valid (et, tp1);
  }
  struct cursor cu= eb->find_check_cursor (tp1);

  cached_cursor_x= cu->ox;
  cached_cursor_y= cu->oy;
  cached_scroll_x= scroll_x;
  cached_scroll_y= scroll_y;
  cached_canvas_x= canvas_x;
  cached_magf    = magf;
}

void
QTMCompletionPopup::getCachedPosition (SI& x, SI& y) {
  x= ((cached_cursor_x - cached_scroll_x - 500) * cached_magf +
      cached_canvas_x) /
     256;
  y= -((cached_cursor_y - 5000 - cached_scroll_y) * cached_magf) / 256;
  // TODO: 5000 is a magic number to add space between completion list box and
  // the text. We need to find a better way to get the position
}

void
QTMCompletionPopup::showComponent () {
  // Ensure the listbox is visible
  show ();
  raise ();
  // activateWindow ();
  // setFocus (Qt::ActiveWindowFocusReason);
}

void
QTMCompletionPopup::keyPressEvent (QKeyEvent* event) {
  return;
}

void
QTMCompletionPopup::focusOutEvent (QFocusEvent* event) {
  hide ();
  QListWidget::focusOutEvent (event);
}

void
QTMCompletionPopup::onItemClicked (QListWidgetItem* item) {
  if (item) {
    emit   completionSelected (item->text ());
    SI     end  = last_item (cached_tp);
    string new_s= getSelectedText ();
    cout << "QTMCompletionPopup::onItemClicked: "
         << "selected text: " << new_s << ", cached_tp: " << cached_tp
         << ", end: " << end << ", lastSelectedText: " << lastSelectedText
         << LF;
    remove (path_up (cached_tp) * (end - N (lastSelectedText)),
            N (lastSelectedText));
    insert (path_up (cached_tp) * (end - N (lastSelectedText)), new_s);
    widget w= widget (owner_widget);
    set_input_mode_normal (w);
    hide ();
  }
}

void
QTMCompletionPopup::wheelEvent (QWheelEvent* event) {
  QListWidget::wheelEvent (event);
}

void
QTMCompletionPopup::selectNextItem () {
  int row= currentRow ();
  if (row < count () - 1) setCurrentRow (row + 1);
  else setCurrentRow (0);
}

void
QTMCompletionPopup::selectPreviousItem () {
  int row= currentRow ();
  if (0 < row) setCurrentRow (row - 1);
  else setCurrentRow (count () - 1);
}

void
QTMCompletionPopup::selectItemIndex (int index) {
  if (index >= 0 && index < count ()) {
    setCurrentRow (index);
  }
  else {
    // std::cerr << "Index out of bounds: " << index << std::endl;
  }
}

void
QTMCompletionPopup::setScrollOrigin (QPoint origin) {
  coord2 origin2 = from_qpoint (origin);
  cached_scroll_x= (SI) (origin2.x1 / cached_magf);
  cached_scroll_y= (SI) (origin2.x2 / cached_magf);
  // updatePosition ();
}

void
QTMCompletionPopup::updatePosition () {
  // Update the position of the completion list box based on cached values
  SI pos_x, pos_y;
  getCachedPosition (pos_x, pos_y);
  move (pos_x, pos_y);
}

void
QTMCompletionPopup::scrollBy (SI x, SI y) {
  // TODO: need convertion to coord2
  cached_scroll_x-= (SI) (x / cached_magf);
  cached_scroll_y-= (SI) (y / cached_magf);
}

string
QTMCompletionPopup::getSelectedText () {
  QListWidgetItem* item= currentItem ();
  if (item) {
    QString qtext= item->text ();
    return from_qstring (qtext);
  }
  return string ("");
}

string
QTMCompletionPopup::getText (SI idx) {
  if (idx < 0 || idx >= count ()) {
    return string ("");
  }
  QListWidgetItem* listItem= item (idx);
  if (listItem) {
    QString qtext= listItem->text ();
    return from_qstring (qtext);
  }
  return string ("");
}

void
QTMCompletionPopup::onCurrentItemChanged (QListWidgetItem* current,
                                          QListWidgetItem* previous) {
  if (previous) {
    lastSelectedText= from_qstring (previous->text ());
  }
}
