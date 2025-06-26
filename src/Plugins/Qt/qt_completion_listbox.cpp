/******************************************************************************
 * MODULE     : qt_completion_listbox.cpp
 * DESCRIPTION: Implementation of Completion ListBox widget for auto-completion
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_completion_listbox.hpp"
#include "QTMScrollView.hpp"
#include "message.hpp"
#include <QApplication>
#include <QKeyEvent>
#include <QScrollBar>

QtCompletionListBox::QtCompletionListBox (QWidget* parent)
    : QListWidget (parent) {
  setWindowFlags (Qt::FramelessWindowHint |
                  Qt::WindowStaysOnTopHint); // Qt::Window, Qt::Popup
  setFocusPolicy (Qt::NoFocus);              // Qt::ClickFocus, Qt::StrongFocus
  setSelectionMode (QAbstractItemView::SingleSelection);
  setMouseTracking (true);

  setStyleSheet ("QListWidget::item:selected {"
                 "  background-color: #3daee9;"
                 "  color: white;"
                 "}"
                 "QListWidget::item:hover {"
                 "  background-color: #93cee9;"
                 "}"
                 "QListWidget {"
                 "  border: 1px solid #bfbfbf;"
                 "  background-color: white;"
                 "  alternate-background-color: #f0f0f0;"
                 "}");

  connect (this, &QListWidget::itemClicked, this,
           &QtCompletionListBox::onItemClicked);
}

void
QtCompletionListBox::showCompletions (array<string>& completions, SI x, SI y) {
  clear ();
  for (int i= 0; i < N (completions); ++i) {
    addItem (QString::fromUtf8 (as_charp (completions[i])));
  }
  if (N (completions) > 0) setCurrentRow (0);

  QSize  size (200, qMin (100, 20 + N (completions) * 22));
  QPoint topLeft (x, y);
  // qDebug () << "QtCompletionListBox::showCompletions: "
  //           << "x=" << x << ", y=" << y << ", size=" << size.width () << "x"
  //           << size.height () << Qt::endl;
  move (topLeft);
  resize (size);
  showComponent ();
}

void
QtCompletionListBox::showComponent () {
  // Ensure the listbox is visible
  show ();
  raise ();
  // activateWindow ();
  // setFocus (Qt::ActiveWindowFocusReason);
}

void
QtCompletionListBox::keyPressEvent (QKeyEvent* event) {
  return;
}

void
QtCompletionListBox::focusOutEvent (QFocusEvent* event) {
  hide ();
  QListWidget::focusOutEvent (event);
}

void
QtCompletionListBox::onItemClicked (QListWidgetItem* item) {
  if (item) {
    emit completionSelected (item->text ());
    hide ();
  }
}

void
QtCompletionListBox::wheelEvent (QWheelEvent* event) {
  QListWidget::wheelEvent (event);
}

void
QtCompletionListBox::selectNextItem () {
  int row= currentRow ();
  if (row < count () - 1) setCurrentRow (row + 1);
  else setCurrentRow (0);
}

void
QtCompletionListBox::selectPreviousItem () {
  int row= currentRow ();
  if (0 < row) setCurrentRow (row - 1);
  else setCurrentRow (count () - 1);
}

void
QtCompletionListBox::selectItemIndex (int index) {
  if (index >= 0 && index < count ()) {
    setCurrentRow (index);
  }
  else {
    // std::cerr << "Index out of bounds: " << index << std::endl;
  }
}

void
QtCompletionListBox::setEventTarget (QWidget* target) {
  eventTarget= target;
}
