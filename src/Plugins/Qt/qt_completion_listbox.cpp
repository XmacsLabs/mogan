/******************************************************************************
 * MODULE     : qt_completion_listbox.cpp
 * DESCRIPTION: Implementation of Completion ListBox widget for code
 *auto-completion COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_completion_listbox.hpp"
#include <QApplication>
#include <QKeyEvent>

QtCompletionListBox::QtCompletionListBox (QWidget* parent)
    : QListWidget (parent) {
  setWindowFlags (Qt::Window | Qt::FramelessWindowHint |
                  Qt::WindowStaysOnTopHint);
  setFocusPolicy (Qt::ClickFocus);
  setSelectionMode (QAbstractItemView::SingleSelection);
  setMouseTracking (true);

  connect (this, &QListWidget::itemClicked, this,
           &QtCompletionListBox::onItemClicked);
}

void
QtCompletionListBox::showCompletions (const QStringList& completions,
                                      const QPoint&      pos) {
  clear ();
  addItems (completions);
  if (!completions.isEmpty ()) setCurrentRow (0);

  QSize  size (200, qMin (100, 20 + completions.size () * 22));
  QPoint topLeft= pos;
  move (topLeft);
  resize (size);
  show ();
  raise ();
  setFocus ();
}

void
QtCompletionListBox::keyPressEvent (QKeyEvent* event) {
  switch (event->key ()) {
  case Qt::Key_Enter:
  case Qt::Key_Return:
    if (currentItem ()) {
      emit completionSelected (currentItem ()->text ());
    }
    hide ();
    event->accept ();
    break;
  case Qt::Key_Escape:
    hide ();
    event->accept ();
    break;
  case Qt::Key_Up:
  case Qt::Key_Down:
    QListWidget::keyPressEvent (event);
    break;
  default:
    hide ();
    if (parentWidget ()) QCoreApplication::sendEvent (parentWidget (), event);
    break;
  }
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
  if (parentWidget ()) {
    QCoreApplication::sendEvent (parentWidget (), event);
  }
  else event->ignore ();
}
