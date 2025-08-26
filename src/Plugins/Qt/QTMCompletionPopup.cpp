
/******************************************************************************
 * MODULE     : QTMCompletionPopup.cpp
 * DESCRIPTION: Implementation of Completion PopUp widget for auto-completion
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMCompletionPopup.hpp"
#include "edit_interface.hpp"
#include "edit_typeset.hpp"
#include "editor.hpp"

#include <QApplication>
#include <QKeyEvent>
#include <QScrollBar>

static const float COMPLETION_POPUP_MAGF     = 1.3f;
static const float COMPLETION_POPUP_FONT_SIZE= 12.0f * COMPLETION_POPUP_MAGF;
static const int   COMPLETION_POPUP_MIN_WIDTH= 150.0f * COMPLETION_POPUP_MAGF;
static const int   COMPLETION_POPUP_MAX_WIDTH= 2 * COMPLETION_POPUP_MIN_WIDTH;
static const int   COMPLETION_POPUP_MAX_ITEMS= 5;

QTMCompletionPopup::QTMCompletionPopup (QTMWidget*            parent,
                                        qt_simple_widget_rep* owner)
    : QListWidget (parent), parent (parent), owner (owner), cached_cursor_x (0),
      cached_cursor_y (0), cached_scroll_x (0), cached_scroll_y (0),
      cached_canvas_x (0), cached_magf (0.0) {
  setObjectName ("completion_popup");
  setWindowFlags (Qt::FramelessWindowHint | Qt::WindowStaysOnTopHint);
  setFocusPolicy (Qt::NoFocus);
  setSelectionMode (QAbstractItemView::SingleSelection);
  setMouseTracking (true);

  connect (this, &QListWidget::itemPressed, this,
           &QTMCompletionPopup::onItemPressed);
  connect (this, &QListWidget::currentItemChanged, this,
           &QTMCompletionPopup::onCurrentItemChanged);

  QFont font= this->font ();
  font.setPointSize (COMPLETION_POPUP_FONT_SIZE);
  this->setFont (font);
}

void
QTMCompletionPopup::resizeHeight () {
  int completions_N= count ();
  int height       = 0;
  int width        = 0;
  for (int i= 0; i < completions_N; i++) {
    if (i < COMPLETION_POPUP_MAX_ITEMS) { // 保留五个补全项
      height+= sizeHintForRow (i);
    }
    width= qMax (width, sizeHintForColumn (i));
  }
  width= qMax (width, COMPLETION_POPUP_MIN_WIDTH);
  width= 20 + width; // 边距留白，也可作为滚动条的边距
  if (width > COMPLETION_POPUP_MAX_WIDTH) {
    width= COMPLETION_POPUP_MAX_WIDTH; // 限制最大宽度
    height+= 20;                       // 滚动条的边距
  }
  QSize size (width, height);
  resize (size);
}

void
QTMCompletionPopup::showCompletions (array<string>& completions, int x, int y) {
  clear ();
  int completions_N= N (completions);
  addItems (to_qstringlist (completions));
  if (completions_N > 0) setCurrentRow (0);
  QPoint topLeft (x, y);
  move (topLeft);
  resizeHeight ();
  raise ();
  show ();
}

void
QTMCompletionPopup::showCompletions (string md, path tp,
                                     array<string>& completions,
                                     struct cursor cu, double magf,
                                     int scroll_x, int scroll_y, int canvas_x,
                                     int canvas_y) {
  // TODO: try to not cache tp
  // TODO: scroll 和 canvas 的位置不用传入，直接从 parent->tm_widget() 获取即可
  mode     = md;
  cached_tp= tp;
  cachePosition (cu, magf, scroll_x, scroll_y, canvas_x, canvas_y);
  int pos_x, pos_y;
  getCachedPosition (pos_x, pos_y);
  showCompletions (completions, pos_x, pos_y);
}

void
QTMCompletionPopup::cachePosition (struct cursor cu, double magf, int scroll_x,
                                   int scroll_y, int canvas_x, int canvas_y) {
  cached_cursor_x= cu->ox;
  cached_cursor_y= cu->oy;
  cached_scroll_x= scroll_x;
  cached_scroll_y= scroll_y;
  cached_canvas_x= canvas_x;
  cached_canvas_y= canvas_y;
  cached_magf    = magf;
}

void
QTMCompletionPopup::updateCache (tree& et, box& eb, path tp, double magf,
                                 int scroll_x, int scroll_y, int canvas_x,
                                 int canvas_y, int index) {
  // 逻辑上要求外部先调用 showCompletions 之后才能调用此方法
  // now that the cursor position has been updated to a completed position
  // we need to get the new cursor based on that position
  cached_tp  = tp;
  path tp1   = tp;
  int  length= N (getTextFromItem (index));
  for (int i= 0; (i < length && last_item (tp1) > 0); ++i) {
    tp1= previous_valid (et, tp1); // 向前移动 length 个字符
  }
  cachePosition (eb->find_check_cursor (tp1), magf, scroll_x, scroll_y,
                 canvas_x, canvas_y);
}

void
QTMCompletionPopup::getCachedPosition (int& x, int& y) {
  // HACK: mapTo() does not work as we expect on the Mac, so we manually
  // calculate the global screen cordinates and substract
  // This hack is from `qt_simple_widget.cpp`
  QPoint parentPosition= parent->mapToGlobal (QPoint (0, 0));
  QRect  wg= parent->tm_widget ()->scrollarea ()->window ()->frameGeometry ();
  int    canvas_x= cached_canvas_x - PIXEL * (parentPosition.x () - wg.x ());
  int    canvas_y= cached_canvas_y + PIXEL * (parentPosition.y () - wg.y ());
  x= ((cached_cursor_x - cached_scroll_x - 500) * cached_magf + canvas_x) /
     PIXEL;
  y= -((cached_cursor_y - cached_scroll_y - 5000) * cached_magf + canvas_y) /
     PIXEL;
  // TODO: 5000 is a magic number to add space between completion list box and
  // the text. We need to find a better way to get the position
}

void
QTMCompletionPopup::keyPressEvent (QKeyEvent* event) {
  QListWidget::keyPressEvent (event);
}

void
QTMCompletionPopup::focusOutEvent (QFocusEvent* event) {
  hide ();
  QListWidget::focusOutEvent (event);
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
}

void
QTMCompletionPopup::setScrollOrigin (QPoint origin) {
  coord2 origin2 = from_qpoint (origin);
  cached_scroll_x= (int) (origin2.x1 / cached_magf);
  cached_scroll_y= (int) (origin2.x2 / cached_magf);
}

void
QTMCompletionPopup::updatePosition () {
  // Update the position of the completion list box based on cached values
  int pos_x, pos_y;
  getCachedPosition (pos_x, pos_y);
  move (pos_x, pos_y);
  resizeHeight ();
}

void
QTMCompletionPopup::scrollBy (int x, int y) {
  // TODO: need convertion to coord2
  cached_scroll_x-= (int) (x / cached_magf);
  cached_scroll_y-= (int) (y / cached_magf);
}

string
QTMCompletionPopup::getTextFromSelectedItem () {
  QListWidgetItem* item= currentItem ();
  if (item) {
    QString qtext= item->text ();
    return from_qstring (qtext);
  }
  return string ("");
}

string
QTMCompletionPopup::getTextFromItem (int idx) {
  int cnt= count ();
  if (idx < 0 || idx >= cnt) {
    idx= (idx % cnt + cnt) % cnt; // wrap around
  }
  QListWidgetItem* listItem= item (idx);
  if (listItem) {
    QString qtext= listItem->text ();
    return from_qstring (qtext);
  }
  return string ("");
}

void
QTMCompletionPopup::onItemPressed (QListWidgetItem* item) {
  hide ();
  if (mode == "source") {
    if (!owner) return;
    edit_interface_rep* editInterface=
        dynamic_cast<edit_interface_rep*> (owner);
    if (!editInterface) return;
    editInterface->source_complete_variant (from_qstring (item->text ()));
  }
}

void
QTMCompletionPopup::onCurrentItemChanged (QListWidgetItem* currentItem,
                                          QListWidgetItem* previousItem) {
  // cast owner to edit_interface_rep
  // and call complete_variant with the text of previous and current item
  if (!owner) return;
  edit_interface_rep* editInterface= dynamic_cast<edit_interface_rep*> (owner);
  if (!editInterface) return;
  if (previousItem && currentItem) {
    if (mode == "text") {
      editInterface->complete_variant (from_qstring (previousItem->text ()),
                                       from_qstring (currentItem->text ()));
    }
    else if (mode == "source") {
      editInterface->source_complete_variant (
          from_qstring (currentItem->text ()));
    }
  }
}
