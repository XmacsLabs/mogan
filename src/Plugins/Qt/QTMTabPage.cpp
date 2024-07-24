
/******************************************************************************
 * MODULE     : QTMTabPage.cpp
 * DESCRIPTION: QT Texmacs tab page classes
 * COPYRIGHT  : (C) 2024 Zhenjun Guo
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMTabPage.hpp"

/******************************************************************************
 * QTMTabPage
 ******************************************************************************/

QTMTabPage::QTMTabPage (url u, QAction* title, QAction* _button, bool active)
    : bufferUrl (u) {
  title->setCheckable (active);
  title->setChecked (active);
  closeBtn= new QToolButton (this);
  closeBtn->setDefaultAction (_button);
  closeBtn->setFixedSize (20, 20); // position will be updated in resizeEvent

  setStyleSheet (
      "QToolButton{ padding: 0 26px; }"); // reserve space for closeBtn
  setDefaultAction (title);
}

void
QTMTabPage::resizeEvent (QResizeEvent* e) {
  int w= closeBtn->width ();
  int h= closeBtn->height ();
  int x= e->size ().width () - w - 8;
  int y= e->size ().height () / 2 - h / 2;
  closeBtn->setGeometry (x, y, w, h);
}