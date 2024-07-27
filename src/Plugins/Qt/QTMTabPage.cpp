
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

QTMTabPage::QTMTabPage (url p_url, QAction* p_title, QAction* p_closeBtn,
                        bool p_isActive)
    : bufferUrl (p_url) {
  p_title->setCheckable (p_isActive);
  p_title->setChecked (p_isActive);
  closeBtn= new QToolButton (this);
  closeBtn->setDefaultAction (p_closeBtn);
  closeBtn->setFixedSize (20, 20); // position will be updated in resizeEvent

  setStyleSheet (
      "QToolButton{ padding: 0 26px; }"); // reserve space for closeBtn
  setDefaultAction (p_title);
}

void
QTMTabPage::resizeEvent (QResizeEvent* e) {
  int w= closeBtn->width ();
  int h= closeBtn->height ();
  int x= e->size ().width () - w - 8;
  int y= e->size ().height () / 2 - h / 2;

  closeBtn->setGeometry (x, y, w, h);
}