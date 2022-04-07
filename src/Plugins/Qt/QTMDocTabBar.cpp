/******************************************************************************
 * MODULE     : texmacs.cpp
 * DESCRIPTION: Document Tab Bar
 * COPYRIGHT  : (C) 2022  PikachuHy
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMDocTabBar.h"
#include "new_buffer.hpp"
#include "new_view.hpp"
#include "new_window.hpp"
#include "qt_utilities.hpp"
#include "scheme.hpp"

QTMDocTabBar::QTMDocTabBar (QWidget *parent) : QTabBar (parent) {
  setDocumentMode (true);
  setDrawBase (true);
  setTabsClosable (true);
  setElideMode (Qt::ElideLeft);
  connect (this, &QTMDocTabBar::tabBarClicked, this,
           &QTMDocTabBar::handleTabBarClicked);
  connect (this, &QTMDocTabBar::tabCloseRequested, this,
           &QTMDocTabBar::handleTabCloseRequested);
}
void
QTMDocTabBar::updateTabs (const url &cur_buffer) {
  for (int i= 0; i < m_buffers.size (); ++i) {
    if (m_buffers[i] == cur_buffer) {
      setCurrentIndex (i);
      return;
    }
  }
  m_buffers.push_back (cur_buffer);
  auto title= get_title_buffer (cur_buffer);
  addTab (to_qstring (title));
  setCurrentIndex (m_buffers.size () - 1);
}
void
QTMDocTabBar::handleTabBarClicked (int index) {
  call ("switch-to-buffer*", object (m_buffers[index]));
}
void
QTMDocTabBar::handleTabCloseRequested (int index) {
  kill_buffer (m_buffers[index]);
  m_buffers.removeAt (index);
  removeTab (index);
}
