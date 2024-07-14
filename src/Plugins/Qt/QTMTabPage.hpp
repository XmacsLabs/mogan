
/******************************************************************************
 * MODULE     : QTMTabPage.hpp
 * DESCRIPTION: QT Texmacs tab page classes
 * COPYRIGHT  : (C) 2024 Zhenjun Guo
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#ifndef QTMTABPAGE_HPP
#define QTMTABPAGE_HPP

#include <QToolBar>

class QTMTabPageBar : public QToolBar {
public:
  explicit QTMTabPageBar (const QString& title, QWidget* parent)
      : QToolBar (title, parent) {}
};

#endif // QTMTABPAGE_HPP