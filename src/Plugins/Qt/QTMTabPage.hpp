
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

#include <QMouseEvent>
#include <QToolBar>
#include <QToolButton>
#include <basic.hpp>
#include <scheme.hpp>

/*! QTMTabPage is used to build a widget for tab page.
 */
class QTMTabPage : public QToolButton {
  Q_OBJECT
  QToolButton* closeBtn;

public:
  const url bufferUrl;

public:
  explicit QTMTabPage (url p_url, QAction* p_title, QAction* p_closeBtn,
                       bool p_isActive);

protected:
  virtual void resizeEvent (QResizeEvent* e) override;
};

/*! QTMTabPageBar is used to wrap the QTMTabPageContainer.
In order to:
1. Add this to the QMainWindow as QToolBar, just like the main icon toolbar;
2. Support dragging and docking like the main icon toolbar.
 */
class QTMTabPageBar : public QToolBar {
public:
  explicit QTMTabPageBar (const QString& p_title, QWidget* p_parent)
      : QToolBar (p_title, p_parent) {}
};

#endif // QTMTABPAGE_HPP