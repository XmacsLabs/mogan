
/******************************************************************************
 * MODULE     : QTMAuxiliaryWidget.cpp
 * DESCRIPTION:
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

// TODO: 整理include
#include "QTMAuxiliaryWidget.hpp"
#include "qt_gui.hpp"
#include "server.hpp"

#include <QDockWidget>
#include <QPainter>

QTMAuxiliaryWidget::QTMAuxiliaryWidget (const QString& p_title,
                                        QWidget*       p_parent)
    : QDockWidget (p_title, p_parent) {}

QTMAuxiliaryWidget::~QTMAuxiliaryWidget () {}

void
QTMAuxiliaryWidget::closeEvent (QCloseEvent* event) {
  // 需要通过 scheme 层设置可见性，否则下次UI刷新会重置到先前的状态
  // 参见 qt_tm_widget_rep::update_visibility ()
  exec_delayed (scheme_cmd ("(show-auxiliary-widget #f)"));
  event->accept ();
}
