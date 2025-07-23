
/******************************************************************************
 * MODULE     : QTMAuxiliaryWidget.hpp
 * DESCRIPTION:
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTM_AUXILIARY_WIDGET_HPP
#define QTM_AUXILIARY_WIDGET_HPP

#include "command.hpp"

#include <QCloseEvent>
#include <QDockWidget>

class QTMAuxiliaryWidget : public QDockWidget {
  Q_OBJECT
public:
  QTMAuxiliaryWidget (const QString& p_title, QWidget* p_parent);
  ~QTMAuxiliaryWidget ();

protected:
  void closeEvent (QCloseEvent* event) override;
};

#endif // QTM_AUXILIARY_WIDGET_HPP
