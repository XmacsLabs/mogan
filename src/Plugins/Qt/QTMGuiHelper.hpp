
/******************************************************************************
 * MODULE     : QTMGuiHelper.hpp
 * DESCRIPTION: QT Gui helper class. Infrastructure for delayed menu
 *installation COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMGUIHELPER_HPP
#define QTMGUIHELPER_HPP

#include "qt_gui.hpp"
#include <QObject>

/*!
 */
class QTMGuiHelper : public QObject {
  Q_OBJECT
  qt_gui_rep* gui;

public:
  inline QTMGuiHelper (qt_gui_rep* _gui) : QObject (), gui (_gui) {}

protected:
  bool eventFilter (QObject* obj, QEvent* event);

public slots:
  void doUpdate ();
  void doRefresh ();

  void aboutToShowMainMenu ();
  void aboutToHideMainMenu ();
  void doPopWaitingWidgets ();

  void emitTmSlotRefresh (string kind);

signals:
  void refresh ();
  void tmSlotRefresh (
      string); //!< qt_widgets which need to refresh connect here.
};

#endif // QTMGUIHELPER_HPP
