/******************************************************************************
 * MODULE     : startup_login_dialog.hpp
 * DESCRIPTION: IntelliJ IDEA style startup login dialog
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef STARTUP_LOGIN_DIALOG_H
#define STARTUP_LOGIN_DIALOG_H

#include <QtWidgets/QDialog>

namespace QWK {

class StartupLoginDialogPrivate;

class StartupLoginDialog : public QDialog {
  Q_OBJECT
  Q_DECLARE_PRIVATE(StartupLoginDialog)

public:
  enum Result {
    LoginClicked,
    SkipClicked,
    DialogRejected
  };

  explicit StartupLoginDialog(QWidget* parent = nullptr);
  ~StartupLoginDialog();

  Result execWithResult();

signals:
  void loginRequested();
  void skipRequested();

protected:
  void showEvent(QShowEvent* event) override;

private:
  QScopedPointer<StartupLoginDialogPrivate> d_ptr;
};

} // namespace QWK

#endif // STARTUP_LOGIN_DIALOG_H