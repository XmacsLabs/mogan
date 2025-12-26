/******************************************************************************
 * MODULE     : startup_login_dialog_p.hpp
 * DESCRIPTION: Private implementation of startup login dialog
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef STARTUP_LOGIN_DIALOG_PRIVATE_H
#define STARTUP_LOGIN_DIALOG_PRIVATE_H

#include <QtWidgets/QPushButton>
#include <QtWidgets/QLabel>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHBoxLayout>

#include "startup_login_dialog.hpp"

namespace QWK {

class StartupLoginDialogPrivate {
  Q_DECLARE_PUBLIC(StartupLoginDialog)
public:
  StartupLoginDialogPrivate();
  virtual ~StartupLoginDialogPrivate();

  void setupUi(QWidget* parent);
  QString styleSheet() const;

  StartupLoginDialog* q_ptr;

  // UI elements
  QLabel* titleLabel;
  QLabel* subtitleLabel;
  QLabel* featureLabel1;
  QLabel* featureLabel2;
  QLabel* featureLabel3;
  QLabel* featureLabel4;
  QPushButton* loginButton;
  QPushButton* skipButton;

  QVBoxLayout* mainLayout;
  QVBoxLayout* featureLayout;
  QHBoxLayout* buttonLayout;

  // Dialog result
  StartupLoginDialog::Result result;

private:
  Q_DISABLE_COPY(StartupLoginDialogPrivate)
};

} // namespace QWK

#endif // STARTUP_LOGIN_DIALOG_PRIVATE_H