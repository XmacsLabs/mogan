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
#include <QtWidgets/QLabel>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHBoxLayout>

namespace QWK {

class StartupLoginDialog : public QDialog {
  Q_OBJECT

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
  void setupUi();
  QString styleSheet() const;

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
  Result result;
};

} // namespace QWK

#endif // STARTUP_LOGIN_DIALOG_H