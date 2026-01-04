/******************************************************************************
 * MODULE     : qt_guide_window.hpp
 * DESCRIPTION: IntelliJ IDEA style startup login dialog
 * COPYRIGHT  : (C) 2025 yinyu
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef STARTUP_LOGIN_DIALOG_H
#define STARTUP_LOGIN_DIALOG_H

#include <QPropertyAnimation>
#include <QtWidgets/QDialog>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QProgressBar>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QVBoxLayout>

#if defined(Q_OS_MAC) || defined(Q_OS_LINUX) || defined(Q_OS_WIN)
#include "../QWindowKit/windowbar.hpp"
#include <QWKWidgets/widgetwindowagent.h>
#endif

namespace QWK {

class StartupLoginDialog : public QDialog {
  Q_OBJECT

public:
  enum Result {
    LoginClicked,
    SkipClicked,
    DialogRejected,
    InitializationComplete
  };

  explicit StartupLoginDialog (QWidget* parent= nullptr);
  ~StartupLoginDialog ();

  Result execWithResult ();
  void   setModal (bool modal);

  // New methods for non-blocking initialization
  void startInitialization ();
  bool isInitializationComplete () const { return initializationComplete; }

signals:
  void loginRequested ();
  void skipRequested ();
  void initializationStarted ();
  void initializationFinished (bool success);
  void windowReadyForTransition ();

protected:
  void showEvent (QShowEvent* event) override;
  void closeEvent (QCloseEvent* event) override;

private:
  void    setupUi ();
  QString styleSheet () const;
  void    initializeProgressUi ();
  void    startBackgroundInitialization ();
  void    fadeOutAndClose ();

  // UI elements
  QLabel*      titleLabel;
  QLabel*      subtitleLabel;
  QLabel*      featureLabel1;
  QLabel*      featureLabel2;
  QLabel*      featureLabel3;
  QLabel*      featureLabel4;
  QPushButton* loginButton;
  QPushButton* skipButton;
  QVBoxLayout* mainLayout;
  QVBoxLayout* featureLayout;
  QHBoxLayout* buttonLayout;

  // Progress UI elements (new)
  QProgressBar* progressBar;
  QLabel*       statusLabel;
  QLabel*       timeEstimationLabel;

  // Window management for frameless window
#if defined(Q_OS_MAC) || defined(Q_OS_LINUX) || defined(Q_OS_WIN)
  QWK::WidgetWindowAgent* windowAgent;
  QWK::WindowBar* windowBar;
#endif

  // Animation
  QPropertyAnimation* fadeAnimation;

  // Dialog result
  Result result;

  // Initialization state
  bool initializationInProgress;
  bool initializationComplete;
  bool userChoiceMade;
};

} // namespace QWK

#endif // STARTUP_LOGIN_DIALOG_H