/******************************************************************************
 * MODULE     : qt_guide_window.cpp
 * DESCRIPTION: IntelliJ IDEA style startup login dialog implementation
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_guide_window.hpp"
#include "qt_guide_task_executor.hpp"
#include "qt_utilities.hpp"
#include <QApplication>
#include <QCloseEvent>
#include <QColor>
#include <QIcon>
#include <QPixmap>
#include <QPropertyAnimation>
#include <QShowEvent>
#include <QStyle>
#include <QTimer>

namespace QWK {

// StartupLoginDialog implementation
void
StartupLoginDialog::setupUi () {
  // Create title label
  titleLabel= new QLabel (qt_translate ("Welcome to Liii STEM"),
                          this); // Welcome to Liii STEM
  titleLabel->setAlignment (Qt::AlignCenter);
  titleLabel->setObjectName ("titleLabel");

  // Create subtitle label
  subtitleLabel= new QLabel (
      qt_translate ("Log in to sync settings and access all features"),
      this); // Log in to sync settings and access all features
  subtitleLabel->setAlignment (Qt::AlignCenter);
  subtitleLabel->setObjectName ("subtitleLabel");

  // Create feature labels
  featureLabel1= new QLabel (
      "1. " + qt_translate ("Register now and receive a 14-day membership."),
      this); // Register now and receive a 14-day membership.
  featureLabel2= new QLabel (
      "2. " + qt_translate ("Log in to sync settings and access all features"),
      this); // Log in to sync settings and access all features
  featureLabel3=
      new QLabel ("3. " + qt_translate ("Log in to chat with the AI."),
                  this); // Log in to chat with the AI.
  featureLabel4= new QLabel (
      "4. " + qt_translate ("Log in and enjoy seamless Markdown import."),
      this); // Log in and enjoy seamless Markdown import.

  featureLabel1->setObjectName ("featureLabel");
  featureLabel2->setObjectName ("featureLabel");
  featureLabel3->setObjectName ("featureLabel");
  featureLabel4->setObjectName ("featureLabel");

  // Create buttons
  loginButton= new QPushButton (qt_translate ("登录"), this); // 登录 Log In
  loginButton->setObjectName ("loginButton");
  loginButton->setDefault (true);

  skipButton=
      new QPushButton (qt_translate ("跳过登录"), this); // 跳过登录 Skip Login
  skipButton->setObjectName ("skipButton");

  // Create layouts
  featureLayout= new QVBoxLayout ();
  featureLayout->setSpacing (4);
  featureLayout->addWidget (featureLabel1);
  featureLayout->addWidget (featureLabel2);
  featureLayout->addWidget (featureLabel3);
  featureLayout->addWidget (featureLabel4);

  buttonLayout= new QHBoxLayout ();
  buttonLayout->addStretch ();
  buttonLayout->addWidget (loginButton);
  buttonLayout->addWidget (skipButton);
  buttonLayout->addStretch ();

  mainLayout= new QVBoxLayout (this);
  mainLayout->addWidget (titleLabel);
  mainLayout->addWidget (subtitleLabel);
  mainLayout->addSpacing (20);
  mainLayout->addLayout (featureLayout);
  mainLayout->addStretch ();
  mainLayout->addLayout (buttonLayout);

  setLayout (mainLayout);

  // Initialize progress UI (initially hidden)
  initializeProgressUi ();
}

QString
StartupLoginDialog::styleSheet () const {
  return QStringLiteral (R"(
        QDialog {
            background-color: #1a1a1a;
            border: 1px solid #333333;
            border-radius: 12px;
        }
        QLabel {
            color: #e0e0e0;
            background-color: transparent;
        }
        QLabel#titleLabel {
            color: #ffffff;
            font-size: 28px;
            font-weight: 700;
            margin: 40px 0 16px 0;
        }
        QLabel#subtitleLabel {
            color: #a0a0a0;
            font-size: 16px;
            margin: 0 0 32px 0;
        }
        QLabel#featureLabel {
            color: #cccccc;
            font-size: 14px;
            padding: 4px 0 4px 20px;
            margin: 0;
        }
        QPushButton {
            border: none;
            border-radius: 8px;
            padding: 12px 24px;
            font-size: 14px;
            font-weight: 600;
            min-width: 120px;
        }
        QPushButton#loginButton {
            background-color: #007AFF;
            color: white;
            font-size: 16px;
            font-weight: 600;
            padding: 14px 32px;
        }
        QPushButton#loginButton:hover {
            background-color: #0063d1;
        }
        QPushButton#loginButton:pressed {
            background-color: #0056CC;
        }
        QPushButton#skipButton {
            background-color: transparent;
            color: #a0a0a0;
            border: 1px solid #444444;
            font-weight: 500;
        }
        QPushButton#skipButton:hover {
            background-color: rgba(255, 255, 255, 0.05);
            color: #ffffff;
            border-color: #666666;
        }
        QPushButton#skipButton:pressed {
            background-color: rgba(255, 255, 255, 0.1);
        }
        QProgressBar {
            border: 1px solid #444444;
            border-radius: 4px;
            text-align: center;
            background-color: #2a2a2a;
            margin: 20px 40px 10px 40px;
        }
        QProgressBar::chunk {
            background-color: #007AFF;
            border-radius: 3px;
        }
        QLabel#statusLabel {
            color: #cccccc;
            font-size: 14px;
            margin: 10px 40px 5px 40px;
        }
        QLabel#timeEstimationLabel {
            color: #a0a0a0;
            font-size: 12px;
            margin: 0 40px 20px 40px;
        }
    )");
}

StartupLoginDialog::StartupLoginDialog (QWidget* parent)
    : QDialog (parent), titleLabel (nullptr), subtitleLabel (nullptr),
      featureLabel1 (nullptr), featureLabel2 (nullptr), featureLabel3 (nullptr),
      featureLabel4 (nullptr), loginButton (nullptr), skipButton (nullptr),
      mainLayout (nullptr), featureLayout (nullptr), buttonLayout (nullptr),
      progressBar (nullptr), statusLabel (nullptr),
      timeEstimationLabel (nullptr),
#if defined(Q_OS_MAC) || defined(Q_OS_LINUX) || defined(Q_OS_WIN)
      windowAgent (nullptr), windowBar (nullptr),
#endif
      fadeAnimation (nullptr),
      result (DialogRejected), initializationInProgress (false),
      initializationComplete (false), userChoiceMade (false) {

  setWindowFlags ((windowFlags () | Qt::FramelessWindowHint) & ~Qt::WindowContextHelpButtonHint);
  QPixmap transparentPixmap (16, 16);
  transparentPixmap.fill (QColor (0, 0, 0, 0));
  setWindowIcon (QIcon (transparentPixmap));
  setFixedSize (500, 400);
  setWindowTitle (QObject::tr (" "));

  // Set style sheet
  setStyleSheet (styleSheet ());

  // Setup frameless window management
#if defined(Q_OS_MAC) || defined(Q_OS_LINUX) || defined(Q_OS_WIN)
  windowAgent = new QWK::WidgetWindowAgent(this);
  windowAgent->setup(this);
  // No title bar for this dialog, make the whole window draggable
  // We'll set hit test visible for appropriate widgets
#endif

  // Setup UI
  setupUi ();

  // Setup draggable areas for frameless window
#if defined(Q_OS_MAC) || defined(Q_OS_LINUX) || defined(Q_OS_WIN)
  if (windowAgent) {
    // Make title and subtitle labels draggable
    if (titleLabel) {
      windowAgent->setHitTestVisible(titleLabel, true);
    }
    if (subtitleLabel) {
      windowAgent->setHitTestVisible(subtitleLabel, true);
    }
    // Optionally make other non-interactive areas draggable
  }
#endif

  // Connect signals
  connect (loginButton, &QPushButton::clicked, this, [this] () {
    result        = StartupLoginDialog::LoginClicked;
    userChoiceMade= true;
    emit loginRequested ();

    if (initializationComplete) {
      // Initialization already complete, fade out and close
      fadeOutAndClose ();
    }
    else if (!initializationInProgress) {
      // Initialization not started yet, start it now
      startInitialization ();
    }
    // If initialization in progress, just wait for completion
    // The completion handler will call fadeOutAndClose()
  });

  connect (skipButton, &QPushButton::clicked, this, [this] () {
    result        = StartupLoginDialog::SkipClicked;
    userChoiceMade= true;
    emit skipRequested ();

    if (initializationComplete) {
      // Initialization already complete, fade out and close
      fadeOutAndClose ();
    }
    else if (!initializationInProgress) {
      // Initialization not started yet, start it now
      startInitialization ();
    }
    // If initialization in progress, just wait for completion
    // The completion handler will call fadeOutAndClose()
  });

  // Connect reject signal (e.g., window close button)
  connect (this, &QDialog::rejected, this,
           [this] () { result= StartupLoginDialog::DialogRejected; });
}

StartupLoginDialog::~StartupLoginDialog () {
#if defined(Q_OS_MAC) || defined(Q_OS_LINUX) || defined(Q_OS_WIN)
  if (windowBar) {
    delete windowBar;
    windowBar = nullptr;
  }
  if (windowAgent) {
    delete windowAgent;
    windowAgent = nullptr;
  }
#endif
}

StartupLoginDialog::Result
StartupLoginDialog::execWithResult () {
  result= DialogRejected;
  if (exec () == QDialog::Accepted) {
    return result;
  }
  return DialogRejected;
}

void
StartupLoginDialog::showEvent (QShowEvent* event) {
  QDialog::showEvent (event);
  // Center the dialog on the screen
  QRect screenGeometry= QApplication::primaryScreen ()->availableGeometry ();
  move (screenGeometry.center () - rect ().center ());

  // Start initialization automatically when dialog is shown
  if (!initializationInProgress && !initializationComplete) {
    startInitialization ();
  }
}

void
StartupLoginDialog::initializeProgressUi () {
  // Create progress bar
  progressBar= new QProgressBar (this);
  progressBar->setObjectName ("progressBar");
  progressBar->setRange (0, 100);
  progressBar->setValue (0);
  progressBar->setTextVisible (true);
  progressBar->setFormat ("%p%");
  progressBar->setVisible (false); // Hidden initially

  // Create status label
  statusLabel= new QLabel (qt_translate ("准备初始化..."), this);
  statusLabel->setObjectName ("statusLabel");
  statusLabel->setAlignment (Qt::AlignCenter);
  statusLabel->setVisible (false);

  // Create time estimation label
  timeEstimationLabel= new QLabel ("", this);
  timeEstimationLabel->setObjectName ("timeEstimationLabel");
  timeEstimationLabel->setAlignment (Qt::AlignCenter);
  timeEstimationLabel->setVisible (false);

  // Add progress widgets to main layout (before buttons)
  mainLayout->insertWidget (mainLayout->count () - 1, progressBar);
  mainLayout->insertWidget (mainLayout->count () - 1, statusLabel);
  mainLayout->insertWidget (mainLayout->count () - 1, timeEstimationLabel);
}

void
StartupLoginDialog::startInitialization () {
  if (initializationInProgress || initializationComplete) {
    return;
  }

  initializationInProgress= true;
  initializationComplete  = false;
  userChoiceMade          = false;

  // Show progress UI
  progressBar->setVisible (true);
  statusLabel->setVisible (true);
  timeEstimationLabel->setVisible (true);

  // Hide feature labels and adjust spacing
  featureLabel1->setVisible (false);
  featureLabel2->setVisible (false);
  featureLabel3->setVisible (false);
  featureLabel4->setVisible (false);

  // Update status
  statusLabel->setText (qt_translate ("正在初始化..."));

  // Start background initialization
  startBackgroundInitialization ();

  emit initializationStarted ();
}

void
StartupLoginDialog::startBackgroundInitialization () {
  // Create and configure the bootstrap task executor (single-threaded)
  BootstrapTaskExecutor* executor= new BootstrapTaskExecutor (this);

  // Connect executor signals
  connect (executor, &BootstrapTaskExecutor::progressUpdated, this,
           [this] (int step, const QString& message, int percentage) {
             progressBar->setValue (percentage);
             statusLabel->setText (message);
           });

  connect (
      executor, &BootstrapTaskExecutor::timeEstimationUpdated, this,
      [this] (qint64 elapsedMs, qint64 estimatedTotalMs) {
        if (estimatedTotalMs > 0) {
          qint64  remainingMs = estimatedTotalMs - elapsedMs;
          int     remainingSec= static_cast<int> (remainingMs / 1000);
          QString timeText;
          if (remainingSec > 60) {
            // 分开翻译，避免 %1 被翻译系统错误处理
            QString prefix = qt_translate ("剩余时间: ");
            QString suffix = qt_translate ("分钟");
            timeText = prefix + QString::number ((remainingSec + 30) / 60) + " " + suffix;
          }
          else {
            // 分开翻译，避免 %1 被翻译系统错误处理
            QString prefix = qt_translate ("剩余时间: ");
            QString suffix = qt_translate ("秒");
            timeText = prefix + QString::number (qMax (remainingSec, 1)) + " " + suffix;
          }
          timeEstimationLabel->setText (timeText);
        }
      });

  connect (executor, &BootstrapTaskExecutor::initializationComplete, this,
           [this, executor] (bool success) {
             initializationInProgress= false;
             initializationComplete  = true;

             if (success) {
               statusLabel->setText (qt_translate ("初始化完成，注册即送14天会员期限哦！"));
               progressBar->setValue (100);
               timeEstimationLabel->setText (qt_translate ("准备就绪"));

               // If user already made a choice, trigger transition
               if (userChoiceMade) {
                 fadeOutAndClose ();
               }
               else {
                 // Enable buttons and update UI for user choice
                 loginButton->setEnabled (true);
                 skipButton->setEnabled (true);

                 // Show feature labels and hide progress UI after
                 // initialization featureLabel1->setVisible(true);
                 // featureLabel2->setVisible(true);
                 // featureLabel3->setVisible(true);
                 // featureLabel4->setVisible(true);
                 progressBar->setVisible (true);
                 statusLabel->setVisible (true);
                 timeEstimationLabel->setVisible (true);

                 emit windowReadyForTransition ();
               }
             }
             else {
               // Initialization failed
               statusLabel->setText (qt_translate ("初始化失败"));
               progressBar->setValue (0);
               timeEstimationLabel->setText (qt_translate ("请重试"));

               // Re-enable buttons for retry (though retry not implemented yet)
               loginButton->setEnabled (true);
               skipButton->setEnabled (true);

               // // Show feature labels even if initialization failed
               // featureLabel1->setVisible(true);
               // featureLabel2->setVisible(true);
               // featureLabel3->setVisible(true);
               // featureLabel4->setVisible(true);
             }

             emit initializationFinished (success);
             executor->deleteLater ();
           });

  connect (executor, &BootstrapTaskExecutor::errorOccurred, this,
           [this] (const QString& error) {
             // 分开翻译，避免 %1 被翻译系统错误处理
             QString prefix = qt_translate ("错误: ");
             statusLabel->setText (prefix + error);
           });

  // Disable buttons during initialization
  loginButton->setEnabled (false);
  skipButton->setEnabled (false);

  // Start the executor in the main thread (not a separate thread)
  executor->start ();
}

void
StartupLoginDialog::fadeOutAndClose () {
  // Create fade-out animation
  fadeAnimation= new QPropertyAnimation (this, "windowOpacity");
  fadeAnimation->setDuration (300);
  fadeAnimation->setStartValue (1.0);
  fadeAnimation->setEndValue (0.0);
  fadeAnimation->setEasingCurve (QEasingCurve::OutCubic);

  connect (fadeAnimation, &QPropertyAnimation::finished, this, [this] () {
    accept (); // Close dialog with acceptance
  });

  fadeAnimation->start ();
}

void
StartupLoginDialog::setModal (bool modal) {
  // Override to ensure dialog stays non-modal for background initialization
  QDialog::setModal (modal);
  // Note: The actual modal state is controlled by the caller
  // We keep this override for compatibility
}

void
StartupLoginDialog::closeEvent (QCloseEvent* event) {
  // Handle window close button (X)
  if (initializationInProgress) {
    // Ask for confirmation if initialization is in progress
    // For now, just prevent closing during initialization
    event->ignore ();
    return;
  }

  result= DialogRejected;
  QDialog::closeEvent (event);
}

} // namespace QWK