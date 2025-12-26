/******************************************************************************
 * MODULE     : startup_login_dialog.cpp
 * DESCRIPTION: IntelliJ IDEA style startup login dialog implementation
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "startup_login_dialog_p.hpp"
#include <QShowEvent>
#include <QCloseEvent>
#include <QApplication>
#include <QStyle>

namespace QWK {

// StartupLoginDialogPrivate implementation
StartupLoginDialogPrivate::StartupLoginDialogPrivate()
    : q_ptr(nullptr)
    , titleLabel(nullptr)
    , subtitleLabel(nullptr)
    , featureLabel1(nullptr)
    , featureLabel2(nullptr)
    , featureLabel3(nullptr)
    , featureLabel4(nullptr)
    , loginButton(nullptr)
    , skipButton(nullptr)
    , mainLayout(nullptr)
    , featureLayout(nullptr)
    , buttonLayout(nullptr)
    , result(StartupLoginDialog::DialogRejected)
{
}

StartupLoginDialogPrivate::~StartupLoginDialogPrivate()
{
}

void StartupLoginDialogPrivate::setupUi(QWidget* parent)
{
    Q_Q(StartupLoginDialog);

    // Create title label
    titleLabel = new QLabel(QObject::tr("Welcome to Mogan"), parent);
    titleLabel->setAlignment(Qt::AlignCenter);
    titleLabel->setObjectName("titleLabel");

    // Create subtitle label
    subtitleLabel = new QLabel(QObject::tr("Log in to sync settings and access all features"), parent);
    subtitleLabel->setAlignment(Qt::AlignCenter);
    subtitleLabel->setObjectName("subtitleLabel");

    // Create feature labels
    featureLabel1 = new QLabel(QStringLiteral("✓ Cloud sync your settings"), parent);
    featureLabel2 = new QLabel(QStringLiteral("✓ Team collaboration features"), parent);
    featureLabel3 = new QLabel(QStringLiteral("✓ Advanced code analysis"), parent);
    featureLabel4 = new QLabel(QStringLiteral("✓ Professional template library"), parent);

    featureLabel1->setObjectName("featureLabel");
    featureLabel2->setObjectName("featureLabel");
    featureLabel3->setObjectName("featureLabel");
    featureLabel4->setObjectName("featureLabel");

    // Create buttons
    loginButton = new QPushButton(QObject::tr("Log In"), parent);
    loginButton->setObjectName("loginButton");
    loginButton->setDefault(true);

    skipButton = new QPushButton(QObject::tr("Skip Login"), parent);
    skipButton->setObjectName("skipButton");

    // Create layouts
    featureLayout = new QVBoxLayout();
    featureLayout->setSpacing(8);
    featureLayout->addWidget(featureLabel1);
    featureLayout->addWidget(featureLabel2);
    featureLayout->addWidget(featureLabel3);
    featureLayout->addWidget(featureLabel4);

    buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch();
    buttonLayout->addWidget(loginButton);
    buttonLayout->addWidget(skipButton);
    buttonLayout->addStretch();

    mainLayout = new QVBoxLayout(parent);
    mainLayout->addWidget(titleLabel);
    mainLayout->addWidget(subtitleLabel);
    mainLayout->addSpacing(20);
    mainLayout->addLayout(featureLayout);
    mainLayout->addStretch();
    mainLayout->addLayout(buttonLayout);

    parent->setLayout(mainLayout);
}

QString StartupLoginDialogPrivate::styleSheet() const
{
    return QStringLiteral(R"(
        QDialog {
            background-color: #2b2b2b;
            border: 1px solid #3c3f41;
            border-radius: 6px;
        }
        QLabel {
            color: #bbbbbb;
            background-color: transparent;
        }
        QLabel#titleLabel {
            color: #ffffff;
            font-size: 28px;
            font-weight: bold;
            margin: 30px 0 20px 0;
        }
        QLabel#subtitleLabel {
            color: #999999;
            font-size: 16px;
            margin: 0 0 40px 0;
        }
        QLabel#featureLabel {
            color: #cccccc;
            font-size: 15px;
            padding-left: 10px;
        }
        QPushButton {
            border: none;
            border-radius: 4px;
            padding: 10px 20px;
            font-size: 14px;
            font-weight: bold;
            min-width: 120px;
        }
        QPushButton:hover {
            background-color: #4c5052;
        }
        QPushButton:pressed {
            background-color: #3c3f41;
        }
        QPushButton#loginButton {
            background-color: #4a8bdf;
            color: white;
            font-size: 16px;
            padding: 12px 24px;
        }
        QPushButton#loginButton:hover {
            background-color: #5a9bef;
        }
        QPushButton#loginButton:pressed {
            background-color: #3a7bcf;
        }
        QPushButton#skipButton {
            background-color: transparent;
            color: #999999;
            border: 1px solid #555555;
        }
        QPushButton#skipButton:hover {
            color: #ffffff;
            border-color: #666666;
        }
    )");
}

// StartupLoginDialog implementation
StartupLoginDialog::StartupLoginDialog(QWidget* parent)
    : QDialog(parent)
    , d_ptr(new StartupLoginDialogPrivate())
{
    Q_D(StartupLoginDialog);
    d->q_ptr = this;

    setWindowTitle(QObject::tr("Welcome to Mogan"));
    setFixedSize(500, 400);
    setWindowFlags(windowFlags() & ~Qt::WindowContextHelpButtonHint);

    // Set style sheet
    setStyleSheet(d->styleSheet());

    // Setup UI
    d->setupUi(this);

    // Connect signals
    connect(d->loginButton, &QPushButton::clicked, this, [this, d]() {
        d->result = StartupLoginDialog::LoginClicked;
        emit loginRequested();
        accept();
    });

    connect(d->skipButton, &QPushButton::clicked, this, [this, d]() {
        d->result = StartupLoginDialog::SkipClicked;
        emit skipRequested();
        accept();
    });

    // Connect reject signal (e.g., window close button)
    connect(this, &QDialog::rejected, this, [this, d]() {
        d->result = StartupLoginDialog::DialogRejected;
    });
}

StartupLoginDialog::~StartupLoginDialog()
{
}

StartupLoginDialog::Result StartupLoginDialog::execWithResult()
{
    Q_D(StartupLoginDialog);
    d->result = DialogRejected;
    if (exec() == QDialog::Accepted) {
        return d->result;
    }
    return DialogRejected;
}

void StartupLoginDialog::showEvent(QShowEvent* event)
{
    QDialog::showEvent(event);
    // Center the dialog on the screen
    QRect screenGeometry = QApplication::primaryScreen()->availableGeometry();
    move(screenGeometry.center() - rect().center());
}

} // namespace QWK