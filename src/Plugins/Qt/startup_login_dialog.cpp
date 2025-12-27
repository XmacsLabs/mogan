/******************************************************************************
 * MODULE     : startup_login_dialog.cpp
 * DESCRIPTION: IntelliJ IDEA style startup login dialog implementation
 * COPYRIGHT  : (C) 2025
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_utilities.hpp"
#include "startup_login_dialog.hpp"
#include <QShowEvent>
#include <QCloseEvent>
#include <QApplication>
#include <QStyle>
#include <QIcon>
#include <QPixmap>
#include <QColor>

namespace QWK {

// StartupLoginDialog implementation
void StartupLoginDialog::setupUi()
{
    // Create title label
    titleLabel = new QLabel(qt_translate("欢迎使用 Liii STEM"), this); // 欢迎使用 Liii STEM
    titleLabel->setAlignment(Qt::AlignCenter);
    titleLabel->setObjectName("titleLabel");

    // Create subtitle label
    subtitleLabel = new QLabel(qt_translate("登录即可同步设置并访问所有功能。"), this); // 登录即可同步设置并访问所有功能。
    subtitleLabel->setAlignment(Qt::AlignCenter);
    subtitleLabel->setObjectName("subtitleLabel");

    // Create feature labels
    featureLabel1 = new QLabel(qt_translate("1. 注册即送14天会员期限"), this); // 注册即送14天会员期限
    featureLabel2 = new QLabel(qt_translate("2. 登录即可同步设置并访问所有功能。"), this); // 登录即可同步设置并访问所有功能。
    featureLabel3 = new QLabel(qt_translate("3. 登录即可与AI进行对话"), this); // 登录即可与AI进行对话
    featureLabel4 = new QLabel(qt_translate("4. 登录即享Markdown无缝导入"), this); // 登录即享Markdown无缝导入

    featureLabel1->setObjectName("featureLabel");
    featureLabel2->setObjectName("featureLabel");
    featureLabel3->setObjectName("featureLabel");
    featureLabel4->setObjectName("featureLabel");

    // Create buttons
    loginButton = new QPushButton(qt_translate("登录"), this); // 登录 Log In
    loginButton->setObjectName("loginButton");
    loginButton->setDefault(true);

    skipButton = new QPushButton(qt_translate("跳过登录"), this); // 跳过登录 Skip Login
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

    mainLayout = new QVBoxLayout(this);
    mainLayout->addWidget(titleLabel);
    mainLayout->addWidget(subtitleLabel);
    mainLayout->addSpacing(20);
    mainLayout->addLayout(featureLayout);
    mainLayout->addStretch();
    mainLayout->addLayout(buttonLayout);

    setLayout(mainLayout);
}

QString StartupLoginDialog::styleSheet() const
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

StartupLoginDialog::StartupLoginDialog(QWidget* parent)
    : QDialog(parent)
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
    , result(DialogRejected)
{

    setWindowFlags(windowFlags() & ~Qt::WindowContextHelpButtonHint);
    QPixmap transparentPixmap(16, 16);
    transparentPixmap.fill(QColor(0, 0, 0, 0));
    setWindowIcon(QIcon(transparentPixmap));
    setFixedSize(500, 400);
    setWindowTitle(QObject::tr(" "));

    // Set style sheet
    setStyleSheet(styleSheet());

    // Setup UI
    setupUi();

    // Connect signals
    connect(loginButton, &QPushButton::clicked, this, [this]() {
        result = StartupLoginDialog::LoginClicked;
        emit loginRequested();
        accept();
    });

    connect(skipButton, &QPushButton::clicked, this, [this]() {
        result = StartupLoginDialog::SkipClicked;
        emit skipRequested();
        accept();
    });

    // Connect reject signal (e.g., window close button)
    connect(this, &QDialog::rejected, this, [this]() {
        result = StartupLoginDialog::DialogRejected;
    });
}

StartupLoginDialog::~StartupLoginDialog()
{
}

StartupLoginDialog::Result StartupLoginDialog::execWithResult()
{
    result = DialogRejected;
    if (exec() == QDialog::Accepted) {
        return result;
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