// Copyright (C) 2023-2024 Stdware Collections (https://www.github.com/stdware)
// Copyright (C) 2021-2023 wangwenx190 (Yuhang Zhao)
// SPDX-License-Identifier: Apache-2.0

#include "guestnotificationbar.hpp"
#include "qt_utilities.hpp"

#include <QApplication>
#include <QFontMetrics>
#include <QIcon>
#include <QPainter>
#include <QStyle>

namespace QWK {

GuestNotificationBar::GuestNotificationBar (QWidget* parent)
    : QFrame (parent), m_layout (nullptr), m_iconLabel (nullptr),
      m_messageLabel (nullptr), m_loginButton (nullptr),
      m_closeButton (nullptr) {
  setupUI ();
  applyStyle ();
  qDebug () << "GuestNotificationBar created, close button:" << m_closeButton;
}

GuestNotificationBar::~GuestNotificationBar ()= default;

void
GuestNotificationBar::setupUI () {
  qDebug () << "GuestNotificationBar::setupUI() called";
  // 设置主布局
  m_layout= new QHBoxLayout (this);
  m_layout->setContentsMargins (12, 8, 12, 8);
  m_layout->setSpacing (0);

  // 添加左侧拉伸空间
  m_layout->addStretch ();

  // 创建内容容器，用于包裹图标、文字和登录按钮
  QWidget*     contentWidget= new QWidget (this);
  QHBoxLayout* contentLayout= new QHBoxLayout (contentWidget);
  contentLayout->setContentsMargins (0, 0, 0, 0);
  contentLayout->setSpacing (8);

  // 警告图标
  m_iconLabel= new QLabel (contentWidget);
  m_iconLabel->setText ("⚠️");
  m_iconLabel->setAlignment (Qt::AlignCenter);
  contentLayout->addWidget (m_iconLabel);

  // 提示文字
  m_messageLabel= new QLabel (contentWidget);
  m_messageLabel->setText (qt_translate (
      "You are currently in guest mode, some features are limited"));
  m_messageLabel->setAlignment (Qt::AlignLeft | Qt::AlignVCenter);
  m_messageLabel->setWordWrap (false);
  contentLayout->addWidget (m_messageLabel);

  // 立即登录按钮
  m_loginButton= new QPushButton (qt_translate ("Login Now"), contentWidget);
  m_loginButton->setCursor (Qt::PointingHandCursor);
  m_loginButton->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Fixed);
  contentLayout->addWidget (m_loginButton);

  // 将内容容器添加到主布局
  m_layout->addWidget (contentWidget);

  // 添加右侧拉伸空间
  m_layout->addStretch ();

  // 关闭按钮 - 单独放在最右侧
  m_closeButton= new QPushButton (this);
  qDebug () << "Close button created:" << m_closeButton;
  m_closeButton->setText ("×"); // 使用乘号字符
  m_closeButton->setCursor (Qt::PointingHandCursor);
  m_closeButton->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Fixed);
  m_closeButton->setFixedSize (24, 24);
  // 设置文本对齐方式为居中
  m_closeButton->setStyleSheet ("text-align: center;");
  m_layout->addWidget (m_closeButton);
  qDebug () << "Close button added to main layout, visible:"
            << m_closeButton->isVisible ();

  // 连接信号
  connect (m_loginButton, &QPushButton::clicked, this,
           &GuestNotificationBar::loginRequested);
  connect (m_closeButton, &QPushButton::clicked, this,
           &GuestNotificationBar::closeRequested);

  // 设置对象名用于样式表
  setObjectName ("guestNotificationBar");
  m_iconLabel->setObjectName ("guestNotificationIcon");
  m_messageLabel->setObjectName ("guestNotificationMessage");
  m_loginButton->setObjectName ("guestNotificationLoginButton");
  m_closeButton->setObjectName ("guestNotificationCloseButton");
}

void
GuestNotificationBar::applyStyle () {
  qDebug () << "GuestNotificationBar::applyStyle() called";
  // 设置浅黄色背景 (#FFF3CD) 和浅橙色边框 (#FFE69C)
  QString styleSheet= R"(
    #guestNotificationBar {
      background-color: #FFF3CD;
      border: 1px solid #FFE69C;
      border-left: none;
      border-right: none;
      border-top: none;
    }

    #guestNotificationMessage {
      color: #856404;
      font-size: 13px;
      font-weight: 500;
    }

    #guestNotificationIcon {
      font-size: 16px;
      color: #856404;
      min-width: 20px;
    }

    #guestNotificationLoginButton {
      background-color: #FD7E14;
      color: white;
      border: none;
      border-radius: 4px;
      padding: 6px 16px;
      font-size: 13px;
      font-weight: 500;
    }

    #guestNotificationLoginButton:hover {
      background-color: #E96B00;
    }

    #guestNotificationLoginButton:pressed {
      background-color: #D45A00;
    }

    #guestNotificationCloseButton {
      background-color: transparent;
      color: #000000;
      border: none;
      font-size: 14px;
      font-weight: bold;
      border-radius: 4px;
      padding: 0;
      margin: 0;
      min-width: 24px;
      min-height: 24px;
    }

    #guestNotificationCloseButton:hover {
      background-color: rgba(0, 0, 0, 0.1);
    }

    #guestNotificationCloseButton:pressed {
      background-color: rgba(0, 0, 0, 0.2);
    }
  )";

  setStyleSheet (styleSheet);
}

void
GuestNotificationBar::setMessage (const QString& message) {
  m_messageLabel->setText (qt_translate (from_qstring (message)));
}

QString
GuestNotificationBar::message () const {
  return m_messageLabel->text ();
}

void
GuestNotificationBar::setLoginButtonVisible (bool visible) {
  m_loginButton->setVisible (visible);
}

bool
GuestNotificationBar::isLoginButtonVisible () const {
  return m_loginButton->isVisible ();
}

void
GuestNotificationBar::setCloseButtonVisible (bool visible) {
  m_closeButton->setVisible (visible);
}

bool
GuestNotificationBar::isCloseButtonVisible () const {
  return m_closeButton->isVisible ();
}

QPushButton*
GuestNotificationBar::loginButton () const {
  return m_loginButton;
}

QPushButton*
GuestNotificationBar::closeButton () const {
  return m_closeButton;
}

} // namespace QWK