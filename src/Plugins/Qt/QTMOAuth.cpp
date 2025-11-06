
/******************************************************************************
 * MODULE     : QTMOAuth.cpp
 * DESCRIPTION: Mogan OAuth Module impl
 * COPYRIGHT  : (C) 2025  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMOAuth.hpp"
#include "qt_utilities.hpp"
#include "scheme.hpp"

#include <QtGui/qdesktopservices.h>

#include <QtNetwork/qrestaccessmanager.h>
#include <QtNetwork/qrestreply.h>

#include <QtCore/qdebug.h>
#include <QtCore/qjsonarray.h>
#include <QtCore/qjsondocument.h>

QTMOAuth::QTMOAuth (QObject* parent) {
  static auto authorizationUrl=
      "http://test-www.liiistem.cn:8080/oauth2/authorize"; // 测试环境
  static auto accessTokenUrl=
      "http://test-www.liiistem.cn:8080/oauth2/token"; // 测试环境
  static auto scope= "openid+profile+email";
  m_reply          = new QOAuthHttpServerReplyHandler (
      QHostAddress (QString::fromUtf8 ("127.0.0.1")), 3000, this);
  m_reply->setCallbackPath ("/callback");
  oauth2.setReplyHandler (m_reply);
  oauth2.setAuthorizationUrl (QUrl (authorizationUrl));
  oauth2.setAccessTokenUrl (QUrl (accessTokenUrl));
  oauth2.setScope (scope);
  oauth2.setClientIdentifier ("public-client");

  connect (&oauth2, &QOAuth2AuthorizationCodeFlow::authorizeWithBrowser, this,
           &QDesktopServices::openUrl);

  QObject::connect (&oauth2, &QAbstractOAuth::granted, this, [this] {
    eval ("(use-modules (liii account))");
    call ("account-save-token", from_qstring (oauth2.token ()));
  });
}

void
QTMOAuth::login () {
  if (m_reply->isListening ()) {
    oauth2.grant ();
  }
}
