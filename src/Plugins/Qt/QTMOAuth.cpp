
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
  // 加载 OAuth2 配置
  eval ("(use-modules (liii account))");

  c_string authorizationUrl (
      as_string (call ("account-oauth2-config", "authorization-url")));
  c_string accessTokenUrl (
      as_string (call ("account-oauth2-config", "access-token-url")));
  c_string clientIdentifier (
      as_string (call ("account-oauth2-config", "client-identifier")));
  c_string scope (as_string (call ("account-oauth2-config", "scope")));
  c_string portStr (as_string (call ("account-oauth2-config", "port")));

  int port= QString ((char*) portStr).toInt ();
  m_reply = new QOAuthHttpServerReplyHandler (
      QHostAddress (QString::fromUtf8 ("127.0.0.1")), port, this);
  m_reply->setCallbackPath ("/callback");

  // 设置自定义成功页面
  QString customHtml=
      "<!doctype html><html><head>"
      "<meta charset='utf-8'>"
      "<style>body{font-family:Arial;background:#f2f2f2;margin:0;padding:20px}"
      "h2{color:#000;margin:0}</style>"
      "</head><body>"
      "<h2>登录成功！您可以关闭此页面并返回应用。</h2>"
      "<script>window.setTimeout(function(){window.close()},3000);</script>"
      "</body></html>";
  m_reply->setCallbackText (customHtml);

  oauth2.setReplyHandler (m_reply);
  oauth2.setAuthorizationUrl (QUrl ((char*) authorizationUrl));
  oauth2.setAccessTokenUrl (QUrl ((char*) accessTokenUrl));
  oauth2.setScope ((char*) scope);
  oauth2.setClientIdentifier ((char*) clientIdentifier);

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
