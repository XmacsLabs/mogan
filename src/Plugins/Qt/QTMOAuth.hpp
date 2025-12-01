
/******************************************************************************
 * MODULE     : QTMOAuth.cpp
 * DESCRIPTION: Mogan OAuth Module headers
 * COPYRIGHT  : (C) 2025  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMOAUTH_H
#define QTMOAUTH_H

#include <QtNetworkAuth/qoauth2authorizationcodeflow.h>

#include <QtNetwork/qnetworkrequestfactory.h>
#include <QtNetworkAuth/qoauthhttpserverreplyhandler.h>

#include <QtCore/qabstractitemmodel.h>
#include <QtCore/qjsonobject.h>
#include <QtCore/qpointer.h>
#include <QtCore/qtimer.h>

QT_FORWARD_DECLARE_CLASS (QRestAccessManager)

class QTMOAuth : public QObject {
  Q_OBJECT

public:
  QTMOAuth (QObject* parent= nullptr);
  void login ();
  void refreshToken ();
  bool checkTokenValidity ();
  void handleAuthorizationCode (const QString& code);

private slots:
  void onTokenGranted ();
  void checkTokenStatus ();

signals:
  void tokenRefreshed ();
  void tokenRefreshFailed (const QString& error);
  void tokenExpired ();

private:
  void    loadExistingToken ();
  QString generateCodeVerifier ();
  QString generateCodeChallenge (const QString& verifier);

public:
  void clearInvalidTokens ();

  QRestAccessManager*           network= nullptr;
  QOAuth2AuthorizationCodeFlow  oauth2;
  QOAuthHttpServerReplyHandler* m_reply;
  QTimer*                       m_tokenCheckTimer;
  QString                       m_refreshToken;
  qint64                        m_tokenExpiryTime= 0;
  QString                       m_codeVerifier;
  QString                       m_codeChallenge;
};

#endif
