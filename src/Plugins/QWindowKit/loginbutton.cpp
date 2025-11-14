/******************************************************************************
 * MODULE     : loginbutton.cpp
 * COPYRIGHT  : (C) 2025 Liii
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "loginbutton.hpp"
#include "loginbutton_p.hpp"

#include <QtCore/QDebug>
#include <QtGui/QtEvents>

namespace QWK {

LoginButtonPrivate::LoginButtonPrivate () {
  hovered= false;
  pressed= false;
}

LoginButtonPrivate::~LoginButtonPrivate ()= default;

void
LoginButtonPrivate::init () {
  Q_Q (LoginButton);
  q->setFixedSize (46, 32);
  q->setFocusPolicy (Qt::NoFocus);
}

void
LoginButtonPrivate::reloadIcon () {
  Q_Q (LoginButton);

  if (!q->isEnabled () && !iconDisabled.isNull ()) {
    q->setIcon (iconDisabled);
    return;
  }

  if (pressed && !iconPressed.isNull ()) {
    q->setIcon (iconPressed);
    return;
  }

  if (hovered && !iconHover.isNull ()) {
    q->setIcon (iconHover);
    return;
  }

  if (!iconNormal.isNull ()) {
    q->setIcon (iconNormal);
  }
}

LoginButton::LoginButton (QWidget* parent)
    : LoginButton (*new LoginButtonPrivate (), parent) {}

LoginButton::~LoginButton ()= default;

QIcon
LoginButton::iconNormal () const {
  Q_D (const LoginButton);
  return d->iconNormal;
}

void
LoginButton::setIconNormal (const QIcon& icon) {
  Q_D (LoginButton);
  d->iconNormal= icon;
  d->reloadIcon ();
}

QIcon
LoginButton::iconHover () const {
  Q_D (const LoginButton);
  return d->iconHover;
}

void
LoginButton::setIconHover (const QIcon& icon) {
  Q_D (LoginButton);
  d->iconHover= icon;
  d->reloadIcon ();
}

QIcon
LoginButton::iconPressed () const {
  Q_D (const LoginButton);
  return d->iconPressed;
}

void
LoginButton::setIconPressed (const QIcon& icon) {
  Q_D (LoginButton);
  d->iconPressed= icon;
  d->reloadIcon ();
}

QIcon
LoginButton::iconDisabled () const {
  Q_D (const LoginButton);
  return d->iconDisabled;
}

void
LoginButton::setIconDisabled (const QIcon& icon) {
  Q_D (LoginButton);
  d->iconDisabled= icon;
  d->reloadIcon ();
}

void
LoginButton::enterEvent (QEnterEvent* event) {
  Q_D (LoginButton);
  d->hovered= true;
  d->reloadIcon ();
  QPushButton::enterEvent (event);
}

void
LoginButton::leaveEvent (QEvent* event) {
  Q_D (LoginButton);
  d->hovered= false;
  d->reloadIcon ();
  QPushButton::leaveEvent (event);
}

void
LoginButton::mousePressEvent (QMouseEvent* event) {
  Q_D (LoginButton);
  if (event->button () == Qt::LeftButton) {
    d->pressed= true;
    d->reloadIcon ();
  }
  QPushButton::mousePressEvent (event);
}

void
LoginButton::mouseReleaseEvent (QMouseEvent* event) {
  Q_D (LoginButton);
  if (event->button () == Qt::LeftButton) {
    d->pressed= false;
    d->reloadIcon ();
  }
  QPushButton::mouseReleaseEvent (event);
}

LoginButton::LoginButton (LoginButtonPrivate& d, QWidget* parent)
    : QPushButton (parent), d_ptr (&d) {
  d.q_ptr= this;

  d.init ();
}

} // namespace QWK