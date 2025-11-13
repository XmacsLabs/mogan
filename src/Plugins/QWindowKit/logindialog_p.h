/******************************************************************************
 * MODULE     : logindialog_p.h
 * COPYRIGHT  : (C) 2025 Liii
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef LOGINDIALOGPRIVATE_H
#define LOGINDIALOGPRIVATE_H

#include <QVBoxLayout>

#include "logindialog.h"

namespace QWK {

    class LoginDialogPrivate {
        Q_DECLARE_PUBLIC(LoginDialog)
    public:
        LoginDialogPrivate();
        virtual ~LoginDialogPrivate();

        void init();

        LoginDialog *q_ptr;

        QVBoxLayout *layout;
        QWidget *contentWidget;

    private:
        Q_DISABLE_COPY(LoginDialogPrivate)
    };

}

#endif // LOGINDIALOGPRIVATE_H