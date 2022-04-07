/******************************************************************************
* MODULE     : texmacs.cpp
* DESCRIPTION: Document Tab Bar
* COPYRIGHT  : (C) 2022  PikachuHy
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEXMACS_DOCTABBAR_H
#define TEXMACS_DOCTABBAR_H
#include "url.hpp"
#include <QList>
#include <QTabBar>
class DocTabBar : public QTabBar {
  Q_OBJECT
public:
  explicit DocTabBar (QWidget *parent= nullptr);
  void updateTabs (const url &cur_buffer= url ());

private slots:
  void handleTabBarClicked (int index);
  void handleTabCloseRequested (int index);

private:
  QList<url> m_buffers;
};

#endif // TEXMACS_DOCTABBAR_H
