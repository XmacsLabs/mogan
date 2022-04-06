//
// Created by PikachuHy on 2022/4/6.
//

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
