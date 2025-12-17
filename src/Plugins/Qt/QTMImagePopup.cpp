
/******************************************************************************
 * MODULE     : QTMMathCompletionPopup.cpp
 * DESCRIPTION:
 * COPYRIGHT  : (C) 2025 Mogan STEM authors
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMImagePopup.hpp"
#include "server.hpp"

#include <QIcon>
#include <QPainter>
#include <QPen>
#include <QPushButton>

QTMImagePopup::QTMImagePopup (QWidget* parent, qt_simple_widget_rep* owner)
    : QWidget (parent), owner (owner), layout (nullptr) {
  setObjectName ("image_popup");
  setWindowFlags (Qt::FramelessWindowHint | Qt::WindowStaysOnTopHint);
  setAttribute (Qt::WA_ShowWithoutActivating);
  setMouseTracking (true);
  setFocusPolicy (Qt::NoFocus);
  layout= new QHBoxLayout (this);
  layout->setContentsMargins (0, 0, 0, 0);
  layout->setSizeConstraint (QLayout::SetMinimumSize);
  layout->setSpacing (10);
  setLayout (layout);
  // 阴影效果
  QGraphicsDropShadowEffect* effect= new QGraphicsDropShadowEffect (this);
  effect->setBlurRadius (40);
  effect->setOffset (0, 4);
  effect->setColor (QColor (0, 0, 0, 120));
  this->setGraphicsEffect (effect);
  QPushButton* left  = new QPushButton ("Left Align");
  QPushButton* center= new QPushButton ("Center");
  QPushButton* right = new QPushButton ("Right Align");
  QPushButton* ocr   = new QPushButton ("OCR");
  layout->addWidget (left);
  layout->addWidget (center);
  layout->addWidget (right);
  layout->addWidget (ocr);
}

QTMImagePopup::~QTMImagePopup () {}

void
QTMImagePopup::showImagePopup (rectangle selr, double magf, int scroll_x,
                               int scroll_y, int canvas_x) {
  cachePosition (selr, magf, scroll_x, scroll_y, canvas_x);
  this->adjustSize ();
  int x, y;
  getCachedPosition (x, y);
  QPoint topLeft (x, y);
  move (topLeft);
  raise ();
  show ();
}

void
QTMImagePopup::setWidget (QWidget* w) {
  cleanLayout ();
  this->setUpdatesEnabled (false);

  w->setParent (this);
  layout->addWidget (w);
  installEventFilterRecursively (w, this);

  w->show ();
  this->adjustSize ();

  this->setUpdatesEnabled (true);
}

void
QTMImagePopup::cleanLayout () {
  // 清空Layout中已有的内容
  QLayoutItem* item;
  while ((item= layout->takeAt (0)) != nullptr) {
    if (item->widget ()) {
      item->widget ()->setParent (nullptr);
    }
    delete item;
  }
  // 如果布局为空，隐藏窗口
  if (layout->count () == 0) {
    this->hide ();
  }
}

void
QTMImagePopup::cachePosition (rectangle selr, double magf, int scroll_x,
                              int scroll_y, int canvas_x) {
  cached_image_mid_x= (selr->x1 + selr->x2) / 2;
  cached_image_mid_y= selr->y2;
  cached_scroll_x   = scroll_x;
  cached_scroll_y   = scroll_y;
  cached_canvas_x   = canvas_x;
  cached_magf       = magf;
}

void
QTMImagePopup::getCachedPosition (int& x, int& y) {
  x= ((cached_image_mid_x - cached_scroll_x - 500) * cached_magf +
      cached_canvas_x) /
         256 -
     (this->width () / 2);
  y= -((cached_image_mid_y - 5000 - cached_scroll_y) * cached_magf) / 256 -
     this->height () * 1.2;
}

void
QTMImagePopup::installEventFilterRecursively (QWidget* widget,
                                              QObject* filterObj) {
  // 给组件和子组件递归装上事件过滤器
  if (!widget) return;

  // 安装事件过滤器到当前组件
  widget->installEventFilter (filterObj);

  // 递归安装事件过滤器到所有子组件
  const QObjectList& children= widget->children ();
  for (QObject* child : children) {
    QWidget* childWidget= qobject_cast<QWidget*> (child);
    if (childWidget) {
      installEventFilterRecursively (childWidget, filterObj);
    }
  }
}

void
QTMImagePopup::paintEvent (QPaintEvent* event) {
  // 保持原有绘制
  QPainter painter (this);
  painter.setRenderHint (QPainter::Antialiasing);
  // 绘制白色背景
  painter.setPen (Qt::NoPen);
  painter.setBrush (QColor (255, 255, 255, 255));
  QRectF bgRect= this->rect ();
  painter.drawRoundedRect (bgRect, 6, 6);
  // 绘制黑色边框
  QPen pen (Qt::black, 1.5);
  painter.setPen (pen);
  painter.setBrush (Qt::NoBrush);
  QRectF rect= this->rect ();
  rect.adjust (0.75, 0.75, -0.75, -0.75); // 居中描边
  painter.drawRoundedRect (rect, 6, 6);   // 圆角
}