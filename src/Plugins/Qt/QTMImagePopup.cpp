
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
  layout->setSpacing (1);
  setLayout (layout);
  // 阴影效果
  QGraphicsDropShadowEffect* effect= new QGraphicsDropShadowEffect (this);
  effect->setBlurRadius (40);
  effect->setOffset (0, 4);
  effect->setColor (QColor (0, 0, 0, 120));
  this->setGraphicsEffect (effect);

  QScreen*     Screen= QGuiApplication::primaryScreen ();
  const double Dpi   = Screen ? Screen->logicalDotsPerInch () : 96.0;
  const double Scale = Dpi / 96.0;
#if defined(Q_OS_MAC)
  const int IconSize= int (50 * Scale);
#else
  const int IconSize= int (40 * Scale);
#endif

  QString btn_style=
      "QPushButton { background-color: transparent; border: none; } "
      "QPushButton:hover { background-color: rgba(128, 128, 128, 0.3); border: "
      "none; } QPushButton:pressed { background-color: rgba(128, 128, 128, "
      "0.5); border: none; }";

  QPushButton* left= new QPushButton ();
  left->setObjectName ("Left Align");
  left->setIcon (QIcon (":/window-bar/left-align.svg"));
  left->setIconSize (QSize (IconSize, IconSize));
  left->setStyleSheet (btn_style);
  QPushButton* middle= new QPushButton ();
  middle->setObjectName ("Middle Align");
  middle->setIcon (QIcon (":/window-bar/middle-align.svg"));
  middle->setIconSize (QSize (IconSize, IconSize));
  middle->setStyleSheet (btn_style);
  QPushButton* right= new QPushButton ();
  right->setObjectName ("Right Align");
  right->setIcon (QIcon (":/window-bar/right-align.svg"));
  right->setIconSize (QSize (IconSize, IconSize));
  right->setStyleSheet (btn_style);
  QPushButton* ocr= new QPushButton ();
  ocr->setObjectName ("OCR");
  ocr->setIcon (QIcon (":/window-bar/ocr.svg"));
  ocr->setIconSize (QSize (IconSize, IconSize));
  ocr->setStyleSheet (btn_style);
  layout->addWidget (left);
  layout->addWidget (middle);
  layout->addWidget (right);
  layout->addWidget (ocr);
  connect (left, &QPushButton::clicked, this,
           [=] () { call ("set-image-alignment", current_tree, "left"); });
  connect (middle, &QPushButton::clicked, this,
           [=] () { call ("set-image-alignment", current_tree, "center"); });
  connect (right, &QPushButton::clicked, this,
           [=] () { call ("set-image-alignment", current_tree, "right"); });
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
QTMImagePopup::setImageTree (tree t) {
  if (t) this->current_tree= t;
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
  QRectF rect= this->rect ();
  rect.adjust (0.75, 0.75, -0.75, -0.75); // 居中描边
  painter.drawRoundedRect (rect, 6, 6);   // 圆角
}