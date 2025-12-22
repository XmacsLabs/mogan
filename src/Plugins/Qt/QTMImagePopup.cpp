
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

const string left_str = "\"left\"";
const string mid_str  = "\"center\"";
const string right_str= "\"right\"";

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

  leftBtn= new QPushButton ();
  leftBtn->setObjectName ("image-align-button");
  leftBtn->setIcon (QIcon (":/window-bar/left-align.svg"));
  leftBtn->setIconSize (QSize (IconSize, IconSize));
  leftBtn->setStyleSheet (btn_style);
  middleBtn= new QPushButton ();
  middleBtn->setObjectName ("image-align-button");
  middleBtn->setIcon (QIcon (":/window-bar/middle-align.svg"));
  middleBtn->setIconSize (QSize (IconSize, IconSize));
  middleBtn->setStyleSheet (btn_style);
  rightBtn= new QPushButton ();
  rightBtn->setObjectName ("image-align-button");
  rightBtn->setIcon (QIcon (":/window-bar/right-align.svg"));
  rightBtn->setIconSize (QSize (IconSize, IconSize));
  rightBtn->setStyleSheet (btn_style);
  ocrBtn= new QPushButton ();
  ocrBtn->setObjectName ("image-align-button");
  ocrBtn->setIcon (QIcon (":/window-bar/ocr.svg"));
  ocrBtn->setIconSize (QSize (IconSize, IconSize));
  ocrBtn->setStyleSheet (btn_style);
  layout->addWidget (leftBtn);
  layout->addWidget (middleBtn);
  layout->addWidget (rightBtn);
  layout->addWidget (ocrBtn);
  connect (leftBtn, &QPushButton::clicked, this, [=] () {
    call ("set-image-alignment", current_tree, "left");
    resetStyleSheet ();
  });
  connect (middleBtn, &QPushButton::clicked, this, [=] () {
    call ("set-image-alignment", current_tree, "center");
    resetStyleSheet ();
  });
  connect (rightBtn, &QPushButton::clicked, this, [=] () {
    call ("set-image-alignment", current_tree, "right");
    resetStyleSheet ();
  });
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
  resetStyleSheet ();
  show ();
}

void
QTMImagePopup::resetStyleSheet () {
  leftBtn->setStyleSheet("QPushButton { background-color: transparent;}");
  middleBtn->setStyleSheet("QPushButton { background-color: transparent;}");
  rightBtn->setStyleSheet("QPushButton { background-color: transparent;}");
  object obj   = call ("get-image-alignment", current_tree);
  current_align= object_to_string (obj);
  if (current_align == left_str) {
    leftBtn->setStyleSheet ("QPushButton { background-color: rgba(92, 184, "
                            "255, 1); border: none; }");
  }
  else if (current_align == mid_str) {
    middleBtn->setStyleSheet ("QPushButton { background-color: rgba(92, 184, "
                              "255, 1); border: none; }");
  }
  else if (current_align == right_str) {
    rightBtn->setStyleSheet ("QPushButton { background-color: rgba(92, 184, "
                             "255, 1); border: none; }");
  }
}

void
QTMImagePopup::setImageTree (tree t) {
  this->current_tree= t;
}

void
QTMImagePopup::scrollBy (int x, int y) {
  cached_scroll_x-= (int) (x / cached_magf);
  cached_scroll_y-= (int) (y / cached_magf);
}

void
QTMImagePopup::updatePosition () {
  int pos_x, pos_y;
  getCachedPosition (pos_x, pos_y);
  move (pos_x, pos_y);
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