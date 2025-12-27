
/******************************************************************************
 * MODULE     : QTMImagePopup.cpp
 * DESCRIPTION:
 * COPYRIGHT  : (C) 2025 MoonLL, Yuki Lu
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMImagePopup.hpp"
#include "qbuttongroup.h"
#include "scheme.hpp"
#include "server.hpp"

#include <QIcon>
#include <QPainter>
#include <QPen>

const string left_str = "\"left\"";
const string mid_str  = "\"center\"";
const string right_str= "\"right\"";

// 悬浮菜单创建函数
QTMImagePopup::QTMImagePopup (QWidget* parent, qt_simple_widget_rep* owner)
    : QWidget (parent), owner (owner), layout (nullptr) {
  Q_INIT_RESOURCE (images);
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

  QGraphicsDropShadowEffect* effect= new QGraphicsDropShadowEffect (this);
  effect->setBlurRadius (40);
  effect->setOffset (0, 4);
  effect->setColor (QColor (0, 0, 0, 120));
  this->setGraphicsEffect (effect);

  leftBtn= new QToolButton ();
  leftBtn->setObjectName ("image-align-button");
  leftBtn->setProperty ("icon-name", "left");
  leftBtn->setCheckable (true);
  middleBtn= new QToolButton ();
  middleBtn->setObjectName ("image-align-button");
  middleBtn->setProperty ("icon-name", "center");
  middleBtn->setCheckable (true);
  rightBtn= new QToolButton ();
  rightBtn->setObjectName ("image-align-button");
  rightBtn->setProperty ("icon-name", "right");
  rightBtn->setCheckable (true);
  ocrBtn= new QToolButton ();
  ocrBtn->setObjectName ("image-align-button");
  ocrBtn->setProperty ("icon-name", "ocr");
  QButtonGroup* alignGroup= new QButtonGroup (this);
  alignGroup->addButton (leftBtn);
  alignGroup->addButton (middleBtn);
  alignGroup->addButton (rightBtn);
  alignGroup->addButton (ocrBtn);
  alignGroup->setExclusive (true);
  eval ("(use-modules (liii ocr))");
  connect (alignGroup,
           QOverload<QAbstractButton*>::of (&QButtonGroup::buttonClicked), this,
           [=] (QAbstractButton* button) {
             if (button == leftBtn)
               call ("set-image-alignment", current_tree, "left");
             else if (button == middleBtn)
               call ("set-image-alignment", current_tree, "center");
             else if (button == rightBtn)
               call ("set-image-alignment", current_tree, "right");
             else if (button == ocrBtn)
               call ("create-temp-image", current_tree);
             current_align=
                 as_string (call ("get-image-alignment", current_tree));
           });
  layout->addWidget (leftBtn);
  layout->addWidget (middleBtn);
  layout->addWidget (rightBtn);
  layout->addWidget (ocrBtn);
}

QTMImagePopup::~QTMImagePopup () {}

// 显示图片悬浮菜单，根据缩放比例决定是否显示
void
QTMImagePopup::showImagePopup (rectangle selr, double magf, int scroll_x,
                               int scroll_y, int canvas_x) {
  cachePosition (selr, magf, scroll_x, scroll_y, canvas_x);
  if (cached_magf <= 0.16) {
    setFixedSize (0, 0);
    return;
  }
  hide ();
  autoSize ();
  int x, y;
  getCachedPosition (x, y);
  QPoint topLeft (x, y);
  move (topLeft);
  raise ();
  updateButtonStates ();
  show ();
}

void
QTMImagePopup::setImageTree (tree t) {
  this->current_tree= t;
}

void
QTMImagePopup::updateButtonStates () {
  if (current_align == "")
    current_align= as_string (call ("get-image-alignment", current_tree));
  if (current_align == "left") leftBtn->setChecked (true);
  else if (current_align == "center") middleBtn->setChecked (true);
  else if (current_align == "right") rightBtn->setChecked (true);
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

// 根据DPI缩放和图片缩放比例自动调整按钮大小和窗口尺寸
void
QTMImagePopup::autoSize () {
  QScreen*     Screen    = QGuiApplication::primaryScreen ();
  const double Dpi       = Screen ? Screen->logicalDotsPerInch () : 96.0;
  const double Scale     = Dpi / 96.0;
  const int    baseWidth = 200;
  const int    baseHeight= 50;
  double       totalScale= Scale * cached_magf * 3.3;
#if defined(Q_OS_MAC)
  const int IconSize= int (50 * Scale);
#else
  const int IconSize= int (40 * totalScale);
#endif
  leftBtn->setIconSize (QSize (IconSize, IconSize));
  middleBtn->setIconSize (QSize (IconSize, IconSize));
  rightBtn->setIconSize (QSize (IconSize, IconSize));
  ocrBtn->setIconSize (QSize (IconSize, IconSize));
  setFixedSize (int (baseWidth * totalScale), int (baseHeight * totalScale));
  updateGeometry ();
  layout->update ();
}

// 缓存菜单显示位置
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

// 计算菜单显示位置
void
QTMImagePopup::getCachedPosition (int& x, int& y) {
  x= ((cached_image_mid_x - cached_scroll_x - 500) * cached_magf +
      cached_canvas_x) /
         256 -
     (this->width () / 2);
  y= -((cached_image_mid_y - 5000 - cached_scroll_y) * cached_magf) / 256 -
     this->height () * 1.2;
}