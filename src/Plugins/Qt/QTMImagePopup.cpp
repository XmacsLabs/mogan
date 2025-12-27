
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

#include <algorithm>

namespace {
static constexpr double kIconScaleFactor = 3.3;
static constexpr double kScreenDenom = 256.0;
static constexpr double kHeightMultiplier = 1.2;
static constexpr int kBaseWidth = 200;
static constexpr int kBaseHeight = 50;
static constexpr int kMaxIconSize = 200;
static constexpr int kMinIconSize = 16;
}


// 悬浮菜单创建函数
QTMImagePopup::QTMImagePopup (QWidget* parent, qt_simple_widget_rep* owner)
    : QWidget (parent), owner (owner), layout (nullptr), effect(nullptr), cached_image_mid_x(0), cached_image_mid_y(0), cached_scroll_x(0), cached_scroll_y(0), cached_canvas_x(0), cached_magf(1.0) {
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

  this->effect = new QGraphicsDropShadowEffect(this);
  this->effect->setBlurRadius(40);
  this->effect->setOffset(0, 4);
  this->effect->setColor(QColor(0, 0, 0, 120));
  this->setGraphicsEffect(this->effect);

  leftBtn= new QToolButton (this);
  leftBtn->setObjectName ("image-align-button");
  leftBtn->setProperty ("icon-name", "left");
  leftBtn->setCheckable (true);
  middleBtn= new QToolButton (this);
  middleBtn->setObjectName ("image-align-button");
  middleBtn->setProperty ("icon-name", "center");
  middleBtn->setCheckable (true);
  rightBtn= new QToolButton (this);
  rightBtn->setObjectName ("image-align-button");
  rightBtn->setProperty ("icon-name", "right");
  rightBtn->setCheckable (true);
  ocrBtn= new QToolButton (this);
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
             {
               current_align = as_string(call("get-image-alignment", current_tree));
               if (current_align == "") {
                 leftBtn->setChecked(false);
                 middleBtn->setChecked(false);
                 rightBtn->setChecked(false);
               }
               updateButtonStates();
             }
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
  int x, y;
  getCachedPosition (x, y);
  QPoint topLeft (x, y);
  move (topLeft);
  autoSize ();
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
  // 先清除所有状态
  leftBtn->setChecked(false);
  middleBtn->setChecked(false);
  rightBtn->setChecked(false);

  if (current_align == "") {
    current_align = as_string(call("get-image-alignment", current_tree));
    if (current_align == "") return;
  }
  leftBtn->setChecked(current_align == "left");
  middleBtn->setChecked(current_align == "center");
  rightBtn->setChecked(current_align == "right");
}

void
QTMImagePopup::scrollBy (int x, int y) {
  if (cached_magf <= 1e-6) return;
  cached_scroll_x -= static_cast<int>(x / cached_magf);
  cached_scroll_y -= static_cast<int>(y / cached_magf);
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
  QPoint center = mapToGlobal(rect().center());
  QScreen* screen = QGuiApplication::screenAt(center);
  if (!screen) screen = QGuiApplication::primaryScreen();
  const double dpi = screen ? screen->logicalDotsPerInch() : 96.0;
  const double scale = dpi / 96.0;
  double mag = (cached_magf > 0.0) ? cached_magf : 1.0;
  double totalScale = scale * mag * kIconScaleFactor;
#if defined(Q_OS_MAC)
  const int baseIcon = 80;
#else
  const int baseIcon = 40;
#endif
  int iconSize = int(baseIcon * totalScale);
  iconSize = std::clamp(iconSize, kMinIconSize, kMaxIconSize);
  leftBtn->setIconSize(QSize(iconSize, iconSize));
  middleBtn->setIconSize(QSize(iconSize, iconSize));
  rightBtn->setIconSize(QSize(iconSize, iconSize));
  ocrBtn->setIconSize(QSize(iconSize, iconSize));
  setFixedSize(int(kBaseWidth * totalScale), int(kBaseHeight * totalScale));
  updateGeometry();
  layout->update();
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
  // 坐标转换：TeXmacs坐标 → 屏幕坐标

  // x坐标
  double image_center_screen_x=
      ((cached_image_mid_x - cached_scroll_x) * cached_magf + cached_canvas_x) /
      kScreenDenom;
  x= static_cast<int>(image_center_screen_x - (this->width() / 2.0));

  // y坐标
  double image_center_screen_y=
      -((cached_image_mid_y - cached_scroll_y) * cached_magf) / kScreenDenom;
  y= static_cast<int>(image_center_screen_y - (this->height() * kHeightMultiplier));

  // 确保弹窗在屏幕可见范围内
  QScreen* screen = QGuiApplication::screenAt(QPoint(x, y));
  if (!screen) screen = QGuiApplication::primaryScreen();
  if (screen) {
    QRect avail = screen->availableGeometry();
    int maxX = avail.right() - this->width();
    int maxY = avail.bottom() - this->height();
    x = std::max(avail.left(), std::min(x, maxX));
    y = std::max(avail.top(), std::min(y, maxY));
  }
}