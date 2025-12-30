
/******************************************************************************
 * MODULE     : QTMImagePopup.hpp
 * DESCRIPTION:
 * COPYRIGHT  : (C) 2025 MoonLL, Yuki Lu
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_IMAGE_POPUP_HPP
#define QT_IMAGE_POPUP_HPP

#include "lolly/data/lolly_tree.hpp"
#include "qt_simple_widget.hpp"
#include "rectangles.hpp"

#include <QGraphicsDropShadowEffect>
#include <QHBoxLayout>
#include <QMouseEvent>
#include <QPaintEvent>
#include <QToolButton>
#include <QWidget>

class QTMImagePopup : public QWidget {
protected:
  qt_simple_widget_rep*      owner;
  QHBoxLayout*               layout;
  QGraphicsDropShadowEffect* effect;
  int                        cached_image_mid_x;
  int                        cached_image_mid_y;
  rectangle                  cached_rect;
  int                        cached_scroll_x; // 页面滚动位置x
  int                        cached_scroll_y; // 页面滚动位置y
  int                        cached_canvas_x;
  int                        cached_canvas_y;
  int                        cached_width;
  int                        cached_height;
  double                     cached_magf; // 缩放因子
  tree                       current_tree;
  path                       current_path;
  string                     current_align;
  QToolButton*               leftBtn;
  QToolButton*               middleBtn;
  QToolButton*               rightBtn;
  QToolButton*               ocrBtn;
  QString                    btn_style;
  bool                       painted;
  int                        painted_count;
  tree&                      et;

public:
  QTMImagePopup (QWidget* parent, qt_simple_widget_rep* owner);
  ~QTMImagePopup ();

  void showImagePopup (qt_renderer_rep* ren, rectangle selr, double magf,
                       int scroll_x, int scroll_y, int canvas_x, int canvas_y);
  void updatePosition (qt_renderer_rep* ren);
  void scrollBy (int x, int y);
  void setImageTree (tree t);
  void setCurrentPath (path p);
  void setTreeref (tree& et);
  void updateButtonStates ();
  void autoSize ();
  bool isDocument ();

protected:
  void cachePosition (rectangle selr, double magf, int scroll_x, int scroll_y,
                      int canvas_x, int canvas_y);
  void getCachedPosition (qt_renderer_rep* ren, int& x, int& y);
};

#endif // QT_IMAGE_POPUP_HPP