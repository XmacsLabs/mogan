
/******************************************************************************
 * MODULE     : QTMTabPage.cpp
 * DESCRIPTION: QT Texmacs tab page classes
 * COPYRIGHT  : (C) 2024 Zhenjun Guo
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMTabPage.hpp"
#include "new_view.hpp"
#include "tm_window.hpp"
#include "string.hpp"

// The minimum width of a single tab page (in pixels).
#define MIN_TAB_PAGE_WIDTH 150

/**
 * What is g_mostRecentlyClosedTab used for? When we close an ACTIVE(!) tab
 * (let's denote it as T), the tab bar is refreshed twice, meaning that
 * QTMTabPageContainer::replaceTabPages is called twice. Specifically:
 *
 * -- During the first call, tab T has not yet been deleted, so T is still
 *    visible, although it is no longer in the active state.
 * -- During the second call, tab T has been deleted, and at this point, T is no
 *    longer visible.
 *
 * As a result, what the user observes is that when they close an ACTIVE tab, it
 * does not disappear immediately. Therefore, we need it to remember which tab
 * was most recently closed and avoid displaying it during the first update.
 */
url g_mostRecentlyClosedTab= url_none ();

url                  g_mostRecentlyDraggedTab= url_none ();
QTMTabPageContainer* g_mostRecentlyDraggedBar= nullptr;
QTMTabPageContainer* g_mostRecentlyEnteredBar= nullptr;

/******************************************************************************
 * QTMTabPage
 ******************************************************************************/

QTMTabPage::QTMTabPage (url p_url, QAction* p_title, QAction* p_closeBtn,
                        bool p_isActive)
    : m_viewUrl (p_url) {
  p_title->setCheckable (p_isActive);
  p_title->setChecked (p_isActive);
  setDefaultAction (p_title);
  setFocusPolicy (Qt::NoFocus); // don't steal focus from the editor (1)

  m_closeBtn= new QToolButton (this);
  m_closeBtn->setObjectName ("closeBtn");
  m_closeBtn->setDefaultAction (p_closeBtn);
  m_closeBtn->setFixedSize (20, 20); // position will be updated in resizeEvent
  m_closeBtn->setFocusPolicy (
      Qt::NoFocus); // don't steal focus from the editor (2) (both are needed)
  connect (m_closeBtn, &QToolButton::clicked, this,
           [=] () { g_mostRecentlyClosedTab= m_viewUrl; });

  setupStyle ();
}

/* We can't align the text to the left of the button by QSS or other methods,
 * so for now we achieve it by overriding the paintEvent. */
void
QTMTabPage::paintEvent (QPaintEvent*) {
  QStylePainter          p (this);
  QStyleOptionToolButton opt;
  initStyleOption (&opt);
  opt.text= "";                                      // don't draw the text now
  p.drawComplexControl (QStyle::CC_ToolButton, opt); // base method

  // draw the text now
  QFontMetrics fm (opt.fontMetrics);
  QRect        rect= fm.boundingRect (opt.rect, Qt::AlignVCenter, text ());
  rect.moveLeft (10);
  p.drawItemText (rect, Qt::AlignLeft, palette (), isEnabled (), text (),
                  QPalette::ButtonText);
}

void
QTMTabPage::resizeEvent (QResizeEvent* e) {
  int w= m_closeBtn->width ();
  int h= m_closeBtn->height ();
  int x= e->size ().width () - w - 8;
  int y= e->size ().height () / 2 - h / 2;

  m_closeBtn->setGeometry (x, y, w, h);
}

void
QTMTabPage::mousePressEvent (QMouseEvent* e) {
  if (e->button () == Qt::LeftButton) {
    g_mostRecentlyDraggedTab= this->m_viewUrl;
    g_mostRecentlyDraggedBar=
        qobject_cast<QTMTabPageContainer*> (this->parentWidget ());
    g_mostRecentlyEnteredBar= g_mostRecentlyDraggedBar;
    m_dragStartPos          = e->pos ();
  }
  QToolButton::mousePressEvent (e);
}

void
QTMTabPage::mouseMoveEvent (QMouseEvent* e) {
  if (!(e->buttons () & Qt::LeftButton)) return QToolButton::mouseMoveEvent (e);
  if ((e->pos () - m_dragStartPos).manhattanLength () < 15) {
    // avoid treating small movement(more like a click) as dragging
    return QToolButton::mouseMoveEvent (e);
  }
  e->accept ();

  QPixmap pixmap (size ());
  render (&pixmap);
  setDown (false); // to avoid keeping the pressed state

  g_mostRecentlyDraggedTab= this->m_viewUrl;
  g_mostRecentlyDraggedBar=
      qobject_cast<QTMTabPageContainer*> (this->parentWidget ());

  QDrag* drag=
      new QDrag (parent ()); // don't point to `this`, it will cause crash
  drag->setHotSpot (QPoint (0, 0)); // align pixmap to the topLeft of the cursor
  drag->setMimeData (new QMimeData ()); // Qt requires
  drag->setPixmap (pixmap);
  drag->exec (Qt::MoveAction);
  // 没有拖拽到其他窗口，则建立新窗口
  if (!g_mostRecentlyEnteredBar && (g_mostRecentlyDraggedTab != url_none ())) {
    view_set_new_window (g_mostRecentlyDraggedTab);
  }
}

void
QTMTabPage::setupStyle () {
  QString qss, bg_color, bg_color_hover, border_color, border_color_top;

  // 检测是否使用liii-night主题
  bool use_liii_night_theme = false;
  extern string tm_style_sheet;  // 声明外部变量
  if (tm_style_sheet != "") {
    use_liii_night_theme = occurs("liii-night", tm_style_sheet);
  }
  
  if (use_liii_night_theme) {
    // liii-night主题的深色配色
    bg_color        = "#535353";
    bg_color_hover  = "#8d8d8d";
    border_color    = "#4c4c4c";
    border_color_top= "#000000";
  } else {
    // 默认浅色配色
    bg_color        = "#C7C8C9";
    bg_color_hover  = "#EFF0F1";
    border_color    = "#A6A6A6";
    border_color_top= "#2c2c2c";
  }

  qss+= QString ("QTMTabPage{ padding: 0 26px; border-radius: 0px; "
                 "background-color: %1; }")
            .arg (bg_color);
  qss+= QString ("QTMTabPage:hover{ background-color: %1; }")
            .arg (bg_color_hover);
  qss+= QString (
            "QTMTabPage:checked{ background-color: %1; border-top: 3px solid "
            "%2; border-left: 1px solid %3; border-right: 1px solid %4; }")
            .arg (bg_color_hover, border_color_top, border_color, border_color);

  qss+= "#closeBtn{ background-color: transparent; border-radius: 0px; "
        "padding: 0px; }";
  qss+= "#closeBtn:hover{ border-radius: 10px; color: #ffffff; "
        "background-color: #E49AA2; }";

  setStyleSheet (qss);
}

/******************************************************************************
 * QTMTabPageContainer
 ******************************************************************************/

QTMTabPageContainer::QTMTabPageContainer (QWidget* p_parent)
    : QWidget (p_parent) {
  m_indicator= new QFrame (this);
  m_indicator->setFrameShape (QFrame::VLine);
  m_indicator->setLineWidth (2);
  m_indicator->hide ();

  setAcceptDrops (true);
  setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Preferred);
}

QTMTabPageContainer::~QTMTabPageContainer () { removeAllTabPages (); }

void
QTMTabPageContainer::replaceTabPages (QList<QAction*>* p_src) {
  removeAllTabPages ();    // remove  old tabs
  extractTabPages (p_src); // extract new tabs
  arrangeTabPages ();
}

void
QTMTabPageContainer::removeAllTabPages () {
  for (int i= 0; i < m_tabPageList.size (); ++i) {
    // remove from parent first to avoid being freed again
    m_tabPageList[i]->setParent (nullptr);
    m_tabPageList[i]->deleteLater ();
  }
  m_tabPageList.clear ();
}

void
QTMTabPageContainer::extractTabPages (QList<QAction*>* p_src) {
  if (!p_src) return;
  for (int i= 0; i < p_src->size (); ++i) {
    // see the definition of QTMTabPageAction why we're using it
    QTMTabPageAction* carrier= qobject_cast<QTMTabPageAction*> ((*p_src)[i]);
    ASSERT (carrier, "QTMTabPageAction expected")

    QTMTabPage* tab= qobject_cast<QTMTabPage*> (carrier->m_widget);
    if (tab) {
      tab->setParent (this);
      m_tabPageList.append (tab);
    }
    else {
      delete carrier->m_widget; // we don't use it so we should delete it
    }

    // We don't need to manually delete carrier, because it(p_src) is a QAction,
    // which will be deleted by the parent widget (QTMTabPageBar) when it
    // is destroyed (by shedule_destruction).
  }
}

void
QTMTabPageContainer::arrangeTabPages () {
  const int windowWidth= this->width ();
  int       padding    = 8;
  int       rowCount   = 0;
  int       accumWidth = padding;
  int       accumTab   = 0;

  for (int i= 0; i < m_tabPageList.size (); ++i) {
    QTMTabPage* tab= m_tabPageList[i];
    if (g_mostRecentlyClosedTab == tab->m_viewUrl) {
      // this tab has just been closed, don't display it
      tab->hide ();
      continue;
    }

    QSize tabSize = tab->minimumSizeHint ();
    int   tabWidth= max (MIN_TAB_PAGE_WIDTH, tabSize.width ());
    if (accumWidth + tabWidth >= windowWidth) {
      rowCount+= 1;
      accumWidth= padding;
      accumTab  = 0;
    }
    tab->setGeometry (accumWidth, rowCount * m_rowHeight, tabWidth,
                      m_rowHeight);
    accumWidth+= tabWidth;
    accumTab+= 1;
  }

  g_mostRecentlyClosedTab= url (); // clear memory
  adjustHeight (rowCount);
}

void
QTMTabPageContainer::adjustHeight (int p_rowCount) {
  int h= m_rowHeight * (p_rowCount + 1);
  // parentWidget's resizeEvent() will resize me
  parentWidget ()->setFixedHeight (h + 2);
}

int
QTMTabPageContainer::mapToPointing (QDropEvent* e, QPoint& p_indicatorPos) {
  QPoint pos= e->pos ();
  for (int i= 0; i < m_tabPageList.size (); ++i) {
    QRect rect= m_tabPageList[i]->geometry ();
    if (rect.contains (pos)) {
      int x_mid= rect.x () + rect.width () / 2;
      if (pos.x () >= x_mid) {
        p_indicatorPos= rect.topRight ();
        if (e->source () == m_tabPageList[i]) return i;
        else return min (i + 1, int (m_tabPageList.size ()));
      }
      p_indicatorPos= rect.topLeft ();
      return i;
    }
  }
  // no valid pointing tab, p_indicatorPos should be at the end
  p_indicatorPos= m_tabPageList.last ()->geometry ().topRight ();
  return m_tabPageList.size ();
}

void
QTMTabPageContainer::dragEnterEvent (QDragEnterEvent* e) {
  g_mostRecentlyEnteredBar= this;
  int index               = -1;
  for (int i= 0; i < m_tabPageList.size (); ++i) {
    if (m_tabPageList[i]->m_viewUrl == g_mostRecentlyDraggedTab) {
      index= i;
      break;
    }
  }
  m_draggingTabIndex= index;
  e->acceptProposedAction ();
}

void
QTMTabPageContainer::dragMoveEvent (QDragMoveEvent* e) {
  if (g_mostRecentlyDraggedTab != url_none ()) {
    e->acceptProposedAction ();
    QPoint pos;
    mapToPointing (e, pos);
    // display a vertical line to tell user where the tab will be inserted
    m_indicator->setGeometry (pos.x (), pos.y (), 2, m_rowHeight);
    m_indicator->raise ();
    m_indicator->show ();
  }
}

void
QTMTabPageContainer::dropEvent (QDropEvent* e) {
  e->acceptProposedAction ();
  if (m_draggingTabIndex != -1) {
    QPoint      _; // dummy argument
    int         pointingIndex= mapToPointing (e, _);
    QTMTabPage* draggingTab  = m_tabPageList[m_draggingTabIndex];
    int         oldIndex     = m_draggingTabIndex;
    int newIndex= pointingIndex > oldIndex ? pointingIndex - 1 : pointingIndex;

    // update tab page positions immediately
    if (pointingIndex != oldIndex) {
      m_tabPageList.removeAt (oldIndex);
      m_tabPageList.insert (newIndex, draggingTab);
    }
    arrangeTabPages ();

    // move the tab pages in the view history
    move_tabpage (oldIndex, newIndex);
    m_draggingTabIndex= -1;
  }
  else if (g_mostRecentlyDraggedTab != url_none () &&
           g_mostRecentlyDraggedBar) {
    // Attach当前标签页到其他窗口
    QObject* src= e->source ();
    if (src && src != this) {
      url       dragged_view  = g_mostRecentlyDraggedTab;
      tm_window dragged_window= concrete_view (dragged_view)->win_tabpage;
      url target_view= m_tabPageList[0]->m_viewUrl; // 通过view来获取window
      tm_window target_window= concrete_view (target_view)->win_tabpage;
      bool      attached     = (concrete_view (dragged_view)->win != NULL);
      // 注意：dragged_window 有可能被 view_set_window 释放
      if (!view_set_window (dragged_view, abstract_window (target_window),
                            attached)) {
        arrangeTabPages ();
        g_mostRecentlyDraggedBar->arrangeTabPages ();
      }
    }
    g_mostRecentlyDraggedTab= url_none ();
    g_mostRecentlyDraggedBar= nullptr;
  }
  m_draggingTabIndex= -1;
  m_indicator->hide ();
}

void
QTMTabPageContainer::dragLeaveEvent (QDragLeaveEvent* e) {
  g_mostRecentlyEnteredBar= nullptr;
  e->accept ();
  m_indicator->hide ();
}

/******************************************************************************
 * QTMTabPageBar
 ******************************************************************************/

QTMTabPageBar::QTMTabPageBar (const QString& p_title, QWidget* p_parent)
    : QToolBar (p_title, p_parent) {
  m_container= new QTMTabPageContainer (this);
}

void
QTMTabPageBar::replaceTabPages (QList<QAction*>* p_src) {
  setUpdatesEnabled (false);
  bool visible= this->isVisible ();
  if (visible) hide (); // TRICK: to avoid flicker of the dest widget

  m_container->replaceTabPages (p_src);

  if (visible) show (); // TRICK: see above
  setUpdatesEnabled (true);
}

void
QTMTabPageBar::resizeEvent (QResizeEvent* e) {
  QSize size= e->size ();
  // Reserve 7px space on the left for the handle of QToolbar
  m_container->setGeometry (7, 0, size.width (), size.height () - 2);
}