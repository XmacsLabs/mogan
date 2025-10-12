
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
#include "string.hpp"
#include "tm_window.hpp"

// The minimum width of a single tab page (in pixels).
const int MIN_TAB_PAGE_WIDTH= 150;

// The horizontal padding for tab container (in pixels).
#ifdef Q_OS_MAC
const int TAB_CONTAINER_PADDING= 75;
#else
const int TAB_CONTAINER_PADDING= 15;
#endif

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
int                  g_tabWidth              = -1;
int                  g_pointingIndex         = -1;
int                  g_hiddentTabIndex       = -1;
url                  g_mostRecentlyClosedTab = url_none ();
url                  g_mostRecentlyDraggedTab= url_none ();
QTMTabPageContainer* g_mostRecentlyDraggedBar= nullptr;
QTMTabPageContainer* g_mostRecentlyEnteredBar= nullptr;

/******************************************************************************
 * QTMTabPage
 ******************************************************************************/

QTMTabPage::QTMTabPage (url p_url, QAction* p_title, QAction* p_closeBtn,
                        bool p_isActive)
    : m_viewUrl (p_url) {
  p_title->setCheckable (true);
  p_title->setChecked (p_isActive);
  setDefaultAction (p_title);
  setFocusPolicy (Qt::NoFocus);

  m_closeBtn= new QToolButton (this);
  m_closeBtn->setObjectName ("tabpage-close-button");
  m_closeBtn->setDefaultAction (p_closeBtn);
  m_closeBtn->setFixedSize (14, 14);
  m_closeBtn->setFocusPolicy (Qt::NoFocus);
  updateCloseButtonVisibility ();
  connect (m_closeBtn, &QToolButton::clicked, this,
           [=] () { g_mostRecentlyClosedTab= m_viewUrl; });
}

QTMTabPage::QTMTabPage () : m_viewUrl (url_none ()) {
  setFocusPolicy (Qt::NoFocus);
  m_closeBtn= new QToolButton (this);
  m_closeBtn->setObjectName ("tabpage-close-button");
  m_closeBtn->setFixedSize (14, 14);
  m_closeBtn->setFocusPolicy (Qt::NoFocus);
  updateCloseButtonVisibility ();
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

  // 计算可用的文字绘制区域，需要排除关闭按钮的空间
  int leftPadding   = 10;
  int rightPadding  = m_closeBtn->isVisible ()
                          ? m_closeBtn->width () + 8
                          : 8; // 如果关闭按钮可见，留出更多空间
  int availableWidth= width () - leftPadding - rightPadding;

  // 如果可用宽度太小，至少保证最小宽度
  if (availableWidth < 20) {
    availableWidth= 20;
    rightPadding  = width () - leftPadding - availableWidth;
  }

  QRect textRect (leftPadding, 0, availableWidth, height ());

  // 使用省略号来处理过长的文字
  QString elidedText= fm.elidedText (text (), Qt::ElideRight, availableWidth);

  p.drawItemText (textRect, Qt::AlignLeft | Qt::AlignVCenter, palette (),
                  isEnabled (), elidedText, QPalette::ButtonText);
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
  if ((e->pos () - m_dragStartPos).manhattanLength () < 3) {
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
  g_mostRecentlyClosedTab= this->m_viewUrl; // hide the tab during dragging
  g_pointingIndex        = -1;
  g_mostRecentlyDraggedBar->arrangeTabPages ();

  QDrag* drag=
      new QDrag (parent ()); // don't point to `this`, it will cause crash
  // 设置热点为鼠标在标签页内的相对位置，这样pixmap会从标签页位置开始显示
  drag->setHotSpot (m_dragStartPos);
  drag->setMimeData (new QMimeData ()); // Qt requires
  drag->setPixmap (pixmap);
  drag->exec (Qt::MoveAction);
  // 没有拖拽到其他窗口，则建立新窗口
  if (!g_mostRecentlyEnteredBar && (g_mostRecentlyDraggedTab != url_none ())) {
    view_set_new_window (g_mostRecentlyDraggedTab);
  }
}

void
QTMTabPage::enterEvent (QEnterEvent* e) {
  updateCloseButtonVisibility ();
  QToolButton::enterEvent (e);
}

void
QTMTabPage::leaveEvent (QEvent* e) {
  updateCloseButtonVisibility ();
  QToolButton::leaveEvent (e);
}

void
QTMTabPage::updateCloseButtonVisibility () {
  // 当鼠标位于标签页上，或者标签页处于被选中状态时才显示关闭按钮
  bool shouldShow= underMouse () || isChecked ();
  bool wasVisible= m_closeBtn->isVisible ();
  m_closeBtn->setVisible (shouldShow);

  // 如果关闭按钮的可见性发生了变化，需要重新绘制文字区域
  if (wasVisible != shouldShow) {
    update ();
  }
}

void
QTMTabPage::setChecked (bool checked) {
  QToolButton::setChecked (checked);
  updateCloseButtonVisibility ();
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
  dummyTabPage= new QTMTabPage ();
  dummyTabPage->setParent (this);
  dummyTabPage->hide ();

  if (parent ()) {
    parent ()->installEventFilter (this);
  }

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
  if (!parentWidget ()) return;
  const int windowWidth=
      parentWidget () ? parentWidget ()->width () : this->width ();

  int visibleTabCount= 0;
  // cout << "most recently closed tab:" << g_mostRecentlyClosedTab << LF;
  for (int i= 0; i < m_tabPageList.size (); ++i) {
    QTMTabPage* tab= m_tabPageList[i];
    if (g_mostRecentlyClosedTab != tab->m_viewUrl) {
      visibleTabCount++;
    }
  }

  if (visibleTabCount == 0) {
    g_mostRecentlyClosedTab= url ();
    adjustHeight (0);
    return;
  }

  if (g_pointingIndex != -1) {
    visibleTabCount++; // leave space for the dragged tab
  }
  // cout << "Visible tab count:" << visibleTabCount << LF;

  // Calculate tab dimensions
  int availableWidth= windowWidth - 2 * TAB_CONTAINER_PADDING;
  int tabWidth      = availableWidth / visibleTabCount;
  tabWidth          = std::max (25, std::min (150, tabWidth));
  g_tabWidth        = tabWidth; // for external use

  int accumWidth= TAB_CONTAINER_PADDING;

  // Set new positions for all tabs
  for (int i= 0; i < m_tabPageList.size (); ++i) {
    QTMTabPage* tab= m_tabPageList[i];

    if (g_pointingIndex == i) {
      // construct a dummy rectangle widget for indication of the inser place of
      // the dragged tab
      dummyTabPage->setGeometry (accumWidth, 0, tabWidth, m_rowHeight);
      dummyTabPage->show ();
      accumWidth+= tabWidth;
    }
    if (g_mostRecentlyClosedTab == tab->m_viewUrl) {
      tab->hide ();
      continue;
    }

    tab->setGeometry (accumWidth, 0, tabWidth, m_rowHeight);
    accumWidth+= tabWidth;
    tab->show ();
  }
  if (g_pointingIndex >= m_tabPageList.size ()) {
    dummyTabPage->setGeometry (accumWidth, 0, tabWidth, m_rowHeight);
    dummyTabPage->show ();
    accumWidth+= tabWidth;
  }

  adjustHeight (0);

  // if not draggin, clear the memory of most recently closed tab
  if (g_mostRecentlyDraggedTab == url_none ()) g_mostRecentlyClosedTab= url ();
}

void
QTMTabPageContainer::adjustHeight (int p_rowCount) {
  int h= m_rowHeight * (p_rowCount + 1);
  setFixedHeight (h - 2);
}

int
QTMTabPageContainer::mapToPointing (QDropEvent* e, QPoint& p_indicatorPos) {
  QPoint pos= e->pos ();
  // Now we use g_tabWidth to calculate the pointing index, instead of geometry
  if (g_tabWidth <= 0) {
    p_indicatorPos= QPoint (0, 0);
    return 0;
  }
  int index= pos.x () / g_tabWidth;
  index    = qMax (0, qMin (index, m_tabPageList.size ()));
  if (index < m_tabPageList.size ()) {
    QRect rect = m_tabPageList[index]->geometry ();
    int   x_mid= rect.x () + rect.width () / 2;
    if (pos.x () >= x_mid) {
      p_indicatorPos= rect.topRight ();
      return std::min (index + 1, int (m_tabPageList.size ()));
    }
    p_indicatorPos= rect.topLeft ();
    return index;
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
    int    pointingIndex= mapToPointing (e, pos);
    if (g_pointingIndex != pointingIndex) {
      g_pointingIndex= pointingIndex;
      arrangeTabPages ();
    }
    // display a vertical line to tell user where the tab will be inserted
    m_indicator->setGeometry (pos.x (), pos.y (), 2, m_rowHeight);
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
    g_mostRecentlyClosedTab= url_none ();
    g_pointingIndex        = -1;

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
        g_pointingIndex= -1;
        g_mostRecentlyDraggedBar->arrangeTabPages ();
        g_mostRecentlyDraggedBar->dummyTabPage->hide (); // 确保隐藏
        arrangeTabPages ();
        dummyTabPage->hide ();
      }
    }
    g_mostRecentlyDraggedTab= url_none ();
    g_mostRecentlyDraggedBar= nullptr;
  }
  m_draggingTabIndex= -1;
  g_pointingIndex   = -1;
  m_indicator->hide ();
  dummyTabPage->hide ();
}

void
QTMTabPageContainer::dragLeaveEvent (QDragLeaveEvent* e) {
  g_mostRecentlyEnteredBar= nullptr;
  if (g_pointingIndex != -1) {
    g_pointingIndex= -1;
    arrangeTabPages ();
  }
  e->accept ();
  m_indicator->hide ();
  dummyTabPage->hide ();
}

void
QTMTabPageContainer::mousePressEvent (QMouseEvent* event) {
  if (event->button () == Qt::LeftButton) {
    dragging    = true;
    dragPosition= event->globalPos () - window ()->frameGeometry ().topLeft ();
    event->accept ();
  }
}

void
QTMTabPageContainer::mouseMoveEvent (QMouseEvent* event) {
  if (dragging && (event->buttons () & Qt::LeftButton)) {
    window ()->move (event->globalPos () - dragPosition);
    event->accept ();
  }
}

void
QTMTabPageContainer::mouseReleaseEvent (QMouseEvent* event) {
  if (event->button () == Qt::LeftButton) {
    dragging= false;
    event->accept ();
  }
}

bool
QTMTabPageContainer::eventFilter (QObject* obj, QEvent* event) {
  if (obj == parent () && event->type () == QEvent::Resize) {
    setFixedWidth (parentWidget () ? parentWidget ()->width () : 1000);
    arrangeTabPages ();
  }
  return QWidget::eventFilter (obj, event);
}

/******************************************************************************
 * QTMTabPageBar
 ******************************************************************************/

QTMTabPageBar::QTMTabPageBar (const QString& p_title, QWidget* p_parent,
                              QTMTabPageContainer* m_container)
    : QToolBar (p_title, p_parent), m_container (m_container) {
  // m_container= new QTMTabPageContainer (this);
  if (m_container) {
    addWidget (m_container);
    // 设置大小策略以允许工具栏扩展
    setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Fixed);
  }
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
  // 确保容器使用全部可用宽度减去左边的留出的拖拽句柄空间
  int availableWidth= size.width () - 7;
  if (availableWidth > 0 && m_container) {
    m_container->setGeometry (7, 0, availableWidth, size.height () - 2);
  }
}