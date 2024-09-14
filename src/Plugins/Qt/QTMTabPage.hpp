
/******************************************************************************
 * MODULE     : QTMTabPage.hpp
 * DESCRIPTION: QT Texmacs tab page classes
 * COPYRIGHT  : (C) 2024 Zhenjun Guo
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#ifndef QTMTABPAGE_HPP
#define QTMTABPAGE_HPP

#include <QDrag>
#include <QDropEvent>
#include <QFrame>
#include <QMimeData>
#include <QMouseEvent>
#include <QMutex>
#include <QStyleOptionToolButton>
#include <QStylePainter>
#include <QToolBar>
#include <QToolButton>
#include <basic.hpp>
#include <scheme.hpp>

/*! QTMTabPage is used to build a widget for tab page.
 */
class QTMTabPage : public QToolButton {
  Q_OBJECT
  QToolButton* m_closeBtn;
  QPoint       m_dragStartPos;

public:
  const url m_bufferUrl;

public:
  explicit QTMTabPage (url p_url, QAction* p_title, QAction* p_closeBtn,
                       bool p_isActive);
  virtual void paintEvent (QPaintEvent*) override;

protected:
  virtual void resizeEvent (QResizeEvent* e) override;
  virtual void mousePressEvent (QMouseEvent* e) override;
  virtual void mouseMoveEvent (QMouseEvent* e) override;

private:
  void setupStyle ();
};

/*! QTMTabPageAction is used as a carrier of QTMTabPage widget.
Why:
If we use the QWidgetAction, once we call setDefaultWidget,
we can't take out the defaultWidget from QWidgetAction, because
when we delete the QWidgetAction, the defaultWidget will also be
deleted. You can see this behavior in the source code of QWidgetAction.
 */
class QTMTabPageAction : public QAction {
  Q_OBJECT

public:
  explicit QTMTabPageAction (QWidget* p_widget) : m_widget (p_widget) {}
  QWidget* const m_widget;
};

/*! QTMTabPageContainer is used to build the container for QTMTabPage.
In order to:
1. Support multi-line display for numerous tab pages;
2. Support drag-and-drop to sort tab page
 */
class QTMTabPageContainer : public QWidget {
  QMutex             m_updateMutex;
  QList<QTMTabPage*> m_tabPageList;
  int                m_rowHeight= 0;
  QTMTabPage*        m_draggingTab= nullptr;
  QFrame*            m_indicator;

public:
  explicit QTMTabPageContainer (QWidget* p_parent);
  ~QTMTabPageContainer ();

  inline void setRowHeight (int p_height) { m_rowHeight= p_height; }
  void        replaceTabPages (QList<QAction*>* p_src);

private:
  void removeAllTabPages ();
  void extractTabPages (QList<QAction*>* p_src);
  void adjustHeight (int p_rowCount);

  // drag and drop events
  int          mapToPointing (QDropEvent* e, QPoint& m_indicator);
  virtual void dragEnterEvent (QDragEnterEvent* e) override;
  virtual void dragMoveEvent (QDragMoveEvent* e) override;
  virtual void dropEvent (QDropEvent* e) override;
  virtual void dragLeaveEvent (QDragLeaveEvent* e) override;
};

/*! QTMTabPageBar is used to wrap the QTMTabPageContainer.
In order to:
1. Add this to the QMainWindow as QToolBar, just like the main icon toolbar;
2. Support dragging and docking like the main icon toolbar.
 */
class QTMTabPageBar : public QToolBar {
  QTMTabPageContainer* m_container;

public:
  explicit QTMTabPageBar (const QString& p_title, QWidget* p_parent);

  inline void setRowHeight (int p_height) {
    m_container->setRowHeight (p_height);
  }

  void replaceTabPages (QList<QAction*>* p_src);

protected:
  virtual void resizeEvent (QResizeEvent* e) override;
};

#endif // QTMTABPAGE_HPP