#ifndef SCREENSHOT_TOOL_H
#define SCREENSHOT_TOOL_H

#include <QApplication>
#include <QClipboard>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QPaintEvent>
#include <QPainter>
#include <QPixmap>
#include <QScreen>
#include <QShortcut>
#include <QTimer>
#include <QWidget>

class ScreenshotTool : public QWidget {
  Q_OBJECT

signals:
  void aboutToClose ();

public:
  explicit ScreenshotTool (QWidget* parent= nullptr);
  ~ScreenshotTool ();

  void startCapture ();

protected:
  void paintEvent (QPaintEvent* event) override;
  void mousePressEvent (QMouseEvent* event) override;
  void mouseMoveEvent (QMouseEvent* event) override;
  void mouseReleaseEvent (QMouseEvent* event) override;
  void mouseDoubleClickEvent (QMouseEvent* event) override;
  void keyPressEvent (QKeyEvent* event) override;

private:
  void captureSelectedArea ();
  void saveScreenshot (const QPixmap& pixmap);
  void exitCapture ();
  int  getMouseRegion (const QPoint& point) const;
  void updateCursorShape (int region);
  void adjustRect (QRect& rect, int region, const QPoint& delta) const;

  enum Region {
    None       = 0,
    Inside     = 1,
    Top        = 2,
    Bottom     = 3,
    Left       = 4,
    Right      = 5,
    TopLeft    = 6,
    TopRight   = 7,
    BottomLeft = 8,
    BottomRight= 9
  };

  QPoint  startPoint;
  QPoint  endPoint;
  QPoint  lastMousePos;
  bool    isSelecting;
  bool    isMoving;
  bool    isResizing;
  int     currentRegion;
  QPixmap screenPixmap;
  QRect   selectedRect;
  qreal   devicePixelRatio;
};

#endif // SCREENSHOT_TOOL_H