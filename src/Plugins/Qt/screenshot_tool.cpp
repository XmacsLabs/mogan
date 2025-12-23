#include "screenshot_tool.h"
#include <QPainter>
#include <QGuiApplication>
#include <QScreen>
#include <QMessageBox>
#include <QKeyEvent>
#include <QCursor>
#include <QClipboard>
#include <QDebug>

ScreenshotTool::ScreenshotTool(QWidget *parent)
    : QWidget(parent),
      isSelecting(false),
      isMoving(false),
      isResizing(false),
      currentRegion(None),
      devicePixelRatio(1.0)
{
    // 设置窗口属性：无边框、全屏、置顶
    setWindowFlags(Qt::FramelessWindowHint | Qt::WindowStaysOnTopHint);
    setAttribute(Qt::WA_TranslucentBackground);

    // 获取设备像素比
    QScreen *screen = QGuiApplication::primaryScreen();
    devicePixelRatio = screen->devicePixelRatio();

    // 获取屏幕尺寸并设置全屏
    QRect screenGeometry = screen->geometry();
    setGeometry(0, 0, screenGeometry.width(), screenGeometry.height());

    // 设置鼠标跟踪
    setMouseTracking(true);
}

ScreenshotTool::~ScreenshotTool()
{
}

void ScreenshotTool::startCapture()
{
    // 先显示窗口，确保窗口位置已确定
    showFullScreen();
    activateWindow();
    raise();

    // 窗口显示后再截图
    QScreen *screen = QGuiApplication::primaryScreen();
    // 更新设备像素比
    devicePixelRatio = screen->devicePixelRatio();
    // 截取整个屏幕
    screenPixmap = screen->grabWindow(0);

    // 调试信息
    qDebug() << "Window geometry:" << geometry();
    qDebug() << "Screen geometry:" << screen->geometry();
    qDebug() << "Device pixel ratio:" << devicePixelRatio;
    qDebug() << "Pixmap size:" << screenPixmap.size();
}

void ScreenshotTool::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);

    QPainter painter(this);

    // 绘制半透明黑色遮罩
    painter.fillRect(rect(), QColor(0, 0, 0, 100));

    // 绘制选中区域（高亮显示）
    if (isSelecting || !selectedRect.isNull()) {
        QRect drawRect = selectedRect;
        if (isSelecting && !startPoint.isNull() && !endPoint.isNull()) {
            drawRect = QRect(startPoint, endPoint).normalized();
        }

        if (drawRect.isValid()) {
            // 调试：输出矩形位置
            static int debugCount = 0;
            if (++debugCount % 30 == 0) {  // 每30帧输出一次，避免刷屏
                qDebug() << "DrawRect:" << drawRect;
            }
            // 将逻辑坐标转换为物理像素坐标（用于从screenPixmap中提取）
            QRect physicalRect(
                static_cast<int>(drawRect.x() * devicePixelRatio),
                static_cast<int>(drawRect.y() * devicePixelRatio),
                static_cast<int>(drawRect.width() * devicePixelRatio),
                static_cast<int>(drawRect.height() * devicePixelRatio)
            );
            // 绘制选中区域的原始内容
            painter.drawPixmap(drawRect.topLeft(), screenPixmap.copy(physicalRect));

            // 绘制矩形边框
            painter.setPen(QPen(Qt::red, 2));
            painter.drawRect(drawRect);

            // 绘制控制点（只在矩形较大时显示）
            if (drawRect.width() > 40 && drawRect.height() > 40) {
                int handleSize = 8;
                painter.setBrush(Qt::red);
                painter.setPen(Qt::NoPen);

                // 四个角
                painter.drawRect(drawRect.topLeft().x() - handleSize/2, drawRect.topLeft().y() - handleSize/2, handleSize, handleSize);
                painter.drawRect(drawRect.topRight().x() - handleSize/2, drawRect.topRight().y() - handleSize/2, handleSize, handleSize);
                painter.drawRect(drawRect.bottomLeft().x() - handleSize/2, drawRect.bottomLeft().y() - handleSize/2, handleSize, handleSize);
                painter.drawRect(drawRect.bottomRight().x() - handleSize/2, drawRect.bottomRight().y() - handleSize/2, handleSize, handleSize);

                // 四条边中点
                painter.drawRect(drawRect.left() + drawRect.width()/2 - handleSize/2, drawRect.top() - handleSize/2, handleSize, handleSize);
                painter.drawRect(drawRect.left() + drawRect.width()/2 - handleSize/2, drawRect.bottom() - handleSize/2, handleSize, handleSize);
                painter.drawRect(drawRect.left() - handleSize/2, drawRect.top() + drawRect.height()/2 - handleSize/2, handleSize, handleSize);
                painter.drawRect(drawRect.right() - handleSize/2, drawRect.top() + drawRect.height()/2 - handleSize/2, handleSize, handleSize);

                painter.setPen(QPen(Qt::red, 2));
            }

            // 绘制尺寸信息
            QString sizeText = QString("%1 x %2").arg(drawRect.width()).arg(drawRect.height());
            QFont font = painter.font();
            font.setPointSize(12);
            painter.setFont(font);
            painter.setPen(Qt::white);
            painter.setBrush(QColor(0, 0, 0, 150));

            // 在矩形左上角外部绘制尺寸信息背景
            QRect textRect(drawRect.topLeft().x(), drawRect.topLeft().y() - 25, 100, 20);
            painter.drawRect(textRect);
            painter.drawText(textRect, Qt::AlignCenter, sizeText);
        }
    }
}

int ScreenshotTool::getMouseRegion(const QPoint &point) const
{
    if (selectedRect.isNull() || !selectedRect.isValid()) {
        return None;
    }

    const int margin = 8; // 边缘检测边距

    // 检查是否在矩形内部（减去边缘区域）
    QRect innerRect = selectedRect.adjusted(margin, margin, -margin, -margin);
    if (innerRect.contains(point)) {
        return Inside;
    }

    // 检查是否在角上
    QRect topLeftCorner(selectedRect.topLeft() - QPoint(margin, margin),
                       QSize(2 * margin, 2 * margin));
    QRect topRightCorner(selectedRect.topRight() - QPoint(margin, -margin),
                        QSize(2 * margin, 2 * margin));
    QRect bottomLeftCorner(selectedRect.bottomLeft() - QPoint(margin, margin),
                          QSize(2 * margin, 2 * margin));
    QRect bottomRightCorner(selectedRect.bottomRight() - QPoint(margin, margin),
                           QSize(2 * margin, 2 * margin));

    if (topLeftCorner.contains(point)) return TopLeft;
    if (topRightCorner.contains(point)) return TopRight;
    if (bottomLeftCorner.contains(point)) return BottomLeft;
    if (bottomRightCorner.contains(point)) return BottomRight;

    // 检查是否在边缘上（包括中点）
    QRect topEdge(selectedRect.left(), selectedRect.top() - margin,
                  selectedRect.width(), 2 * margin);
    QRect bottomEdge(selectedRect.left(), selectedRect.bottom() - margin,
                     selectedRect.width(), 2 * margin);
    QRect leftEdge(selectedRect.left() - margin, selectedRect.top(),
                   2 * margin, selectedRect.height());
    QRect rightEdge(selectedRect.right() - margin, selectedRect.top(),
                    2 * margin, selectedRect.height());

    if (topEdge.contains(point)) return Top;
    if (bottomEdge.contains(point)) return Bottom;
    if (leftEdge.contains(point)) return Left;
    if (rightEdge.contains(point)) return Right;

    return None;
}

void ScreenshotTool::updateCursorShape(int region)
{
    switch (region) {
    case Inside:
        setCursor(Qt::SizeAllCursor);
        break;
    case Top:
    case Bottom:
        setCursor(Qt::SizeVerCursor);
        break;
    case Left:
    case Right:
        setCursor(Qt::SizeHorCursor);
        break;
    case TopLeft:
    case BottomRight:
        setCursor(Qt::SizeFDiagCursor);
        break;
    case TopRight:
    case BottomLeft:
        setCursor(Qt::SizeBDiagCursor);
        break;
    default:
        setCursor(Qt::CrossCursor);
        break;
    }
}

void ScreenshotTool::adjustRect(QRect &rect, int region, const QPoint &delta) const
{
    switch (region) {
    case Inside:
        rect.translate(delta);
        break;
    case Top:
        rect.setTop(rect.top() + delta.y());
        break;
    case Bottom:
        rect.setBottom(rect.bottom() + delta.y());
        break;
    case Left:
        rect.setLeft(rect.left() + delta.x());
        break;
    case Right:
        rect.setRight(rect.right() + delta.x());
        break;
    case TopLeft:
        rect.setTopLeft(rect.topLeft() + delta);
        break;
    case TopRight:
        rect.setTopRight(rect.topRight() + delta);
        break;
    case BottomLeft:
        rect.setBottomLeft(rect.bottomLeft() + delta);
        break;
    case BottomRight:
        rect.setBottomRight(rect.bottomRight() + delta);
        break;
    default:
        break;
    }

    // 确保矩形有效（最小尺寸）
    if (rect.width() < 10) {
        if (region == Left || region == TopLeft || region == BottomLeft) {
            rect.setLeft(rect.right() - 10);
        } else if (region == Right || region == TopRight || region == BottomRight) {
            rect.setRight(rect.left() + 10);
        }
    }
    if (rect.height() < 10) {
        if (region == Top || region == TopLeft || region == TopRight) {
            rect.setTop(rect.bottom() - 10);
        } else if (region == Bottom || region == BottomLeft || region == BottomRight) {
            rect.setBottom(rect.top() + 10);
        }
    }
}

void ScreenshotTool::mousePressEvent(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton) {
        lastMousePos = event->pos();

        if (selectedRect.isValid()) {
            currentRegion = getMouseRegion(event->pos());

            if (currentRegion == Inside) {
                isMoving = true;
                isResizing = false;
                isSelecting = false;
            } else if (currentRegion != None) {
                isResizing = true;
                isMoving = false;
                isSelecting = false;
            } else {
                // 点击矩形外部，开始新的选择
                startPoint = event->pos();
                endPoint = startPoint;
                isSelecting = true;
                isMoving = false;
                isResizing = false;
                selectedRect = QRect();
            }
        } else {
            // 没有现有矩形，开始新的选择
            startPoint = event->pos();
            endPoint = startPoint;
            isSelecting = true;
            isMoving = false;
            isResizing = false;
        }

        update();
    }
}

void ScreenshotTool::mouseMoveEvent(QMouseEvent *event)
{
    QPoint currentPos = event->pos();

    if (!selectedRect.isValid()) {
        // 没有矩形时显示十字光标
        setCursor(Qt::CrossCursor);
    } else {
        // 更新光标形状
        int region = getMouseRegion(currentPos);
        updateCursorShape(region);
    }

    if (event->buttons() & Qt::LeftButton) {
        QPoint delta = currentPos - lastMousePos;

        if (isSelecting) {
            endPoint = currentPos;
        } else if (isMoving) {
            selectedRect.translate(delta);
        } else if (isResizing) {
            adjustRect(selectedRect, currentRegion, delta);
        }

        lastMousePos = currentPos;
        update();
    }
}

void ScreenshotTool::mouseReleaseEvent(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton) {
        if (isSelecting) {
            endPoint = event->pos();
            selectedRect = QRect(startPoint, endPoint).normalized();

            // 如果矩形太小，则清除
            if (selectedRect.width() < 10 || selectedRect.height() < 10) {
                selectedRect = QRect();
            }

            isSelecting = false;
        } else if (isMoving || isResizing) {
            isMoving = false;
            isResizing = false;
        }

        currentRegion = None;
        update();
    }
}

void ScreenshotTool::keyPressEvent(QKeyEvent *event)
{
    if (event->key() == Qt::Key_Escape) {
        exitCapture();
    } else if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter) {
        if (selectedRect.isValid()) {
            captureSelectedArea();
        } else if (isSelecting && startPoint != endPoint) {
            selectedRect = QRect(startPoint, endPoint).normalized();
            if (selectedRect.isValid()) {
                captureSelectedArea();
            }
        }
    } else if (event->key() == Qt::Key_Space) {
        // 空格键完成截图
        if (selectedRect.isValid()) {
            captureSelectedArea();
        }
    } else {
        QWidget::keyPressEvent(event);
    }
}

void ScreenshotTool::captureSelectedArea()
{
    if (selectedRect.isValid()) {
        // 确保矩形在屏幕范围内
        QRect screenRect = QGuiApplication::primaryScreen()->geometry();
        QRect finalRect = selectedRect.intersected(screenRect);

        if (finalRect.isValid()) {
            // 将逻辑坐标转换为物理像素坐标
            QRect physicalRect(
                static_cast<int>(finalRect.x() * devicePixelRatio),
                static_cast<int>(finalRect.y() * devicePixelRatio),
                static_cast<int>(finalRect.width() * devicePixelRatio),
                static_cast<int>(finalRect.height() * devicePixelRatio)
            );

            // 从原始截图中裁剪选中的区域
            QPixmap cropped = screenPixmap.copy(physicalRect);

            // 保存截图
            saveScreenshot(cropped);

            // 退出截图模式
            exitCapture();
        }
    }
}

void ScreenshotTool::saveScreenshot(const QPixmap &pixmap)
{
    // 直接将图片复制到剪贴板
    QClipboard *clipboard = QGuiApplication::clipboard();
    clipboard->setPixmap(pixmap);

    QMessageBox::information(this, "成功", "截图已复制到剪贴板");
}

void ScreenshotTool::exitCapture()
{
    hide();
    emit aboutToClose();
}