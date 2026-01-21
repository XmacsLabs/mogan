#ifndef QT_SHORTCUT_WIDGET_HPP
#define QT_SHORTCUT_WIDGET_HPP

#include "qcontainerfwd.h"
#include <QLineEdit>
#include <QStyledItemDelegate>
#include <QTreeWidget>
#include <QtWidgets/qwidget.h>
#include <cstddef>

// ==========================================
// 类 1: 只读代理
// ==========================================
class ReadOnlyDelegate : public QStyledItemDelegate {
  Q_OBJECT
public:
  explicit ReadOnlyDelegate (QObject* parent= nullptr)
      : QStyledItemDelegate (parent) {}
  QWidget* createEditor (QWidget*, const QStyleOptionViewItem&,
                         const QModelIndex&) const override {
    return nullptr;
  }
};

// ==========================================
// 类 2: 快捷键录入代理
// ==========================================
class KeySequenceDelegate : public QStyledItemDelegate {
  Q_OBJECT
public:
  explicit KeySequenceDelegate (QObject* parent= nullptr);
  QWidget* createEditor (QWidget* parent, const QStyleOptionViewItem& option,
                         const QModelIndex& index) const override;
  void setEditorData (QWidget* editor, const QModelIndex& index) const override;
  void setModelData (QWidget* editor, QAbstractItemModel* model,
                     const QModelIndex& index) const override;
  void updateEditorGeometry (QWidget*                    editor,
                             const QStyleOptionViewItem& option,
                             const QModelIndex&          index) const override;
};

// ==========================================
// 类 3: 主窗口
// ==========================================
class Qshortcut_widget : public QWidget {
  Q_OBJECT

public:
  explicit Qshortcut_widget (QString tittle, QWidget* parent= nullptr);
  ~Qshortcut_widget ();

private slots:
  // 搜索过滤槽函数
  void onSearchTextChanged (const QString& text);
  // 快捷键变化时的槽函数
  void onShortcutChanged (QTreeWidgetItem* item, int column);

private:
  QLineEdit*   searchEdit; // 搜索框
  QTreeWidget* treeWidget;

  // 初始化界面布局
  void setupUi ();
  // 初始化样式表
  void setupStyle ();
  // 初始化数据
  void initData ();
  // 辅助函数：添加快捷键项
  void addShortcutItem (QTreeWidgetItem* parent, const QString& name,
                        const QString& key);
  // 辅助函数：递归过滤树节点
  bool filterTreeItems (QTreeWidgetItem* item, const QString& text);
};

#endif
