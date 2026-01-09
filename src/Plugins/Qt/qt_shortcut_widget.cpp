#include "qt_shortcut_widget.hpp"
#include "qcontainerfwd.h"
#include <QDebug>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QKeySequenceEdit>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>

// ==========================================
// KeySequenceDelegate 实现
// ==========================================

KeySequenceDelegate::KeySequenceDelegate (QObject* parent)
    : QStyledItemDelegate (parent) {}

QWidget*
KeySequenceDelegate::createEditor (QWidget* parent, const QStyleOptionViewItem&,
                                   const QModelIndex&) const {
  QKeySequenceEdit* editor= new QKeySequenceEdit (parent);
  // 保持背景透明，融入单元格
  editor->setStyleSheet (
      "QKeySequenceEdit { border: none; background: transparent; }");
  return editor;
}

void
KeySequenceDelegate::setEditorData (QWidget*           editor,
                                    const QModelIndex& index) const {
  QString value= index.model ()->data (index, Qt::EditRole).toString ();
  QKeySequenceEdit* keyEditor= qobject_cast<QKeySequenceEdit*> (editor);
  if (keyEditor) {
    keyEditor->setKeySequence (QKeySequence (value));
  }
}

void
KeySequenceDelegate::setModelData (QWidget* editor, QAbstractItemModel* model,
                                   const QModelIndex& index) const {
  QKeySequenceEdit* keyEditor= qobject_cast<QKeySequenceEdit*> (editor);
  if (keyEditor) {
    QKeySequence seq= keyEditor->keySequence ();
    model->setData (index, seq.toString (QKeySequence::NativeText),
                    Qt::EditRole);
  }
}

void
KeySequenceDelegate::updateEditorGeometry (QWidget*                    editor,
                                           const QStyleOptionViewItem& option,
                                           const QModelIndex&) const {
  editor->setGeometry (option.rect);
}

// ==========================================
// Widget 主窗口实现
// ==========================================

Qshortcut_widget::Qshortcut_widget (QString title, QWidget* parent)
    : QWidget (parent) {
  this->setWindowTitle (title);
  setupUi ();
  setupStyle ();
  initData ();
}

Qshortcut_widget::~Qshortcut_widget () {}

void
Qshortcut_widget::setupUi () {
  resize (800, 600);

  QVBoxLayout* mainLayout= new QVBoxLayout (this);
  mainLayout->setSpacing (10);
  mainLayout->setContentsMargins (15, 15, 15, 15);

  // --- 1. 顶部搜索栏 ---
  searchEdit= new QLineEdit (this);
  searchEdit->setPlaceholderText ("搜索...");
  searchEdit->setClearButtonEnabled (true);
  connect (searchEdit, &QLineEdit::textChanged, this,
           &Qshortcut_widget::onSearchTextChanged);
  mainLayout->addWidget (searchEdit);

  // --- 2. 中间树形列表 ---
  treeWidget= new QTreeWidget (this);
  treeWidget->setColumnCount (2);
  treeWidget->setHeaderLabels (QStringList () << "操作" << "快捷键");

  // 设置列宽
  treeWidget->header ()->setSectionResizeMode (0, QHeaderView::Stretch);
  treeWidget->header ()->setSectionResizeMode (1, QHeaderView::Fixed);
  treeWidget->header ()->resizeSection (1, 200);
  treeWidget->header ()->setStretchLastSection (false);

  treeWidget->setSelectionBehavior (QAbstractItemView::SelectItems);

  // 只能单选
  treeWidget->setSelectionMode (QAbstractItemView::SingleSelection);

  treeWidget->setAlternatingRowColors (true);
  treeWidget->setAnimated (true);
  treeWidget->setFocusPolicy (Qt::NoFocus);

  treeWidget->setItemDelegateForColumn (0, new ReadOnlyDelegate (this));
  treeWidget->setItemDelegateForColumn (1, new KeySequenceDelegate (this));

  treeWidget->setEditTriggers (QAbstractItemView::DoubleClicked |
                               QAbstractItemView::EditKeyPressed |
                               QAbstractItemView::AnyKeyPressed);

  mainLayout->addWidget (treeWidget);

  // --- 3. 底部按钮栏 ---
  QHBoxLayout* bottomLayout= new QHBoxLayout ();
  QPushButton* btnDefault  = new QPushButton ("默认", this);
  QPushButton* btnCustomize= new QPushButton ("自定义快捷键", this);
  QPushButton* btnOk       = new QPushButton ("确定", this);
  QPushButton* btnCancel   = new QPushButton ("取消", this);

  btnOk->setDefault (true);

  bottomLayout->addWidget (btnDefault);
  bottomLayout->addStretch ();
  bottomLayout->addWidget (btnCustomize);
  bottomLayout->addWidget (btnOk);
  bottomLayout->addWidget (btnCancel);

  mainLayout->addLayout (bottomLayout);
}

void
Qshortcut_widget::setupStyle () {
  QString qss= R"(
        /* === 整体窗口 === */
        QWidget {
            font-family: "Microsoft YaHei", "Segoe UI", sans-serif;
            font-size: 14px;
            background-color: #fcfcfc;
            color: #232629;
        }

        /* === 搜索框 === */
        QLineEdit {
            border: 1px solid #dcdcdc;
            border-radius: 4px;
            padding: 6px;
            background-color: #ffffff;
            selection-background-color: #3daee9;
        }
        QLineEdit:focus {
            border: 1px solid #3daee9;
        }

        /* === 树形列表 === */
        QTreeWidget {
            border: 1px solid #dcdcdc;
            background-color: #ffffff;
            alternate-background-color: #f7f8fa;
            outline: 0;
        }

        QTreeWidget::item {
            height: 32px;
            padding-left: 5px;
            border: none;
        }

        QTreeWidget::item:hover {
            background-color: #eaf5fc;
        }

        QTreeWidget::item:selected {
            background-color: #3daee9;
            color: white;
        }

        QTreeWidget::item:selected:!active {
            background-color: #e0e0e0;
            color: black;
        }

        /* === 表头 === */
        QHeaderView::section {
            background-color: #eff0f1;
            border: none;
            border-bottom: 1px solid #dcdcdc;
            border-right: 1px solid #dcdcdc;
            padding: 4px;
            font-weight: bold;
            color: #555;
            min-height: 25px;
        }
        QHeaderView::section:last {
            border-right: none;
        }

        /* === 按钮 === */
        QPushButton {
            border: 1px solid #c0c0c0;
            border-radius: 4px;
            padding: 5px 15px;
            background-color: #fdfdfd;
            min-width: 70px;
        }
        QPushButton:hover {
            background-color: #f0f0f0;
            border-color: #3daee9;
        }
        QPushButton:pressed {
            background-color: #e0e0e0;
        }
        QPushButton[default="true"] {
            background-color: #3daee9;
            color: white;
            border: 1px solid #3298cc;
        }
        QPushButton[default="true"]:hover {
            background-color: #4dbfff;
        }


        /* 1. 垂直滚动条整体槽道 */
        QScrollBar:vertical {
            border: none;
            background: #f7f8fa; /* 极浅的灰色背景，融合感更好 */
            width: 10px;         /* 宽度变窄 */
            margin: 0px 0px 0px 0px;
        }

        /* 2. 滚动条滑块 (Handle) */
        QScrollBar::handle:vertical {
            background: #c1c1c1; /* 默认浅灰 */
            min-height: 30px;    /* 防止滑块太小 */
            border-radius: 5px;  /* 圆角胶囊形状 */
            margin: 2px;         /* 给滑块四周留一点空隙 */
        }

        /* 3. 鼠标悬停在滑块上 */
        QScrollBar::handle:vertical:hover {
            background: #a8a8a8; /* 加深颜色提示可点击 */
        }

        /* 4. 鼠标按下拖拽时 */
        QScrollBar::handle:vertical:pressed {
            background: #787878; /* 更深 */
        }

        /* 5. 隐藏上下箭头按钮 (add-line 和 sub-line) */
        QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {
            height: 0px;
            background: none;
            border: none;
        }

        /* 6. 隐藏箭头区域的背景 */
        QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {
            background: transparent;
            border: none;
        }

        QScrollBar:horizontal {
            border: none;
            background: #f7f8fa;
            height: 10px;
            margin: 0px;
        }
        QScrollBar::handle:horizontal {
            background: #c1c1c1;
            min-width: 30px;
            border-radius: 5px;
            margin: 2px;
        }
        QScrollBar::handle:horizontal:hover { background: #a8a8a8; }
        QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal { width: 0px; }
    )";
  this->setStyleSheet (qss);
}

void
Qshortcut_widget::addShortcutItem (QTreeWidgetItem* parent, const QString& name,
                                   const QString& key) {
  QTreeWidgetItem* item= new QTreeWidgetItem (parent);
  item->setText (0, name);
  item->setText (1, key);

  item->setFlags (Qt::ItemIsSelectable | Qt::ItemIsEditable |
                  Qt::ItemIsEnabled);
}

void
Qshortcut_widget::initData () {

  QTreeWidgetItem* globalGroup= new QTreeWidgetItem (treeWidget);
  globalGroup->setText (0, "全局");
  globalGroup->setFont (0, QFont ("Microsoft YaHei", 10, QFont::Bold));

  globalGroup->setFlags (Qt::ItemIsEnabled | Qt::ItemIsSelectable);
  globalGroup->setExpanded (true);

  int globalIndex= treeWidget->indexOfTopLevelItem (globalGroup);

  treeWidget->setFirstColumnSpanned (globalIndex, QModelIndex (), true);
  addShortcutItem (globalGroup, "保存文件 (Save)", "Ctrl+S");
  addShortcutItem (globalGroup, "打开文件 (Open)", "Ctrl+O");
  addShortcutItem (globalGroup, "新建窗口 (New Window)", "Ctrl+Shift+N");
  addShortcutItem (globalGroup, "关闭标签页 (Close Tab)", "Ctrl+W");

  QTreeWidgetItem* editGroup= new QTreeWidgetItem (treeWidget);
  editGroup->setText (0, "文本模式");
  editGroup->setFont (0, QFont ("Microsoft YaHei", 10, QFont::Bold));
  editGroup->setFlags (Qt::ItemIsEnabled | Qt::ItemIsSelectable);
  editGroup->setExpanded (true);
  int editIndex= treeWidget->indexOfTopLevelItem (editGroup);
  treeWidget->setFirstColumnSpanned (editIndex, QModelIndex (), true);

  addShortcutItem (editGroup, "复制当前行", "Ctrl+D");
  addShortcutItem (editGroup, "删除当前行", "Ctrl+Shift+K");
  addShortcutItem (editGroup, "向上移动行", "Alt+Up");
  addShortcutItem (editGroup, "向下移动行", "Alt+Down");

  QTreeWidgetItem* viewGroup= new QTreeWidgetItem (treeWidget);
  viewGroup->setText (0, "数学模式");
  viewGroup->setFont (0, QFont ("Microsoft YaHei", 10, QFont::Bold));
  viewGroup->setFlags (Qt::ItemIsEnabled | Qt::ItemIsSelectable);
  int viewIndex= treeWidget->indexOfTopLevelItem (viewGroup);
  treeWidget->setFirstColumnSpanned (viewIndex, QModelIndex (), true);

  addShortcutItem (viewGroup, "放大字体", "Ctrl++");
  addShortcutItem (viewGroup, "缩小字体", "Ctrl+-");
  addShortcutItem (viewGroup, "重置缩放", "Ctrl+0");
  connect (treeWidget, &QTreeWidget::itemChanged, this,
           &Qshortcut_widget::onShortcutChanged);
}

void
Qshortcut_widget::onSearchTextChanged (const QString& text) {
  if (text.isEmpty ()) {
    for (int i= 0; i < treeWidget->topLevelItemCount (); ++i) {
      QTreeWidgetItem* topItem= treeWidget->topLevelItem (i);
      topItem->setHidden (false);
      for (int j= 0; j < topItem->childCount (); ++j) {
        topItem->child (j)->setHidden (false);
      }
    }
    return;
  }

  for (int i= 0; i < treeWidget->topLevelItemCount (); ++i) {
    filterTreeItems (treeWidget->topLevelItem (i), text);
  }
}

void
Qshortcut_widget::onShortcutChanged (QTreeWidgetItem* item, int column) {
  if (column == 1) {
    QString actionName= item->text (0); // 操作名
    QString newKey    = item->text (1); // 新快捷键

    // qDebug () << "快捷键已更新 -> 操作:" << actionName << "新键位:" <<
    // newKey;
  }
}

bool
Qshortcut_widget::filterTreeItems (QTreeWidgetItem* item, const QString& text) {
  bool childVisible= false;
  for (int i= 0; i < item->childCount (); ++i) {
    bool v      = filterTreeItems (item->child (i), text);
    childVisible= childVisible || v;
  }
  bool selfMatch= item->text (0).contains (text, Qt::CaseInsensitive) ||
                  item->text (1).contains (text, Qt::CaseInsensitive);
  bool shouldShow= selfMatch || childVisible;
  item->setHidden (!shouldShow);
  if (childVisible) {
    item->setExpanded (true);
  }
  return shouldShow;
}
