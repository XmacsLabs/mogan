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
  //this->setStyleSheet (qss);
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
  QTreeWidgetItem* tabGrop = new QTreeWidgetItem (viewGroup);
  tabGrop->setText(0, "tab循环");
  // 3. 设置样式（粗体，类似顶级分组）
    tabGrop->setFont(0, QFont("Microsoft YaHei", 10, QFont::Bold));
    
    // 4. 设置Flag：允许选中，但禁止编辑
    tabGrop->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);

    // 5. 默认展开
    tabGrop->setExpanded(true);
    addShortcutItem (tabGrop, "复制当前行", "Ctrl+D");
  // 1. 获取父节点 (viewGroup) 的 ModelIndex
    QModelIndex parentIndex = treeWidget->indexFromItem(viewGroup);

    // 2. 获取 tabGrop 在父节点中的行号
    int childRow = viewGroup->indexOfChild(tabGrop);

    // 3. 设置跨列 (注意第二个参数传入了 parentIndex)
    treeWidget->setFirstColumnSpanned(childRow, parentIndex, true);
    // 6. 让这个分组标题跨越两列（这样更好看，防止分割线穿过文字）
    // 注意：需要通过 treeWidget 去设置跨列
    //treeWidget->setFirstColumnSpanned(tabGrop, true);

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
