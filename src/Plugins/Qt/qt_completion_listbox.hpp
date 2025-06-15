/******************************************************************************
 * MODULE     : qt_completion_listbox.hpp
 * DESCRIPTION: Completion ListBox widget for code auto-completion
 * COPYRIGHT  : (C) 2025 JimZhouZZY
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_COMPLETION_LISTBOX_HPP
#define QT_COMPLETION_LISTBOX_HPP

#include <QListWidget>
#include <QPoint>
#include <QStringList>

class QtCompletionListBox : public QListWidget {
  Q_OBJECT
public:
  explicit QtCompletionListBox (QWidget* parent= nullptr);

  void showCompletions (const QStringList& completions, const QPoint& pos);
  void selectNextItem ();
  void selectPreviousItem ();
  void selectItemIndex (int index);

signals:
  void completionSelected (const QString& text);

protected:
  void keyPressEvent (QKeyEvent* event) override;
  void focusOutEvent (QFocusEvent* event) override;
  void wheelEvent (QWheelEvent* event) override;

private slots:
  void onItemClicked (QListWidgetItem* item);
};

#endif // QT_COMPLETION_LISTBOX_HPP
