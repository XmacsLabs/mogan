
/******************************************************************************
 * MODULE     : qt_utilities_test.cpp
 * COPYRIGHT  : (C) 2019  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Qt/qt_utilities.hpp"
#include "base.hpp"
#include "sys_utils.hpp"
#include <Qt>
#include <QtTest/QtTest>

class TestQtUtilities : public QObject {
  Q_OBJECT

private slots:
  void test_qt_supports ();
  void test_from_modifiers ();
  void test_from_key_press_event ();
  void test_unit_test_function ();
};

void
TestQtUtilities::test_qt_supports () {
#ifdef QTTEXMACS
  QVERIFY (qt_supports (url ("x.svg")));
  QVERIFY (qt_supports (url ("x.png")));
  QVERIFY (!qt_supports (url ("x.eps")));
  QVERIFY (!qt_supports (url ("x.ps")));
  QVERIFY (!qt_supports (url ("x.pdf")));
#endif
}

void
TestQtUtilities::test_from_modifiers () {
  qcompare (from_modifiers (Qt::NoModifier), "");
  qcompare (from_modifiers (Qt::ShiftModifier), "S-");
  qcompare (from_modifiers (Qt::AltModifier), "A-");
  qcompare (from_modifiers (Qt::AltModifier | Qt::ShiftModifier), "A-S-");
  if (os_macos ()) {
    qcompare (from_modifiers (Qt::MetaModifier), "C-");
    qcompare (from_modifiers (Qt::MetaModifier | Qt::AltModifier), "C-A-");
    qcompare (
        from_modifiers (Qt::MetaModifier | Qt::AltModifier | Qt::ShiftModifier),
        "C-A-S-");
    qcompare (from_modifiers (Qt::MetaModifier | Qt::ShiftModifier), "C-S-");
    qcompare (from_modifiers (Qt::ControlModifier), "M-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::AltModifier), "M-A-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::AltModifier |
                              Qt::ShiftModifier),
              "M-A-S-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::ShiftModifier), "M-S-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::MetaModifier), "M-C-");
  }
  else {
    qcompare (from_modifiers (Qt::MetaModifier), "M-");
    qcompare (from_modifiers (Qt::MetaModifier | Qt::AltModifier), "M-A-");
    qcompare (
        from_modifiers (Qt::MetaModifier | Qt::AltModifier | Qt::ShiftModifier),
        "M-A-S-");
    qcompare (from_modifiers (Qt::MetaModifier | Qt::ShiftModifier), "M-S-");
    qcompare (from_modifiers (Qt::ControlModifier), "C-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::AltModifier), "C-A-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::AltModifier |
                              Qt::ShiftModifier),
              "C-A-S-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::ShiftModifier), "C-S-");
    qcompare (from_modifiers (Qt::ControlModifier | Qt::MetaModifier), "M-C-");
  }
}

void
TestQtUtilities::test_from_key_press_event () {
  if (os_macos ()) {
    auto ctrl_plus= QKeyEvent (QEvent::KeyPress, (int) '=',
                               Qt::ControlModifier | Qt::ShiftModifier, "=");
    qcompare (from_key_press_event (&ctrl_plus), "M-S-=");

    // A-<number>
    auto alt_1= QKeyEvent (QEvent::KeyPress, (int) '1', Qt::AltModifier, "¡");
    qcompare (from_key_press_event (&alt_1), "A-1");
    // A-<alpha>
    auto alt_v= QKeyEvent (QEvent::KeyPress, (int) 'V', Qt::AltModifier, "√");
    qcompare (from_key_press_event (&alt_v), "A-v");
    // A-<not alpha and not number>
    auto alt_dot= QKeyEvent (QEvent::KeyPress, (int) '.', Qt::AltModifier, "≥");
    qcompare (from_key_press_event (&alt_dot), "≥");
  }
}

void
TestQtUtilities::test_unit_test_function () {
  QVERIFY(additional_function(2) == 4);
  QVERIFY(additional_function(3) == 9);
  QVERIFY(additional_function(0) == 0);
  QVERIFY(additional_function(-1) == 1);
}


QTEST_MAIN (TestQtUtilities)
#include "qt_utilities_test.moc"
