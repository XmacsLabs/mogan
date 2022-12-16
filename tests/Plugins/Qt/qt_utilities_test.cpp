
/******************************************************************************
* MODULE     : qt_utilities_test.cpp
* COPYRIGHT  : (C) 2019-2022  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include <QUrl>
#include "Qt/qt_utilities.hpp"


class TestQtUtilities: public QObject {
  Q_OBJECT

private slots:
  void test_qt_supports();
  void test_to_qurl();
};

void TestQtUtilities::test_qt_supports () {
#ifdef QTTEXMACS
  QVERIFY (qt_supports (url ("x.svg")));
  QVERIFY (qt_supports (url ("x.png")));
  QVERIFY (qt_supports (url ("x.webp")));
#endif
}

void TestQtUtilities::test_to_qurl () {
#ifdef QTTEXMACS
  QVERIFY (QUrl("x.svg") == to_qurl("x.svg"));
#endif
}

QTEST_MAIN(TestQtUtilities)
#include "qt_utilities_test.moc"
