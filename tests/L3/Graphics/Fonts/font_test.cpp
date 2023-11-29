
/*****************************************************************************
 * MODULE     : font_test.cpp
 * DESCRIPTION: Tests on font
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "font.hpp"
#include <QtTest/QtTest>

class TestFont : public QObject {
  Q_OBJECT

private slots:
  void test_get_extents ();
};

void
TestFont::test_get_extents () {
  // sys-chinese-rm-medium-right-10-600-smart
  // font fn= smart_font ("sys-chinese", "rm", "medium", "right", 10, 600);
  // cout << fn->res_name << LF;
}

QTEST_MAIN (TestFont)
#include "font_test.moc"
