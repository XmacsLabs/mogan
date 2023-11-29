
/*****************************************************************************
 * MODULE     : font_test.cpp
 * DESCRIPTION: Tests on font
 * COPYRIGHT  : (C) 2023  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Metafont/load_tex.hpp"
#include "base.hpp"
#include "font.hpp"
#include "sys_utils.hpp"
#include <QtTest/QtTest>

class TestFont : public QObject {
  Q_OBJECT

private slots:
  void init () { init_lolly (); }
  void test_get_extents ();
};

void
TestFont::test_get_extents () {
  set_new_fonts (true);
  init_tex ();
  font fn= smart_font ("sys-chinese", "rm", "medium", "right", 10, 600);
  qcompare (fn->res_name, "sys-chinese-rm-medium-right-10-600-smart");
}

QTEST_MAIN (TestFont)
#include "font_test.moc"
