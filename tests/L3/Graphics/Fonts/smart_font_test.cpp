
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
#include "converter.hpp"
#include "font.hpp"
#include "smart_font.hpp"
#include "sys_utils.hpp"
#include "tree_helper.hpp"
#include <QtTest/QtTest>

class TestSmartFont : public QObject {
  Q_OBJECT

private slots:
  void init () {
    init_lolly ();
    set_new_fonts (true);
    init_tex ();
  }
  void test_resolve ();
};

void
TestSmartFont::test_resolve () {
  auto which_arr= array<string> ();
  which_arr << string ("roman") << string ("rm") << string ("medium")
            << string ("right") << string ("$s") << string ("$d");
  tree by= tuple (tree ("ec"), tree ("ecrm"), tree ("$s"), tree ("$d"));
  font_rule (as_tree (which_arr), by);
  font fn= smart_font ("sys-chinese", "rm", "medium", "right", 10, 600);
  qcompare (fn->res_name, "sys-chinese-rm-medium-right-10-600-smart");
  smart_font_rep* fn_rep= (smart_font_rep*) fn.rep;
  int             nr    = fn_rep->resolve ("1");
  qcompare (fn_rep->fn[nr]->res_name, "ec:ecrm10@600");
}

QTEST_MAIN (TestSmartFont)
#include "smart_font_test.moc"
