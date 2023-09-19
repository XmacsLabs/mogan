
/******************************************************************************
 * MODULE     : hyphenate_test.cpp
 * COPYRIGHT  : (C) 2023  Jeroen Wouters
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

// This test needs to read hyphenation files, run with TEXMACS_PATH correctly
// set

// Note: currently TeXmacs/Mogan hyphenation differs from LaTeX in that
// TeXmacs/Mogan don't break words close to the start or end of the word. It is
// not clear if this was an intentional decision.

#include "base.hpp"
#include "hyphenate.hpp"
#include "language.hpp"
#include "sys_utils.hpp"
#include <QtTest/QtTest>

class TestHyphens : public QObject {
  Q_OBJECT

private:
  void test_hyphens (language lan, string s, string t);

private slots:
  void init () { init_lolly (); }
  // TODO: add tests for predefined hyphenations
  void russian ();
  void english ();
  void french ();
  void chinese ();
};

void
TestHyphens::test_hyphens (language lan, string s, string t) {
  array<int> penalty;
  penalty = lan->get_hyphens (s);
  string h= "";
  int    i= 0, j= 0, k= 0;
  for (i= 0; i < N (s); goto_next_char (s, i, false), k++) {
    j= i;
    goto_next_char (s, j, false);
    h << s (i, j);
    if (penalty[k] == HYPH_STD) {
      h << "-";
    }
  }
  if (!(h == t)) cout << "Expected: " << t << "; got: " << h << LF;
  QCOMPARE (h, t);
}

void
TestHyphens::russian () {
  language lan= text_language ("russian");

  //"Нет" -> "Нет"
  test_hyphens (lan, "<#41D><#435><#442>", "<#41D><#435><#442>");
  //"пять" -> "пять"
  test_hyphens (lan, "<#43F><#44F><#442><#44C>", "<#43F><#44F><#442><#44C>");
  // "привет" -> "привет"
  test_hyphens (lan, "<#43F><#440><#438><#432><#435><#442>",
                "<#43F><#440><#438><#432><#435><#442>");
  // достопримечательности -> досто-при-ме-ча-тель-ности"
  test_hyphens (
      lan,
      "<#434><#43E><#441><#442><#43E><#43F><#440><#438><#43C><#435><#447><#430>"
      "<#442><#435><#43B><#44C><#43D><#43E><#441><#442><#438>",
      "<#434><#43E><#441><#442><#43E>-<#43F><#440><#438>-<#43C><#435>-<#447><#"
      "430>-<#442><#435><#43B><#44C>-<#43D><#43E><#441><#442><#438>");
}

void
TestHyphens::english () {
  language lan= text_language ("english");

  test_hyphens (lan, string ("baby"), string ("baby"));
  test_hyphens (lan, string ("manifests"), string ("man-i-fests"));
  // test_hyphens (lan,string("instruments"), string("in-stru-ments"));
  test_hyphens (lan, string ("instruments"), string ("instru-ments"));
  test_hyphens (lan, string ("he"), string ("he"));
  // texlive gives anal-y-ses, but hyphenate.py gives analy-ses
  // TeXmacs and hyphenate.py have an extra pattern "anal6ys" preventing the
  // break after 'l' test_hyphens (lan,string("analyses"), string("analy-ses"));
  test_hyphens (lan, string ("analyses"), string ("analyses"));
  test_hyphens (lan, string ("samples"), string ("sam-ples"));
  test_hyphens (lan, string ("submits"), string ("sub-mits"));
  // texlive gives fed-eral, but hyphenate.py gives fed-er-al
  // test_hyphens (lan,string("federal"), string("fed-er-al"));
  test_hyphens (lan, string ("federal"), string ("fed-eral"));
  // test_hyphens (lan,string("office"), string("of-fice"));
  test_hyphens (lan, string ("office"), string ("office"));
  test_hyphens (lan, string ("supercalifragilisticexpialidocious"),
                // string("su-per-cal-ifrag-ilis-tic-ex-pi-ali-do-cious"));
                string ("super-cal-ifrag-ilis-tic-ex-pi-ali-do-cious"));
}

void
TestHyphens::french () {
  language lan= text_language ("french");
  test_hyphens (lan, string ("mâcher"), string ("mâ-cher"));
  test_hyphens (lan, string ("léguer"), string ("lé-guer"));
}

void
TestHyphens::chinese () {
  language lan= text_language ("chinese");
  // 汉语族为分析语的一支家族
  test_hyphens (lan,
                "<#6C49><#8BED><#65CF><#4E3A><#5206><#6790><#8BED><#7684><#"
                "4E00><#652F><#5BB6><#65CF>",
                "<#6C49><#8BED><#65CF><#4E3A><#5206><#6790><#8BED><#7684><#"
                "4E00><#652F><#5BB6><#65CF>");
}

QTEST_MAIN (TestHyphens)
#include "hyphenate_test.moc"
