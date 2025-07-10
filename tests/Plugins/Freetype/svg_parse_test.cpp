
/******************************************************************************
 * MODULE     : svg_parse_test.cpp
 * DESCRIPTION: Test cases for SVG table parsing in OpenType fonts
 * COPYRIGHT  : (C) 2025 songhahaha66
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Freetype/tt_tools.hpp"
#include "QtTest/qtestcase.h"
#include "analyze.hpp"
#include "hashset.ipp"
#include "sys_utils.hpp"
#include "tm_ostream.hpp"
#include "tm_url.hpp"
#include <QtTest/QtTest>

class TestSVGTableParse : public QObject {
  Q_OBJECT
private:
  ot_svgtable table;
private slots:
  void init ();
  void test_svg_parsing ();
  void test_get_svg_from_glyphid ();
};

/*
Verification data for SVG table parsing.
This test uses NotoColorEmoji-Regular.ttf as the test font.
*/
void
TestSVGTableParse::init () {
  lolly::init_tbox ();
  url u= concretize_url (
      "$TEXMACS_PATH/fonts/truetype/NotoColorEmoji-Regular.ttf");
  table= parse_svgtable (u);
}

void
TestSVGTableParse::test_svg_parsing () {
  QVERIFY (N (table->records) > 0);

  const SVGDocumentRecord& record0= table->records[0];
  QVERIFY (record0.startGlyphID == 2 && record0.endGlyphID == 3);

  const SVGDocumentRecord& record1= table->records[1];
  QVERIFY (record1.startGlyphID == 4 && record1.endGlyphID == 2799);

  // Verify the SVG content
  string svg_content= record0.svgDocument;
  bool   has_svg_tag= (search_forwards ("<svg", svg_content) >= 0);
  QVERIFY (has_svg_tag);
}

void
TestSVGTableParse::test_get_svg_from_glyphid () {
  // Test get_svg_from_glyphid function
  const SVGDocumentRecord& first_record = table->records[0];
  unsigned int             test_glyph_id= first_record.startGlyphID;

  string svg_content= table->get_svg_from_glyphid (test_glyph_id);

  // Verify the SVG format is correct
  QVERIFY (N (svg_content) > 0);
  QVERIFY (svg_content (0, 4) == "<svg");
  QVERIFY (svg_content (N (svg_content) - 6, N (svg_content)) == "</svg>");
  QVERIFY (search_forwards ("<g", svg_content) >= 0);
  QVERIFY (search_forwards ("</g>", svg_content) >= 0);
}

QTEST_MAIN (TestSVGTableParse)
#include "svg_parse_test.moc"
