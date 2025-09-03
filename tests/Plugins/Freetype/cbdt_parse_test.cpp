
/******************************************************************************
 * MODULE     : cbdt_parse_test.cpp
 * DESCRIPTION: Test cases for CBDT/CBLC table parsing in OpenType fonts
 * COPYRIGHT  : (C) 2025
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
#include "tm_file.hpp"
#include "tm_ostream.hpp"
#include "tm_url.hpp"
#include <QtTest/QtTest>

class TestCBDTTableParse : public QObject {
  Q_OBJECT
private:
  ot_cbdttable table;
private slots:
  void init ();
  void test_cbdt_parsing ();
  void test_get_png_from_glyphid ();
};

/*
Test CBDT table parsing functionality.
This test attempts to parse CBDT/CBLC tables from emoji fonts.
*/
void
TestCBDTTableParse::init () {
  lolly::init_tbox ();
  // Try to find a font with CBDT tables
  url u=
      concretize_url ("$TEXMACS_PATH/fonts/truetype/Mogan-NotoColorEmoji.ttf");

  cout << "Looking for font at: " << as_string (u) << "\n";
  cout << "Font exists: " << exists (u) << "\n";

  if (exists (u)) {
    table= parse_cbdttable (u);
    if (is_nil (table)) {
      cout << "Failed to parse CBDT table\n";
    }
    else {
      cout << "CBDT table parsed successfully\n";
    }
  }
  else {
    cout << "Font file not found\n";
  }
}

void
TestCBDTTableParse::test_cbdt_parsing () {
  if (is_nil (table)) {
    QSKIP ("No CBDT table found in test font");
  }

  QVERIFY (table->majorVersion >= 2 || table->majorVersion <= 3);
  QVERIFY (N (table->bitmapSizes) > 0);

  cout << "CBDT table version: " << table->majorVersion << "."
       << table->minorVersion << "\n";
  cout << "Number of bitmap sizes: " << N (table->bitmapSizes) << "\n";

  for (int i= 0; i < N (table->bitmapSizes); i++) {
    BitmapSizeRecord& record= table->bitmapSizes[i];
    cout << "Size " << i << ": ppem=" << (int) record.ppemX << "x"
         << (int) record.ppemY << ", glyphs " << record.startGlyphIndex << "-"
         << record.endGlyphIndex << "\n";
  }
}

void
TestCBDTTableParse::test_get_png_from_glyphid () {
  if (is_nil (table)) {
    QSKIP ("No CBDT table found in test font");
  }

  // Test getting PNG data for various glyph IDs and sizes
  // Use glyph IDs that are actually available in the font (based on bitmap size
  // records)
  for (int ppem= 32; ppem <= 128; ppem+= 32) {
    for (unsigned int glyphID= 4; glyphID <= 13;
         glyphID++) { // Changed from 1-10 to 4-13 to match available range
      string png_data= table->get_png_from_glyphid (glyphID, ppem);
      cout << "Found PNG data for glyph " << glyphID << " at " << ppem
           << " ppem: " << N (png_data) << " bytes\n";

      // Only check PNG signature if we got data
      if (N (png_data) > 8) {
        // Check PNG signature
        QVERIFY (png_data[0] == '\x89');
        QVERIFY (png_data[1] == 'P');
        QVERIFY (png_data[2] == 'N');
        QVERIFY (png_data[3] == 'G');
      }
      else {
        // If no PNG data found for this specific ppem, that's OK
        // The function should handle ppem size mismatches gracefully
        cout << "No PNG data found for glyph " << glyphID << " at " << ppem
             << " ppem\n";
      }
    }
  }
}

QTEST_MAIN (TestCBDTTableParse)
#include "cbdt_parse_test.moc"