
/******************************************************************************
 * MODULE     : image_files_test.cpp
 * COPYRIGHT  : (C) 2019  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "sys_utils.hpp"
#include "url.hpp"
#include <QtTest/QtTest>

class TestImageFiles : public QObject {
  Q_OBJECT

private slots:
  void init () { init_lolly (); }
  void test_svg_image_size ();
  void test_image_size ();
};

void
TestImageFiles::test_svg_image_size () {
  int w= 0, h= 0;
  url u1= url ("$TEXMACS_PATH/misc/images/fancy-c.svg");
  svg_image_size (u1, w, h);
  QCOMPARE (w, 24);
  QCOMPARE (h, 24);

  w= h  = 0;
  url u2= url_ramdisc (string_load (u1)) * url ("fancy-c.svg");
  svg_image_size (u2, w, h);
  QCOMPARE (w, 24);
  QCOMPARE (h, 24);
}

void
TestImageFiles::test_image_size () {
  int w= 0, h= 0;
  url u1= url ("$TEXMACS_PATH/misc/images/fancy-c.svg");
  image_size (u1, w, h);
  QCOMPARE (w, 24);
  QCOMPARE (h, 24);

  w= h  = 0;
  url u2= url_ramdisc (string_load (u1)) * url ("fancy-c.svg");
  image_size (u2, w, h);
  QCOMPARE (w, 24);
  QCOMPARE (h, 24);
}

QTEST_MAIN (TestImageFiles)
#include "image_files_test.moc"
