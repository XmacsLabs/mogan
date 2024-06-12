
/******************************************************************************
 * MODULE     : image_files_test.cpp
 * COPYRIGHT  : (C) 2019  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "image_files.hpp"
#include "sys_utils.hpp"
#include "url.hpp"
#include <QtTest/QtTest>

class TestImageFiles : public QObject {
  Q_OBJECT

private slots:
  void test_svg_image_size ();
};

void
TestImageFiles::test_svg_image_size () {
#ifndef KERNEL_L2
  int w= 0, h= 0;
  svg_image_size (url ("$TEXMACS_PATH/misc/images/fancy-c.svg"), w, h);
  QCOMPARE (w, 24);
  QCOMPARE (h, 24);
#endif
}

QTEST_MAIN (TestImageFiles)
#include "image_files_test.moc"
