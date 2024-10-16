
/******************************************************************************
 * MODULE     : pdf_test.cpp
 * COPYRIGHT  : (C) 2023    Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Pdf/pdf.hpp"
#include "base.hpp"
#include "sys_utils.hpp"
#include <QtTest/QtTest>

class TestPDF : public QObject {
  Q_OBJECT

private slots:
  void init () { lolly::init_tbox (); }
  void test_pdf_version ();
};

void
TestPDF::test_pdf_version () {
  qcompare (pdf_version (url ("https://baidu.com")), default_pdf_version ());
  qcompare (pdf_version (url ("path/to/hello.png")), default_pdf_version ());

  qcompare (pdf_version (url ("$TEXMACS_PATH/tests/PDF/pdf_1_4_sample.pdf")),
            "1.4");
  qcompare (pdf_version (url ("$TEXMACS_PATH/tests/PDF/pdf_1_5_sample.pdf")),
            "1.5");
  qcompare (pdf_version (url ("$TEXMACS_PATH/tests/PDF/pdf_1_6_sample.pdf")),
            "1.6");
  qcompare (pdf_version (url ("$TEXMACS_PATH/tests/PDF/pdf_1_7_sample.pdf")),
            "1.7");
}

QTEST_MAIN (TestPDF)
#include "pdf_test.moc"
