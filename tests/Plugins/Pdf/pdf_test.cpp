
/******************************************************************************
 * MODULE     : pdf_test.cpp
 * COPYRIGHT  : (C) 2023    Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Pdf/pdf.hpp"
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
  pdf_version (url ("https://baidu.com"));
}

QTEST_MAIN (TestPDF)
#include "pdf_test.moc"
