
/******************************************************************************
 * MODULE     : pdf_make_attachment_test.cpp
 * COPYRIGHT  : (C) Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "file.hpp"
#include "pdf_hummus_get_attachment.hpp"
#include "pdf_hummus_make_attachment.hpp"
#include "sys_utils.hpp"
#include <QtTest/QtTest>

class TestHummusPdfMakeAttachment : public QObject {
  Q_OBJECT

private slots:
  void init () { lolly::init_tbox (); }
  void test_pdf_hummus_make_attachment ();
};

void
TestHummusPdfMakeAttachment::test_pdf_hummus_make_attachment () {

  bool attach_judge= pdf_hummus_make_attachment (
      url ("$TEXMACS_PATH/tests/images/29_1_1.pdf"),
      url ("$TEXMACS_PATH/tests/29_1_1.tm"),
      url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"));
  QVERIFY (attach_judge);

  bool out_pdf_judge=
      is_regular (url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"));
  QVERIFY (out_pdf_judge);

  url  attachment;
  bool separate_tm_judge= get_tm_attachment_in_pdf (
      url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"), attachment);
  QVERIFY (separate_tm_judge);

  bool tm_exist_judge= is_regular (attachment);
  QVERIFY (separate_tm_judge);

  QVERIFY (attachment == url ("$TEXMACS_PATH/tests/images/29_1_1.tm"));

  remove (url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"));
  remove (attachment);
}

QTEST_MAIN (TestHummusPdfMakeAttachment)
#include "pdf_make_attachment_test.moc"
