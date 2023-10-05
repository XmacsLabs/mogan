
/******************************************************************************
 * MODULE     : pdf_make_attachment_test.cpp
 * COPYRIGHT  : (C) Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Pdf/pdf_hummus_extract_attachment.hpp"
#include "Pdf/pdf_hummus_make_attachment.hpp"
#include "base.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tree_helper.hpp"
#include <QtTest/QtTest>
class TestHummusPdfMakeAttachment : public QObject {
  Q_OBJECT

private slots:
  void init () { init_lolly (); }
  void test_pdf_hummus_make_single_attachment ();
  void test_pdf_hummus_make_multiple_attachments ();
  void test_pdf_hummus_make_zero_attachment ();
  void test_pdf_hummus_make_attachment_for_wrong_pdf ();
  void test_pdf_hummus_make_attachment_for_no_pdf ();
  void test_get_linked_file_paths ();
  void test_replace_with_relative_path ();
  void test_get_main_tm ();
};
void
TestHummusPdfMakeAttachment::test_pdf_hummus_make_single_attachment () {

  array<url> attachmen_0;
  attachmen_0 << url ("$TEXMACS_PATH/tests/29_1_1.tm");
  bool attach_judge= pdf_hummus_make_attachments (
      url ("$TEXMACS_PATH/tests/images/29_1_1.pdf"), attachmen_0,
      url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"));
  QVERIFY (attach_judge);
  bool out_pdf_judge=
      is_regular (url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"));
  QVERIFY (out_pdf_judge);

  list<url> attachment;
  bool      separate_tm_judge= extract_attachments_from_pdf (
      url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"), attachment);
  QVERIFY (separate_tm_judge);

  QVERIFY (N (attachment) == 1);

  bool tm_exist_judge= is_regular (attachment[0]);
  QVERIFY (tm_exist_judge);

  QVERIFY (attachment[0] == url ("$TEXMACS_PATH/tests/images/29_1_1.tm"));
  QVERIFY (string_load (url ("$TEXMACS_PATH/tests/29_1_1.tm")) ==
           string_load (url ("$TEXMACS_PATH/tests/images/29_1_1.tm")));
  remove (url ("$TEXMACS_PATH/tests/images/29_1_1_attach.pdf"));
  remove (attachment[0]);
}

void
TestHummusPdfMakeAttachment::test_pdf_hummus_make_multiple_attachments () {

  auto multiple_tm= array<url> (
      url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main.tm"),
      url ("$TEXMACS_PATH/tests/29_4_2multiple-files/tsts/myslides.ts"),
      url ("$TEXMACS_PATH/tests/29_4_2multiple-files/p/logo.pdf"));

  bool attach_judge= pdf_hummus_make_attachments (
      url ("$TEXMACS_PATH/tests/images/29_4_2.pdf"), multiple_tm,
      url ("$TEXMACS_PATH/tests/images/29_4_2_attach.pdf"));
  QVERIFY (attach_judge);

  bool out_pdf_judge=
      is_regular (url ("$TEXMACS_PATH/tests/images/29_4_2_attach.pdf"));
  QVERIFY (out_pdf_judge);

  list<url> attachments;
  bool      separate_tm_judge= extract_attachments_from_pdf (
      url ("$TEXMACS_PATH/tests/images/29_4_2_attach.pdf"), attachments);
  QVERIFY (separate_tm_judge);

  QVERIFY (N (attachments) == 3);
  for (int i= 0; i < N (attachments); i++) {
    bool tm_exist_judge= is_regular (attachments[i]);
    QVERIFY (tm_exist_judge);
  }

  QVERIFY (contains (attachments, url ("$TEXMACS_PATH/tests/images/main.tm")));
  QVERIFY (
      contains (attachments, url ("$TEXMACS_PATH/tests/images/myslides.ts")));
  QVERIFY (contains (attachments, url ("$TEXMACS_PATH/tests/images/logo.pdf")));
  QVERIFY (
      string_load (url ("$TEXMACS_PATH/tests/images/main.tm")) ==
      string_load (url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main.tm")));
  QVERIFY (string_load (url ("$TEXMACS_PATH/tests/images/myslides.ts")) ==
           string_load (url (
               "$TEXMACS_PATH/tests/29_4_2multiple-files/tsts/myslides.ts")));
  QVERIFY (string_load (url ("$TEXMACS_PATH/tests/images/logo.pdf")) ==
           string_load (
               url ("$TEXMACS_PATH/tests/29_4_2multiple-files/p/logo.pdf")));

  remove (url ("$TEXMACS_PATH/tests/images/29_4_2_attach.pdf"));

  for (int i= 0; i < N (attachments); i++) {
    remove (attachments[i]);
  }
}

void
TestHummusPdfMakeAttachment::test_pdf_hummus_make_zero_attachment () {
  bool attach_judge= pdf_hummus_make_attachments (
      url ("$TEXMACS_PATH/tests/images/29_4_3.pdf"), array<url> (),
      url ("$TEXMACS_PATH/tests/images/29_4_3_attach.pdf"));
  QVERIFY (!attach_judge);
  bool out_pdf_judge=
      is_regular (url ("$TEXMACS_PATH/tests/images/29_4_3_attach.pdf"));
  QVERIFY (!out_pdf_judge);

  list<url> attachment;
  bool      separate_tm_judge= extract_attachments_from_pdf (
      url ("$TEXMACS_PATH/tests/images/29_4_3_attach.pdf"), attachment);
  QVERIFY (!separate_tm_judge);
  QVERIFY (N (attachment) == 0);
}

void
TestHummusPdfMakeAttachment::test_pdf_hummus_make_attachment_for_wrong_pdf () {
  array<url> attachment_0;
  attachment_0 << url ("$TEXMACS_PATH/tests/29_1_1.tm");
  bool attach_judge= pdf_hummus_make_attachments (
      url ("$TEXMACS_PATH/tests/images/29_4_4.pdf"), attachment_0,
      url ("$TEXMACS_PATH/tests/images/29_4_4_attach.pdf"));
  QVERIFY (!attach_judge);
  bool out_pdf_judge=
      is_regular (url ("$TEXMACS_PATH/tests/images/29_4_4_attach.pdf"));
  QVERIFY (out_pdf_judge);
  remove (url ("$TEXMACS_PATH/tests/images/29_4_4_attach.pdf"));
}
void
TestHummusPdfMakeAttachment::test_pdf_hummus_make_attachment_for_no_pdf () {
  QVERIFY (!is_regular (url ("$TEXMACS_PATH/tests/images/29_4_5.pdf")));
  array<url> attachment_0;
  attachment_0 << url ("$TEXMACS_PATH/tests/29_1_1.tm");
  bool attach_judge= pdf_hummus_make_attachments (
      url ("$TEXMACS_PATH/tests/images/29_4_5.pdf"), attachment_0,
      url ("$TEXMACS_PATH/tests/images/29_4_5_attach.pdf"));
  QVERIFY (!attach_judge);
  bool out_pdf_judge=
      is_regular (url ("$TEXMACS_PATH/tests/images/29_4_5_attach.pdf"));
  QVERIFY (!out_pdf_judge);
}

void
TestHummusPdfMakeAttachment::test_get_linked_file_paths () {
  array<url> attachment_0;
  attachment_0 << url ("$TEXMACS_PATH/tests/29_1_1.tm");
  string     texmacs_doc_1= string_load (url ("$TEXMACS_PATH/tests/29_1_1.tm"));
  tree       texmacs_tree_1= texmacs_to_tree (texmacs_doc_1);
  array<url> linked_0      = get_linked_file_paths (
      texmacs_tree_1, url ("$TEXMACS_PATH/tests/29_1_1.tm"));
  QVERIFY (N (linked_0) == 0);

  string texmacs_doc_2=
      string_load (url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main.tm"));
  tree       texmacs_tree_2= texmacs_to_tree (texmacs_doc_2);
  array<url> linked        = get_linked_file_paths (
      texmacs_tree_2, url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main.tm"));
  QVERIFY (N (linked) == 2);
  QVERIFY (is_regular (linked[0]));
  QVERIFY (tail (linked[0]) == url ("myslides.ts"));
  QVERIFY (is_regular (linked[1]));
  QVERIFY (tail (linked[1]) == url ("logo.pdf"));
}
void
TestHummusPdfMakeAttachment::test_replace_with_relative_path () {
  string texmacs_doc_1=
      string_load (url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main.tm"));
  tree texmacs_tree_1= texmacs_to_tree (texmacs_doc_1);

  string texmacs_doc_2= string_load (
      url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main_convert_path.tm"));
  tree texmacs_tree_2= texmacs_to_tree (texmacs_doc_2);
  tree after_1       = replace_with_relative_path (
      texmacs_tree_1, url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main.tm"));
  tree after_2= replace_with_relative_path (
      texmacs_tree_2,
      url ("$TEXMACS_PATH/tests/29_4_2multiple-files/main_convert_path.tm"));
  QVERIFY (after_2 == after_1);
}
void
TestHummusPdfMakeAttachment::test_get_main_tm () {
  QVERIFY (get_main_tm (url ("$TEXMACS_PATH/tests/images/29_5_1.pdf")) ==
           url ("$TEXMACS_PATH/tests/images/29_5_1.tm"));
  QVERIFY (get_main_tm (url ("$TEXMACS_PATH/tests/images/29_5_2.pdf")) ==
           url ("$TEXMACS_PATH/tests/images/29_5_2.tm"));
}

QTEST_MAIN (TestHummusPdfMakeAttachment)
#include "pdf_make_attachment_test.moc"
