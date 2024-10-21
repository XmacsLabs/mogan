#include "Freetype/unicode_font.hpp"
#include <QtTest/QtTest>

class TestParseVariant : public QObject {
  Q_OBJECT
private slots:
  void test ();
};

void
test_variant (string s, string r, int v) {
  string r2;
  int    v2= parse_variant (s, r2);
  QVERIFY (r == r2);
  QVERIFY (v == v2);
}

void
TestParseVariant::test () {
  test_variant ("<big-sum-2>", "sum", 2);
  test_variant ("<big-sum-1>", "sum", 1);
  test_variant ("<left-{-0>", "{", 0);
  test_variant ("<int>", "int", 0);
  test_variant ("<>", "", 0);
}

QTEST_MAIN (TestParseVariant)
#include "parse_variant_test.moc"