#include "Freetype/unicode_font.hpp"
#include <QtTest/QtTest>

class TestParseVariant : public QObject {
  Q_OBJECT
private slots:
  void test ();
};

void
test_variant (string s, string h, string r, int v) {
  string h2;
  string r2;
  int    v2= parse_variant (s, h2, r2);
  QVERIFY (h == h2);
  QVERIFY (r == r2);
  QVERIFY (v == v2);
}

void
TestParseVariant::test () {
  test_variant ("<big-sum-2>", "big", "sum", 2);
  test_variant ("<big-sum-1>", "big", "sum", 1);
  test_variant ("<left-{-0>", "left", "{", 0);
  test_variant ("<int>", "", "int", 0);
  test_variant ("<>", "", "", 0);
}

QTEST_MAIN (TestParseVariant)
#include "parse_variant_test.moc"