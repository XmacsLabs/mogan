#include <QtTest/QtTest>
#include "base.hpp"


void
qcompare (string actual, string expected) {
  QCOMPARE (as_charp(actual), as_charp(expected));
}