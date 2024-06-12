
/******************************************************************************
 * MODULE     : base.cpp
 * DESCRIPTION: Implementation of base.hpp for test purpose
 * COPYRIGHT  : (C) 2022  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "base.hpp"
#include <QtTest/QtTest>

#ifndef KERNEL_L1
bool headless_mode= false;
#endif

void
qcompare (string actual, string expected) {
  QCOMPARE (as_charp (actual), as_charp (expected));
}
