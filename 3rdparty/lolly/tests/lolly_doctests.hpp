#ifndef LOLLY_TBOX_MACROS_H
#define LOLLY_TBOX_MACROS_H

#include "doctest/doctest.h"
#include "string.hpp"

inline void
string_eq (string left, string right) {
  if (left != right) {
    cout << "left : " << left << LF;
    cout << "right: " << right << LF;
  }
  CHECK_EQ (left == right, true);
}

#endif
