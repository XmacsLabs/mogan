#include "tm_ostream.hpp"
enum class cursor_style {
  OPENHAND,
  CLOSEHAND,
  NORMAL,
  CROSS,
  UP_ARROW,
  IBEAM,
  WAIT,
  FORBIDDEN,
  POINTING_HAND,
  SIZE_VER,
  SIZE_HOR,
  SIZE_BDIAG,
  SIZE_FDIAG,
  SIZE_ALL,
};

tm_ostream& operator<<(tm_ostream& os, cursor_style cs);