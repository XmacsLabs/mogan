#include "tm_ostream.hpp"
#include "mouse_cursor_style.hpp"

tm_ostream& operator<< (tm_ostream& os, cursor_style cs) {
  os->flush ();
  os->write ("CURSOR_STYLE");
  return os;
}
