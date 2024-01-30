#include "mouse_cursor_style.hpp"
#include "tm_ostream.hpp"

tm_ostream&
operator<< (tm_ostream& os, cursor_style cs) {
  os->flush ();
  os->write ("CURSOR_STYLE");
  return os;
}
