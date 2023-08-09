#ifndef TM_DEBUG_H
#define TM_DEBUG_H
#include "string.hpp"

class tree;
void debug_message (string channel, string msg);
void debug_formatted (string channel, tree msg);
tree get_debug_messages (string kind, int max_number);
void clear_debug_messages ();
void clear_debug_messages (string channel);

extern tm_ostream debug_std;
extern tm_ostream debug_qt;
extern tm_ostream debug_aqua;
extern tm_ostream debug_widgets;
extern tm_ostream debug_fonts;
extern tm_ostream debug_convert;
extern tm_ostream debug_typeset;
extern tm_ostream debug_edit;
extern tm_ostream debug_packrat;
extern tm_ostream debug_history;
extern tm_ostream debug_keyboard;
extern tm_ostream debug_automatic;
extern tm_ostream debug_boot;
extern tm_ostream debug_events;
extern tm_ostream debug_shell;
extern tm_ostream debug_io;
extern tm_ostream debug_spell;
extern tm_ostream debug_updater;

extern tm_ostream std_error;
extern tm_ostream failed_error;
extern tm_ostream boot_error;
extern tm_ostream widkit_error;
extern tm_ostream qt_error;
extern tm_ostream font_error;
extern tm_ostream convert_error;
extern tm_ostream io_error;
extern tm_ostream bibtex_error;

extern tm_ostream std_warning;
extern tm_ostream convert_warning;
extern tm_ostream typeset_warning;
extern tm_ostream io_warning;
extern tm_ostream widkit_warning;
extern tm_ostream bibtex_warning;
extern tm_ostream std_bench;

#endif // defined TM_DEBUG_H
