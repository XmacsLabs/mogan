
/******************************************************************************
 * MODULE     : tm_server.cpp
 * DESCRIPTION: The TeXmacs server
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_server.hpp"
#include "analyze.hpp"
#include "boot.hpp"
#include "config.h"
#include "connect.hpp"
#include "convert.hpp"
#include "cork.hpp"
#include "dictionary.hpp"
#include "file.hpp"
#include "glue.hpp"
#ifndef OS_WASM
#include "goldfish.hpp"
#endif
#include "lolly/system/subprocess.hpp"
#include "new_style.hpp"
#include "s7_blackbox.hpp"
#include "socket_notifier.hpp"
#include "sys_utils.hpp"
#include "tm_link.hpp"
#include "tm_sys_utils.hpp"
#include <moebius/drd/drd_std.hpp>
#include <s7_tm.hpp>

#ifdef QTTEXMACS
#include <QApplication>
#include <QProcess>
#include <QStringList>
#endif

server* the_server     = NULL;
bool    texmacs_started= false;
bool    headless_mode  = false;
url     tm_init_file   = url_none ();
url     my_init_file   = url_none ();
string  my_init_cmds   = "";

/******************************************************************************
 * Init applications
 ******************************************************************************/

void
init_app (app_type app) {
  if (is_none (tm_init_file)) {
    if (app == app_type::DRAW) {
      tm_init_file= "$TEXMACS_PATH/progs/init-draw.scm";
    }
    else if (app == app_type::RESEARCH) {
      tm_init_file= "$TEXMACS_PATH/progs/init-research.scm";
    }
  }
  exec_file (materialize (tm_init_file));
  if (is_none (my_init_file))
    my_init_file= "$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm";
  bench_start ("initialize scheme");
  if (exists (my_init_file)) exec_file (materialize (my_init_file));
  bench_cumul ("initialize scheme");
  if (my_init_cmds != "") {
    my_init_cmds= "(begin" * my_init_cmds * ")";
    exec_delayed (scheme_cmd (my_init_cmds));
  }
}

/******************************************************************************
 * Execution of commands
 ******************************************************************************/

void          reset_inclusions ();
extern string printing_dpi;
extern string printing_on;
extern int    nr_windows;

#ifdef QTTEXMACS
void del_obj_qt_renderer (void);
#endif

/******************************************************************************
 * Texmacs server constructor and destructor
 ******************************************************************************/

void
texmacs_interpose_handler () {
  if (the_server != NULL) (*the_server)->interpose_handler ();
}

void
texmacs_wait_handler (string message, string arg, int level) {
  (void) level;
  if (texmacs_started && the_server != NULL)
    (*the_server)->wait_handler (message, arg);
  else cout << "TeXmacs] Please wait: " << message << " " << arg << "\n";
}

bool
is_server_started () {
  return the_server != NULL;
}

server
get_server () {
  ASSERT (is_server_started (), "TeXmacs server not yet started");
  return *the_server;
}

bool
in_presentation_mode () {
  return get_server ()->in_full_screen_mode ();
}

tree
get_subtree (path p) {
  return get_current_editor ()->the_subtree (p);
}

void
gui_set_output_language (string lan) {
  set_output_language (lan);
  get_server ()->refresh ();
  gui_refresh ();
}

server_rep::server_rep () {}
server_rep::~server_rep () {}

tm_server_rep::tm_server_rep (app_type app) : def_zoomf (1.0) {
  the_server= tm_new<server> (this);
  //  "(read-set! keywords 'prefix)\n"
  //  "(read-enable 'positions)\n"
  //  "(debug-enable 'debug)\n"
  // #ifdef DEBUG_ON
  //  "(debug-enable 'backtrace)\n"
  // #endif
  const char* init_prg= "(begin \n"
                        "(define (display-to-string obj)\n"
                        "  (call-with-output-string\n"
                        "    (lambda (port) (display obj port))))\n"
                        "\n"
                        "(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
                        "(define (xmacs-version) \"" XMACS_VERSION "\")\n"
                        "(define object-stack '(()))\n"
                        ")";
  eval_scheme_root (init_prg);
  initialize_smobs (initialize_scheme ());
  initialize_glue ();
#ifndef OS_WASM
  goldfish::glue_for_community_edition (tm_s7);
#endif
  gui_interpose (texmacs_interpose_handler);
  set_wait_handler (texmacs_wait_handler);

  init_app (app);

#ifdef OS_GNU_LINUX
  return; // in order to avoid segmentation faults
#elif defined OS_POWERPC_GNU_LINUX
  return; // in order to avoid segmentation faults
#endif
}

tm_server_rep::~tm_server_rep () {}
server::server (app_type app) : rep (tm_new<tm_server_rep> (app)) {}
server_rep*
tm_server_rep::get_server () {
  return this;
}

/******************************************************************************
 * Miscellaneous routines
 ******************************************************************************/

void
tm_server_rep::style_clear_cache () {
  style_invalidate_cache ();

  array<url> vs= get_all_views ();
  for (int i= 0; i < N (vs); i++)
    view_to_editor (vs[i])->init_style ();
}

void
tm_server_rep::refresh () {
  array<url> l= windows_list ();
  for (int i= 0; i < N (l); i++) {
    url u= window_to_view (l[i]);
    if (!is_none (u)) concrete_view (u)->win->refresh ();
  }
}

void
tm_server_rep::interpose_handler () {
#ifndef OS_WASM
#ifdef QTTEXMACS
  // TeXmacs/Qt handles delayed messages and socket notification
  // in its own runloop
#ifndef QTPIPES
  perform_select ();
#endif
  process_all_pipes ();
#else
  perform_select ();
  exec_pending_commands ();
#endif
#endif // not OS_WASM

  if (!headless_mode) {
    int i, j;
    for (i= 0; i < N (bufs); i++) {
      tm_buffer buf= (tm_buffer) bufs[i];

      for (j= 0; j < N (buf->vws); j++) {
        tm_view vw= (tm_view) buf->vws[j];
        if (vw->win != NULL) vw->ed->apply_changes ();
      }

      for (j= 0; j < N (buf->vws); j++) {
        tm_view vw= (tm_view) buf->vws[j];
        if (vw->win != NULL) vw->ed->animate ();
      }
    }
    windows_refresh ();
  }
}

void
tm_server_rep::wait_handler (string message, string arg) {
  if (has_current_window ())
    show_wait_indicator (concrete_window ()->win, translate (message), arg);
  else cout << "TeXmacs] Please wait: " << message << " " << arg << "\n";
}

void
tm_server_rep::set_script_status (int i) {
  script_status= i;
}

void
tm_server_rep::set_printing_command (string cmd) {
  ::set_printing_cmd (cmd);
}

void
tm_server_rep::set_printer_page_type (string type) {
  printing_on= type;
}

string
tm_server_rep::get_printer_page_type () {
  return printing_on;
}

void
tm_server_rep::set_printer_dpi (string dpi) {
  printing_dpi= dpi;
}

void
tm_server_rep::set_default_zoom_factor (double zoom) {
  if (zoom >= 25.0) zoom= 25.0;
  if (zoom <= 0.04) zoom= 0.04;
  zoom     = normal_zoom (zoom);
  def_zoomf= zoom;
}

double
tm_server_rep::get_default_zoom_factor () {
  return def_zoomf;
}

void
tm_server_rep::inclusions_gc (string which) {
  (void) which;
  reset_inclusions ();
  typeset_update_all ();
}

void
tm_server_rep::typeset_update (path p) {
  array<url> vs= get_all_views ();
  for (int i= 0; i < N (vs); i++)
    view_to_editor (vs[i])->typeset_invalidate (p);
}

void
tm_server_rep::typeset_update_all () {
  array<url> vs= get_all_views ();
  for (int i= 0; i < N (vs); i++)
    view_to_editor (vs[i])->typeset_invalidate_all ();
}

bool
tm_server_rep::is_yes (string s) {
  s        = locase_all (s);
  string st= locase_all (translate ("yes"));
  return tm_forward_access (s, 0) == tm_forward_access (st, 0) || s == st;
}

void
tm_server_rep::quit () {
  close_all_pipes ();
  call ("quit-TeXmacs-scheme");
  clear_pending_commands ();
#ifdef QTTEXMACS
  del_obj_qt_renderer ();
#endif
  exit (0);
}

void
tm_server_rep::restart () {
  close_all_pipes ();
  call ("quit-TeXmacs-scheme");
  clear_pending_commands ();

#ifdef QTTEXMACS
  del_obj_qt_renderer ();
  array<url>  buffers= get_all_buffers ();
  QStringList args   = QApplication::arguments ();

  if (!args.isEmpty ()) args.removeFirst ();

  for (int i= 0; i < N (buffers); i++) {
    string file_path= as_string (buffers[i]);
    args << to_qstring (file_path);
  }

  QProcess::startDetached (QApplication::applicationFilePath (), args);
#endif

  _exit (0);
}

/******************************************************************************
 * System commands
 ******************************************************************************/

void
tm_server_rep::shell (string s) {
  lolly::system::call (s);
}
