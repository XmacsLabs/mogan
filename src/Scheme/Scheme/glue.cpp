
/******************************************************************************
* MODULE     : glue.cpp
* DESCRIPTION: Glue for linking TeXmacs commands to scheme
* COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "glue.hpp"
#include "object_l1.hpp"
#include "object_l2.hpp"
#include "glue_l1.hpp"
#include "glue_l2.hpp"

#include "promise.hpp"
#include "tree.hpp"
#include "drd_mode.hpp"
#include "tree_search.hpp"
#include "modification.hpp"
#include "patch.hpp"

#include "boxes.hpp"
#include "editor.hpp"
#include "universal.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "locale.hpp"
#include "iterator.hpp"
#include "Freetype/tt_tools.hpp"
#include "Database/database.hpp"
#include "Sqlite3/sqlite3.hpp"
#include "Updater/tm_updater.hpp"

#define content tree

#if 0
template<class T> tmscm box_to_tmscm (T o) {
  return blackbox_to_tmscm (close_box<T> (o)); }
template<class T> T tmscm_to_box (tmscm obj) { 
  return open_box<T>(tmscm_to_blackbox (obj));  }
template<class T> tmscm cmp_box (tmscm o1, tmscm o2) { 
  return bool_to_tmscm (tmscm_to_box<T> (o1) == tmscm_to_box<T> (o2)); }
template<class T> tmscm boxP (tmscm t) {
  bool b= tmscm_is_blackbox (t) && 
          (type_box (blackboxvalue(t)) == type_helper<T>::id);
  return bool_to_tmscm (b);
}
#endif

/******************************************************************************
* Miscellaneous routines for use by glue only
******************************************************************************/

string original_path;

string
get_original_path () {
  return original_path;
}

string
texmacs_version (string which) {
  if (which == "tgz") return TM_DEVEL;
  if (which == "rpm") return TM_DEVEL_RELEASE;
  if (which == "stgz") return TM_STABLE;
  if (which == "srpm") return TM_STABLE_RELEASE;
  if (which == "devel") return TM_DEVEL;
  if (which == "stable") return TM_STABLE;
  if (which == "devel-release") return TM_DEVEL_RELEASE;
  if (which == "stable-release") return TM_STABLE_RELEASE;
  if (which == "revision") return TEXMACS_REVISION;
  return TEXMACS_VERSION;
}

void
set_fast_environments (bool b) {
  enable_fastenv= b;
}

void
win32_display (string s) {
  cout << s;
  cout.flush ();
}

void
tm_output (string s) {
  cout << s;
  cout.flush ();
}

void
tm_errput (string s) {
  cerr << s;
  cerr.flush ();
}

void
cpp_error () {
  //char *np= 0; *np= 1;
  FAILED ("an error occurred");
}

array<int>
get_bounding_rectangle (tree t) {
  editor ed= get_current_editor ();
  rectangle wr= ed -> get_window_extents ();
  path p= reverse (obtain_ip (t));
  selection sel= ed->search_selection (p * start (t), p * end (t));
  SI sz= ed->get_pixel_size ();
  double sf= ((double) sz) / 256.0;
  rectangle r (0, 0, 0, 0);
  if (!is_nil (sel->rs)) {
    rectangle selr= least_upper_bound (sel->rs) / sf;
    r= translate (selr, wr->x1, wr->y2);
  }
  array<int> ret;
  ret << (r->x1) << (r->y1) << (r->x2) << (r->y2);
  //ret << (r->x1/PIXEL) << (r->y1/PIXEL) << (r->x2/PIXEL) << (r->y2/PIXEL);
  return ret;
}

bool
supports_native_pdf () {
#ifdef PDF_RENDERER
  return true;
#else
  return false;
#endif
}

bool
supports_ghostscript () {
#ifdef USE_GS
  return true;
#else
  return false;
#endif
}

bool
is_busy_versioning () {
  return busy_versioning;
}

array<SI>
get_screen_size () {
  array<SI> r;
  SI w, h;
  gui_root_extents (w, h);
  r << w << h;
  return r;
}

/******************************************************************************
* Redirections
******************************************************************************/

void
cout_buffer () {
  cout.buffer ();
}

string
cout_unbuffer () {
  return cout.unbuffer ();
}

tree
coerce_string_tree (string s) {
  return s;
}

string
coerce_tree_string (tree t) {
  return as_string (t);
}

tree
tree_ref (tree t, int i) {
  return t[i];
}

tree
tree_set (tree t, int i, tree u) {
  t[i]= u;
  return u;
}

tree
tree_range (tree t, int i, int j) {
  return t(i,j);
}

tree
tree_append (tree t1, tree t2) {
  return t1 * t2;
}

bool
tree_active (tree t) {
  path ip= obtain_ip (t);
  return is_nil (ip) || last_item (ip) != DETACHED;
}

tree
tree_child_insert (tree t, int pos, tree x) {
  //cout << "t= " << t << "\n";
  //cout << "x= " << x << "\n";
  int i, n= N(t);
  tree r (t, n+1);
  for (i=0; i<pos; i++) r[i]= t[i];
  r[pos]= x;
  for (i=pos; i<n; i++) r[i+1]= t[i];
  return r;
}

/******************************************************************************
* Document modification routines
******************************************************************************/

extern tree the_et;

tree
tree_assign (tree r, tree t) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    assign (reverse (ip), copy (t));
    return subtree (the_et, reverse (ip));
  }
  else {
    assign (r, copy (t));
    return r;
  }
}

tree
tree_insert (tree r, int pos, tree t) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    insert (reverse (path (pos, ip)), copy (t));
    return subtree (the_et, reverse (ip));
  }
  else {
    insert (r, pos, copy (t));
    return r;
  }
}

tree
tree_remove (tree r, int pos, int nr) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    remove (reverse (path (pos, ip)), nr);
    return subtree (the_et, reverse (ip));
  }
  else {
    remove (r, pos, nr);
    return r;
  }
}

tree
tree_split (tree r, int pos, int at) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    split (reverse (path (at, pos, ip)));
    return subtree (the_et, reverse (ip));
  }
  else {
    split (r, pos, at);
    return r;
  }
}

tree
tree_join (tree r, int pos) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    join (reverse (path (pos, ip)));
    return subtree (the_et, reverse (ip));
  }
  else {
    join (r, pos);
    return r;
  }
}

tree
tree_assign_node (tree r, tree_label op) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    assign_node (reverse (ip), op);
    return subtree (the_et, reverse (ip));
  }
  else {
    assign_node (r, op);
    return r;
  }
}

tree
tree_insert_node (tree r, int pos, tree t) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    insert_node (reverse (path (pos, ip)), copy (t));
    return subtree (the_et, reverse (ip));
  }
  else {
    insert_node (r, pos, copy (t));
    return r;
  }
}

tree
tree_remove_node (tree r, int pos) {
  path ip= copy (obtain_ip (r));
  if (ip_attached (ip)) {
    remove_node (reverse (path (pos, ip)));
    return subtree (the_et, reverse (ip));
  }
  else {
    remove_node (r, pos);
    return r;
  }
}

/******************************************************************************
* Widgets
******************************************************************************/

#define TMSCM_ASSERT_WIDGET(o,arg,rout) \
TMSCM_ASSERT (tmscm_is_widget (o), o, arg, rout)

bool
tmscm_is_widget (tmscm u) {
  return (tmscm_is_blackbox (u) &&
         (type_box (tmscm_to_blackbox(u)) == type_helper<widget>::id));
}


static tmscm 
widget_to_tmscm (widget o) {
  return blackbox_to_tmscm (close_box<widget> (o));
}

widget
tmscm_to_widget (tmscm o) {
  return open_box<widget> (tmscm_to_blackbox (o));
}

/******************************************************************************
*  Widget Factory
******************************************************************************/

typedef promise<widget> promise_widget;

#define TMSCM_ASSERT_PROMISE_WIDGET(o,arg,rout) \
TMSCM_ASSERT (tmscm_is_promise_widget (o), o, arg, rout)

bool
tmscm_is_promise_widget (tmscm u) {
  return (tmscm_is_blackbox (u) && 
         (type_box (tmscm_to_blackbox(u)) == type_helper<promise_widget>::id));
}

static tmscm 
promise_widget_to_tmscm (promise_widget o) {
  return blackbox_to_tmscm (close_box<promise_widget> (o));
}

static promise_widget
tmscm_to_promise_widget (tmscm o) {
  return open_box<promise_widget> (tmscm_to_blackbox (o));
}


void string_save (string s, url u) { (void) save_string (u, s); }
string string_load (url u) {
  string s; (void) load_string (u, s, false); return s; }
void string_append_to_file (string s, url u) { (void) append_string (u, s); }
url url_ref (url u, int i) { return u[i]; }


tree
var_apply (tree& t, modification m) {
  apply (t, copy (m));
  return t;
}

tree
var_clean_apply (tree& t, modification m) {
  return clean_apply (t, copy (m));
}

/******************************************************************************
* Patch
******************************************************************************/

bool
tmscm_is_patch (tmscm p) {
  return (tmscm_is_blackbox (p) &&
	  (type_box (tmscm_to_blackbox(p)) == type_helper<patch>::id))
    || (tmscm_is_string (p));
}

tmscm 
patch_to_tmscm (patch p) {
  return blackbox_to_tmscm (close_box<patch> (p));
}

patch
tmscm_to_patch (tmscm obj) {
  return open_box<patch> (tmscm_to_blackbox (obj));
}

tmscm 
patchP (tmscm t) {
  bool b= tmscm_is_patch (t);
  return bool_to_tmscm (b);
}

patch
branch_patch (array<patch> a) {
  return patch (true, a);
}

tree
var_clean_apply (tree t, patch p) {
  return clean_apply (copy (p), t);
}

tree
var_apply (tree& t, patch p) {
  apply (copy (p), t);
  return t;
}

/******************************************************************************
* Table types
******************************************************************************/

typedef hashmap<string,string> table_string_string;

bool
tmscm_is_table_string_string (tmscm p) {
  if (tmscm_is_null (p)) return true;
  else if (!tmscm_is_pair (p)) return false;
  else {
    tmscm f= tmscm_car (p);
    return tmscm_is_pair (f) &&
    tmscm_is_string (tmscm_car (f)) &&
    tmscm_is_string (tmscm_cdr (f)) &&
    tmscm_is_table_string_string (tmscm_cdr (p));
  }
}

#define TMSCM_ASSERT_TABLE_STRING_STRING(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_table_string_string (p), p, arg, rout)

tmscm 
table_string_string_to_tmscm (hashmap<string,string> t) {
  tmscm p= tmscm_null ();
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    string s= it->next ();
    tmscm n= tmscm_cons (string_to_tmscm (s), string_to_tmscm (t[s]));
    p= tmscm_cons (n, p);
  }
  return p;
}

hashmap<string,string>
tmscm_to_table_string_string (tmscm p) {
  hashmap<string,string> t;
  while (!tmscm_is_null (p)) {
    tmscm n= tmscm_car (p);
    t (tmscm_to_string (tmscm_car (n)))= tmscm_to_string (tmscm_cdr (n));
    p= tmscm_cdr (p);
  }
  return t;
}

#define tmscm_is_solution tmscm_is_table_string_string
#define TMSCM_ASSERT_SOLUTION(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_solution(p), p, arg, rout)
#define solution_to_tmscm table_string_string_to_tmscm
#define tmscm_to_solution tmscm_to_table_string_string

typedef array<patch> array_patch;
typedef array<widget> array_widget;

static bool
tmscm_is_array_widget (tmscm p) {
  if (tmscm_is_null (p)) return true;
  else return tmscm_is_pair (p) &&
    tmscm_is_widget (tmscm_car (p)) &&
    tmscm_is_array_widget (tmscm_cdr (p));
}

#define TMSCM_ASSERT_ARRAY_WIDGET(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_array_widget (p), p, arg, rout)

/* static */ tmscm 
array_widget_to_tmscm (array<widget> a) {
  int i, n= N(a);
  tmscm p= tmscm_null ();
  for (i=n-1; i>=0; i--) p= tmscm_cons (widget_to_tmscm (a[i]), p);
  return p;
}

/* static */ array<widget>
tmscm_to_array_widget (tmscm p) {
  array<widget> a;
  while (!tmscm_is_null (p)) {
    a << tmscm_to_widget (tmscm_car (p));
    p= tmscm_cdr (p);
  }
  return a;
}

static bool
tmscm_is_array_patch (tmscm p) {
  if (tmscm_is_null (p)) return true;
  else return tmscm_is_pair (p) &&
    tmscm_is_patch (tmscm_car (p)) &&
    tmscm_is_array_patch (tmscm_cdr (p));
}


#define TMSCM_ASSERT_ARRAY_PATCH(p,arg,rout) \
TMSCM_ASSERT (tmscm_is_array_patch (p), p, arg, rout)

/* static */ tmscm 
array_patch_to_tmscm (array<patch> a) {
  int i, n= N(a);
  tmscm p= tmscm_null ();
  for (i=n-1; i>=0; i--) p= tmscm_cons (patch_to_tmscm (a[i]), p);
  return p;
}

/* static */ array<patch>
tmscm_to_array_patch (tmscm p) {
  array<patch> a;
  while (!tmscm_is_null (p)) {
    a << tmscm_to_patch (tmscm_car (p));
    p= tmscm_cdr (p);
  }
  return a;
}

void register_glyph (string s, array_array_array_double gl);
string recognize_glyph (array_array_array_double gl);

/******************************************************************************
* Gluing
******************************************************************************/

#include "server.hpp"
#include "tm_window.hpp"
#include "boot.hpp"
#include "connect.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "client_server.hpp"
#include "analyze.hpp"
#include "wencoding.hpp"
#include "base64.hpp"
#include "tree_traverse.hpp"
#include "tree_analyze.hpp"
#include "tree_correct.hpp"
#include "tree_modify.hpp"
#include "tree_math_stats.hpp"
#include "tm_frame.hpp"
#include "Concat/concater.hpp"
#include "converter.hpp"
#include "tm_timer.hpp"
#include "Metafont/tex_files.hpp"
#include "Freetype/tt_file.hpp"
#include "LaTeX_Preview/latex_preview.hpp"
#include "Bibtex/bibtex.hpp"
#include "Bibtex/bibtex_functions.hpp"
#include "link.hpp"
#include "dictionary.hpp"
#include "patch.hpp"
#include "packrat.hpp"
#include "new_style.hpp"
#include "persistent.hpp"

#include "Pdf/pdf_hummus_extract_attachment.hpp"
#include "Pdf/pdf_hummus_make_attachment.hpp"

#include "glue_basic.cpp"
#include "glue_editor.cpp"
#include "glue_server.cpp"

void
initialize_glue () {
  tmscm_install_procedure ("patch?", patchP, 1, 0, 0);
  
  initialize_glue_l1 ();
  initialize_glue_l2 ();
  initialize_glue_basic ();
  initialize_glue_editor ();
  initialize_glue_server ();
}
