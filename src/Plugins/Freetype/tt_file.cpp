
/******************************************************************************
 * MODULE     : tt_file.cpp
 * DESCRIPTION: Finding a True Type font
 * COPYRIGHT  : (C) 2003  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tt_file.hpp"
#include "Metafont/tex_files.hpp"
#include "analyze.hpp"
#include "data_cache.hpp"
#include "file.hpp"
#include "font.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"
#include "preferences.hpp"
#include "scheme.hpp"
#include "sys_utils.hpp"
#include "tm_file.hpp"
#include "tm_timer.hpp"
#include "tt_tools.hpp"

#ifdef USE_FONTCONFIG
#include <fontconfig/fontconfig.h>
#endif

static hashset<string>      tt_fonts;
static hashmap<string, url> tt_font_locations;

url
add_to_path (url u, url d) {
  if (is_or (d)) return add_to_path (add_to_path (u, d[1]), d[2]);
  if (is_none (u) || u == d) return d;
  if (is_or (u) && u[1] == d) return u;
  if (is_or (u)) return u[1] | add_to_path (u[2], d);
  return u | d;
}

void
tt_extend_font_path (url u) {
  if (!is_directory (u)) u= head (u);
  string old= get_preference ("imported fonts", "");
  if (old == "") set_preference ("imported fonts", as_unix_string (u));
  else {
    url dirs= add_to_path (url_unix (old), u);
    set_preference ("imported fonts", as_unix_string (dirs));
  }
}

url
tt_font_search_path () {
  // NOTICE: the order of the search path here is very important
  // Please use ret= ret | u to append the url, the former appended url will
  // precede the latter appended url
  url    ret= url_none ();
  string xtt= get_env ("TEXMACS_FONT_PATH");
  if (!is_empty (xtt)) {
    ret= ret | url (xtt);
  }
  string ximp= get_preference ("imported fonts", "");
  if (!is_empty (ximp)) {
    ret= ret | url_system (ximp);
  }
  ret= ret | url ("$TEXMACS_HOME_PATH/fonts/truetype") |
       url ("$TEXMACS_PATH/fonts/truetype");
  if (os_win () || os_mingw ()) {
    ret= ret | url ("$windir/Fonts");
  }
  else if (os_macos ()) {
    ret= ret | url ("$HOME/Library/Fonts") | url ("/Library/Fonts") |
         url ("/Library/Application Support/Apple/Fonts/iLife") |
         url ("/Library/Application Support/Apple/Fonts/iWork") |
         url ("/System/Library/Fonts") |
         url ("/System/Library/PrivateFrameworks/FontServices.framework/"
              "Versions/A/Resources/Fonts/ApplicationSupport") |
         url ("/opt/local/share/texmf-texlive/fonts/opentype") |
         url ("/opt/local/share/texmf-texlive/fonts/truetype") |
         url ("/opt/local/share/texmf-texlive-dist/fonts/opentype") |
         url ("/opt/local/share/texmf-texlive-dist/fonts/truetype") |
         url ("/usr/local/texlive/2020/texmf-dist/fonts/opentype") |
         url ("/usr/local/texlive/2020/texmf-dist/fonts/truetype") |
         url ("/usr/local/texlive/2021/texmf-dist/fonts/opentype") |
         url ("/usr/local/texlive/2021/texmf-dist/fonts/truetype") |
         url ("/usr/local/texlive/2022/texmf-dist/fonts/opentype") |
         url ("/usr/local/texlive/2022/texmf-dist/fonts/truetype");
  }
  else {
#ifdef _FONTCONFIG_H_
    FcConfig*  config  = FcInitLoadConfig ();
    FcStrList* fontdirs= FcConfigGetFontDirs (config);
    FcChar8*   fontdir = NULL;

    FcStrListFirst (fontdirs);
    while (fontdir= FcStrListNext (fontdirs)) {
      ret= ret | url_system ((char*) fontdir);
    }
#endif
    ret= ret | url ("/usr/share/texlive/texmf-dist/fonts/opentype") |
         url ("/usr/share/texlive/texmf-dist/fonts/truetype");
  }
  return ret;
}

url
tt_font_path () {
  bench_start ("tt_font_path");
  url xu= search_sub_dirs (tt_font_search_path ());
  bench_end ("tt_font_path");
  return xu;
}

static void
tt_locate_all () {
  url suffixes = url ("*.ttf") | url ("*.ttc") | url ("*.otf");
  url all_fonts= expand (complete (tt_font_path () * suffixes));
  url iter     = all_fonts;
  while (is_or (iter)) {
    url font_u= iter[1];
    // eg. (basename.ttf, url(/path/to/basename.ttf))
    tt_font_locations (as_string (tail (font_u)))= font_u;
    tt_fonts->insert (basename (font_u));
    iter= iter[2];
  }
  url font_last                                   = iter;
  tt_font_locations (as_string (tail (font_last)))= font_last;
  tt_fonts->insert (basename (font_last));
}

array<url>
tt_font_paths () {
  int num_of_fonts= N (tt_font_locations);
  if (num_of_fonts == 0) {
    tt_locate_all ();
  }
  array<url>       ret;
  iterator<string> it= iterate (tt_font_locations);
  while (it->busy ()) {
    string key= it->next ();
    ret << tt_font_locations[key];
  }
  return ret;
}

static url
tt_locate_pfb (string name) {
  bench_start ("tt_locate_pfb " * name);
  /*
  if (starts (name, "rpag")) name= "uag" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rpbk")) name= "ubk" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rpcr")) name= "ucr" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rphv")) name= "uhv" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rpnc")) name= "unc" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rppl")) name= "upl" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rpsy")) name= "usy" * name (4, N (name));
  if (starts (name, "rptm")) name= "utm" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rpzc")) name= "uzc" * name (4, N (name) - 4) * "8a.pfb";
  if (starts (name, "rpzd")) name= "uzd" * name (4, N (name));
  */
  url u= resolve_tex (name * ".pfb");
  bench_end ("tt_locate_pfb " * name, 10);
  return u;
}

static url
tt_fast_locate (string name) {
  array<string> suffixes;
  suffixes << string ("ttf") << string ("ttc") << string ("otf");
  for (int i= 0; i < N (suffixes); i++) {
    if (tt_font_locations->contains (name * "." * suffixes[i])) {
      return tt_font_locations[name * "." * suffixes[i]];
    }
  }
  return url_none ();
}

url
tt_font_find_sub (string name) {
  url u= tt_unpack (name);
  if (!is_none (u)) return u;

  if (font_database_exists (name)) {
    // Init the fonts location at startup
    int num_of_fonts= N (tt_font_locations);
    if (num_of_fonts == 0) tt_locate_all ();

    return tt_fast_locate (name);
  }
  else {
    debug_fonts << "Font " << name << " does not exist in font database" << LF;
    debug_fonts << "Please report it to us if it is a ttf/ttc/otf font!" << LF;
    debug_fonts << "Locating it as " * name * ".pfb, might be slow :(" << LF;
    // If the font is not in font database
    // It must not be a ttf/ttc/otf/tfm font
    return tt_locate_pfb (name);
  }
}

url
tt_font_find (string name) {
  string s= "ttf:" * name;
  if (is_cached ("font_cache.scm", s)) {
    string r= cache_get ("font_cache.scm", s)->label;
    if (r == "") return url_none ();
    url u= url_system (r);
    if (exists (u)) return u;
    cache_reset ("font_cache.scm", s);
  }

  url r= tt_font_find_sub (name);
  if (is_none (r)) cache_set ("font_cache.scm", s, "");
  else cache_set ("font_cache.scm", s, as_string (r));
  return r;
}

bool
tt_font_exists (string name) {
  // cout << "tt_font_exists? " << name << "\n";
  if (tt_fonts->contains (name)) return true;
  bench_start ("tt_font_exists " * name);
  bool yes= !is_none (tt_font_find (name));
  if (yes) tt_fonts->insert (name);
  bench_end ("tt_font_exists " * name, 10);
  return yes;
}

string
tt_find_name_sub (string name, int size) {
  if (size == 0) {
    if (tt_font_exists (name)) return name;
    else return "";
  }
  if (tt_font_exists (name * as_string (size))) return name * as_string (size);
  if (size > 333) size= (size + 50) / 100;
  if (tt_font_exists (name * as_string (size))) return name * as_string (size);

  if ((size >= 15) && tt_font_exists (name * "17")) return name * "17";
  if ((size > 12) && tt_font_exists (name * "12")) return name * "12";
  if ((size < 5) && tt_font_exists (name * "5")) return name * "5";
  if ((size < 6) && tt_font_exists (name * "6")) return name * "6";
  if ((size < 7) && tt_font_exists (name * "7")) return name * "7";
  if ((size < 8) && tt_font_exists (name * "8")) return name * "8";
  if ((size < 9) && tt_font_exists (name * "9")) return name * "9";
  if ((size < 9) && tt_font_exists (name * "7")) return name * "7";
  if (tt_font_exists (name * "10")) return name * "10";
  if ((size < 9) && tt_font_exists (name * "700")) return name * "700";
  if ((size >= 15) && tt_font_exists (name * "1700")) return name * "1700";
  if (tt_font_exists (name * "1000")) return name * "1000";
  if (tt_font_exists (name)) return name;
  return "";
}

string
tt_find_name (string name, int size) {
  string s= "tt:" * name * as_string (size);
  if (is_cached ("font_cache.scm", s)) {
    string r= cache_get ("font_cache.scm", s)->label;
    if (tt_font_exists (r)) return r;
    cache_reset ("font_cache.scm", s);
  }

  bench_start ("tt find name");
  string r= tt_find_name_sub (name, size);
  // cout << name << size << " -> " << r << "\n";
  bench_cumul ("tt find name");

  if (r != "") cache_set ("font_cache.scm", s, r);
  return r;
}
