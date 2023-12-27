
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

// Caching for Font in tuple (filename, path)
// eg. (basename.ttf, url(/path/to/basename.ttf))
static hashmap<string, url> tt_font_locations;
// A hashset of all basename of the fonts
static hashset<string> tt_fonts;

static void
tt_locate_update_cache (url font_u, bool must_in_db= true) {
  string name     = as_string (tail (font_u));
  string base_name= basename (font_u);

  if (must_in_db && !font_database_exists (base_name)) {
    // the font basename does not exist in font database
    return;
  }
  array<string> suffixes= font_database_suffixes (base_name);
  if (must_in_db && !contains (suffix (font_u), suffixes)) {
    // the font basename does exist in the font database
    // but the font suffix does not exist in the font database
    return;
  }

  tt_font_locations (name)= font_u;
  tt_fonts->insert (base_name);
  cache_set ("font_cache.scm", "ttf:" * base_name, as_string (font_u));
}

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
  if (!is_directory (u)) {
    // Sync font info to tt_fonts and tt_font_location
    tt_locate_update_cache (u, false);

    // Only reserve the directory part
    u= head (u);
  }
  // Append the directory to the "imported fonts" preference
  string old= get_preference ("imported fonts", "");
  if (is_empty (old)) {
    set_preference ("imported fonts", as_string (u));
  }
  else {
    url dirs= add_to_path (url_system (old), u);
    set_preference ("imported fonts", as_string (dirs));
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
              "Versions/A/Resources/Fonts") |
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
    if (exists (url_system ("/usr/share/fonts/fonts-cesi"))) {
      ret= ret | url_system ("/usr/share/fonts/fonts-cesi");
    }
  }
  return ret;
}

url
tt_font_path () {
  url xu= search_sub_dirs (tt_font_search_path ());
  return xu;
}

static void
tt_locate_all () {
  if (N (tt_font_locations) > 0) return;

  bench_start ("tt_locate_all");
  url suffixes= url ("*.ttf") | url ("*.ttc") | url ("*.otf") | url ("*.TTF") |
                url ("*.TTC") | url ("*.OTF");
  url all_fonts= expand (complete (tt_font_path () * suffixes));
  url iter     = all_fonts;
  while (is_or (iter)) {
    url font_u= iter[1];
    tt_locate_update_cache (font_u);
    iter= iter[2];
  }
  url font_last= iter;
  tt_locate_update_cache (font_last);
  cache_memorize ();
  bench_end ("tt_locate_all");
}

array<url>
tt_font_paths () {
  tt_locate_all ();

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
  suffixes << string ("ttf") << string ("ttc") << string ("otf")
           << string ("TTF") << string ("TTC") << string ("OTF");
  for (int i= 0; i < N (suffixes); i++) {
    if (tt_font_locations->contains (name * "." * suffixes[i])) {
      return tt_font_locations[name * "." * suffixes[i]];
    }
  }
  return url_none ();
}

url
tt_font_find_sub (string font_basename) {
  if (font_database_exists (font_basename)) {
    return tt_fast_locate (font_basename);
  }
  else {
    debug_fonts << "Font " << font_basename
                << " does not exist in font database" << LF;
    debug_fonts << "Please report it to us if it is a ttf/ttc/otf font!" << LF;
    debug_fonts << "Locating it as " * font_basename * ".pfb, might be slow :("
                << LF;
    // If the font is not in font database
    // It must not be a ttf/ttc/otf/tfm font
    return tt_locate_pfb (font_basename);
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
    // if the font does not exist, clear the cache in `font_cache.scm`
    cache_reset ("font_cache.scm", s);
  }

  tt_locate_all ();

  url r= tt_font_find_sub (name);
  if (is_none (r)) cache_set ("font_cache.scm", s, "");
  else cache_set ("font_cache.scm", s, as_string (r));
  return r;
}

bool
tt_font_exists (string name) {
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
    // if the font does not exist, clear the cache in `font_cache.scm`
    cache_reset ("font_cache.scm", s);
  }

  bench_start ("tt find name");
  string r= tt_find_name_sub (name, size);
  // cout << name << size << " -> " << r << "\n";
  bench_cumul ("tt find name");

  if (r != "") cache_set ("font_cache.scm", s, r);
  return r;
}
