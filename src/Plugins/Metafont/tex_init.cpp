
/******************************************************************************
 * MODULE     : tex_init.cpp
 * DESCRIPTION: initializations for using Metafont
 * COPYRIGHT  : (C) 1999  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "convert.hpp"
#include "file.hpp"
#include "path.hpp"
#include "preferences.hpp"
#include "sys_utils.hpp"
#include "tex_files.hpp"
#include "tm_file.hpp"

/******************************************************************************
 * Determine installed programs
 ******************************************************************************/

static void
init_helper_binaries () {
  if (exists_in_path ("kpsepath")) {
    debug_boot << "kpsepath works with your TeX distribution\n";
    set_user_preference ("texlive:kpsepath", "true");
  }
  else {
    set_user_preference ("texlive:kpsepath", "false");
  }

  if (exists_in_path ("kpsewhich")) {
    debug_boot << "kpsewhich works with your TeX distribution\n";
    set_user_preference ("texlive:kpsewhich", "true");
  }
  else {
    set_user_preference ("texlive:kpsewhich", "false");
  }

  if (exists_in_path ("mktextfm")) {
    debug_boot << "mktextfm works with your TeX distribution\n";
    set_user_preference ("texlive.maketfm", "mktextfm");
  }
  else if (exists_in_path ("MakeTeXTFM")) {
    debug_boot << "MakeTeXTFM works with your TeX distribution\n";
    set_user_preference ("texlive.maketfm", "MakeTeXTFM");
  }
  else if (exists_in_path ("maketfm")) {
    debug_boot << "maketfm works with your TeX distribution\n";
    set_user_preference ("texlive.maketfm", "maketfm");
  }
  else set_user_preference ("texlive.maketfm", "false");

  if (exists_in_path ("mktexpk")) {
    debug_boot << "mktexpk works with your TeX distribution\n";
    set_user_preference ("texlive.mktexpk", "mktexpk");
  }
  else if (exists_in_path ("MakeTeXPK")) {
    debug_boot << "MakeTeXPK works with your TeX distribution\n";
    set_user_preference ("texlive.mktexpk", "MakeTeXPK");
  }
  else if (exists_in_path ("makepk")) {
    debug_boot << "makepk works with your TeX distribution\n";
    set_user_preference ("texlive.mktexpk", "makepk");
  }
  else set_user_preference ("texlive.mktexpk", "false");

  if (exists_in_path ("texhash")) {
    debug_boot << "texhash works with your TeX distribution\n";
    set_user_preference ("texlive.texhash", "true");
  }
  else set_user_preference ("texlive.texhash", "false");
}

/******************************************************************************
 * Heuristic determination of path with TeX files
 ******************************************************************************/

static void
search_sub_dirs_sub (url base, url u, url& tfm, url& pk, url& pfb, int status) {
  if (is_concat (u)) {
    if (u[1] == "tfm") status= 1;
    if (u[1] == "pk") status= 2;
    if (u[1] == "pfb" || u[1] == "type1") status= 3;
    search_sub_dirs_sub (base * u[1], u[2], tfm, pk, pfb, status);
  }
  if (is_or (u)) {
    search_sub_dirs_sub (base, u[2], tfm, pk, pfb, status);
    search_sub_dirs_sub (base, u[1], tfm, pk, pfb, status);
  }
  if ((status == 1) || (u == "tfm")) tfm= (base * u) | tfm;
  if ((status == 2) || (u == "pk")) pk= (base * u) | pk;
  if ((status == 3) || (u == "pfb") || (u == "pfb")) pfb= (base * u) | pfb;
}

static void
search_sub_dirs (url root, url& tfm, url& pk, url& pfb) {
  url dirs= search_sub_dirs (root);
  if (!is_none (dirs)) {
    debug_boot << "Found TeX directory " << root << "\n";
    search_sub_dirs_sub (url_here (), dirs, tfm, pk, pfb, 0);
  }
}

static void
init_heuristic_tex_paths () {
  url tfm= url_none (), pk= url_none (), pfb= url_none ();

  // Try some 'standard' directories
#ifdef OS_WIN
  tfm= search_sub_dirs ("$TEX_HOME/fonts/tfm");
  pk = search_sub_dirs ("$TEX_HOME/fonts/pk");
  pfb= search_sub_dirs ("$TEX_HOME/fonts/type1");
#else
  search_sub_dirs ("/opt/local/share/texmf-texlive-dist/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/lib/tetex/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/lib/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/local/lib/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/share/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/TeX/lib/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/var/texfonts", tfm, pk, pfb);
  search_sub_dirs ("/var/tmp/texfonts", tfm, pk, pfb);
#endif

#ifdef OS_WIN
  set_user_preference ("texlive.tfm", as_string (tfm));
  set_user_preference ("texlive.pk", as_string (pk));
  set_user_preference ("texlive.pfb", as_string (pfb));
#else
  set_user_preference ("texlive.tfm", as_string (expand (factor (tfm))));
  set_user_preference ("texlive.pk", as_string (expand (factor (pk))));
  set_user_preference ("texlive.pfb", as_string (expand (factor (pfb))));
#endif
}

/******************************************************************************
 * Setting up and initializing TeX fonts
 ******************************************************************************/

void
setup_tex () {
  remove ("$TEXMACS_HOME_PATH/fonts/font-index.scm");
  if (get_user_preference ("texlive:fonts") == "on") {
    init_helper_binaries ();
    init_heuristic_tex_paths ();
  }
}

void
init_tex () {
  reset_tfm_path (false);
  reset_pk_path (false);
  reset_pfb_path ();
}
