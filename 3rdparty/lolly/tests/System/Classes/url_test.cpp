/** \file url.cpp
 *  \copyright GPLv3
 *  \details Unitests for url.
 *  \author Darcy
 *  \date   2019-2023
 */
#include "a_tbox_main.cpp"
#include "url.hpp"

url ustc_edu     = url_system ("https://ustc.edu.cn");
url texmacs_org  = url_system ("http://texmacs.org");
url none_url     = url_none ();
url file_root    = url_root ("file");
url ftp_root     = url_root ("ftp");
url wsl_ubuntu   = url_system ("\\\\wsl.localhost\\Ubuntu");
url unix_root    = url_system ("/");
url unix_root_txt= url_system ("/abc.txt");
url unix_tmp     = url_system ("/tmp");
url unix_tmp_a   = url_system ("/tmp/a");
url abc_url      = url_system ("abc");

TEST_CASE ("label of url") {
  string_eq (ustc_edu.label (), "concat");
  string_eq (texmacs_org.label (), "concat");
  string_eq (none_url.label (), "none");
  string_eq (file_root.label (), "root");
  string_eq (ftp_root.label (), "root");
#if defined(OS_MINGW) || defined(OS_WIN)
  SUBCASE ("on host windows") { string_eq (wsl_ubuntu.label (), "concat"); }
#endif
#if defined(OS_MACOS) || defined(OS_LINUX) || defined(OS_WASM)
  SUBCASE ("on host macos/linux/wasm") {
    string_eq (unix_root.label (), "root");
    string_eq (unix_root_txt.label (), "concat");
    string_eq (unix_tmp.label (), "concat");
    string_eq (unix_tmp_a.label (), "concat");
  }
#endif
  string_eq (abc_url.label (), "");
}

TEST_CASE ("protocol of url") {
  string_eq (ustc_edu.protocol (), "https");
  string_eq (texmacs_org.protocol (), "http");
  string_eq (none_url.protocol (), "default");
  string_eq (file_root.protocol (), "file");
  string_eq (ftp_root.protocol (), "ftp");
  string_eq (wsl_ubuntu.protocol (), "default");
  string_eq (unix_root.protocol (), "default");
  string_eq (unix_root_txt.protocol (), "default");
  string_eq (unix_tmp.protocol (), "default");
  string_eq (unix_tmp_a.protocol (), "default");
  string_eq (abc_url.protocol (), "default");

  string_eq (get_root (ustc_edu), "https");
  string_eq (get_root (texmacs_org), "http");
  string_eq (get_root (file_root), "file");
  string_eq (get_root (ftp_root), "ftp");
#if defined(OS_MINGW) || defined(OS_WIN)
  string_eq (get_root (wsl_ubuntu), "default");
#endif
  string_eq (get_root (none_url), "default");
  string_eq (get_root (file_root | texmacs_org), "");
}

TEST_CASE ("url with env") {
#if defined(OS_MINGW) || defined(OS_WIN)
  SUBCASE ("on windows") {
    set_env ("TEST_ENV", "System;Plugins;Kernel");
    url u= url ("$TEST_ENV");
    CHECK (is_or (u));
  }
#endif
#if defined(OS_MACOS) || defined(OS_LINUX) || defined(OS_WASM)
  SUBCASE ("on macos/linux/wasm") {
    set_env ("TEST_ENV", "System:Plugins:Kernel");
    url u= url ("$TEST_ENV");
    CHECK (is_or (u));
  }
#endif
}

TEST_CASE ("is here/parent/ancestor") { CHECK (is_here (url_here ())); }

TEST_CASE ("is_none") {
  CHECK (!is_none (ustc_edu));
  CHECK (!is_none (texmacs_org));
  CHECK (!is_none (wsl_ubuntu));
  CHECK (!is_none (file_root));
  CHECK (is_none (none_url));
}

TEST_CASE ("is_root") {
  CHECK (!is_root (ustc_edu));
  CHECK (!is_root (texmacs_org));
  CHECK (!is_root (wsl_ubuntu));
  CHECK (is_root (file_root));
  CHECK (!is_root (none_url));
#if defined(OS_MACOS) || defined(OS_LINUX)
  CHECK (is_root (unix_root));
#endif
  CHECK (!is_root (abc_url));
}

TEST_CASE ("is_concat") {
  CHECK (!is_concat (abc_url));
  CHECK (!is_concat (none_url));
}

#if defined(OS_MACOS) || defined(OS_LINUX) || defined(OS_WASM)
TEST_CASE ("is_concat on unix and wasm") {
  CHECK (!is_concat (unix_root));
  CHECK (is_concat (unix_tmp));
  CHECK (is_concat (unix_tmp_a));
}
#endif

TEST_CASE ("suffix") {
  // empty suffix should work
  url no_suffix= url ("/a/b/c/d/no_suffix");
  CHECK (is_empty (suffix (no_suffix)));
  url no_suffix2= url ("/a/b.c/d/no_suffix");
  CHECK (is_empty (suffix (no_suffix2)));

  // normal suffix should work
  url png= url ("/a/b/c/d.png");
  string_eq (suffix (png), "png");
  url png2= url ("/a/b.c/d.png");
  string_eq (suffix (png2), "png");
}

TEST_CASE ("as_string") {
  url dirs= url ("Data") | url ("Kernel") | url ("Plugins");
  string_eq (as_string (dirs), string ("Data") * URL_SEPARATOR * "Kernel" *
                                   URL_SEPARATOR * "Plugins");
#if defined(OS_MINGW) || defined(OS_WIN)
  SUBCASE ("on host windows") {
    url app_mogan= url_system ("$ProgramFiles\\Mogan");
    string_eq (as_string (app_mogan), string ("C:\\Program Files\\Mogan"));
  }
#endif
}
