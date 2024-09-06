/** \file url.cpp
 *  \copyright GPLv3
 *  \details Unitests for url.
 *  \author Darcy
 *  \date   2019-2023
 */
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest/doctest.h"
#include "url.hpp"

url ustc_edu   = url_system ("https://ustc.edu.cn");
url texmacs_org= url_system ("http://texmacs.org");
url none_url   = url_none ();
url file_root  = url_root ("file");
url ftp_root   = url_root ("ftp");
url wsl_ubuntu = url_system ("\\\\wsl.localhost\\Ubuntu");

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
}

TEST_CASE ("suffix") {
  // empty suffix should work
  url no_suffix= url ("/a/b/c/d/no_suffix");
  CHECK (is_empty (suffix (no_suffix)));
  url no_suffix2= url ("/a/b.c/d/no_suffix");
  CHECK (is_empty (suffix (no_suffix2)));

  // normal suffix should work
  url png= url ("/a/b/c/d.png");
  CHECK_EQ (suffix (png) == string ("png"), true);
  url png2= url ("/a/b.c/d.png");
  CHECK_EQ (suffix (png2) == string ("png"), true);
}

TEST_CASE ("get_root") {
  CHECK_EQ (get_root (ustc_edu) == "https", true);
  CHECK_EQ (get_root (texmacs_org) == "http", true);
  CHECK_EQ (get_root (file_root) == "file", true);
  CHECK_EQ (get_root (ftp_root) == "ftp", true);
#if defined(OS_MINGW) || defined(OS_WIN)
  CHECK_EQ (get_root (wsl_ubuntu) == "default", true);
#endif
  CHECK_EQ (get_root (none_url) == "", true);
  CHECK_EQ (get_root (file_root | texmacs_org) == "", true);
}
