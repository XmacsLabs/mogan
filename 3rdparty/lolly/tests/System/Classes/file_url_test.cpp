/** \file url.cpp
 *  \copyright GPLv3
 *  \details Unitests for url.
 *  \author Darcy
 *  \date   2019-2023
 */
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest/doctest.h"
#include "url.hpp"

#if defined(OS_MINGW) || defined(OS_WIN)
url win_c_windows= url_system ("C:/Windows");
url win_c        = url_system ("C:/");
url system_root  = url_system ("%SystemRoot%");
url windir       = url_system ("%windir%");

TEST_CASE ("as_string on windows") {
  CHECK_EQ (as_string (win_c) == string ("C:\\"), true);
  CHECK_EQ (as_string (win_c_windows) == string ("C:\\Windows"), true);
  CHECK_EQ (as_string (system_root) == string ("%SystemRoot%"), true);
  CHECK_EQ (as_string (windir) == string ("%windir%"), true);
}
#else
url unix_root= url_system ("/");
url unix_tmp = url_system ("/tmp");

TEST_CASE ("as_string on nix") {
  CHECK_EQ (as_string (unix_root) == "/", true);
  CHECK_EQ (as_string (unix_tmp) == "/tmp", true);
}
#endif
