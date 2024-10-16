#include "a_tbox_main.cpp"
#include "sys_utils.hpp"

TEST_MEMORY_LEAK_INIT

TEST_CASE ("get_process_id") { CHECK (get_process_id () >= 1); }

TEST_CASE ("system with output") {
  string result;
  CHECK (N (result) == 0);
  if (!os_wasm () && !os_mingw ()) {
    lolly::system ("xmake --version", result);
    CHECK (N (result) > 0);
  }
}

TEST_CASE ("system") {
#ifdef OS_WASM
  CHECK_EQ (lolly::system ("xmake --version"), -1);
  CHECK_EQ (lolly::system ("no_such_command"), -1);
  CHECK_EQ (lolly::system (""), -1);
#else
#ifndef OS_MINGW
  CHECK (lolly::system ("xmake --version") == 0);
#endif
  CHECK (lolly::system ("no_such_command") != 0);
  CHECK (lolly::system ("") != 0);
#endif
}

TEST_CASE ("get_env/set_env") {
#if defined(OS_MINGW) || defined(OS_WIN)
  CHECK_EQ (get_env ("ProgramFiles") == "C:\\Program Files", true);
  set_env ("LOLLY_PATH", "C:\\lolly");
  CHECK_EQ (get_env ("LOLLY_PATH") == "C:\\lolly", true);
#endif

  set_env ("SHELL", "/bin/zsh");
  CHECK_EQ (get_env ("SHELL") == "/bin/zsh", true);
}

TEST_CASE ("get_user_name") {
  CHECK (N (get_user_name ()) >= 0);
  // cout << get_user_name () << LF;
}

TEST_MEMORY_LEAK_ALL
