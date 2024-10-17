/** \file file.cpp
 *  \copyright GPLv3
 *  \details Unitests for file.
 *  \author Darcy
 *  \date   2019-2023
 */
#include "a_tbox_main.cpp"
#include "file.hpp"
#include "tbox/tbox.h"

url unix_root= url_system ("/");
url xmake_lua= url_pwd () * "xmake.lua";

url
get_lolly_tmp () {
#if defined(OS_WIN) || defined(OS_MINGW)
  return url_system ("$TMP") * url (".lolly");
#elif defined(OS_MACOS)
  return url_system ("/private/tmp") * url (".lolly");
#else
  return url_system ("/tmp") * url (".lolly");
#endif
}

void
remove_if_exist (const url& u1) {
  c_string       path1 (as_string (u1));
  tb_file_info_t info;
  if (tb_file_info (path1, &info)) {
    tb_file_remove (path1);
  };
}

TEST_MEMORY_LEAK_INIT

#if defined(OS_WIN) || defined(OS_MINGW)
TEST_CASE ("is_directory on Windows") {
  CHECK (is_directory (url_system ("C:/Windows")));
}
#endif

TEST_CASE ("is_directory/is_regular") {
#if defined(OS_LINUX) || defined(OS_MACOS) || defined(OS_WASM)
  CHECK (is_directory (unix_root));
#endif
  CHECK (is_directory (url_pwd () * "tests"));

  CHECK (is_directory (url_pwd ()));
  CHECK (!is_regular (url_pwd ()));
  CHECK (!is_symbolic_link (url_pwd ()));

  CHECK (!is_directory (xmake_lua));
  CHECK (is_regular (xmake_lua));
  CHECK (!is_symbolic_link (xmake_lua));
}

TEST_CASE ("is_newer") {
  url lolly_tmp= get_lolly_tmp ();
  mkdir (lolly_tmp);
  url old_dir= lolly_tmp * url ("old");
  url new_dir= lolly_tmp * url ("new");
  mkdir (old_dir);
  tb_sleep (1);
  mkdir (new_dir);
  CHECK (is_newer (new_dir, old_dir));
  rmdir (old_dir | new_dir);
}

TEST_CASE ("is_of_type") {
  CHECK (is_of_type (url_pwd (), "d"));
  CHECK (!is_of_type (url_pwd (), "f"));
  CHECK (is_of_type (xmake_lua, "fr"));
#if defined(OS_MINGW) || defined(OS_WIN)
  CHECK (is_of_type (url_pwd () * url ("bin/format.bat"), "x"));
#endif
}

TEST_CASE ("file_size") { CHECK (file_size (xmake_lua) > 0); }

TEST_CASE ("last_modified") { CHECK (last_modified (xmake_lua) > 0); }

TEST_CASE ("mkdir/rmdir") {
  url lolly_tmp = get_lolly_tmp ();
  url test_mkdir= lolly_tmp * url ("tmp_dir");
  mkdir (test_mkdir);
  CHECK (is_directory (test_mkdir));
  rmdir (test_mkdir);
  CHECK (!is_directory (test_mkdir));
}

TEST_CASE ("chdir") {
  url lolly_tmp= get_lolly_tmp ();
  url old      = url_pwd ();

  SUBCASE ("tmp directory") {
    url test_mkdir= lolly_tmp * url ("ch_dir");

    mkdir (test_mkdir);
    CHECK (is_directory (test_mkdir));

    chdir (test_mkdir);
    url cur= url_pwd ();
    CHECK (cur == test_mkdir);

    // restore the test dir
    chdir (old);
  }

  SUBCASE ("Root directory") {
#if defined(OS_MINGW) || defined(OS_WIN)
    url test_mkdir= url_system ("C:\\");
#else
    url test_mkdir= url_system ("/");
#endif

    chdir (test_mkdir);
    url cur= url_pwd ();
    CHECK (cur == test_mkdir);

    // restore the test dir
    chdir (old);
  }

  SUBCASE ("chinese") {
    url test_mkdir= lolly_tmp * url ("中文");

    mkdir (test_mkdir);
    CHECK (is_directory (test_mkdir));

    chdir (test_mkdir);
    url cur= url_pwd ();
    CHECK (cur == test_mkdir);

    // restore the test dir
    chdir (old);
  }

  SUBCASE ("dir with Space") {
    url test_mkdir= lolly_tmp * url (" spa ce");

    mkdir (test_mkdir);
    CHECK (is_directory (test_mkdir));

    chdir (test_mkdir);
    url cur= url_pwd ();
    CHECK (cur == test_mkdir);

    // restore the test dir
    chdir (old);
  }
}

TEST_CASE ("remove") {
  url       lolly_tmp= get_lolly_tmp ();
  tb_hong_t time     = tb_time ();

  SUBCASE ("single file") {
    url xyz_txt= lolly_tmp * url ("xyz.txt");
    tb_file_touch (c_string (as_string (xyz_txt)), time, time);
    CHECK (file_size (xyz_txt) == 0);
    remove (xyz_txt);
    CHECK (file_size (xyz_txt) == -1);
  }

  SUBCASE ("multiple files") {
    url xyz1_txt= lolly_tmp * url ("xyz1.txt");
    url xyz2_txt= lolly_tmp * url ("xyz2.txt");
    url xyz3_txt= lolly_tmp * url ("xyz3.txt");
    tb_file_touch (c_string (as_string (xyz1_txt)), time, time);
    tb_file_touch (c_string (as_string (xyz2_txt)), time, time);
    tb_file_touch (c_string (as_string (xyz3_txt)), time, time);
    CHECK (file_size (xyz1_txt) == 0);
    CHECK (file_size (xyz2_txt) == 0);
    CHECK (file_size (xyz3_txt) == 0);
    remove (xyz1_txt | xyz2_txt | xyz3_txt);
    CHECK (file_size (xyz1_txt) == -1);
    CHECK (file_size (xyz2_txt) == -1);
    CHECK (file_size (xyz3_txt) == -1);
  }
}

TEST_CASE ("move") {
  url       lolly_tmp= get_lolly_tmp ();
  tb_hong_t time     = tb_time ();
  url       m1       = lolly_tmp * url ("move_1.txt");
  url       m2       = lolly_tmp * url ("move_2.txt");
  tb_file_touch (c_string (as_string (m1)), time, time);
  move (m1, m2);
  CHECK (file_size (m1) == -1);
  CHECK (file_size (m2) == 0);
  remove (m2);
}

TEST_CASE ("copy") {
  url       lolly_tmp= get_lolly_tmp ();
  tb_hong_t time     = tb_time ();
  url       c1       = lolly_tmp * url ("copy_1.txt");
  url       c2       = lolly_tmp * url ("copy_2.txt");
  tb_file_touch (c_string (as_string (c1)), time, time);
  CHECK (file_size (c2) == -1);
  copy (c1, c2);
  CHECK (file_size (c1) == 0);
  CHECK (file_size (c2) == 0);
  remove (c1);
  remove (c2);
}

TEST_MEMORY_LEAK_ALL

TEST_CASE ("url_temp") {
  url       tmp1= url_temp ("png");
  tb_hong_t time= tb_time ();
  tb_file_touch (c_string (as_string (tmp1)), time, time);
  url tmp2= url_temp ("png");
  CHECK_EQ (tmp1 == tmp2, false);
  CHECK (is_directory (url_temp_dir ()));
}
// because url_temp_dir will initialize static variable, occupied memory will
// increase
TEST_MEMORY_LEAK_RESET

TEST_CASE ("read_directory") {
  bool flag1= false;
  CHECK (N (read_directory (url_pwd () * "tests", flag1)) > 0);
  CHECK (!flag1); // no error
  bool flag2= false;
  CHECK (N (read_directory (url_system ("no_such_dir"), flag2)) == 0);
  CHECK (flag2); // error
}

TEST_CASE ("load_string from empty file") {
  url    lolly_tmp= get_lolly_tmp ();
  url    u1       = lolly_tmp * url ("load_empty.txt");
  string s1;
  tb_file_create (c_string (as_string (u1)));
  CHECK (!load_string (u1, s1, false));
  CHECK_EQ (s1 == string (""), true);
}

TEST_CASE ("load_string from newly created file") {
  url      lolly_tmp= get_lolly_tmp ();
  url      u1       = lolly_tmp * url ("load_string_1.txt");
  c_string s1 (as_string (u1));
  // can access file?
  if (tb_file_access (s1, TB_FILE_MODE_RO)) {
    while (!tb_file_remove (s1)) {
      tb_sleep (1);
    }
  }
  // create test file
  if (tb_file_create (s1)) {
    const char* s2    = "hello world";
    tb_size_t   size  = strlen (s2);
    tb_byte_t*  buffer= (tb_byte_t*) tb_malloc_bytes (size);
    int         seek  = 0;
    while (seek < size) {
      tb_byte_t c = s2[seek];
      buffer[seek]= c;
      seek++;
    }

    auto file_ref= tb_file_init (s1, TB_FILE_MODE_RW);
    tb_file_writ (file_ref, buffer, strlen (s2));
    string s;
    CHECK (!load_string (u1, s, false));
    string_eq (s, string ("hello world"));
  }
}

TEST_MEMORY_LEAK_ALL

TEST_CASE ("load_string from 3 local files and check exception") {
  url    lolly_tmp= get_lolly_tmp ();
  url    u1       = url_pwd () * url ("tests/System/Files/sample_file.txt");
  url    u2= url_pwd () * url ("tests/System/Files/sample_file_copy.txt");
  url    u3= url_pwd () * url ("tests/System/Files/sample_file_throw.txt");
  string s1, s2, s3;
  CHECK (!load_string (u1, s1, false));
  CHECK (!load_string (u2, s2, false));
  string_eq (s1, s2);

  CHECK_THROWS (load_string (u3, s3, true));
}
// because exception throw, some object will not be released.
TEST_MEMORY_LEAK_RESET

TEST_CASE ("load_string from url with :") {
  url    u ("tests/Kernel/Containers:tests/Kernel/Types", "list_test.cpp");
  string s;
  load_string (u, s, false);
  CHECK (N (s) > 0);
}

#if defined(OS_MINGW) || defined(OS_WIN)
TEST_CASE ("load_string from read only file") {
  url    hosts= url_system ("C:\\Windows\\System32\\drivers\\etc\\hosts");
  string s;
  load_string (hosts, s, false);
  CHECK (N (s) > 0);
}
#endif

#if defined(OS_LINUX) || defined(OS_MACOS)
TEST_CASE ("load_string from read only file") {
  url    hosts= url_system ("/etc/hosts");
  string s;
  load_string (hosts, s, false);
  CHECK (N (s) > 0);
}
#endif

TEST_CASE ("save to empty file") {
  url    lolly_tmp= get_lolly_tmp ();
  url    u1       = lolly_tmp * url ("save_empty.txt");
  string s1 ("test");
  tb_file_touch (c_string (as_string (u1)), 0, 0);
  CHECK (!save_string (u1, s1, false));
  string s2;
  CHECK (!load_string (u1, s2, false));
  string_eq (s1, s2);
}

TEST_CASE ("save to empty file with unicode filename") {
  url    lolly_tmp= get_lolly_tmp ();
  url    u1       = lolly_tmp * url ("保存到空文件.txt");
  string s1 ("测试内容");
  tb_file_touch (c_string (as_string (u1)), 0, 0);
  CHECK (!save_string (u1, s1, false));
  string s2;
  CHECK (!load_string (u1, s2, false));
  string_eq (s1, s2);
}

TEST_CASE ("create and save to file") {
  url lolly_tmp= get_lolly_tmp ();
  url u1       = lolly_tmp * url ("save_nonexist.txt");
  remove_if_exist (u1);
  string s1 ("test");
  CHECK (!save_string (u1, s1, true));
  string s2;
  CHECK (!load_string (u1, s2, true));
  string_eq (s1, s2);
}

TEST_CASE ("save to exist file") {
  url      lolly_tmp= get_lolly_tmp ();
  url      u1       = lolly_tmp * url ("save_exist.txt");
  c_string path1 (as_string (u1));
  tb_file_touch (path1, 0, 0);
  tb_file_ref_t file=
      tb_file_init (path1, TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC);
  tb_file_writ (
      file, reinterpret_cast<const tb_uint8_t*> ("longer text for test"), 20);
  tb_file_exit (file);
  string s1 ("test");
  CHECK (!save_string (u1, s1, false));
  string s2;
  CHECK (!load_string (u1, s2, false));
  string_eq (s1, s2);
}

TEST_CASE ("append to empty file") {
  url lolly_tmp= get_lolly_tmp ();

  SUBCASE ("file not exist") {
    url    u1= lolly_tmp * url ("append_not_exist.txt");
    string s1 ("file not exist");
    string s2;
    remove_if_exist (u1);

    CHECK (!append_string (u1, s1, false));
    CHECK (!load_string (u1, s2, false));
    string_eq (s1, s2);
  }

  SUBCASE ("file empty") {
    url    u1= lolly_tmp * url ("append_empty.txt");
    string s1 ("file empty");
    string s2;

    tb_file_create (c_string (as_string (u1)));
    CHECK (!append_string (u1, s1, false));

    CHECK (!load_string (u1, s2, false));
    string_eq (s1, s2);
  }
}

TEST_CASE ("append file not empty") {
  url lolly_tmp= get_lolly_tmp ();
  SUBCASE ("test file not empty") {
    url u1= lolly_tmp * url ("append_not_empty.txt");
    tb_file_create (c_string (as_string (u1)));

    string s1 ("head-");
    string s2 ("file not empty");
    string s3;

    CHECK (!save_string (u1, s1, false));
    CHECK (!append_string (u1, s2, false));

    CHECK (!load_string (u1, s3, false));
    string_eq ("head-file not empty", s3);
  }

  SUBCASE ("test file include unicode") {
    url u1= lolly_tmp * url ("unicode文件.txt");
    tb_file_create (c_string (as_string (u1)));

    string s1 ("头-");
    string s2 ("unicode文件");
    string s3;

    CHECK (!save_string (u1, s1, false));
    CHECK (!append_string (u1, s2, false));

    CHECK (!load_string (u1, s3, false));
    string_eq ("头-unicode文件", s3);
  }
}

TEST_MEMORY_LEAK_ALL
