/** \file file.cpp
 *  \copyright GPLv3
 *  \details Unitests for file.
 *  \author Darcy
 *  \date   2019-2023
 */
#include "a_tbox_main.cpp"
#include "file.hpp"
#include "tbox/tbox.h"

url
get_lolly_tmp () {
#if defined(OS_WIN) || defined(OS_MINGW)
  return url_system ("$TMP") * url (".lolly");
#else
  return url_system ("/tmp") * url (".lolly");
#endif
}

#if defined(OS_WIN) || defined(OS_MINGW)
TEST_CASE ("is_directory on Windows") {
  CHECK (is_directory (url_system ("C:/Windows")));
}
#endif

#if defined(OS_LINUX)
TEST_CASE ("is_symbolic_link on linux") {
  CHECK (is_symbolic_link (url_system ("/usr/bin/python")));
}
#endif

TEST_CASE ("is_directory/is_regular") {
  CHECK (is_directory (url_pwd ()));
  CHECK (!is_regular (url_pwd ()));
  CHECK (!is_symbolic_link (url_pwd ()));

  url xmake_lua= url_pwd () * url ("xmake.lua");
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
  CHECK (is_of_type (url_pwd () * url ("xmake.lua"), "fr"));
#if defined(OS_MINGW) || defined(OS_WIN)
  CHECK (is_of_type (url_pwd () * url ("bin/format.bat"), "x"));
#endif
}

TEST_CASE ("file_size") {
  CHECK (file_size (url_pwd () * url ("xmake.lua")) > 0);
}

TEST_CASE ("last_modified") {
  CHECK (last_modified (url_pwd () * url ("xmake.lua")) > 0);
}

TEST_CASE ("mkdir/rmdir") {
  url lolly_tmp = get_lolly_tmp ();
  url test_mkdir= lolly_tmp * url ("tmp_dir");
  mkdir (test_mkdir);
  CHECK (is_directory (test_mkdir));
  rmdir (test_mkdir);
  CHECK (!is_directory (test_mkdir));
}

TEST_CASE ("remove") {
  url       lolly_tmp= get_lolly_tmp ();
  tb_hong_t time     = tb_time ();

  SUBCASE ("single file") {
    url xyz_txt= lolly_tmp * url ("xyz.txt");
    tb_file_touch (as_charp (as_string (xyz_txt)), time, time);
    CHECK (file_size (xyz_txt) == 0);
    remove (xyz_txt);
    CHECK (file_size (xyz_txt) == -1);
  }

  SUBCASE ("multiple files") {
    url xyz1_txt= lolly_tmp * url ("xyz1.txt");
    url xyz2_txt= lolly_tmp * url ("xyz2.txt");
    url xyz3_txt= lolly_tmp * url ("xyz3.txt");
    tb_file_touch (as_charp (as_string (xyz1_txt)), time, time);
    tb_file_touch (as_charp (as_string (xyz2_txt)), time, time);
    tb_file_touch (as_charp (as_string (xyz3_txt)), time, time);
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
  tb_file_touch (as_charp (as_string (m1)), time, time);
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
  tb_file_touch (as_charp (as_string (c1)), time, time);
  CHECK (file_size (c2) == -1);
  copy (c1, c2);
  CHECK (file_size (c1) == 0);
  CHECK (file_size (c2) == 0);
  remove (c1);
  remove (c2);
}

TEST_CASE ("url_temp") {
  url       tmp1= url_temp ("test");
  tb_hong_t time= tb_time ();
  tb_file_touch (as_charp (as_string (tmp1)), time, time);
  url tmp2= url_temp ("test");
  CHECK_EQ (tmp1 == tmp2, false);
  CHECK (is_directory (url_temp_dir ()));
}

TEST_CASE ("read_directory") {
  bool flag1= false;
  CHECK (N (read_directory (url_pwd (), flag1)) > 0);
  CHECK (!flag1); // no error
  bool flag2= false;
  CHECK (N (read_directory (url_system ("no_such_dir"), flag2)) == 0);
  CHECK (flag2); // error
}
