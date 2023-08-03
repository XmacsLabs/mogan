
/******************************************************************************
 * MODULE     : sort_dict.cpp
 * DESCRIPTION: sort the dict by lexicographical order
 * COPYRIGHT  : (C) 2023 Oyyko
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

// Usage: xmake build translation_sort && xmake run translation_sort

#include <algorithm>
#include <fstream>
#include <iostream>
#include <ostream>
#include <string>
#include <vector>
// #include <filesystem>

constexpr char RED[]     = "\033[31m";
constexpr char GREEN[]   = "\033[32m";
constexpr char NO_COLOR[]= "\033[0m";

void
print_red (const std::string& s) {
  std::cout << RED << s << NO_COLOR << std::endl;
}

void
print_green (const std::string& s) {
  std::cout << GREEN << s << NO_COLOR << std::endl;
}

bool
cmp (const std::string& a, const std::string& b) {
  return a < b;
}

int
main () {
  // std::cout << "Current path is " << std::filesystem::current_path() << '\n';
  std::ifstream fin;
  std::ofstream fout;
  fin.open ("../../../../TeXmacs/langs/natural/dic/english-chinese.scm",
            std::ios::in);
  if (fin.is_open ()) {
    print_green ("[sort_dict.cpp]: OPEN FILE SUCCESS!");
  }
  else {
    print_red ("[sort_dict.cpp]: OPEN FILE ERROR");
    exit (1);
  }
  std::string              tmp_line;
  std::vector<std::string> v;
  while (getline (fin, tmp_line)) {
    v.emplace_back (tmp_line);
  }
  fout.open ("../../../../TeXmacs/langs/natural/dic/english-chinese.scm",
             std::ios::out);
  sort (v.begin (), v.end (), cmp);
  for (auto& line : v) {
    fout << line << std::endl;
  }
  print_green ("[sort_dict.cpp]: TRANSLATION SUCCESS!");
}