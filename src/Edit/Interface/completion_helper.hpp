#ifndef COMPLETION_HELPER_HPP
#define COMPLETION_HELPER_HPP

#include "path.hpp"
#include "tree.hpp"

#include <string>

bool is_word_char (char c);

tree& subtree_modified (tree& t, path p);

string get_string_at_path (tree& et, path tp);

int get_pos_in_line (path p);

std::string get_prefix_from_line (std::string line, int pos_in_line);

int get_start_pos (std::string line, int pos_in_line);

#endif
