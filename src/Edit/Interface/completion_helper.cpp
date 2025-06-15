#include "tree_analyze.hpp"

bool
is_word_char (char c) {
  return std::isalnum (static_cast<unsigned char> (c)) || c == '_';
}

tree&
subtree_modified (tree& t, path p) {
  if (is_nil (p) || is_nil (p->next)) return t;
  else if (N (t) > p->item) {
    cout << N (t) << " " << p->item << " " << p->next << "\n";
    return subtree_modified (t[p->item], p->next);
  }
  else {
    cout << N (t) << " " << p->item << " " << p->next << "\n";
    cout << "The required path does not exist\n";
    return t;
  }
}

string
get_string_at_path (tree& et, path tp) {
  // TODO: seg fault here if tp is invalid
  cout << "et: " << et << "\n";
  cout << "tp: " << tp << "\n";
  cout << N (et) << " " << tp->item << " " << tp->next << "\n";
  tree node= subtree_modified (et, tp);
  cout << "nd: " << node << "\n";
  if (is_atomic (node)) {
    cout << "Found atomic node: " << node->label << "\n";
    return node->label;
  }
  return "";
}

int
get_pos_in_line (path p) {
  // Return the last digit in the path
  if (is_nil (p)) return 0;
  if (is_atom (p)) {
    return p->item;
  }
  else {
    return get_pos_in_line (p->next);
  }
}

std::string
get_prefix_from_line (std::string line, int pos_in_line) {
  // Clamp pos_in_line to the valid range
  if (pos_in_line > (int) line.size ()) pos_in_line= (int) line.size ();
  if (pos_in_line < 0) pos_in_line= 0;
  // Find the last word character before or at the cursor position
  int end= pos_in_line - 1;
  while (end >= 0 && !is_word_char (line[end])) {
    end--;
  }
  if (end < 0) return "";
  int start= end;
  while (start > 0 && is_word_char (line[start - 1])) {
    start--;
  }
  return line.substr (start, end - start + 1);
}

int
get_start_pos (std::string line, int pos_in_line) {
  // Clamp pos_in_line to the valid range
  if (pos_in_line > (int) line.size ()) pos_in_line= (int) line.size ();
  if (pos_in_line < 0) pos_in_line= 0;
  // Find the last word character before or at the cursor position
  int end= pos_in_line - 1;
  while (end >= 0 && !is_word_char (line[end])) {
    end--;
  }
  if (end < 0) return 0;
  int start= end;
  while (start > 0 && is_word_char (line[start - 1])) {
    start--;
  }
  return start;
}
