
/******************************************************************************
 * MODULE     : converter.cpp
 * DESCRIPTION: Applies dictionaries to strings in an efficient manner.
 * COPYRIGHT  : (C) 2002  Felix Breuer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "converter.hpp"
#ifdef USE_ICONV
#include <iconv.h>
#endif
#include "tree.hpp"
#include "tree_helper.hpp"
#include <errno.h>

#include <lolly/data/numeral.hpp>
#include <lolly/data/unicode.hpp>
#include <moebius/data/scheme.hpp>

using namespace moebius;

using lolly::data::decode_from_utf8;
using lolly::data::encode_as_utf8;
using lolly::data::from_hex;
using lolly::data::to_Hex;
using moebius::data::block_to_scheme_tree;

#define from_hexadecimal from_hex

/******************************************************************************
 * converter methods
 ******************************************************************************/

void
operator<< (converter c, string str) {
  int index= 0;
  while (index < N (str))
    c->match (str, index);
}

string
apply (converter c, string str) {
  c->output= string ();
  c << str;
  return flush (c);
}

string
flush (converter c) {
  string result= c->output;
  c->output    = string ();
  return result;
}

/******************************************************************************
 * method for loading converters
 ******************************************************************************/

converter
load_converter (string from, string to) {
  string name= from * "-" * to;
  if (converter::instances->contains (name)) return converter (name);
  converter conv= tm_new<converter_rep> (from, to);
  return conv;
}

/******************************************************************************
 * converter_rep methods
 ******************************************************************************/

inline bool
converter_rep::has_value (hashtree<char, string> node) {
  return node->label != nil_string;
}

inline void
converter_rep::match (string& str, int& index) {
  int                    forward   = index;
  int                    last_match= -1;
  string                 value ("");
  bool                   done= false;
  hashtree<char, string> node= ht;
  // cout << "[";
  while (!done && forward < N (str)) {
    if (node->contains (str[forward])) {
      node= node (str[forward]);
      // printf("->%x",str[forward]);
      if (has_value (node)) {
        last_match= forward;
        value     = node->label;
      }
      forward++;
    }
    else done= true;
  }
  if (last_match == -1) {
    if (copy_unmatched) output << string (str[index]);
    index++;
  }
  else {
    // printf(":");for(int i = 0; i < N(value);i++) printf("%x ",value[i]);
    output << value;
    index= last_match + 1;
  }
  // cout << "]";
}

void
converter_rep::load () {
  // to handle each case individually seems unelegant, but there is simply more
  // to be done here than just loading a file.
  // cout << "TeXmacs] load converter " << from << " -> " << to << "\n";
  if (from == "Cork" && to == "UTF-8") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "corktounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "cork-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "tmuniversaltounicode", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-oneway", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-fallback", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-math", BIT2BIT, UTF8, false);
    ht= dic;
  }
  else if (from == "UTF-8" && to == "Cork") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "corktounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic, "unicode-cork-oneway", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic, "tmuniversaltounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic, "unicode-symbol-oneway", UTF8, BIT2BIT,
                              true);
    ht= dic;
  }

  if (from == "Hex-Cork" && to == "UTF-8") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "herktounicode", BIT2BIT, UTF8, false);
    ht= dic;
  }
  else if (from == "UTF-8" && to == "Hex-Cork") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "herktounicode", UTF8, BIT2BIT, true);
    ht= dic;
  }

  if (from == "Strict-Cork" && to == "UTF-8") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "corktounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "cork-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "tmuniversaltounicode", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-oneway", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-math", BIT2BIT, UTF8, false);
    ht= dic;
  }
  else if (from == "UTF-8" && to == "HTML") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "HTMLlat1", CHAR_ENTITY, ENTITY_NAME, true);
    hashtree_from_dictionary (dic, "HTMLspecial", CHAR_ENTITY, ENTITY_NAME,
                              true);
    hashtree_from_dictionary (dic, "HTMLsymbol", CHAR_ENTITY, ENTITY_NAME,
                              true);
    ht= dic;
  }
  else if (from == "T2A" && to == "UTF-8") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "corktounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "cork-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "tmuniversaltounicode", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-oneway", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-fallback", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-math", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "t2atounicode", BIT2BIT, UTF8, false);
    ht= dic;
  }
  else if (from == "UTF-8" && to == "T2A") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "corktounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic, "unicode-cork-oneway", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic, "tmuniversaltounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic, "unicode-symbol-oneway", UTF8, BIT2BIT,
                              true);
    hashtree_from_dictionary (dic, "t2atounicode", UTF8, BIT2BIT, true);
    ht= dic;
  }
  else if (from == "T2A.CY" && to == "CODEPOINT") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "t2atounicode", BIT2BIT, CHAR_ENTITY, false);
    hashtree_from_dictionary (dic, "t2atounicode", CHAR_ENTITY, CHAR_ENTITY,
                              false);
    ht= dic;
  }
  else if (from == "CODEPOINT" && to == "T2A.CY") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "t2atounicode", CHAR_ENTITY, BIT2BIT, true);
    hashtree_from_dictionary (dic, "t2atounicode", CHAR_ENTITY, CHAR_ENTITY,
                              true);
    ht= dic;
  }
  else if (from == "UTF-8" && to == "LaTeX") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "utf8tolatex", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic, "utf8tolatex-onedir", UTF8, BIT2BIT, false);
    ht= dic;
  }
  else if (from == "LaTeX" && to == "UTF-8") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "utf8tolatex", BIT2BIT, UTF8, true);
    hashtree_from_dictionary (dic, "utf8tolatex-back", BIT2BIT, UTF8, true);
    ht= dic;
  }
  else if (from == "Cork" && to == "ASCII") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "cork-escaped-to-ascii", BIT2BIT, UTF8,
                              false);
    ht= dic;
  }
  else if (from == "Cork" && to == "SourceCode") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "corktounicode", BIT2BIT, UTF8, false);
    // hashtree_from_dictionary (dic,"cork-unicode-oneway", BIT2BIT, UTF8,
    // false);
    hashtree_from_dictionary (dic, "tmuniversaltounicode", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-oneway", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-fallback", BIT2BIT, UTF8,
                              false);
    hashtree_from_dictionary (dic, "symbol-unicode-math", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic, "cork-to-real-ascii", BIT2BIT, BIT2BIT,
                              false);
    ht= dic;
  }
  else if (from == "SourceCode" && to == "Cork") {
    hashtree<char, string> dic;
    hashtree_from_dictionary (dic, "corktounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic, "unicode-cork-oneway", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic, "tmuniversaltounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic, "unicode-symbol-oneway", UTF8, BIT2BIT,
                              true);
    hashtree_from_dictionary (dic, "cork-to-real-ascii", UTF8, BIT2BIT, true);
    ht= dic;
  }
}

/******************************************************************************
 * convenience functions
 ******************************************************************************/

bool
check_encoding (string input, string encoding) {
  if (encoding == "Cork") return true;
#ifdef USE_ICONV
  else return check_using_iconv (input, encoding);
#else
  (void) input;
  return true;  // Assume everything is fine.
#endif
}

string
convert (string input, string from, string to) {
  if (from == "Cork") return convert_from_cork (input, to);
  else if (to == "Cork") return convert_to_cork (input, from);
  else if (from == "LaTeX" && to == "UTF-8")
    return convert_LaTeX_to_utf8 (input);
  else if (from == "UTF-8" && to == "LaTeX")
    return convert_utf8_to_LaTeX (input);
#ifdef USE_ICONV
  else return convert_using_iconv (input, from, to);
#else
  return input; // can't do anything.
#endif
}

string
convert_to_cork (string input, string from) {
  if (from == "UTF-8") return utf8_to_cork (input);
  else if (from == "SourceCode") return sourcecode_to_cork (input);
#ifdef USE_ICONV
  string str= convert_using_iconv (input, from, "UTF-8");
  return utf8_to_cork (str);
#else
  return input; // can't do anything.
#endif
}

string
convert_from_cork (string input, string to) {
  if (to == "UTF-8") return cork_to_utf8 (input);
  else if (to == "SourceCode") return cork_to_sourcecode (input);
#ifdef USE_ICONV
  string str= cork_to_utf8 (input);
  return convert_using_iconv (str, "UTF-8", to);
#else
  return input; // can't do anything.
#endif
}

string
convert_LaTeX_to_utf8 (string input) {
  converter conv= load_converter ("LaTeX", "UTF-8");
  string    output;
  output= apply (conv, input);
  return output;
}

string
convert_utf8_to_LaTeX (string input) {
  converter conv= load_converter ("UTF-8", "LaTeX");
  int       i, start, n= N (input);
  string    output, r;
  for (i= 0; i < n;) {
    if (((unsigned char) input[i]) < 128 && ((unsigned char) input[i]) > 31) {
      output << input[i++];
      continue;
    }
    else {
      start               = i;
      unsigned int code   = decode_from_utf8 (input, i);
      string       unicode= input (start, i);
      r                   = apply (conv, unicode);
      if (r == unicode) {
        if (code == 10) output << "-";
        else {
          output << r;
          cout << "TeXmacs] non ascii character <#" << to_Hex (code)
               << "> on output: " << unicode
               << "\nLaTeX output may not compile.\n";
          // output << "(error)";
        }
      }
      else output << r;
    }
  }
  return output;
}

string
utf8_to_cork (string input) {
  converter conv= load_converter ("UTF-8", "Cork");
  int       start, i, n= N (input);
  string    output;
  for (i= 0; i < n;) {
    start            = i;
    unsigned int code= decode_from_utf8 (input, i);
    string       s   = input (start, i);
    string       r   = apply (conv, s);
    if (r == s && code >= 256) r= "<#" * to_Hex (code) * ">";
    output << r;
  }
  return output;
}

string
utf8_to_herk (string input) {
  converter conv= load_converter ("UTF-8", "Hex-Cork");
  int       start, i, n= N (input);
  string    output;
  for (i= 0; i < n;) {
    start        = i;
    uint32_t code= decode_from_utf8 (input, i);
    string   s   = input (start, i);
    string   r   = apply (conv, s);
    if (r == s) {
      if (code < 16) {
        r= "<#0" * to_Hex (code) * ">";
      }
      else if (code < 32 || code >= 128) {
        r= "<#" * to_Hex (code) * ">";
      }
    }
    output << r;
  }
  return output;
}

string
sourcecode_to_cork (string input) {
  converter conv= load_converter ("SourceCode", "Cork");
  int       start, i, n= N (input);
  string    output;
  for (i= 0; i < n;) {
    start            = i;
    unsigned int code= decode_from_utf8 (input, i);
    string       s   = input (start, i);
    string       r   = apply (conv, s);
    if (r == s && code >= 256) r= "<#" * to_Hex (code) * ">";
    output << r;
  }
  return output;
}

string_u8
cork_to_utf8 (string input) {
  converter conv = load_converter ("Cork", "UTF-8");
  int       start= 0, i, n= N (input);
  string    r;
  for (i= 0; i < n; i++)
    if (input[i] == '<' && i + 1 < n && input[i + 1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i + 2;
      while (i < n && input[i] != '>')
        i++;
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i + 1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string_u8
herk_to_utf8 (string input) {
  converter conv = load_converter ("Hex-Cork", "UTF-8");
  int       start= 0, i, n= N (input);
  string    r;
  for (i= 0; i < n; i++)
    if (input[i] == '<' && i + 1 < n && input[i + 1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i + 2;
      while (i < n && input[i] != '>')
        i++;
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i + 1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string_u8
strict_cork_to_utf8 (string input) {
  converter conv = load_converter ("Strict-Cork", "UTF-8");
  int       start= 0, i, n= N (input);
  string    r;
  for (i= 0; i < n; i++)
    if (input[i] == '<' && i + 1 < n && input[i + 1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i + 2;
      while (i < n && input[i] != '>')
        i++;
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i + 1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string
cork_to_sourcecode (string input) {
  converter conv = load_converter ("Cork", "SourceCode");
  int       start= 0, i, n= N (input);
  string    r;
  for (i= 0; i < n; i++)
    if (input[i] == '<' && i + 1 < n && input[i + 1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i + 2;
      while (i < n && input[i] != '>')
        i++;
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i + 1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string
utf8_to_t2a (string_u8 input) {
  converter conv= load_converter ("UTF-8", "T2A");
  int       start, i, n= N (input);
  string    output;
  for (i= 0; i < n;) {
    start            = i;
    unsigned int code= decode_from_utf8 (input, i);
    string       s   = input (start, i);
    string       r   = apply (conv, s);
    if (r == s && code >= 256) r= "<#" * to_Hex (code) * ">";
    output << r;
  }
  return output;
}

string
cyrillic_subset_in_t2a_to_code_point (string input) {
  converter conv= load_converter ("T2A.CY", "CODEPOINT");
  string    r, tmp;
  int       i, j, n= N (input);

  for (i= 0; i < n; i++) {
    if (input[i] == '<') {
      for (j= i + 1; j < n && input[j] != '>'; j++)
        ;
      r << input (i, j + 1);
      i= j;
    }
    else {
      tmp= apply (conv, input[i]);
      if (tmp == string (input[i])) r << tmp;
      else r << '<' * tmp * '>';
    }
  }
  return r;
}

string
code_point_to_cyrillic_subset_in_t2a (string input) {
  converter conv= load_converter ("CODEPOINT", "T2A.CY");
  string    r, tmp;
  int       i= 0, n= N (input);

  while (i < n) {
    int start= i;
    if (input[i] == '<') {
      i++;
      while (i < n && input[i] != '>')
        i++;
    }
    i++;
    string s= apply (conv, input (start, i));
    if (N (s) == 5 && s[0] == '<' && s[1] == '#' && s[4] == '>')
      r << string ((char) from_hexadecimal (s (2, 4)));
    else r << s;
  }
  return r;
}

string_u8
t2a_to_utf8 (string input) {
  converter conv = load_converter ("T2A", "UTF-8");
  int       start= 0, i, n= N (input);
  string    r;
  for (i= 0; i < n; i++)
    if (input[i] == '<' && i + 1 < n && input[i + 1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i + 2;
      while (i < n && input[i] != '>')
        i++;
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i + 1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string
utf8_to_html (string_u8 input) {
  converter conv= load_converter ("UTF-8", "HTML");
  string    s   = apply (conv, input);

  bool should_convert_to_hex= true; // Transcoding is required by default

  // Iterate over the string and check if it contains CJK characters
  for (int i= 0; i < N (s);) {
    unsigned int code= decode_from_utf8 (s, i);

    // Determining whether a character is a CJK character
    if ((code >= 0x4e00 &&
         code <= 0x9fff) || // commonly used Chinese characters
        (code >= 0x3400 && code <= 0x4dbf) ||   // CJK extension A
        (code >= 0x20000 && code <= 0x2a6df) || // CJK extension B
        (code >= 0x2A700 && code <= 0x2B73F) || // CJK extension C
        (code >= 0x2B740 && code <= 0x2B81F) || // CJK extension D
        (code >= 0x2B820 && code <= 0x2CEAF) || // CJK extension E
        (code >= 0x2CEB0 && code <= 0x2EBEF) || // CJK extension F
        (code >= 0xF900 && code <= 0xFAFF) ||   // CJK-compatible characters
        (code >= 0xAC00 && code <= 0xD7AF) ||   // Korean alphabet
        (code >= 0x3040 && code <= 0x309F) ||   // Japanese hiragana
        (code >= 0x30A0 && code <= 0x30FF) ||   // Japanese katakana
        (code >= 0xFF00 && code <= 0xFFEF)) {   // full-point punctuation
      should_convert_to_hex= false;
      break;
    }
  }

  // Based on the result, decide whether to call utf8_to_hex_entities or not
  if (should_convert_to_hex) {
    return utf8_to_hex_entities (s);
  }
  else {
    return s;
  }
}

string
cork_to_ascii (string input) {
  converter conv= load_converter ("Cork", "ASCII");
  return apply (conv, input);
}

#ifdef USE_ICONV

class iconv_converter {
  string  from;
  string  to;
  iconv_t cd;
  bool    show_errors;
  bool    successful;

public:
  iconv_converter (string from, string to, bool errors= true);
  ~iconv_converter ();
  inline bool   is_valid () { return cd != (iconv_t) -1; }
  inline bool   is_successful () { return successful; }
  friend string apply (iconv_converter& conv, string input);
};

iconv_converter::iconv_converter (string from2, string to2, bool errors)
    : from (from2), to (to2), show_errors (errors), successful (false) {
  c_string from_cp (from);
  c_string to_cp (to);
  cd= iconv_open (to_cp, from_cp);
  if (!is_valid () && show_errors)
    convert_error << "Initialization of iconv from " << from << " to " << to
                  << " failed\n";
  successful= true;
}

iconv_converter::~iconv_converter () {
  if (is_valid ()) iconv_close (cd);
}

// From the standard C++ library (remember, TeXmacs does _not_ use std!)
template <typename T>
inline size_t
iconv_adaptor (size_t (*iconv_func) (iconv_t, T, size_t*, char**, size_t*),
               iconv_t cd, char** inbuf, size_t* inbytesleft, char** outbuf,
               size_t* outbytesleft) {
  return iconv_func (cd, (T) ((void*) inbuf), inbytesleft, outbuf,
                     outbytesleft);
}

string
apply (iconv_converter& conv, string input) {
  if (!conv.is_valid ()) {
    conv.successful= false;
    convert_error << "Conversion cancelled\n";
    return input;
  }
  string   result;
  c_string in_cp (input);
  char*    in_cursor  = in_cp;
  size_t   in_left    = N (input);
  double   expansion  = 1.1;
  size_t   out_counter= 0;
  while (in_left > 0) {
    size_t   out_left= max (int (in_left * expansion), 1024);
    c_string out_cp (out_left);
    char*    out_cursor= out_cp;
    size_t r= iconv_adaptor (iconv, conv.cd, &in_cursor, &in_left, &out_cursor,
                             &out_left);
    if (r == (size_t) -1 && errno != E2BIG) {
      if (conv.show_errors) {
        convert_error << "Iconv conversion from " << conv.from << " to "
                      << conv.to << " failed\n";
      }
      conv.successful= false;
      return input;
    }
    size_t used_out= out_cursor - out_cp;
    result << string (out_cp, used_out);
    out_counter+= used_out;
    expansion= max ((double) out_counter / (in_cursor - in_cp), 1.0) + 0.1;
  }
  conv.successful= true;
  return result;
}

#endif // defined USE_ICONV

bool
check_using_iconv (string input, string encoding) {
#ifdef USE_ICONV
  iconv_converter conv (encoding, encoding, false);
  apply (conv, input);
  return conv.is_successful ();
#else
  (void) input;
  (void) encoding;
  TM_FAILED ("iconv not enabled");
  return false;
#endif
}

string
convert_using_iconv (string input, string from, string to) {
  if (from == to) return input;
#ifdef USE_ICONV
  iconv_converter conv (from, to, true);
  return apply (conv, input);
#else
  (void) input;
  (void) from;
  (void) to;
  TM_FAILED ("iconv not enabled");
  return input;
#endif
}

/******************************************************************************
 * Functions for hashtree handling
 ******************************************************************************/

void
put_prefix_code (string key, string value, hashtree<char, string> tree) {
  /*
  if (DEBUG_CONVERT) {
    hashtree<char,string> ht= find_node (key,tree);
    if (ht->label != "")
      debug_convert << "overwriting " << ht->label << " with " << value << '\n';
  }
  */
  find_node (key, tree)->set_label (value);
}

hashtree<char, string>
find_node (string key, hashtree<char, string> ht) {
  int i;
  for (i= 0; i < N (key); i++)
    ht= ht (key[i]);
  return ht;
}

static string
str_unquote (string s) {
  int start, end;
  for (start= 0; start < N (s) && s[start] != '"'; start++)
    ;
  for (end= N (s) - 1; end > start && s[end] != '"'; end--)
    ;
  return s (start + 1, end);
}

void
hashtree_from_dictionary (hashtree<char, string> dic, string file_name,
                          escape_type key_escape, escape_type val_escape,
                          bool reverse) {
  string key_string, val_string, file;
  file_name= file_name * ".scm";
  if (load_string (url ("$TEXMACS_PATH/langs/encoding", file_name), file,
                   false)) {
    convert_error << "Couldn't open encoding dictionary " << file_name << LF;
    return;
  }
  if (DEBUG_CONVERT)
    debug_convert << "Loaded dictionary " << file_name << " with file size "
                  << N (file) << LF;
  tree t= block_to_scheme_tree (file);
  if (!is_tuple (t)) {
    convert_error << "Malformed encoding dictionary " << file_name << LF;
    return;
  }
  for (int i= 0; i < N (t); i++) {
    if (is_func (t[i], TUPLE, 2) && is_atomic (t[i][0]) &&
        is_atomic (t[i][1])) {
      // cout << N(pairs[i]) << "\n" << as_string(pairs[i]) << "\n";
      reverse ? key_string= t[i][1]->label : key_string= t[i][0]->label;
      reverse ? val_string= t[i][0]->label : val_string= t[i][1]->label;
      if (is_quoted (key_string)) key_string= str_unquote (key_string);
      if (is_quoted (val_string)) val_string= str_unquote (val_string);
      if (key_escape == BIT2BIT)
        key_string= convert_escapes (key_string, false);
      else if (key_escape == UTF8)
        key_string= convert_escapes (key_string, true);
      else if (key_escape == CHAR_ENTITY)
        key_string= convert_char_entities (key_string);
      if (val_escape == BIT2BIT)
        val_string= convert_escapes (val_string, false);
      else if (val_escape == UTF8)
        val_string= convert_escapes (val_string, true);
      else if (val_escape == ENTITY_NAME) val_string= "&" * val_string * ";";
      // cout << "key: " << key_string << " val: " << val_string << "\n";
      put_prefix_code (key_string, val_string, dic);
    }
  }
}

/***************************************************************************
 * Functions for UTF-8 handling
 * These functions are helper functions to convert escape string a la "#23F7"
 * and HTML/XML character entities to and from UTF-8 byte sequences.
 ***************************************************************************/

int
hex_digit_to_int (unsigned char c) {
  if (48 <= c && c <= 57) return c - 0x30;
  else if (65 <= c && c <= 70) return c - 0x41 + 0x0A;
  else if (97 <= c && c <= 102) return c - 0x61 + 0x0A;
  else return 0;
}

string
convert_escapes (string in, bool utf8) {
  // cout << "converting " << in ;
  string result;
  int    i= 0;
  while (i < N (in)) {
    if (in[i] != '#') result << in[i++];
    else {
      i++;
      unsigned int num= 0;
      while (i < N (in) && is_hex_digit (in[i]))
        num= 0x10 * num + hex_digit_to_int ((unsigned char) in[i++]);
      // cout << " to num "; printf("%x",num); cout << " then to ";
      if (utf8) result << encode_as_utf8 (num);
      else result << string ((char) num);
    }
  }
  // for(int i = 0; i < N(result);i++)
  //   printf("%x ", (unsigned char)result[i]); printf("\n");
  return result;
}

string
convert_char_entities (string s) {
  int    i, n= N (s);
  string r;
  for (i= 0; i < n; /* noop */) {
    if (s[i] == '&' && i + 1 < n && s[i + 1] == '#') {
      i+= 2;
      bool   okay= false;
      string rr  = convert_char_entity (s, i, okay);
      if (okay) r << rr;
      else {
        r << "&#";
        continue;
      }
    }
    else r << s[i++];
  }
  return r;
}

static unsigned int
as_unsigned_int (string s) {
  int          i= 0, n= N (s);
  unsigned int val= 0;
  if (n == 0) return 0;
  while (i < n) {
    if (s[i] < '0') break;
    if (s[i] > '9') break;
    val*= 10;
    val+= (int) (s[i] - '0');
    i++;
  }
  return val;
}

string
convert_char_entity (string s, int& start, bool& success) {
  // start: position in s after the character entity marker "&#".
  success         = false;
  int          i  = start;
  int          n  = N (s);
  unsigned int num= 0;
  if (i >= n) return "";
  else if (s[i] == 'x' || s[i] == 'X') {
    i++;
    // int j=i;
    while (i < n && is_hex_digit (s[i])) {
      success= true;
      num    = 0x10 * num + hex_digit_to_int (s[i]);
      i++;
    }
    // if (success) cout << "hex-ent: " << s(j,i) ;
  }
  else {
    int j= i;
    while (i < n && is_digit (s[i])) {
      success= true;
      i++;
    }
    // if (success) cout << "dec-ent: " << s(j,i) ;
    num= as_unsigned_int (s (j, i));
  }
  if (success) {
    if (i < n && s[i] == ';') i++;
    start= i;
    // cout << " --> (" << num << ") " << encode_as_utf8 (num) << '\n' ;
    return encode_as_utf8 (num);
  }
  else return "";
}

string
utf8_to_hex_entities (string_u8 s) {
  string result;
  int    i, n= N (s);
  for (i= 0; i < n;) {
    unsigned char c= s[i];
    if ((0x80 & c) == 0 || ((0xF8 & c) == 0xF8)) {
      result << c;
      i++;
    }
    else {
      unsigned int code= decode_from_utf8 (s, i);
      string       hex = to_Hex (code);
      while (N (hex) < 4)
        hex= "0" * hex;
      // cout << "entity: " << hex << " (" << code << ")\n";
      result << "&#x" << hex << ";";
    }
  }
  return result;
}

string
utf8_to_utf16be_string (string_u8 s) {
  string result, hex;
  int    i, n= N (s);
  for (i= 0; i < n;) {
    unsigned int code= decode_from_utf8 (s, i);
    // see e.g. https://en.wikipedia.org/wiki/UTF-16
    if (code >= 0x10000) {
      // supplementary planes
      unsigned int code2= code - 0x10000;
      unsigned int w1   = 0xD800 + (code2 >> 10);
      unsigned int w2   = 0xDC00 + (code2 & 0x3FF);
      hex               = to_Hex (w1);
      while (N (hex) < 4)
        hex= "0" * hex;
      result << hex;
      hex= to_Hex (w2);
      while (N (hex) < 4)
        hex= "0" * hex;
      result << hex;
    }
    else {
      // basic planes
      string hex= to_Hex (code);
      while (N (hex) < 4)
        hex= "0" * hex;
      result << hex;
    }
  }
  return result;
}

string
utf8_to_pdf_hex_string (string_u8 s) {
  return "<FEFF" * utf8_to_utf16be_string (cork_to_utf8 (s)) * ">";
}

/******************************************************************************
 * Convert TS1 defined symbols to universal encoding
 ******************************************************************************/
tree
convert_OTS1_symbols_to_universal_encoding (tree t) {
  if (is_atomic (t)) return t;
  if (N (t) == 0) {
    static tree symbols (CONCAT);
    if (N (symbols) == 0)
      symbols << tree ("cent") << tree ("copyright") << tree ("currency")
              << tree ("yen") << tree ("twosuperior") << tree ("threesuperior")
              << tree ("onesuperior") << tree ("mu") << tree ("onequarter")
              << tree ("onehalf") << tree ("threequarters")
              << tree ("trademark");
    tree l= tree (as_string (L (t)));
    if (contains (l, A (symbols))) return "<" * as_string (L (t)) * ">";
    else if (l == "degreesign") return "<degree>";
    else if (l == "copyleft")
      return "<copyright>"; // Copyleft is nor defined in TeXmacs universal
                            // encoding, neither in utf8, neither buildable
                            // with TeXmacs primitive construction.
    else if (l == "registered") return "<circledR>";
    else if (l == "paragraphsign") return "<paragraph>";
    else if (l == "euro") return "<#20AC>";
    else return t;
  }
  int  i, n= N (t);
  tree r (t, n);
  for (i= 0; i < n; i++)
    r[i]= convert_OTS1_symbols_to_universal_encoding (t[i]);
  return r;
}
