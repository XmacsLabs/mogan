
/******************************************************************************
 * MODULE     : packrat_grammar.hpp
 * DESCRIPTION: packrat grammars
 * COPYRIGHT  : (C) 2010  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PACKRAT_GRAMMAR_H
#define PACKRAT_GRAMMAR_H
#include "array.hpp"
#include "hashmap.hpp"
#include "packrat.hpp"
#include "resource.hpp"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#define C int32_t
#define D int64_t
#else
#define C int
#define D long long int
#endif

RESOURCE (packrat_grammar);

/******************************************************************************
 * Important constants
 ******************************************************************************/

#define PACKRAT_TOKENS ((C) 0)
#define PACKRAT_OR ((C) 100000000)
#define PACKRAT_CONCAT ((C) 100000001)
#define PACKRAT_WHILE ((C) 100000002)
#define PACKRAT_REPEAT ((C) 100000003)
#define PACKRAT_RANGE ((C) 100000004)
#define PACKRAT_NOT ((C) 100000005)
#define PACKRAT_EXCEPT ((C) 100000006)
#define PACKRAT_PARTIAL ((C) 100000007)
#define PACKRAT_TM_OPEN ((C) 100000010)
#define PACKRAT_TM_ANY ((C) 100000011)
#define PACKRAT_TM_ARGS ((C) 100000012)
#define PACKRAT_TM_LEAF ((C) 100000013)
#define PACKRAT_TM_CHAR ((C) 100000014)
#define PACKRAT_TM_CURSOR ((C) 100000015)
#define PACKRAT_TM_FAIL ((C) 100000016)
#define PACKRAT_SYMBOLS ((C) 100000020)

/******************************************************************************
 * Encoding of tokens and symbols
 ******************************************************************************/

extern int                packrat_nr_tokens;
extern int                packrat_nr_symbols;
extern hashmap<string, C> packrat_tokens;
extern hashmap<tree, C>   packrat_symbols;
extern hashmap<C, tree>   packrat_decode;
extern tree               packrat_uninit;

C        encode_token (string s);
array<C> encode_tokens (string s);
C        encode_symbol (tree t);

/******************************************************************************
 * The packrat_grammar resource
 ******************************************************************************/

struct packrat_grammar_rep : rep<packrat_grammar> {
  string               lan_name; // name of the packrat_grammar
  hashmap<C, array<C>> grammar;
  hashmap<C, tree>     productions;
  hashmap<D, string>   properties;

  packrat_grammar_rep (string s);

  void     accelerate (array<C>& def);
  array<C> define (tree t);
  void     define (string s, tree t);
  void     set_property (string s, string var, string val);
  bool     has_property (string s, string var);
  string   get_property (string s, string var);

  string        decode_as_string (C sym);
  array<string> decode_as_array_string (C sym);
  array<string> members (string s);
};

packrat_grammar find_packrat_grammar (string s);

#endif // PACKRAT_GRAMMAR_H
