
/******************************************************************************
* MODULE     : itoa_jeaiii.hpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ITOA_JEAIII_H
#define ITOA_JEAIII_H

#include <stdint.h>

char* u32toa_jeaiii (uint32_t u, char* b);
char* i32toa_jeaiii (int32_t i, char* b);
char* u64toa_jeaiii (uint64_t n, char* b);
char* i64toa_jeaiii (int64_t i, char* b);

#endif // ITOA_JEAIII_H
