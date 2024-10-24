/** \file lolly_string.hpp
 *  \copyright GPLv3
 *  \details Strings with different type of char, and corresponding readonly
 *           view of string. Zero-characters are allowed in strings.
 *  \author jingkaimori
 *  \date   2024
 */

#pragma once

#include "classdef.hpp"
#include "fast_alloc.hpp"

namespace lolly {
namespace data {
using char8_t= int8_t;

template <typename T> class lolly_string_view;
template <typename T> class lolly_string;
template <typename T> int N (lolly_string<T> a);
template <typename T> class lolly_string_rep : concrete_struct {
  int n;
  int a_N;
  T*  a;

public:
  inline lolly_string_rep () : n (0), a_N (0), a (NULL) {}
  lolly_string_rep (int n);
  inline ~lolly_string_rep () {
    if (n != 0) tm_delete_array (a);
  }
  /**
   * @brief expand (or shrink) string by delta, but do not release memory when
   * string is shrinked.
   *
   * @return string length before expansion
   */
  int expand_or_shrink_by (int delta);

  /**
   * @brief expand (or shrink) string to given length n, and try to release
   * memory when string is shrinked.
   *
   * @note expand_or_shrink_by may be faster if memory space is reserved
   */
  void resize (int n);

  /**
   * @brief reserve memory space to contain at least n word in the whole string.
   * Do not affect length of string, and do not release memory when n is smaller
   * than current space.
   */
  void reserve (int n);

  friend class lolly_string<T>;
  friend int N<> (lolly_string<T> a);
};

template <typename T> class lolly_string {
  CONCRETE_TEMPLATE (lolly_string, T);
  inline lolly_string () : rep (tm_new<lolly_string_rep<T>> ()) {}
  inline explicit lolly_string (int n)
      : rep (tm_new<lolly_string_rep<T>> (n)) {}
  lolly_string (T c);
  lolly_string (const lolly_string_view<T>& c);
  lolly_string (T c, int n);
  template <size_t N> lolly_string (const T (&s)[N]);
  inline T* buffer () { return rep->a; }
  inline T* buffer (int size) {
    rep->resize (size);
    return rep->a;
  }
  inline operator lolly_string_view<T> () {
    return lolly_string_view<T> (rep->n, rep->a);
  }
  inline T&            operator[] (int i) { return rep->a[i]; }
  lolly_string_view<T> operator() (int start, int end);
};
CONCRETE_TEMPLATE_CODE (lolly_string, typename, T);

using string    = lolly_string<char>;
using string_u16= lolly_string<char16_t>;
using string_u8 = lolly_string<char8_t>;

/**
 * Readonly view of string, in which char is store in a continuous space. NUL is
 * allowed in string. this view can be construct from lolly_string et al.
 */
template <typename T> class lolly_string_view {
public:
  const int      N;
  const T* const a;
  /**
   * construct from arbitary const pointer with length. NUL can occurs in given
   * array
   */
  lolly_string_view (int N_, const T* a_) : N (N_), a (a_){};
  /**
   * construct from string literal
   */
  template <size_t N_>
  constexpr lolly_string_view (const T null_end_str[N_])
      : N (N_), a (null_end_str){};
  /**
   * empty view is not allowed.
   */
  lolly_string_view ()                                        = delete;
  lolly_string_view (const lolly_string_view<T>&)             = delete;
  lolly_string_view (lolly_string_view<T>&&)                  = default;
  lolly_string_view&   operator= (const lolly_string_view<T>&)= delete;
  lolly_string_view&   operator= (lolly_string_view<T>&&)     = delete;
  lolly_string_view<T> operator() (int start, int end) const;
};

template <typename T>
inline int
N (lolly_string<T> a) {
  return a->n;
}

template <typename T> lolly_string<T> copy (const lolly_string_view<T>& a);
template <typename T> lolly_string<T> copy (lolly_string<T> a);
template <typename T, size_t N> lolly_string<T> copy (const T b[N]);

template <typename T> lolly_string<T>& operator<< (lolly_string<T>& a, T);
template <typename T>
inline lolly_string<T>& operator<< (lolly_string<T>& a, lolly_string<T> b);
template <typename T>
inline lolly_string<T>& operator<< (lolly_string<T>&            a,
                                    const lolly_string_view<T>& b);
template <typename T, size_t Nb>
inline lolly_string<T>& operator<< (lolly_string<T>& a, const T (&b)[Nb]);

template <typename T>
lolly_string<T> operator* (const lolly_string_view<T>& a,
                           const lolly_string_view<T>& b);
template <typename T>
lolly_string<T> operator* (const lolly_string_view<T>& a, lolly_string<T> b);
template <typename T, size_t Nb>
lolly_string<T> operator* (const lolly_string_view<T>& a, const T (&b)[Nb]);
template <typename T>
lolly_string<T> operator* (lolly_string<T> a, lolly_string<T> b);
template <typename T>
lolly_string<T> operator* (lolly_string<T> a, const lolly_string_view<T>& b);
template <typename T, size_t Nb>
lolly_string<T> operator* (lolly_string<T> a, const T (&b)[Nb]);
template <typename T, size_t Na>
lolly_string<T> operator* (const T (&a)[Na], const lolly_string_view<T>& b);
template <typename T, size_t Na>
lolly_string<T> operator* (const T (&a)[Na], lolly_string<T> b);

template <typename T>
bool operator== (const lolly_string_view<T>& a, const lolly_string_view<T>& b);
template <typename T>
bool operator== (const lolly_string_view<T>& a, lolly_string<T> b);
template <typename T, size_t Nb>
bool operator== (const lolly_string_view<T>& a, const T (&b)[Nb]);
template <typename T> bool operator== (lolly_string<T> a, lolly_string<T> b);
template <typename T>
bool operator== (lolly_string<T> a, const lolly_string_view<T>& b);
template <typename T, size_t Nb>
bool operator== (lolly_string<T> a, const T (&b)[Nb]);
template <typename T, size_t Na>
bool operator== (const T (&a)[Na], const lolly_string_view<T>& b);
template <typename T, size_t Na>
bool operator== (const T (&a)[Na], lolly_string<T> b);

template <typename T>
bool operator!= (const lolly_string_view<T>& a, const lolly_string_view<T>& b);
template <typename T>
bool operator!= (const lolly_string_view<T>& a, lolly_string<T> b);
template <typename T, size_t Nb>
bool operator!= (const lolly_string_view<T>& a, const T (&b)[Nb]);
template <typename T> bool operator!= (lolly_string<T> a, lolly_string<T> b);
template <typename T>
bool operator!= (lolly_string<T> a, const lolly_string_view<T>& b);
template <typename T, size_t Nb>
bool operator!= (lolly_string<T> a, const T (&b)[Nb]);
template <typename T, size_t Na>
bool operator!= (const T (&a)[Na], const lolly_string_view<T>& b);
template <typename T, size_t Na>
bool operator!= (const T (&a)[Na], lolly_string<T> b);

template <typename T>
bool operator< (const lolly_string_view<T>& a, const lolly_string_view<T>& b);
template <typename T>
bool operator< (const lolly_string_view<T>& a, lolly_string<T> b);
template <typename T, size_t Nb>
bool operator< (const lolly_string_view<T>& a, const T (&b)[Nb]);
template <typename T> bool operator< (lolly_string<T> a, lolly_string<T> b);
template <typename T>
bool operator< (lolly_string<T> a, const lolly_string_view<T>& b);
template <typename T, size_t Nb>
bool operator< (lolly_string<T> a, const T (&b)[Nb]);
template <typename T, size_t Na>
bool operator< (const T (&a)[Na], const lolly_string_view<T>& b);
template <typename T, size_t Na>
bool operator< (const T (&a)[Na], lolly_string<T> b);

template <typename T>
bool operator<= (const lolly_string_view<T>& a, const lolly_string_view<T>& b);
template <typename T>
bool operator<= (const lolly_string_view<T>& a, lolly_string<T> b);
template <typename T, size_t Nb>
bool operator<= (const lolly_string_view<T>& a, const T (&b)[Nb]);
template <typename T> bool operator<= (lolly_string<T> a, lolly_string<T> b);
template <typename T>
bool operator<= (lolly_string<T> a, const lolly_string_view<T>& b);
template <typename T, size_t Nb>
bool operator<= (lolly_string<T> a, const T (&b)[Nb]);
template <typename T, size_t Na>
bool operator<= (const T (&a)[Na], const lolly_string_view<T>& b);
template <typename T, size_t Na>
bool operator<= (const T (&a)[Na], lolly_string<T> b);

template <typename T> int hash (lolly_string_view<T> s);
template <typename T> int hash (lolly_string<T> s);
} // namespace data
} // namespace lolly

#include "lolly_string.ipp"
