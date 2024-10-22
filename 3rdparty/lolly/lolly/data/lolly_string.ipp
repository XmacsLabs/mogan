/** \file lolly_string.ipp
 *  \copyright GPLv3
 *  \details Strings with different type of char, and corresponding readonly
 *           view of string. Zero-characters are allowed in strings.
 *  \author jingkaimori
 *  \date   2024
 */

#pragma once
#include "lolly_string.hpp"
namespace lolly {
namespace data {

static inline int
round_length (int n) {
  n= (n + 3) & (0xfffffffc);
  if (n < 24) return n;
  int i= 32;
  while (n > i)
    i<<= 1;
  return i;
}

template <class T>
lolly_string_rep<T>::lolly_string_rep (int n2)
    : n (n2), a ((n == 0) ? ((T*) NULL) : tm_new_array<T> (round_length (n))) {}

template <class T>
void
lolly_string_rep<T>::resize (int m) {
  int nn= round_length (n);
  int mm= round_length (m);
  if (mm != nn) {
    if (mm != 0) {
      if (nn != 0) {
        a= tm_resize_array<T> (mm, a);
      }
      else {
        a= tm_new_array<T> (mm);
      }
    }
    else if (nn != 0) tm_delete_array (a);
  }
  n= m;
}

template <class T>
lolly_string<T>::lolly_string (T c) : rep (tm_new<lolly_string_rep<T>> (1)) {
  rep->a[0]= c;
};

template <class T>
lolly_string<T>::lolly_string (const lolly_string_view<T>& c)
    : rep (tm_new<lolly_string_rep<T>> (c.N)) {
  for (int i= 0; i < c.N; i++)
    rep->a[i]= c.a[i];
};

template <class T>
lolly_string<T>::lolly_string (T c, int n)
    : rep (tm_new<lolly_string_rep<T>> (n)) {
  for (int i= 0; i < n; i++)
    rep->a[i]= c;
};
template <class T>
template <size_t N>
lolly_string<T>::lolly_string (const T (&s)[N])
    : rep (tm_new<lolly_string_rep<T>> (N - 1)) {
  constexpr int n= N - 1;
  for (int i= 0; i < n; i++)
    rep->a[i]= s[i];
};
template <class T>
lolly_string_view<T>
lolly_string<T>::operator() (int start, int end) {
  if (end <= start) {
    return lolly_string_view<T> (0, nullptr);
  }
  start= max (min (rep->n, start), 0);
  end  = max (min (rep->n, end), 0);
  return lolly_string_view<T> (end - start, rep->a + start);
}
template <class T>
lolly_string_view<T>
lolly_string_view<T>::operator() (int start, int end) const {
  if (end <= start) {
    return lolly_string_view<T> (0, nullptr);
  }
  start= max (min (N, start), 0);
  end  = max (min (N, end), 0);
  return lolly_string_view<T> (end - start, a + start);
}

template <typename T>
lolly_string<T>
copy (const lolly_string_view<T>& a) {
  int    i;
  string r (a.N);
  for (i= 0; i < a.N; i++)
    r[i]= a.a[i];
  return r;
};

template <typename T>
lolly_string<T>
copy (lolly_string<T> a) {
  int    i, n= N (a);
  string r (a);
  for (i= 0; i < a; i++)
    r[i]= a[i];
  return r;
};

template <typename T, size_t N>
lolly_string<T>
copy (const T a[N]) {
  int    i;
  string r ((int) N);
  for (i= 0; i < N; i++)
    r[i]= a[i];
  return r;
};

tm_ostream&
operator<< (tm_ostream& out, lolly_string_view<char> a) {
  int i, n= a.N;
  if (n == 0) return out;
  for (i= 0; i < n; i++)
    out << a.a[i];
  return out;
};

template <typename T>
lolly_string<T>&
operator<< (lolly_string<T>& a, T ch) {
  int i, na= N (a);
  a->resize (na + 1);
  a[na]= ch;
  return a;
};

template <typename T>
inline lolly_string<T>&
operator<< (lolly_string<T>& a, lolly_string<T> b) {
  int i, na= N (a), nb= N (b);
  a->resize (na + nb);
  for (i= 0; i < nb; i++)
    a[i + na]= b[i];
  return a;
};

template <typename T>
inline lolly_string<T>&
operator<< (lolly_string<T>& a, const lolly_string_view<T>& b) {
  int i, na= N (a);
  a->resize (na + b.N);
  for (i= 0; i < b.N; i++)
    a[i + na]= b.a[i];
  return a;
};

template <typename T, size_t Nb>
inline lolly_string<T>&
operator<< (lolly_string<T>& a, const T (&b)[Nb]) {
  int           i, na= N (a);
  constexpr int nb= Nb - 1;
  a->resize (na + nb);
  for (i= 0; i < nb; i++)
    a[i + na]= b[i];
  return a;
}

template <typename T>
lolly_string<T>
operator* (const lolly_string_view<T>& a, const lolly_string_view<T>& b) {
  int             i, na= a.N, nb= b.N;
  lolly_string<T> c ((int) (na + nb));
  const T *       Sa= a.a, *Sb= b.a;
  for (i= 0; i < na; i++)
    c[i]= Sa[i];
  for (i= 0; i < nb; i++)
    c[i + na]= Sb[i];
  return c;
};
template <typename T>
lolly_string<T>
operator* (const lolly_string_view<T>& a, lolly_string<T> b) {
  return a * ((lolly_string_view<T>) b);
};
template <typename T, size_t Nb>
lolly_string<T>
operator* (const lolly_string_view<T>& a, const T (&b)[Nb]) {
  int             i;
  constexpr int   nb= Nb - 1;
  lolly_string<T> c ((int) (a.N + nb));
  const T*        Sa= a.a;
  for (i= 0; i < a.N; i++)
    c[i]= Sa[i];
  for (i= 0; i < nb; i++)
    c[i + a.N]= b[i];
  return c;
};
template <typename T>
lolly_string<T>
operator* (lolly_string<T> a, lolly_string<T> b) {
  return ((lolly_string_view<T>) a) * ((lolly_string_view<T>) b);
};
template <typename T>
lolly_string<T>
operator* (lolly_string<T> a, const lolly_string_view<T>& b) {
  return ((lolly_string_view<T>) a) * b;
};
template <typename T, size_t Nb>
lolly_string<T>
operator* (lolly_string<T> a, const T (&b)[Nb]) {
  return ((lolly_string_view<T>) a) * b;
};
template <typename T, size_t Na>
lolly_string<T>
operator* (const T (&a)[Na], const lolly_string_view<T>& b) {
  int             i;
  constexpr int   na= Na - 1;
  lolly_string<T> c ((int) (na + b.N));
  const T*        Sb= b.a;
  for (i= 0; i < na; i++)
    c[i]= a[i];
  for (i= 0; i < b.N; i++)
    c[i + na]= Sb[i];
  return c;
};
template <typename T, size_t Na>
lolly_string<T>
operator* (const T (&a)[Na], lolly_string<T> b) {
  return a * ((lolly_string_view<T>) b);
};

template <typename T>
bool
operator== (const lolly_string_view<T>& a, const lolly_string_view<T>& b) {
  if (a.N != b.N) return false;
  const T *Sa= a.a, *Sb= b.a;
  for (int i= 0; i < a.N; i++)
    if (Sa[i] != Sb[i]) return false;
  return true;
};
template <typename T>
bool
operator== (const lolly_string_view<T>& a, lolly_string<T> b) {
  return a == ((lolly_string_view<T>) b);
};
template <typename T, size_t Nb>
bool
operator== (const lolly_string_view<T>& a, const T (&b)[Nb]) {
  constexpr int nb= Nb - 1;
  if (a.N != nb) return false;
  const T* Sa= a.a;
  for (int i= 0; i < nb; i++)
    if (Sa[i] != b[i]) return false;
  return true;
};
template <typename T>
bool
operator== (lolly_string<T> a, lolly_string<T> b) {
  return ((lolly_string_view<T>) a) == ((lolly_string_view<T>) b);
};
template <typename T>
bool
operator== (lolly_string<T> a, const lolly_string_view<T>& b) {
  return ((lolly_string_view<T>) a) == b;
};
template <typename T, size_t Nb>
bool
operator== (lolly_string<T> a, const T (&b)[Nb]) {
  return ((lolly_string_view<T>) a) == b;
};
template <typename T, size_t Na>
bool
operator== (const T (&a)[Na], const lolly_string_view<T>& b) {
  constexpr int na= Na - 1;
  if (na != b.N) return false;
  const T* Sb= b.a;
  for (int i= 0; i < na; i++)
    if (a[i] != Sb[i]) return false;
  return true;
};
template <typename T, size_t Na>
bool
operator== (const T (&a)[Na], lolly_string<T> b) {
  return a == ((lolly_string_view<T>) b);
};

template <typename T>
bool
operator!= (const lolly_string_view<T>& a, const lolly_string_view<T>& b) {
  if (a.N != b.N) return true;
  const T *Sa= a.a, *Sb= b.a;
  for (int i= 0; i < a.N; i++)
    if (Sa[i] != Sb[i]) return true;
  return false;
};
template <typename T>
bool
operator!= (const lolly_string_view<T>& a, lolly_string<T> b) {
  return a != ((lolly_string_view<T>) b);
};
template <typename T, size_t Nb>
bool
operator!= (const lolly_string_view<T>& a, const T (&b)[Nb]) {
  constexpr int nb= Nb - 1;
  if (a.N != nb) return true;
  const T* Sa= a.a;
  for (int i= 0; i < nb; i++)
    if (Sa[i] != b[i]) return true;
  return false;
};
template <typename T>
bool
operator!= (lolly_string<T> a, lolly_string<T> b) {
  return ((lolly_string_view<T>) a) != ((lolly_string_view<T>) b);
};
template <typename T>
bool
operator!= (lolly_string<T> a, const lolly_string_view<T>& b) {
  return ((lolly_string_view<T>) a) != b;
};
template <typename T, size_t Nb>
bool
operator!= (lolly_string<T> a, const T (&b)[Nb]) {
  return ((lolly_string_view<T>) a) != b;
};
template <typename T, size_t Na>
bool
operator!= (const T (&a)[Na], const lolly_string_view<T>& b) {
  constexpr int na= Na - 1;
  const T*      Sb= b.a;
  if (na != b.N) return true;
  for (int i= 0; i < na; i++)
    if (a[i] != Sb[i]) return true;
  return false;
};
template <typename T, size_t Na>
bool
operator!= (const T (&a)[Na], lolly_string<T> b) {
  return a != ((lolly_string_view<T>) b);
};

template <typename T>
bool
operator< (const lolly_string_view<T>& a, const lolly_string_view<T>& b) {
  int      i, na= a.N, nb= b.N, nmin= min (na, nb);
  const T *Sa= a.a, *Sb= b.a;
  for (i= 0; i < nmin; i++) {
    if (Sa[i] < Sb[i]) return true;
    if (Sb[i] < Sa[i]) return false;
  }
  return na < nb;
};
template <typename T>
bool
operator< (const lolly_string_view<T>& a, lolly_string<T> b) {
  return a < ((lolly_string_view<T>) b);
};
template <typename T, size_t Nb>
bool
operator< (const lolly_string_view<T>& a, const T (&b)[Nb]) {
  constexpr int nb= Nb - 1;
  int           i, na= a.N, nmin= min (na, nb);
  const T*      Sa= a.a;
  for (i= 0; i < nmin; i++) {
    if (Sa[i] < b[i]) return true;
    if (b[i] < Sa[i]) return false;
  }
  return na < nb;
};
template <typename T>
bool
operator< (lolly_string<T> a, lolly_string<T> b) {
  return ((lolly_string_view<T>) a) < ((lolly_string_view<T>) b);
};
template <typename T>
bool
operator< (lolly_string<T> a, const lolly_string_view<T>& b) {
  return ((lolly_string_view<T>) a) < b;
};
template <typename T, size_t Nb>
bool
operator< (lolly_string<T> a, const T (&b)[Nb]) {
  return ((lolly_string_view<T>) a) < b;
};
template <typename T, size_t Na>
bool
operator< (const T (&a)[Na], const lolly_string_view<T>& b) {
  constexpr int na= Na - 1;
  int           i, nb= b.N, nmin= min (na, nb);
  const T*      Sb= b.a;
  for (i= 0; i < nmin; i++) {
    if (a[i] < Sb[i]) return true;
    if (Sb[i] < a[i]) return false;
  }
  return na < nb;
};
template <typename T, size_t Na>
bool
operator< (const T (&a)[Na], lolly_string<T> b) {
  return a < ((lolly_string_view<T>) b);
};

template <typename T>
bool
operator<= (const lolly_string_view<T>& a, const lolly_string_view<T>& b) {
  int      i, na= a.N, nb= b.N, nmin= min (na, nb);
  const T *Sa= a.a, *Sb= b.a;
  for (i= 0; i < nmin; i++) {
    if (Sa[i] < Sb[i]) return true;
    if (Sb[i] < Sa[i]) return false;
  }
  return na <= nb;
};
template <typename T>
bool
operator<= (const lolly_string_view<T>& a, lolly_string<T> b) {
  return a <= ((lolly_string_view<T>) b);
};
template <typename T, size_t Nb>
bool
operator<= (const lolly_string_view<T>& a, const T (&b)[Nb]) {
  constexpr int nb= Nb - 1;
  int           i, na= a.N, nmin= min (na, nb);
  const T*      Sa= a.a;
  for (i= 0; i < nmin; i++) {
    if (Sa[i] < b[i]) return true;
    if (b[i] < Sa[i]) return false;
  }
  return na <= nb;
};
template <typename T>
bool
operator<= (lolly_string<T> a, lolly_string<T> b) {
  return ((lolly_string_view<T>) a) <= ((lolly_string_view<T>) b);
};
template <typename T>
bool
operator<= (lolly_string<T> a, const lolly_string_view<T>& b) {
  return ((lolly_string_view<T>) a) <= b;
};
template <typename T, size_t Nb>
bool
operator<= (lolly_string<T> a, const T (&b)[Nb]) {
  return ((lolly_string_view<T>) a) <= b;
};
template <typename T, size_t Na>
bool
operator<= (const T (&a)[Na], const lolly_string_view<T>& b) {
  constexpr int na= Na - 1;
  int           i, nb= b.N, nmin= min (na, nb);
  const T*      Sb= b.a;
  for (i= 0; i < nmin; i++) {
    if (a[i] < Sb[i]) return true;
    if (Sb[i] < a[i]) return false;
  }
  return na <= nb;
};
template <typename T, size_t Na>
bool
operator<= (const T (&a)[Na], lolly_string<T> b) {
  return a <= ((lolly_string_view<T>) b);
};

template <typename T>
int
hash (lolly_string<T> s) {
  int i, h= 0, n= N (s);
  for (i= 0; i < n; i++) {
    h= (h << 9) + (h >> 23);
    h= h + ((int) s[i]);
  }
  return h;
};
template <typename T>
int
hash (lolly_string_view<T> s) {
  int i, h= 0, n= s.N;
  for (i= 0; i < n; i++) {
    h= (h << 9) + (h >> 23);
    h= h + ((int) s.a[i]);
  }
  return h;
};
} // namespace data
} // namespace lolly
