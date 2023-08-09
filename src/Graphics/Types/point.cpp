
/******************************************************************************
 * MODULE     : point.cpp
 * DESCRIPTION: points
 * COPYRIGHT  : (C) 2003  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "point.hpp"
#include "math_util.hpp"
#include "tree_helper.hpp"

point
operator- (point p) {
  int   i, n= N (p);
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= -p[i];
  return r;
}

point
operator+ (point p1, point p2) {
  int   i, n= min (N (p1), N (p2));
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= p1[i] + p2[i];
  return r;
}

point
operator- (point p1, point p2) {
  int   i, n= min (N (p1), N (p2));
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= p1[i] - p2[i];
  return r;
}

point
operator* (double x, point p) {
  int   i, n= N (p);
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= x * p[i];
  return r;
}

point
operator* (point p1, point p2) {
  int   i, n= min (N (p1), N (p2));
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= p1[i] * p2[i];
  return r;
}

point
operator/ (point p, double x) {
  int   i, n= N (p);
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= p[i] / x;
  return r;
}

point
operator/ (point p1, point p2) {
  int   i, n= min (N (p1), N (p2));
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= p1[i] / p2[i];
  return r;
}

bool
operator== (point p1, point p2) {
  if (N (p1) != N (p2)) return false;
  int i, n= N (p1);
  for (i= 0; i < n; i++)
    if (!fnull (p1[i] - p2[i], 1e-6)) return false;
  return true;
}

point
abs (point p) {
  int   i, n= N (p);
  point r (n);
  for (i= 0; i < n; i++)
    r[i]= fabs (p[i]);
  return r;
}

double
min (point p) {
  int i, n= N (p);
  ASSERT (N (p) > 0, "non empty point expected");
  double r= p[0];
  for (i= 1; i < n; i++)
    r= min (r, p[i]);
  return r;
}

double
max (point p) {
  int i, n= N (p);
  ASSERT (N (p) > 0, "non empty point expected");
  double r= p[0];
  for (i= 1; i < n; i++)
    r= max (r, p[i]);
  return r;
}

bool
is_point (tree t) {
  return L (t) == _POINT;
}

point
as_point (tree t) {
  if (!is_tuple (t) && !is_point (t)) return point ();
  else {
    int   i, n= N (t);
    point p (n);
    for (i= 0; i < n; i++)
      p[i]= as_double (t[i]);
    return p;
  }
}

tree
as_tree (point p) {
  int  i, n= N (p);
  tree t (_POINT, n);
  for (i= 0; i < n; i++)
    t[i]= as_string (p[i]);
  return t;
}

double
inner (point p1, point p2) {
  int    i, n= min (N (p1), N (p2));
  double r= 0;
  for (i= 0; i < n; i++)
    r+= p1[i] * p2[i];
  return r;
}

static point
mult (double re, double im, point p) {
  if (N (p) == 0) p= point (0.0, 0.0);
  if (N (p) == 1) p= point (p[0], 0.0);
  return point (re * p[0] - im * p[1], re * p[1] + im * p[0]);
}

point
rotate_2D (point p, point o, double angle) {
  return mult (cos (angle), sin (angle), p - o) + o;
}

point
slanted (point p, double slant) {
  return point (p[0] + p[1] * slant, p[1]);
}

double
norm (point p) {
  return sqrt (inner (p, p));
}

double
arg (point p) {
  double n= norm (p);
  p       = p / n;
  if (p[1] < 0) return 2 * tm_PI - acos (p[0]);
  else return acos (p[0]);
}

point
proj (axis ax, point p) {
  int   i, n= min (N (ax.p0), N (ax.p1));
  point a (n), b (n);
  for (i= 0; i < n; i++) {
    a[i]= ax.p1[i] - ax.p0[i];
    b[i]= ax.p0[i];
  }
  if (norm (a) < 1.0e-6) return ax.p0;
  else return b + ((inner (a, p) - inner (a, b)) / inner (a, a)) * a;
}

double
dist (axis ax, point p) {
  return norm (p - proj (ax, p));
}

double
seg_dist (axis ax, point p) {
  point ab= ax.p1 - ax.p0;
  point ba= ax.p0 - ax.p1;
  point ap= p - ax.p0;
  point bp= p - ax.p1;
  if (inner (ab, ap) > 0 && inner (ba, bp) > 0) return dist (ax, p);
  else return min (norm (ap), norm (bp));
}

double
seg_dist (point p1, point p2, point p) {
  axis ax;
  ax.p0= p1;
  ax.p1= p2;
  return seg_dist (ax, p);
}

bool
collinear (point p1, point p2) {
  return fnull (fabs (inner (p1, p2)) - norm (p1) * norm (p2), 1.0e-6);
}

bool
linearly_dependent (point p1, point p2, point p3) {
  return fnull (norm (p1 - p2), 1e-6) || fnull (norm (p2 - p3), 1e-6) ||
         fnull (norm (p3 - p1), 1e-6) || collinear (p2 - p1, p3 - p1);
}

bool
orthogonalize (point& i, point& j, point p1, point p2, point p3) {
  if (linearly_dependent (p1, p2, p3)) return false;
  i= (p2 - p1) / norm (p2 - p1);
  j= (p3 - p1) - inner (p3 - p1, i) * i;
  j= j / norm (j);
  return true;
}

axis
midperp (point p1, point p2, point p3) {
  axis a;
  if (linearly_dependent (p1, p2, p3)) a.p0= a.p1= point (0);
  else {
    point i, j;
    orthogonalize (i, j, p1, p2, p3);
    a.p0= (p1 + p2) / 2;
    a.p1= a.p0 + j;
  }
  return a;
}

point
intersection (axis A, axis B) {
  point i, j;
  if (!orthogonalize (i, j, A.p0, A.p1, B.p0)) {
    if (orthogonalize (i, j, A.p0, A.p1, B.p1)) return B.p0;
    else return point (0);
  }
  point a (2), b (2), u (2), v (2), p (2);
  a[0]= a[1]= 0;
  u[0]      = inner (A.p1 - A.p0, i);
  u[1]      = inner (A.p1 - A.p0, j);
  b[0]      = inner (B.p0 - A.p0, i);
  b[1]      = inner (B.p0 - A.p0, j);
  v[0]      = inner (B.p1 - B.p0, i);
  v[1]      = inner (B.p1 - B.p0, j);
  if (fnull (norm (u), 1e-6) || fnull (norm (v), 1e-6) || collinear (u, v))
    return point (0);
  else {
    double t;
    t= (v[0] * (b[1] - a[1]) + v[1] * (a[0] - b[0])) /
       (v[0] * u[1] - v[1] * u[0]);
    return A.p0 + t * (u[0] * i + u[1] * j);
  }
}

bool
inside_rectangle (point p, point p1, point p2) {
  return p[0] >= p1[0] && p[1] >= p1[1] && p[0] <= p2[0] && p[1] <= p2[1];
}
