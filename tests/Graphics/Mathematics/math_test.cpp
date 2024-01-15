
/******************************************************************************
* MODULE     : test_math.cpp
* DESCRIPTION: Test mathematical functions
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "math_tree.hpp"
#include "vector.hpp"
#include "matrix.hpp"
#include "polynomial.hpp"

void
test_math () {
  {
    tree t= add ("x", mul (pow ("y", "2"), "z"));
    cout << "t\t= " << as_math_string (t) << "\n";
    cout << "t*t\t= " << as_math_string (mul (t, t)) << "\n";
    cout << "[t,t]\t= " << vector<tree> (t, t) << "\n";
    cout << "\n";
  }

  {
    vector<double> v (1.0, 2.0, 3.0);
    cout << "v\t= " << v << "\n";
    cout << "exp v\t= " << exp (v) << "\n";
    cout << "|v|\t= " << norm (v) << "\n";
    cout << "\n";
  }

  {
    vector<double> v (1.0, 2.0);
    matrix<double> m (1.0, 2, 2);
    m (0, 1)= 4;
    cout << "m\t= " << m << "\n";
    cout << "v\t= " << v << "\n";
    cout << "m*m\t= " << m*m << "\n";
    cout << "m*v\t= " << m*v << "\n";
    cout << "\n";
  }

  {
    polynomial<double> p (1.0, 2.0, 3.0);
    polynomial<double> q= p*p;
    cout << "p\t= " << p << "\n";
    cout << "q\t= " << q << "\n";
    cout << "p-p\t= " << p-p << "\n";
    cout << "p*p\t= " << p*p << "\n";
    cout << "p*p+p\t= " << p*p + p << "\n";
    cout << "p*p*p\t= " << p*p*p << "\n";
    cout << "p(3)\t= " << p (3.0) << "\n";
    cout << "q(3)\t= " << q (3.0, 0) << "\n";
    cout << "q'(3)\t= " << q (3.0, 1) << "\n";
    cout << "q''(3)\t= " << q (3.0, 2) << "\n";
    cout << "\n";
  }
}

int main(){
    test_math();
    return 0;
}
