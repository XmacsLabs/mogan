
/******************************************************************************
 * MODULE     : adjust_termes.cpp
 * DESCRIPTION: Microtypography for the TeX Gyre Termes font
 * COPYRIGHT  : (C) 2017  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "font.hpp"

/******************************************************************************
 * Table initialization
 ******************************************************************************/

void
lsub_adjust_termes (hashmap<string, double>& t) {
  adjust_pair (t, "T", 0.02);
  adjust_pair (t, "V", 0.02);
  adjust_pair (t, "Y", 0.02);
  adjust_pair (t, "<tau>", 0.03);
  adjust_pair (t, "<chi>", 0.1);
  adjust_pair (t, "<Upsilon>", 0.03);
  adjust_pair (t, "<Phi>", -0.03);
  adjust_pair (t, "<bbb-A>", 0.03);
  adjust_pair (t, "<bbb-T>", 0.08);
  adjust_pair (t, "<bbb-U>", 0.05);
  adjust_pair (t, "<bbb-V>", 0.08);
  adjust_pair (t, "<bbb-W>", 0.08);
  adjust_pair (t, "<bbb-Y>", 0.08);
  adjust_pair (t, "<bbb-X>", 0.03);
  adjust_pair (t, "<bbb-u>", 0.02);
  adjust_pair (t, "<bbb-v>", 0.05);
  adjust_pair (t, "<bbb-w>", 0.03);
  adjust_pair (t, "<vee>", -0.05);
  adjust_pair (t, "<curlyvee>", -0.07);
  adjust_integral (t, "1", -0.07);
  adjust_integral (t, "2", -0.1);
  adjust_contour_integral (t, "1", -0.1);
  adjust_contour_integral (t, "2", -0.07);
}

void
lsup_adjust_termes (hashmap<string, double>& t) {
  adjust_pair (t, "<beta>", 0.05);
  adjust_pair (t, "<rho>", 0.02);
  adjust_pair (t, "<chi>", 0.1);
  adjust_pair (t, "<omega>", 0.02);
  adjust_pair (t, "<Alpha>", 0.02);
  adjust_pair (t, "<Delta>", 0.02);
  adjust_pair (t, "<Lambda>", 0.02);
  adjust_pair (t, "<cal-A>", 0.08);
  adjust_pair (t, "<cal-M>", 0.08);
  adjust_pair (t, "<cal-N>", 0.08);
  adjust_pair (t, "<cal-S>", 0.05);
  adjust_pair (t, "<bbb-A>", 0.05);
  adjust_pair (t, "<bbb-j>", 0.03);
  adjust_integral (t, "1", 0.13);
  adjust_integral (t, "2", 0.15);
  adjust_contour_integral (t, "1", 0.07);
  adjust_contour_integral (t, "2", 0.13);
}

void
rsub_adjust_termes (hashmap<string, double>& t) {
  adjust_pair (t, "A", 0.05);
  adjust_pair (t, "I", 0.05);
  adjust_pair (t, "K", 0.05);
  adjust_pair (t, "M", 0.05);
  adjust_pair (t, "P", -0.03);
  adjust_pair (t, "Q", 0.05);
  adjust_pair (t, "R", 0.05);
  adjust_pair (t, "T", -0.05);
  adjust_pair (t, "V", -0.05);
  adjust_pair (t, "W", -0.05);
  adjust_pair (t, "Y", -0.05);
  adjust_pair (t, "<up-T>", -0.05);
  adjust_pair (t, "<up-V>", -0.05);
  adjust_pair (t, "<up-W>", -0.02);
  adjust_pair (t, "<up-Y>", -0.05);
  adjust_pair (t, "a", 0.03);
  adjust_pair (t, "d", 0.03);
  adjust_pair (t, "g", 0.05);
  adjust_pair (t, "h", 0.03);
  adjust_pair (t, "i", 0.03);
  adjust_pair (t, "k", 0.03);
  adjust_pair (t, "l", 0.03);
  adjust_pair (t, "m", 0.03);
  adjust_pair (t, "n", 0.03);
  adjust_pair (t, "q", 0.05);
  adjust_pair (t, "t", 0.03);
  adjust_pair (t, "u", 0.03);
  adjust_pair (t, "v", -0.05);
  adjust_pair (t, "w", -0.05);
  adjust_pair (t, "x", 0.03);
  adjust_pair (t, "y", -0.05);
  adjust_pair (t, "z", 0.03);
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "/", -0.02);
  adjust_pair (t, "<Alpha>", 0.02);
  adjust_pair (t, "<Delta>", 0.01);
  adjust_pair (t, "<Gamma>", -0.02);
  adjust_pair (t, "<Theta>", -0.04);
  adjust_pair (t, "<Iota>", 0.02);
  adjust_pair (t, "<Kappa>", 0.02);
  adjust_pair (t, "<Lambda>", 0.02);
  adjust_pair (t, "<Nu>", -0.03);
  adjust_pair (t, "<Omicron>", -0.02);
  adjust_pair (t, "<Pi>", 0.02);
  adjust_pair (t, "<Rho>", -0.02);
  adjust_pair (t, "<Tau>", -0.02);
  adjust_pair (t, "<Upsilon>", -0.05);
  adjust_pair (t, "<Chi>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.05);
  adjust_pair (t, "<Backsigma>", 0.05);
  adjust_char (t, "<#1D6FC>", -0.03);
  adjust_char (t, "<b-alpha>", -0.03);
  adjust_char (t, "<#1D6FD>", -0.03);
  adjust_char (t, "<b-beta>", -0.03);
  adjust_char (t, "<#1D6FE>", -0.1);
  adjust_char (t, "<b-gamma>", -0.1);
  adjust_char (t, "<#1D701>", -0.05);
  adjust_char (t, "<b-zeta>", -0.05);
  adjust_char (t, "<#1D703>", -0.02);
  adjust_char (t, "<b-theta>", -0.02);
  adjust_char (t, "<#1D706>", -0.02);
  adjust_char (t, "<b-kappa>", -0.02);
  adjust_char (t, "<#1D708>", -0.05);
  adjust_char (t, "<b-nu>", -0.05);
  adjust_char (t, "<#1D709>", -0.05);
  adjust_char (t, "<b-xi>", -0.05);
  adjust_char (t, "<#1D70B>", -0.03);
  adjust_char (t, "<b-pi>", -0.03);
  adjust_char (t, "<#1D70C>", -0.03);
  adjust_char (t, "<b-rho>", -0.03);
  adjust_char (t, "<#1D70E>", -0.05);
  adjust_char (t, "<b-sigma>", -0.05);
  adjust_char (t, "<#1D70F>", -0.04);
  adjust_char (t, "<b-tau>", -0.04);
  adjust_char (t, "<#1D712>", -0.07);
  adjust_char (t, "<b-chi>", -0.07);
  adjust_char (t, "<#1D713>", -0.07);
  adjust_char (t, "<b-psi>", -0.07);
  adjust_char (t, "<#1D714>", -0.02);
  adjust_char (t, "<b-omega>", -0.02);
  adjust_char (t, "<#1D716>", -0.05);
  adjust_char (t, "<b-varepsilon>", -0.05);
  adjust_char (t, "<#1D71B>", -0.08);
  adjust_char (t, "<b-varpi>", -0.08);
  adjust_char (t, "<gamma>", -0.03);
  adjust_char (t, "<b-up-gamma>", -0.03);
  adjust_char (t, "<psi>", -0.05);
  adjust_char (t, "<b-up-psi>", -0.05);
  adjust_pair (t, "<cal-A>", 0.03);
  adjust_pair (t, "<cal-C>", -0.1);
  adjust_pair (t, "<cal-F>", -0.2);
  adjust_pair (t, "<cal-G>", -0.1);
  adjust_pair (t, "<cal-H>", 0.05);
  adjust_pair (t, "<cal-I>", -0.12);
  adjust_pair (t, "<cal-J>", -0.05);
  adjust_pair (t, "<cal-K>", -0.05);
  adjust_pair (t, "<cal-M>", 0.03);
  adjust_pair (t, "<cal-N>", -0.3);
  adjust_pair (t, "<cal-P>", -0.03);
  adjust_pair (t, "<cal-S>", -0.1);
  adjust_pair (t, "<cal-T>", -0.3);
  adjust_pair (t, "<cal-V>", -0.3);
  adjust_pair (t, "<cal-W>", -0.3);
  adjust_pair (t, "<cal-X>", -0.1);
  adjust_pair (t, "<cal-Y>", -0.05);
  adjust_pair (t, "<cal-a>", -0.05);
  adjust_pair (t, "<cal-c>", -0.02);
  adjust_pair (t, "<cal-d>", -0.08);
  adjust_pair (t, "<cal-e>", -0.03);
  adjust_pair (t, "<cal-f>", -0.12);
  adjust_pair (t, "<cal-g>", -0.05);
  adjust_pair (t, "<cal-h>", -0.05);
  adjust_pair (t, "<cal-i>", -0.03);
  adjust_pair (t, "<cal-j>", -0.05);
  adjust_pair (t, "<cal-l>", -0.1);
  adjust_pair (t, "<cal-p>", -0.03);
  adjust_pair (t, "<cal-q>", -0.02);
  adjust_pair (t, "<cal-t>", -0.03);
  adjust_pair (t, "<cal-x>", -0.02);
  adjust_pair (t, "<cal-y>", -0.02);
  adjust_pair (t, "<cal-z>", -0.05);
  adjust_pair (t, "<bbb-A>", -0.03);
  adjust_pair (t, "<bbb-D>", -0.02);
  adjust_pair (t, "<bbb-F>", -0.07);
  adjust_pair (t, "<bbb-H>", 0.02);
  adjust_pair (t, "<bbb-I>", 0.03);
  adjust_pair (t, "<bbb-M>", 0.02);
  adjust_pair (t, "<bbb-P>", -0.03);
  adjust_pair (t, "<bbb-Q>", 0.05);
  adjust_pair (t, "<bbb-S>", 0.03);
  adjust_pair (t, "<bbb-T>", -0.05);
  adjust_pair (t, "<bbb-U>", -0.05);
  adjust_pair (t, "<bbb-V>", -0.12);
  adjust_pair (t, "<bbb-W>", -0.12);
  adjust_pair (t, "<bbb-Y>", -0.15);
  adjust_pair (t, "<bbb-Z>", 0.03);
  adjust_pair (t, "<bbb-f>", -0.03);
  adjust_pair (t, "<bbb-j>", 0.02);
  adjust_pair (t, "<bbb-v>", -0.05);
  adjust_pair (t, "<bbb-w>", -0.05);
  adjust_pair (t, "<bbb-y>", -0.05);
  adjust_pair (t, "<partial>", -0.03);
  adjust_integral (t, "1", -0.18);
  adjust_integral (t, "2", -0.17);
  adjust_contour_integral (t, "1", -0.05);
  adjust_contour_integral (t, "2", -0.13);
}

void
rsup_adjust_termes (hashmap<string, double>& t) {
  adjust_pair (t, "A", -0.05);
  adjust_pair (t, "L", -0.05);
  adjust_pair (t, "g", 0.02);
  adjust_pair (t, "r", 0.02);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "?", 0.05);
  adjust_pair (t, "/", 0.05);
  adjust_pair (t, "0", 0.03);
  adjust_pair (t, "<Alpha>", -0.05);
  adjust_pair (t, "<Gamma>", 0.02);
  adjust_pair (t, "<Delta>", -0.02);
  adjust_pair (t, "<Epsilon>", -0.02);
  adjust_pair (t, "<Lambda>", -0.05);
  adjust_pair (t, "<Rho>", 0.02);
  adjust_pair (t, "<Phi>", 0.02);
  adjust_pair (t, "<Psi>", 0.01);
  adjust_pair (t, "<Tau>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.04);
  adjust_pair (t, "<Backsigma>", 0.04);
  adjust_pair (t, "<Mho>", 0.02);
  adjust_char (t, "<#1D6FD>", 0.02);
  adjust_char (t, "<b-beta>", 0.02);
  adjust_char (t, "<#1D701>", 0.03);
  adjust_char (t, "<b-zeta>", 0.03);
  adjust_char (t, "<#1D703>", 0.03);
  adjust_char (t, "<b-theta>", 0.03);
  adjust_char (t, "<#1D709>", 0.03);
  adjust_char (t, "<b-xi>", 0.03);
  adjust_char (t, "<#1D70D>", 0.02);
  adjust_char (t, "<b-varsigma>", 0.02);
  adjust_pair (t, "<mho>", 0.02);
  adjust_char (t, "<beta>", -0.03);
  adjust_char (t, "<b-up-beta>", -0.03);
  adjust_char (t, "<zeta>", -0.05);
  adjust_char (t, "<b-up-zeta>", -0.05);
  adjust_char (t, "<eta>", -0.03);
  adjust_char (t, "<b-up-eta>", -0.03);
  adjust_char (t, "<theta>", -0.03);
  adjust_char (t, "<b-up-theta>", -0.03);
  adjust_char (t, "<iota>", -0.05);
  adjust_char (t, "<b-up-iota>", -0.05);
  adjust_char (t, "<lambda>", -0.17);
  adjust_char (t, "<b-up-lambda>", -0.17);
  adjust_char (t, "<mu>", -0.07);
  adjust_char (t, "<b-up-mu>", -0.07);
  adjust_char (t, "<xi>", -0.01);
  adjust_char (t, "<b-up-xi>", -0.01);
  adjust_char (t, "<omicron>", -0.03);
  adjust_char (t, "<b-up-omicron>", -0.03);
  adjust_char (t, "<psi>", -0.02);
  adjust_char (t, "<b-up-psi>", -0.02);
  adjust_char (t, "<chi>", -0.05);
  adjust_char (t, "<b-up-chi>", -0.05);
  adjust_char (t, "<omega>", -0.03);
  adjust_char (t, "<b-up-omega>", -0.03);
  adjust_char (t, "<varepsilon>", -0.03);
  adjust_char (t, "<b-up-varepsilon>", -0.03);
  adjust_char (t, "<vartheta>", -0.02);
  adjust_char (t, "<b-up-vartheta>", -0.02);
  adjust_pair (t, "<cal-I>", -0.03);
  adjust_pair (t, "<bbb-A>", -0.05);
  adjust_pair (t, "<bbb-D>", 0.02);
  adjust_pair (t, "<bbb-H>", 0.02);
  adjust_pair (t, "<bbb-J>", 0.01);
  adjust_pair (t, "<bbb-K>", -0.03);
  adjust_pair (t, "<bbb-L>", -0.05);
  adjust_pair (t, "<bbb-M>", 0.01);
  adjust_pair (t, "<bbb-P>", 0.02);
  adjust_pair (t, "<bbb-R>", -0.03);
  adjust_pair (t, "<bbb-a>", -0.03);
  adjust_pair (t, "<bbb-d>", -0.03);
  adjust_pair (t, "<bbb-h>", -0.03);
  adjust_pair (t, "<bbb-i>", -0.02);
  adjust_pair (t, "<bbb-j>", 0.02);
  adjust_pair (t, "<bbb-k>", -0.03);
  adjust_pair (t, "<bbb-l>", -0.03);
  adjust_pair (t, "<bbb-m>", -0.03);
  adjust_pair (t, "<bbb-n>", -0.03);
  adjust_pair (t, "<bbb-q>", -0.03);
  adjust_pair (t, "<frak-Q>", -0.02);
  adjust_pair (t, "<frak-U>", -0.03);
  adjust_pair (t, "<frak-a>", -0.03);
  adjust_pair (t, "<frak-b>", -0.01);
  adjust_pair (t, "<frak-i>", -0.02);
  adjust_pair (t, "<frak-j>", 0.01);
  adjust_pair (t, "<frak-l>", -0.01);
  adjust_pair (t, "<frak-p>", -0.01);
  adjust_pair (t, "<frak-u>", -0.03);
  adjust_pair (t, "<frak-v>", -0.02);
  adjust_pair (t, "<frak-w>", -0.02);
  adjust_pair (t, "<frak-y>", -0.01);
  adjust_pair (t, "<partial>", -0.03);
  adjust_integral (t, "1", -0.05);
  adjust_integral (t, "2", -0.03);
  adjust_contour_integral (t, "1", -0.03);
  adjust_contour_integral (t, "2", -0.03);
}

void
above_adjust_termes (hashmap<string, double>& t) {
  adjust_pair (t, "b", -0.02);
  adjust_pair (t, "d", 0.06);
  adjust_pair (t, "f", -0.04);
  adjust_pair (t, "h", -0.02);
  adjust_pair (t, "k", -0.02);
  adjust_pair (t, "m", -0.02);
  adjust_pair (t, "n", -0.02);
  adjust_pair (t, "r", -0.04);
  adjust_pair (t, "s", -0.04);
  adjust_pair (t, "x", -0.04);
  adjust_pair (t, "z", -0.04);
  adjust_pair (t, "A", 0.04);
  adjust_pair (t, "I", -0.02);
  adjust_pair (t, "J", 0.06);
  adjust_pair (t, "M", -0.04);
  adjust_pair (t, "N", -0.04);
  adjust_pair (t, "T", -0.04);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "V", -0.08);
  adjust_pair (t, "W", -0.06);
  adjust_pair (t, "Y", -0.08);
  adjust_pair (t, "<beta>", 0.1);
  adjust_pair (t, "<gamma>", -0.04);
  adjust_pair (t, "<varepsilon>", 0.02);
  adjust_pair (t, "<zeta>", 0.02);
  adjust_pair (t, "<kappa>", -0.02);
  adjust_char (t, "<lambda>", 0.14);
  adjust_pair (t, "<mu>", 0.06);
  adjust_pair (t, "<phi>", 0.04);
  adjust_pair (t, "<omicron>", 0.04);
  adjust_pair (t, "<rho>", 0.04);
  adjust_pair (t, "<sigma>", -0.06);
  adjust_pair (t, "<tau>", -0.02);
  adjust_pair (t, "<psi>", -0.04);
  adjust_pair (t, "<chi>", 0.06);
  adjust_pair (t, "<cal-d>", 0.16);
  adjust_pair (t, "<cal-f>", 0.16);
  adjust_pair (t, "<cal-h>", 0.1);
  adjust_pair (t, "<cal-i>", 0.06);
  adjust_pair (t, "<cal-j>", 0.14);
  adjust_pair (t, "<cal-k>", 0.1);
  adjust_pair (t, "<cal-l>", 0.1);
  adjust_pair (t, "<cal-t>", 0.06);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.08);
  adjust_pair (t, "<cal-A>", 0.1);
  adjust_pair (t, "<cal-B>", 0.04);
  adjust_pair (t, "<cal-D>", 0.04);
  adjust_pair (t, "<cal-I>", 0.02);
  adjust_pair (t, "<cal-J>", -0.02);
  adjust_pair (t, "<cal-L>", 0.06);
  adjust_pair (t, "<cal-M>", 0.1);
  adjust_pair (t, "<cal-N>", 0.04);
  adjust_pair (t, "<cal-P>", 0.04);
  adjust_pair (t, "<cal-R>", 0.04);
  adjust_pair (t, "<cal-S>", 0.06);
  above_adjust_frak (t, 1.0);
  adjust_pair (t, "<frak-d>", -0.04);
  adjust_pair (t, "<frak-t>", 0.005);
  adjust_pair (t, "<frak-L>", -0.02);
  adjust_pair (t, "<frak-M>", 0.02);
  adjust_pair (t, "<frak-N>", 0.02);
  above_adjust_bbb (t, 1.0);
  adjust_pair (t, "<bbb-f>", 0.04);
  adjust_pair (t, "<bbb-g>", 0.02);
  adjust_pair (t, "<bbb-h>", -0.02);
  adjust_pair (t, "<bbb-k>", -0.02);
  adjust_pair (t, "<bbb-l>", 0.02);
  adjust_pair (t, "<bbb-m>", -0.01);
  adjust_pair (t, "<bbb-n>", -0.02);
  adjust_pair (t, "<bbb-o>", 0.01);
  adjust_pair (t, "<bbb-t>", 0.01);
  adjust_pair (t, "<bbb-C>", 0.04);
  adjust_pair (t, "<bbb-J>", -0.02);
  adjust_pair (t, "<bbb-K>", -0.02);
  adjust_pair (t, "<bbb-L>", -0.01);
  adjust_pair (t, "<bbb-R>", -0.04);
  adjust_pair (t, "1", -0.01);
  adjust_pair (t, "2", -0.02);
  adjust_pair (t, "3", -0.01);
  adjust_pair (t, "4", 0.02);
  adjust_pair (t, "5", 0.02);
  adjust_pair (t, "6", 0.02);
  adjust_pair (t, "9", -0.015);
}

/******************************************************************************
 * Interface
 ******************************************************************************/

static hashmap<string, double> lsub_termes (0.0);
static hashmap<string, double> lsup_termes (0.0);
static hashmap<string, double> rsub_termes (0.0);
static hashmap<string, double> rsup_termes (0.0);
static hashmap<string, double> above_termes (0.0);

hashmap<string, double>
lsub_termes_table () {
  if (N (lsub_termes) == 0) {
    lsub_adjust_std (lsub_termes);
    lsub_adjust_termes (lsub_termes);
  }
  return lsub_termes;
}

hashmap<string, double>
lsup_termes_table () {
  if (N (lsup_termes) == 0) {
    lsup_adjust_std (lsup_termes);
    lsup_adjust_termes (lsup_termes);
  }
  return lsup_termes;
}

hashmap<string, double>
rsub_termes_table () {
  if (N (rsub_termes) == 0) {
    rsub_adjust_std (rsub_termes);
    rsub_adjust_termes (rsub_termes);
  }
  return rsub_termes;
}

hashmap<string, double>
rsup_termes_table () {
  if (N (rsup_termes) == 0) {
    rsup_adjust_std (rsup_termes);
    rsup_adjust_termes (rsup_termes);
  }
  return rsup_termes;
}

hashmap<string, double>
above_termes_table () {
  if (N (above_termes) == 0) above_adjust_termes (above_termes);
  return above_termes;
}

/******************************************************************************
 * Table initialization
 ******************************************************************************/

void
lsub_adjust_termes_italic (hashmap<string, double>& t) {
  adjust_pair (t, "p", 0.03);
  adjust_pair (t, "U", 0.03);
}

void
lsup_adjust_termes_italic (hashmap<string, double>& t) {
  adjust_pair (t, "b", 0.02);
  adjust_pair (t, "B", 0.02);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "E", 0.02);
  adjust_pair (t, "F", 0.02);
  adjust_pair (t, "H", 0.02);
  adjust_pair (t, "I", 0.02);
  adjust_pair (t, "J", 0.02);
  adjust_pair (t, "K", 0.02);
  adjust_pair (t, "L", 0.02);
  adjust_pair (t, "M", 0.02);
  adjust_pair (t, "N", 0.02);
  adjust_pair (t, "P", 0.02);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "S", 0.02);
  adjust_pair (t, "U", 0.02);
  adjust_pair (t, "V", 0.02);
  adjust_pair (t, "W", 0.02);
}

void
rsub_adjust_termes_italic (hashmap<string, double>& t) {
  adjust_pair (t, "A", 0.03);
  adjust_pair (t, "B", 0.01);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "F", -0.05);
  adjust_pair (t, "G", 0.03);
  adjust_pair (t, "I", 0.03);
  adjust_pair (t, "J", 0.02);
  adjust_pair (t, "L", 0.01);
  adjust_pair (t, "N", -0.02);
  adjust_pair (t, "O", 0.01);
  adjust_pair (t, "P", -0.02);
  adjust_pair (t, "Q", 0.02);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "S", 0.03);
  adjust_pair (t, "T", -0.02);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "Z", 0.05);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, string (c), 0.02);
  adjust_pair (t, "h", 0.02);
  adjust_pair (t, "l", 0.02);
  adjust_pair (t, "m", 0.02);
  adjust_pair (t, "n", 0.01);
  adjust_pair (t, "x", 0.02);
  adjust_pair (t, "z", 0.02);
}

void
rsup_adjust_termes_italic (hashmap<string, double>& t) {
  adjust_pair (t, "B", 0.03);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "H", -0.02);
  adjust_pair (t, "I", -0.02);
  adjust_pair (t, "P", 0.03);
  adjust_pair (t, "Q", 0.01);
  adjust_pair (t, "R", 0.03);
  adjust_pair (t, "W", 0.02);
  adjust_pair (t, "Z", 0.02);
  adjust_pair (t, "a", 0.01);
  adjust_pair (t, "b", 0.02);
  adjust_pair (t, "c", 0.02);
  adjust_pair (t, "d", 0.01);
  adjust_pair (t, "e", 0.02);
  adjust_pair (t, "i", 0.02);
  adjust_pair (t, "l", 0.02);
  adjust_pair (t, "q", 0.01);
  adjust_pair (t, "r", 0.02);
  adjust_pair (t, "s", 0.02);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "x", 0.02);
}

void
above_adjust_termes_italic (hashmap<string, double>& t) {
  adjust_pair (t, "b", -0.02);
  adjust_pair (t, "d", 0.06);
  adjust_pair (t, "f", -0.04);
  adjust_pair (t, "h", -0.02);
  adjust_pair (t, "k", -0.02);
  adjust_pair (t, "m", -0.02);
  adjust_pair (t, "n", -0.02);
  adjust_pair (t, "r", -0.04);
  adjust_pair (t, "s", -0.04);
  adjust_pair (t, "x", -0.04);
  adjust_pair (t, "z", -0.04);
  adjust_pair (t, "A", 0.04);
  adjust_pair (t, "I", -0.02);
  adjust_pair (t, "J", 0.06);
  adjust_pair (t, "M", -0.04);
  adjust_pair (t, "N", -0.04);
  adjust_pair (t, "T", -0.04);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "V", -0.08);
  adjust_pair (t, "W", -0.06);
  adjust_pair (t, "Y", -0.08);
}

/******************************************************************************
 * Interface
 ******************************************************************************/

static hashmap<string, double> lsub_termes_italic (0.0);
static hashmap<string, double> lsup_termes_italic (0.0);
static hashmap<string, double> rsub_termes_italic (0.0);
static hashmap<string, double> rsup_termes_italic (0.0);
static hashmap<string, double> above_termes_italic (0.0);

hashmap<string, double>
lsub_termes_italic_table () {
  if (N (lsub_termes_italic) == 0) {
    lsub_adjust_std (lsub_termes_italic);
    lsub_adjust_termes_italic (lsub_termes_italic);
  }
  return lsub_termes_italic;
}

hashmap<string, double>
lsup_termes_italic_table () {
  if (N (lsup_termes_italic) == 0) {
    lsup_adjust_std (lsup_termes_italic);
    lsup_adjust_termes_italic (lsup_termes_italic);
  }
  return lsup_termes_italic;
}

hashmap<string, double>
rsub_termes_italic_table () {
  if (N (rsub_termes_italic) == 0) {
    rsub_adjust_std (rsub_termes_italic);
    rsub_adjust_termes_italic (rsub_termes_italic);
  }
  return rsub_termes_italic;
}

hashmap<string, double>
rsup_termes_italic_table () {
  if (N (rsup_termes_italic) == 0) {
    rsup_adjust_std (rsup_termes_italic);
    rsup_adjust_termes_italic (rsup_termes_italic);
  }
  return rsup_termes_italic;
}

hashmap<string, double>
above_termes_italic_table () {
  if (N (above_termes_italic) == 0)
    above_adjust_termes_italic (above_termes_italic);
  return above_termes_italic;
}
