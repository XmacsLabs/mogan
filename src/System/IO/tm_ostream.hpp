
/******************************************************************************
* MODULE     : tm_ostream.hpp
* DESCRIPTION: Output stream class
* COPYRIGHT  : (C) 2009-2013  David MICHEL, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef OUT_STREAM_HPP
#define OUT_STREAM_HPP

//#include "url.hpp"
#include <cstdio>
class string;
class tm_ostream;
class formatted;
class tree;

class tm_ostream_rep {
  int ref_count;

public:
  tm_ostream_rep ();
  virtual ~tm_ostream_rep ();

  virtual bool is_writable () const;
  virtual void write (const char*);
  virtual void write (tree);
  virtual void flush ();
  virtual void clear ();

  friend class tm_ostream;
};

class tm_ostream {
public:
  tm_ostream_rep *rep;

public:
  static tm_ostream private_cout;
  static tm_ostream private_cerr;
  static tm_ostream& cout;
  static tm_ostream& cerr;

public:
  tm_ostream ();
  tm_ostream (char*);
  tm_ostream (FILE*);
  tm_ostream (const tm_ostream&);
  tm_ostream (tm_ostream_rep*);
  ~tm_ostream ();
  tm_ostream_rep* operator -> ();
  tm_ostream& operator = (tm_ostream x);
  bool operator == (tm_ostream&);

  void clear ();
  void flush ();
  void buffer ();
  string unbuffer ();
  void redirect (tm_ostream x);

  tm_ostream& operator << (bool);
  tm_ostream& operator << (char);
  tm_ostream& operator << (short);
  tm_ostream& operator << (unsigned short);
  tm_ostream& operator << (int);
  tm_ostream& operator << (unsigned int);
  tm_ostream& operator << (long);
  tm_ostream& operator << (unsigned long);
  tm_ostream& operator << (long long int);
  tm_ostream& operator << (unsigned long long int);
  tm_ostream& operator << (float);
  tm_ostream& operator << (double);
  tm_ostream& operator << (long double);
  tm_ostream& operator << (const char*);
  tm_ostream& operator << (formatted);
};

extern tm_ostream& cout;
extern tm_ostream& cerr;


#endif // defined OUT_STREAM_HPP
