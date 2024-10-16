#include "a_lolly_test.hpp"
#include "fast_alloc.hpp"

struct Complex {
public:
  double re, im;
  Complex (double re_, double im_) : re (re_), im (im_) {}
  ~Complex () {}
};

TEST_CASE ("test for memory leaks") {
  char* p_char= tm_new<char> ('a');
  CHECK (*p_char == 'a');

  p_char= tm_new<char> ('b');
  CHECK (*p_char == 'b');

  *p_char= 'c';
  CHECK (*p_char == 'c');

  tm_delete (p_char);

  char* q_char= tm_new<char> ('z'); // here p_char is modified to 'z'

  *p_char= 'c'; // here q_char is modified to 'c'
}

TEST_MEMORY_LEAK_INIT

TEST_CASE ("test basic data types") {

#ifdef OS_WASM
#define NUM 100
#else
#define NUM 10000
#endif

  char*   ch[NUM];
  int*    in[NUM];
  long*   lo[NUM];
  double* dou[NUM];

  for (int i= 0; i < NUM; i++) { // for gprof
    ch[i]= tm_new<char> ();
    tm_delete (ch[i]);

    in[i]= tm_new<int> ();
    tm_delete (in[i]);

    lo[i]= tm_new<long> ();
    tm_delete (lo[i]);

    dou[i]= tm_new<double> ();
    tm_delete (dou[i]);
  }

  for (int i= 0; i < NUM; i++) {
    ch[i] = tm_new<char> ();
    in[i] = tm_new<int> ();
    lo[i] = tm_new<long> ();
    dou[i]= tm_new<double> ();
  }

  for (int i= 0; i < NUM; i++) {
    tm_delete (ch[i]);
    tm_delete (in[i]);
    tm_delete (lo[i]);
    tm_delete (dou[i]);
  }
}

TEST_CASE ("test class") {
  Complex* p_complex= tm_new<Complex> (35.8, 26.2);
  tm_delete (p_complex);
}

TEST_MEMORY_LEAK_ALL
