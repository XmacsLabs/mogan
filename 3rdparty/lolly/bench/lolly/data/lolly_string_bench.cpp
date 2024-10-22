/** \file lolly_string_bench.cpp
 *  \copyright GPLv3
 *  \details Benchmark for string
 *  \author jingkaimori
 *  \date   2024
 */

#include "lolly/data/lolly_string.hpp"
#include <nanobench.h>

static ankerl::nanobench::Bench bench;

namespace lolly {
extern void init_tbox ();
} // namespace lolly

int
main () {
  lolly::init_tbox ();
#ifdef OS_WASM
  bench.minEpochIterations (2000);
#else
  bench.minEpochIterations (200000);
#endif
  bench.run ("construct string", [&] {
    lolly::data::string ("abc");
    lolly::data::string ();
  });
  bench.run ("equality of string", [&] {
    static lolly::data::string a ("abc"), b;
    a == b;
  });
  bench.run ("equality of larger string", [&] {
    static lolly::data::string a ("equality of larger string"),
        b ("equality of larger string");
    a == b;
  });
  bench.run ("compare string", [&] {
    static lolly::data::string a ("ab"), b ("b");
    a <= b;
  });
  bench.run ("compare larger string", [&] {
    static lolly::data::string a ("compare larger string"),
        b ("compare LARGEr string");
    a <= b;
  });
  bench.run ("slice string", [&] {
    static lolly::data::string a ("abcdefgh");
    a (2, 3);
  });
  bench.run ("slice string with larger range", [&] {
    static lolly::data::string a ("abcdefgh");
    a (1, 6);
  });
  bench.run ("concat string", [&] {
    static lolly::data::string a ("abc"), b ("de");
    a*                         b;
  });
  bench.run ("append string", [&] {
    static lolly::data::string a ("abc"), b ("de");
    a << b;
  });
}
