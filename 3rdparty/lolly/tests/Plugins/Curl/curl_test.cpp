/** \file curl_test.cpp
 *  \copyright GPLv3
 *  \details Unitests for curl.
 *  \author Darcy Shen
 *  \date   2023
 */
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "curl.hpp"
#include "doctest/doctest.h"

TEST_CASE ("curl_version") { CHECK (N (lolly::curl_version ()) > 0); }
