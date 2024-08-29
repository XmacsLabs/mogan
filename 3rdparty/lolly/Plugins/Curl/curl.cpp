
/******************************************************************************
 * MODULE     : curl.cpp
 * DESCRIPTION: interface with CURL
 * COPYRIGHT  : (C) 2022  Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "curl.hpp"

#include <curl/curl.h>
#include <string>

namespace lolly {
string
curl_version () {
  curl_version_info_data* data= curl_version_info (CURLVERSION_NOW);
  return string (data->version);
}

static size_t
write_data (void* contents, size_t size, size_t nmemb, void* userp) {
  ((std::string*) userp)->append ((char*) contents, size * nmemb);
  return size * nmemb;
}

string
curl_get (string source, string user_agent) {
  std::string readBuffer;
  CURL*       curl= curl_easy_init ();

  if (curl) {
    string url;
    if (is_quoted (source)) url= source (1, N (source) - 1);
    else url= source;

    curl_easy_setopt (curl, CURLOPT_URL, as_charp (url));
    curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, write_data);
    curl_easy_setopt (curl, CURLOPT_WRITEDATA, &readBuffer);
    curl_easy_setopt (curl, CURLOPT_USERAGENT, as_charp (user_agent));
    curl_easy_setopt (curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt (curl, CURLOPT_REDIR_PROTOCOLS,
                      CURLPROTO_HTTP | CURLPROTO_HTTPS);
    CURLcode res= curl_easy_perform (curl);

    curl_easy_cleanup (curl);
    return string (readBuffer.c_str ());
  }
  else {
    return string ();
  }
}

} // namespace lolly
