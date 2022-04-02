
/******************************************************************************
* MODULE     : curl.cpp
* DESCRIPTION: interface with CURL
* COPYRIGHT  : (C) 2022  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Curl/curl.hpp"
#include <curl/curl.h>

static size_t write_data (void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written= fwrite (ptr, size, nmemb, stream);
    return written;
}

void curl_download (string source, string target, string user_agent) {
  debug_io << "curl --user-agent " << user_agent << " "
           << source << " --output " << target << LF;
  CURL *curl= curl_easy_init ();
  if (curl) {
    FILE *fp= fopen (as_charp (target), "wb");

    curl_easy_setopt (curl, CURLOPT_URL, as_charp (source));
    curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, write_data);
    curl_easy_setopt (curl, CURLOPT_WRITEDATA, fp);
    curl_easy_setopt (curl, CURLOPT_USERAGENT, as_charp (user_agent));
    CURLcode res= curl_easy_perform (curl);

    curl_easy_cleanup (curl);
    fclose (fp);
  } else {
    debug_io << "Failed to init the easy curl client" << LF;
  }
}
