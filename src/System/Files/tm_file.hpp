#include "string.hpp"
#include "url.hpp"

url  url_numbered (url dir, string prefix, string postfix, int i=1);
url  url_scratch (string prefix="no_name_", string postfix=".tm", int i=1);
bool is_scratch (url u);
string file_format (url u);

int search_score (url u, array<string> a);
