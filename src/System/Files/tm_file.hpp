#include "string.hpp"
#include "url.hpp"

url  url_numbered (url dir, string prefix, string postfix, int i=1);
url  url_scratch (string prefix="no_name_", string postfix=".tm", int i=1);
bool is_scratch (url u);
string file_format (url u);

url search_sub_dirs (url root);
array<string> file_completions (url search, url dir);

url grep (string what, url u);
url search_file_in (url u, string name);
url search_file_upwards (url u, string name, array<string> stops);

int search_score (url u, array<string> a);

#define CMD_GET_FROM_WEB    1
#define CMD_GET_FROM_SERVER 2
#define CMD_APPLY_EFFECT    3
url make_file (int cmd, tree data, array<url> args);
