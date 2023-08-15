
#ifndef URL_HELPER_H
#define URL_HELPER_H


#include "url.hpp"
#include "tm_debug.hpp"

/******************************************************************************
* url resolution
******************************************************************************/
url  complete (url u);                      
url  complete (url u, string filter); // wildcard completion
url  resolve (url u);                       
url  resolve (url u, string filter);  // find first match only
url  resolve_in_path (url u);               // find file in path
bool exists (url u);                        // file exists
bool exists_in_path (url u);                // file exists in path
bool has_permission (url u, string filter); // check file permissions
url  descendance (url u);                   // utility for style&package menus
url  subdirectories (url u);                // similarly for patters
url  concretize_url (url u);                // variant of concretize below
string concretize (url u);                  // system name for resolved url
string materialize (url u);  
string materialize (url u, string f); // resolve + concretize


#endif // URL_HELPER_H