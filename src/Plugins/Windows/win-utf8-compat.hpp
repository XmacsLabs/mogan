/******************************************************************************
* MODULE     : win-utf8-compat.hpp
* DESCRIPTION: The windows version of TeXmacs makes use of
*              Boost.Nowide standalone library for accessing
*              the Windows UTF-16 file API
*******************************************************************************
* As most application originating from the *nix world, TeXmacs
* uses UTF-8 encoding for files names, file pathes and 
* environment variables. Porting such applications to Windows
* is complicated due to the fact that Windows API only accepts
* UTF-16 for unicode.
* 
* Boost.Nowide provides an elegant light weight solution to this problem,
* making porting from *nix to windows UTF-16 unicode API mostly transparent
*
* See http://cppcms.com/files/nowide/html/index.html
******************************************************************************/

#include "nowide/cstdio.hpp"
#include "nowide/cstdlib.hpp"
#include "nowide/stat.hpp"
#include "nowide/stackstring.hpp"


#ifndef S_ISLNK
#define S_ISLNK(x) 0
#endif

// nowide/cstdio.hpp
#ifdef fopen
 #undef fopen
#endif
 #define fopen(a,b) nowide::fopen(a,b)

#ifdef rename
 #undef rename
#endif
 #define rename nowide::rename
 

// nowide/stat.hpp
#define struct_stat nowide::stat_t 
#ifdef stat
 #undef stat
#endif
  #define stat(a,b) nowide::stat(a,b)


// nowide/cstdlib.hpp
#ifdef getenv
 #undef getenv
#endif
 #define getenv nowide::getenv
 
#ifdef setenv
 #undef setenv
#endif
 #define setenv nowide::setenv

#ifdef putenv
 #undef putenv
#endif
 #define putenv nowide::putenv
