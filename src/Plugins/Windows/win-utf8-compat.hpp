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
*
* Texmacs specific changes to the original Boost.Nowide standalone files dated 
* 2017-Jun-10 are indicated in nowide/texmacs_modifications.patch
* ****************************************************************************** 
* LICENCE :
* Boost.Nowide is released under Boost Software License - Version 1.0.
* see https://www.boost.org/users/license.html
******************************************************************************/

#include "nowide/args.hpp"
#include <sys/stat.h>
#include <dirent.h>

#ifdef __MINGW64__
typedef struct _stat64 struct_stat;
#else
typedef struct _stat32 struct_stat;
#endif

#include "nowide/cstdio.hpp"
#include "nowide/cstdlib.hpp"
#include "nowide/stat.hpp"


#ifndef S_ISLNK
#define S_ISLNK(x) 0
#endif

#ifdef fopen
 #undef fopen
#endif
 #define fopen(a,b) nowide::fopen(a,b)


inline int mkdir(char const *name, int const mode)
{
    nowide::basic_stackstring<> wname;
    if(wname.convert(name) == nullptr ) {
        errno = EINVAL;
        return -1;
    }
    return _wmkdir (wname.get());
}

 
#ifdef stat
 #undef stat
#endif
#define stat(a,b) nowide::stat(a,b)
//#define struct_stat GStatBuf
//#define struct_stat _stat32 
//#ifdef remove
// #undef remove
//#endif
// #define ::remove nowide::remove

#ifdef rename
 #undef rename
#endif
 #define rename nowide::rename
 
inline int chmod(char const *name, int const mode)
{
    nowide::basic_stackstring<> wname;
    if(!wname.convert(name) ) {
        errno = EINVAL;
        return -1;
    }
    return _wchmod (wname.get(), mode);
}

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
