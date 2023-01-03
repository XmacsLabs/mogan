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

#ifdef __MINGW64__
typedef struct _stat64 struct_stat;
#else
typedef struct _stat32 struct_stat;
#endif

#include "nowide/cstdio.hpp"
#include "nowide/cenv.hpp"


#ifndef S_ISLNK
#define S_ISLNK(x) 0
#endif

/**
 * add missing macros in msvc stat.h
*/
#ifdef OS_WIN32
#define S_ISDIR(m) (((m) &S_IFMT) == S_IFDIR)
#define S_ISFIFO(m) (((m) &S_IFMT) == S_IFIFO)
#define S_ISCHR(m) (((m) &S_IFMT) == S_IFCHR)
#define S_ISBLK(m) (((m) &S_IFMT) == S_IFBLK)
#define S_ISREG(m) (((m) &S_IFMT) == S_IFREG)
#define S_IROTH    (S_IRGRP >> 3)
#define S_IWOTH    (S_IWGRP >> 3)
#define S_IXOTH    (S_IXGRP >> 3)
#define S_IRGRP    (S_IRUSR >> 3)
#define S_IWGRP    (S_IWUSR >> 3)
#define S_IXGRP    (S_IXUSR >> 3)
#define	S_IXUSR		_S_IXUSR
#define	S_IWUSR		_S_IWUSR
#define	S_IRUSR		_S_IRUSR
#define	_S_IRWXU	(_S_IREAD | _S_IWRITE | _S_IEXEC)
#define	_S_IXUSR	_S_IEXEC
#define	_S_IWUSR	_S_IWRITE

#define	S_IRWXU		_S_IRWXU
#define	_S_IRUSR	_S_IREAD
#endif // OS_WIN32

#ifdef fopen
 #undef fopen
#endif
 #define fopen(a,b) nowide::fopen(a,b)


//do not redefine mkdir with single argument (url)
//if two args point to nowide::mkdir
#define GET_MACRO(_1,_2,NAME,...) NAME
#define mkdir(...) GET_MACRO(__VA_ARGS__, nowide::mkdir, mkdir)(__VA_ARGS__)


#ifdef opendir
 #undef opendir
#endif
 #define opendir(a) nowide::opendir(a)
 
#ifdef closedir
 #undef closedir
#endif
#define closedir _wclosedir

//typedef _WDIR DIR;
#define DIR _WDIR
 
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
 
#ifdef chmod
 #undef chmod
#endif
 #define chmod nowide::chmod

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
