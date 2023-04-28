
###############################################################################
#
# MODULE      : CMakeLists.txt
# DESCRIPTION : CMake file for TeXmacs
# COPYRIGHT   : (C) 2008-2009  Massimiliano Gubinelli
#               (C) 2017-2022  Darcy Shen
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


# The name of our project is "TEXMACS". CMakeLists files in this project can 
# refer to the root source directory of the project as ${TEXMACS_SOURCE_DIR} and 
# to the root binary directory of the project as ${TEXMACS_BINARY_DIR}. 


### --------------------------------------------------------------------
### Basic settings (project independent)
### --------------------------------------------------------------------

cmake_minimum_required (VERSION 3.15)

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_AUTOMOC ON)
set (CMAKE_INCLUDE_CURRENT_DIR ON)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Debug CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
    FORCE)
endif (NOT CMAKE_BUILD_TYPE)

#INCLUDE(CMakeDetermineSystem)

# Configure CCache if available
find_program (CCACHE_FOUND ccache)
if (CCACHE_FOUND)
  set_property (GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
  set_property (GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif (CCACHE_FOUND)


### --------------------------------------------------------------------
### Project name 
### --------------------------------------------------------------------

project (TEXMACS CXX C) # check for C/C++ compiler

set (PACKAGE TeXmacs)
list (APPEND CMAKE_MODULE_PATH "${TEXMACS_SOURCE_DIR}/cmake")

set (VERSION_MAJOR "2")
set (VERSION_MINOR "1")
set (VERSION_BUILD "2")
set (TEXMACS_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BUILD})

set (DEVEL_VERSION ${TEXMACS_VERSION})
set (DEVEL_RELEASE "1")
set (STABLE_VERSION ${TEXMACS_VERSION})
set (STABLE_RELEASE "1")
set (TEXMACS_REVISION ${TEXMACS_VERSION})

set (XMACS_VERSION "1.1.2")

### --------------------------------------------------------------------
### Options
### --------------------------------------------------------------------
if (NOT TEXMACS_GUI)
  set (TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, X11, Aqua)")
endif (NOT TEXMACS_GUI)

option (USE_ICONV "Use Iconv" ON)
if (USE_ICONV)
  find_package(Iconv)
endif (USE_ICONV)


option (USE_FREETYPE "use Freetype" ON)
if (USE_FREETYPE)
  find_package (Freetype)
  option (LINKED_FREETYPE "linked Freetype" ON)
endif (USE_FREETYPE)

option (PDFHUMMUS_NO_TIFF "Disable TIFF in PDF Hummus" ON)

if (${CMAKE_SYSTEM_NAME} MATCHES "Emscripten")
  set(OS_WASM 1)
  add_definitions("-DOS_WASM")
  set (TEXMACS_GUI "Qt6")
  option (QTPIPES "use Qt pipes" OFF)
  option (USE_QT_PRINTER "use Qt printer" OFF)
  option (USE_CURL "use curl library" OFF)
  option (USE_SQLITE3 "Use SQLite3" OFF)
else()
  option (QTPIPES "use Qt pipes" ON)
  add_definitions("-DQTPIPES") # used by qt moc
  
  option (USE_QT_PRINTER "use Qt printer" ON)
  add_definitions("-DUSE_QT_PRINTER") # used by qt moc
  
  option (USE_CURL "use curl library" ON)
  
  option (USE_SQLITE3 "Use SQLite3" ON)
  if (USE_SQLITE3)
    find_package (SQLite3)
    option (LINKED_SQLITE3 "Use Linked SQLite3" ON)
  endif (USE_SQLITE3)
endif()

option (USE_CAIRO "use Cairo" OFF)
if (USE_CAIRO)
  find_package(Cairo)
  option (LINKED_CAIRO "linked Cairo" ON)
endif (USE_CAIRO)

option (USE_IMLIB2 "use Imlib2" OFF)
if (USE_IMLIB2)
  find_package(Imlib2)
  option (LINKED_IMLIB2 "linked Imlib2" ON)
endif (USE_IMLIB2)

option (USE_SPARKLE "use Sparkle" OFF)


if (WIN32)
  option (USE_GMP "use GMP" ON)
else (WIN32)
  option (USE_GMP "use GMP" OFF)
endif (WIN32)

if (USE_GMP)
  find_package (GMP)
endif (USE_GMP)

if (WIN32)
  option (OS_MINGW "Using OS MINGW" ON)
else (WIN32)
  option (OS_MINGW "Using OS MINGW" OFF)
endif (WIN32)

if (APPLE)
  set (MACOSX_EXTENSIONS 1)
  set (OS_MACOS 1)
  option (MACOS_QT_MENU "Disable MacOS native menu" OFF)
endif (APPLE)


### --------------------------------------------------------------------
### Include standard modules
### --------------------------------------------------------------------

include (CheckFunctionExists)
include (CheckLibraryExists)
include (CheckIncludeFile)
include (CheckTypeSize)
include (FindPkgConfig)
include (CheckCSourceCompiles)
include (FindX11)

include(FetchContent)

FetchContent_Declare(
  nowide
  URL https://github.com/boostorg/nowide/releases/download/v11.2.0/nowide_standalone_v11.2.0.tar.gz
)
FetchContent_MakeAvailable(nowide)
FetchContent_Declare(
  PDFHummus
  URL https://github.com/galkahana/PDF-Writer/archive/refs/tags/v4.5.6.tar.gz
)
FetchContent_MakeAvailable(PDFHummus)


### --------------------------------------------------------------------
### Check for standard functionalities
### --------------------------------------------------------------------
check_type_size (FILE HAVE_FILE)
check_type_size (intptr_t HAVE_INTPTR_T)
check_type_size (time_t HAVE_TIME_T)
check_include_file (inttypes.h HAVE_INTTYPES_H)
check_include_file (memory.h HAVE_MEMORY_H)
check_include_file (stdlib.h HAVE_STDLIB_H)
check_include_file (strings.h HAVE_STRINGS_H)
check_include_file (string.h HAVE_STRING_H)
check_include_file (sys/stat.h HAVE_SYS_STAT_H)
check_include_file (unistd.h HAVE_UNISTD_H)
check_include_file (X11/Xlib.h HAVE_X11_XLIB_H)
check_include_file (X11/Xutil.h HAVE_X11_XUTIL_H)
check_include_file (pty.h HAVE_PTY_H)
check_include_file (util.h HAVE_UTIL_H)
check_function_exists (gettimeofday HAVE_GETTIMEOFDAY)


### --------------------------------------------------------------------
### Check for dl library
### --------------------------------------------------------------------

check_function_exists (dlopen HAVE_DLOPEN)

if (NOT ${HAVE_DLOPEN})
  check_library_exists (dl dlopen "" HAVE_DLOPEN)
  if (${HAVE_DLOPEN})
    set (CONFIG_BDL "-ldl")
  endif (${HAVE_DLOPEN})
endif (NOT ${HAVE_DLOPEN})

if (NOT ${HAVE_DLOPEN})
  check_library_exists (dld dld_link "" HAVE_DLOPEN)
  if (${HAVE_DLOPEN})
    set (CONFIG_BDL "-ldl")
  endif (${HAVE_DLOPEN})
endif (NOT ${HAVE_DLOPEN})

if (NOT ${HAVE_DLOPEN})
  check_function_exists (shl_load HAVE_DLOPEN)
  if (${HAVE_DLOPEN})
    set (CONFIG_BDL "-ldl")
  endif (${HAVE_DLOPEN})
endif (NOT ${HAVE_DLOPEN})


if (${HAVE_DLOPEN})
  #SET(TM_DYNAMIC_LINKING 1) # dynamic linking works
endif (${HAVE_DLOPEN})

### --------------------------------------------------------------------
### Handle different systems case by case
### --------------------------------------------------------------------

set (CONFIG_OS "GNU_LINUX")
set (CONFIG_OS_SUFFIX "gnu-linux")
set (CONFIG_CXXTEMPLATE "")
set (CONFIG_STD_SETENV "#define STD_SETENV")
set (CONFIG_SO "so")
set (CONFIG_LIB_PATH "LD_LIBRARY_PATH")
set (CONFIG_CHMOD "chmod -f")
set (CONFIG_CXXOPTIMIZE "-O2")
set (CONFIG_BSTATIC "-Wl,-Bstatic")
set (CONFIG_BSHARED "-Wl,-Bdynamic")
set (CONFIG_BFLAGS "")
set (CONFIG_BPATH "-Wl,-rpath,")
set (CONFIG_WORD_LENGTH "4")
set (CONFIG_WORD_LENGTH_INC "3")
set (CONFIG_WORD_MASK "0xfffffffc")
set (CONFIG_MAX_FAST "260 // WORD_LENGTH more than power of 2")

check_type_size ("void*" SIZE_OF_VOIDP)

message (STATUS "Check if we are on a 64-bits computer")
if (${SIZE_OF_VOIDP} EQUAL 8)
  set (CONFIG_WORD_LENGTH "8")
  set (CONFIG_WORD_LENGTH_INC "7")
  set (CONFIG_WORD_MASK "0xfffffffffffffff8")
  set (CONFIG_MAX_FAST "264 // WORD_LENGTH more than power of 2")
endif (${SIZE_OF_VOIDP} EQUAL 8)


if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  message (STATUS "final adjustments for an Intel or AMD GNU/Linux host")
  set (CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")

if (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
  message (STATUS "final adjustments for an Intel or AMD GNU/BSD host")
  set (CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")

if (${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")
  message (STATUS "final adjustments for an Intel or AMS Solaris host")
  set (CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
  set (CONFIG_CXXOPTIMIZE "-O3")
  set (CONFIG_BPATH "-Wl,-R,")
#  SET(X11_LDFLAGS "${X_LIBS} -lXext -lX11 -lsocket")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")

if (${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")
  message (STATUS "final adjustments for cygwin host")
  set (CONFIG_OS "CYGWIN")
  set (CONFIG_BFLAGS "-Wl,-stack,8388608")
endif (${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")

if(${CMAKE_SYSTEM_NAME} STREQUAL "Emscripten")
  message (STATUS "final adjustments for wasm host")
  set (CONFIG_OS "WASM")
  set (CONFIG_OS_SUFFIX "wasm")
  set (CONFIG_HOST_OS "wasm-emscripten")
  set (CONFIG_HOST_VENDOR "pc")
  set (CONFIG_HOST_CPU ${CMAKE_SYSTEM_PROCESSOR})
endif(${CMAKE_SYSTEM_NAME} STREQUAL "Emscripten")

### --------------------------------------------------------------------
### Package Data
### --------------------------------------------------------------------

set (PACKAGE_BUGREPORT 1)
set (PACKAGE_NAME 1)
set (PACKAGE_STRING 1)
set (PACKAGE_TARNAME 1)
set (PACKAGE_URL 1)
set (PACKAGE_VERSION 1)

### --------------------------------------------------------------------
### Memory allocation options
### --------------------------------------------------------------------

#if(${DISABLE_FASTALLOC})
#  set(NO_FAST_ALLOC 1)
#endif(${DISABLE_FASTALLOC})


### --------------------------------------------------------------------
### Experimental options
### --------------------------------------------------------------------

#  SET(EXPERIMENTAL 1)

### --------------------------------------------------------------------
###  Test for Libraries
### --------------------------------------------------------------------


if (${CMAKE_SYSTEM_NAME} MATCHES "Emscripten")
  set(USE_LIB_FLAGS "-s USE_FREETYPE=1")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${USE_LIB_FLAGS}")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${USE_LIB_FLAGS}")
else()
	find_package (PNG REQUIRED)
	find_package (ZLIB REQUIRED)
	find_package (JPEG REQUIRED)
	find_package (CURL REQUIRED)
endif()

find_package (Iconv REQUIRED)

# Allow for selection of Scheme implementation
set (SCHEME_IMPL "s7" CACHE STRING "Implementation of Scheme to compile for.")

### --------------------------------------------------------------------
### Generate configure headers
### --------------------------------------------------------------------
set (DEBUG_ASSERT 1)

if (WIN32)
  set (GS_EXE bin/gs.exe)
else (WIN32)
  set (GS_EXE /usr/bin/gs)
endif (WIN32)
set (GS_FONTS ../share/ghostscript/fonts:/usr/share/fonts:)
set (GS_LIB ../share/ghostscript/9.06/lib:)

set (GUILE_NUM 1)
if (OS_WASM)
    option(PDF_RENDERER "use the PDF renderer" OFF) # not supported yet for wasm
else()
set (PDF_RENDERER 1)
set (USE_GS 1)
endif()
set (SIZEOF_VOID_P ${SIZE_OF_VOIDP})
set (STDC_HEADERS 1)

if (WIN32 OR OS_WASM)
  option (USE_STACK_TRACE "Use stack trace" OFF)
else (WIN32)
  option (USE_STACK_TRACE "Use stack trace" ON)
endif (WIN32 OR OS_WASM)

option (USE_JEAIII "Use JEAIII itoa" ON)

set (CONFIG_STD_SETENV "#define STD_SETENV")
set (tm_orig ${TEXMACS_SOURCE_DIR})
set (CONFIG_USER $ENV{USER})
string (TIMESTAMP CONFIG_DATE)
set (tm_devel TeXmacs-${DEVEL_VERSION})
set (tm_stable TeXmacs-${STABLE_VERSION})
set (tm_devel_release ${tm_devel}-${DEVEL_RELEASE})
set (tm_stable_release ${tm_stable}-${STABLE_RELEASE})

if (WIN32)
  set (CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}/${XMACS_VERSION})
endif (WIN32)
set (prefix ${CMAKE_INSTALL_PREFIX})
set (exec_prefix ${prefix})
set (datarootdir ${prefix}/share)
set (datadir ${datarootdir})
set (tmdata ${datarootdir}/Xmacs)
set (tmbin ${exec_prefix}/lib/xmacs/Xmacs)

configure_file (${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_BINARY_DIR}/src/System/config.h)
configure_file (${TEXMACS_SOURCE_DIR}/src/System/tm_configure.hpp.cmake ${TEXMACS_BINARY_DIR}/src/System/tm_configure.hpp)
if (WIN32)
  configure_file (${TEXMACS_SOURCE_DIR}/packages/windows/Xmacs.iss.in ${TEXMACS_BINARY_DIR}/Xmacs.iss)
endif (WIN32)

set (CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -include ${TEXMACS_BINARY_DIR}/src/System/config.h")
if (WIN32)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DCURL_STATICLIB")
endif (WIN32)

if (APPLE)
  set (CMAKE_C_FLAGS_DEBUG          "-g")
  set (CMAKE_C_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
  set (CMAKE_C_FLAGS_RELEASE        "-O3 -DNDEBUG")
  set (CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -g")
  
  set (CMAKE_CXX_FLAGS_DEBUG          "-g")
  set (CMAKE_CXX_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
  set (CMAKE_CXX_FLAGS_RELEASE        "-O3 -DNDEBUG")
  set (CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
endif (APPLE)

### --------------------------------------------------------------------
### the include dirs
### --------------------------------------------------------------------
set (TeXmacs_Include_Dirs
  ${TEXMACS_SOURCE_DIR}/src/Data/Convert
  ${TEXMACS_SOURCE_DIR}/src/Data/Document
  ${TEXMACS_SOURCE_DIR}/src/Data/Drd
  ${TEXMACS_SOURCE_DIR}/src/Data/History
  ${TEXMACS_SOURCE_DIR}/src/Data/Observers
  ${TEXMACS_SOURCE_DIR}/src/Data/Parser
  ${TEXMACS_SOURCE_DIR}/src/Data/String
  ${TEXMACS_SOURCE_DIR}/src/Data/Tree
  ${TEXMACS_SOURCE_DIR}/src/Edit
  ${TEXMACS_SOURCE_DIR}/src/Edit/Editor
  ${TEXMACS_SOURCE_DIR}/src/Edit/Interface
  ${TEXMACS_SOURCE_DIR}/src/Edit/Modify
  ${TEXMACS_SOURCE_DIR}/src/Edit/Process
  ${TEXMACS_SOURCE_DIR}/src/Edit/Replace
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Bitmap_fonts
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Colors
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Fonts
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Gui
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Handwriting
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Mathematics
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Pictures
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Renderer
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Spacial
  ${TEXMACS_SOURCE_DIR}/src/Graphics/Types
  ${TEXMACS_SOURCE_DIR}/src/Kernel/Abstractions
  ${TEXMACS_SOURCE_DIR}/src/Kernel/Containers
  ${TEXMACS_SOURCE_DIR}/src/Kernel/Types
  ${TEXMACS_SOURCE_DIR}/src/Plugins
  ${TEXMACS_SOURCE_DIR}/src/Plugins/UniversalStacktrace
  ${TEXMACS_SOURCE_DIR}/src/Scheme
  ${TEXMACS_SOURCE_DIR}/src/Scheme/S7
  ${TEXMACS_SOURCE_DIR}/src/Scheme/Scheme
  ${TEXMACS_SOURCE_DIR}/src/Style/Environment
  ${TEXMACS_SOURCE_DIR}/src/Style/Evaluate
  ${TEXMACS_SOURCE_DIR}/src/Style/Memorizer
  ${TEXMACS_SOURCE_DIR}/src/System
  ${TEXMACS_SOURCE_DIR}/src/System/IO
  ${TEXMACS_SOURCE_DIR}/src/System/Memory
  ${TEXMACS_SOURCE_DIR}/src/System/Boot
  ${TEXMACS_SOURCE_DIR}/src/System/Classes
  ${TEXMACS_SOURCE_DIR}/src/System/Files
  ${TEXMACS_SOURCE_DIR}/src/System/Language
  ${TEXMACS_SOURCE_DIR}/src/System/Link
  ${TEXMACS_SOURCE_DIR}/src/System/Misc
  ${TEXMACS_SOURCE_DIR}/src/Texmacs
  ${TEXMACS_SOURCE_DIR}/src/Texmacs/Data
  ${TEXMACS_SOURCE_DIR}/src/Typeset
  ${TEXMACS_SOURCE_DIR}/src/Typeset/Bridge
  ${TEXMACS_SOURCE_DIR}/src/Typeset/Concat
  ${TEXMACS_SOURCE_DIR}/src/Typeset/Page
  ${TEXMACS_SOURCE_DIR}/TeXmacs/include 
  ${TEXMACS_BINARY_DIR}/src/System/ 
)
if (NOT OS_WASM)
  set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs} ${TEXMACS_SOURCE_DIR}/src/Plugins/Pdf/LibAesgm ${pdfhummus_SOURCE_DIR})
endif()

if (WIN32)
  set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs}
    ${TEXMACS_SOURCE_DIR}/src/Plugins/Windows
    ${TEXMACS_SOURCE_DIR}/src/Plugins/Windows/nowide
  )
else (WIN32)
  set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs} ${GMP_INCLUDES})
endif (WIN32)

set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs}
  ${Cairo_INCLUDE_DIRS} ${IMLIB2_INCLUDE_DIR}
)

### --------------------------------------------------------------------
### the main sources
### --------------------------------------------------------------------
file (GLOB_RECURSE TeXmacs_Base_SRCS
  "${TEXMACS_SOURCE_DIR}/src/Data/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Edit/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Graphics/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Kernel/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Scheme/Scheme/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Scheme/S7/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Scheme/S7/*.c"
  "${TEXMACS_SOURCE_DIR}/src/System/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Texmacs/Data/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Texmacs/Server/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Texmacs/Window/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Typeset/*.cpp"
)

file (GLOB_RECURSE TeXmacs_Std_Plugins_SRCS
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Axel/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Bibtex/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Database/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Freetype/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Jeaiii/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Ispell/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Metafont/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/LaTeX_Preview/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Openssl/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Sqlite3/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Updater/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Curl/*.cpp"
)

file (GLOB_RECURSE TeXmacs_Wasm_Excluded_SRCS
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Cairo/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Pdf/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Pdf/*.c"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Ghostscript/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Imlib2/*.cpp"
  "${TEXMACS_SOURCE_DIR}/src/Plugins/Mplayer/*.cpp"
)

file (GLOB_RECURSE TeXmacs_Style_SRCS
  "${TEXMACS_SOURCE_DIR}/src/Style/*.cpp"
)

if (OS_WASM)
  set (TeXmacs_All_SRCS ${TeXmacs_Base_SRCS}  ${TeXmacs_Std_Plugins_SRCS})
else()
  set (TeXmacs_All_SRCS ${TeXmacs_Base_SRCS}  ${TeXmacs_Std_Plugins_SRCS} ${TeXmacs_Wasm_Excluded_SRCS})
endif()

### --------------------------------------------------------------------
### GUI Interfaces
### --------------------------------------------------------------------

file (GLOB_RECURSE TeXmacs_Widkit_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/Widkit/*.cpp")
file (GLOB_RECURSE TeXmacs_X11_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/X11/*.cpp")
file (GLOB_RECURSE TeXmacs_Qt_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/*.cpp")

set (TeXmacs_Qt_Moc_HDRS
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMApplication.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMFileDialog.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMGuiHelper.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMInteractiveInputHelper.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMInteractivePrompt.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMMenuHelper.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMPipeLink.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMPrintDialog.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMPrinterSettings.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMScrollView.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMSockets.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMStyle.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMTreeModel.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMWidget.hpp
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Qt/QTMWindow.hpp
)

set (TeXmacs_Cocoa_SRCS
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_dialogues.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_gui.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_menu.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_renderer.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_utilities.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/aqua_widget.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/mac_utilities.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/TMButtonsController.m
  ${TEXMACS_SOURCE_DIR}/src/Plugins/Cocoa/TMView.mm
)

set (TeXmacs_MacOS_SRCS
  ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/HIDRemote.m
  ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/mac_spellservice.mm
  ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/mac_utilities.mm
)

### --------------------------------------------------------------------
### Determine TeXmacs_Libraries
### --------------------------------------------------------------------
if(NOT ${CMAKE_SYSTEM_NAME} MATCHES "Emscripten")
  set (TeXmacs_Libraries
    PNG::PNG ZLIB::ZLIB JPEG::JPEG CURL::libcurl Iconv::Iconv PDFHummus::PDFWriter nowide::nowide
    -lpthread
  )
  if (LINKED_CAIRO)
    set (TeXmacs_Libraries ${TeXmacs_Libraries} ${CAIRO_LIBRARIES})
  endif (LINKED_CAIRO)

  if (LINKED_SQLITE3)
    set (TeXmacs_Libraries ${TeXmacs_Libraries} SQLite::SQLite3)
  endif (LINKED_SQLITE3)

  if (LINKED_FREETYPE)
    set (TeXmacs_Libraries ${TeXmacs_Libraries} Freetype::Freetype)
  endif (LINKED_FREETYPE)
else()
endif()

if (WIN32)
# explore this
# CMAKE_INSTALL_UCRT_LIBRARIES
# I only to manage to get it working for msys2-ucrt.
set (TeXmacs_Libraries
  ${TeXmacs_Libraries}
  wsock32 ws2_32 crypt32
)
endif (WIN32)


### --------------------------------------------------------------------
### GUI selection
### --------------------------------------------------------------------

set (GUI_TYPE) # nothing or WIN32, MACOSX bundles are treated independently

if (TEXMACS_GUI MATCHES "Qt.*")

  if (TEXMACS_GUI STREQUAL "Qt4")
    message (FATAL_ERROR "Qt 4.x is not supported")
  else (TEXMACS_GUI STREQUAL "Qt4")
    # Homebrew installs Qt5 in /usr/local/opt/qt5
    if (APPLE AND EXISTS /usr/local/opt/qt5)
      list (APPEND CMAKE_PREFIX_PATH "/usr/local/opt/qt5")
    endif ()
    # compat with Qt6
    if (TEXMACS_GUI STREQUAL "Qt6")
      find_package(QT NAMES Qt6 Qt5 COMPONENTS Core Gui Widgets PrintSupport Svg REQUIRED)
    else(TEXMACS_GUI STREQUAL "Qt6")
      find_package(QT NAMES Qt5 COMPONENTS Core Gui Widgets PrintSupport Svg REQUIRED)
    endif()
    message(STATUS "Found Qt: ${QT_VERSION_MAJOR}")
    find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Core Gui Widgets PrintSupport Svg REQUIRED)
    set (QT_LIBRARIES
            Qt${QT_VERSION_MAJOR}::Core
            Qt${QT_VERSION_MAJOR}::Gui
            Qt${QT_VERSION_MAJOR}::Widgets
            Qt${QT_VERSION_MAJOR}::PrintSupport
            Qt${QT_VERSION_MAJOR}::Svg
            )
    if (APPLE AND ${QT_VERSION_MAJOR} LESS_EQUAL 5)
      find_package (Qt5 COMPONENTS MacExtras REQUIRED)
      set (QT_LIBRARIES ${QT_LIBRARIES} Qt5::MacExtras)
    endif ()

    if (Qt5_POSITION_INDEPENDENT_CODE)
      set (CMAKE_POSITION_INDEPENDENT_CODE ON)
    endif (Qt5_POSITION_INDEPENDENT_CODE)

    if (APPLE)
      add_definitions ("-DQ_WS_MAC")
    endif (APPLE)
  endif (TEXMACS_GUI STREQUAL "Qt4")

  add_definitions (${QT_DEFINITIONS})
  add_definitions ("-DQTTEXMACS")

  if (CMAKE_BUILD_TYPE STREQUAL "Release")
    add_definitions ("-DQT_NO_DEBUG")
  endif (CMAKE_BUILD_TYPE STREQUAL "Release")

  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Qt_SRCS} ${TeXmacs_Qt_Moc_HDRS})
  set (TeXmacs_Include_Dirs ${TeXmacs_Include_Dirs} ${QT_INCLUDES})
  set (TeXmacs_Libraries ${TeXmacs_Libraries}  ${QT_LIBRARIES})

  if (WIN32)
    set (GUI_TYPE WIN32)
  endif (WIN32)

  set (QTTEXMACS 1)
  set (CONFIG_GUI "QT")
  set (CONFIG_QT "Qt")
  message (STATUS "Enabling Qt port")
elseif (TEXMACS_GUI STREQUAL "Aqua")
  find_library (COCOA_LIBRARY Cocoa)
  find_path (COCOA_INCLUDE_DIR Cocoa/Cocoa.h)
  set (TeXmacs_Include_Dirs
    ${TeXmacs_Include_Dirs}
    ${COCOA_INCLUDE_DIR}
  )
  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Cocoa_SRCS})
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${COCOA_LIBRARY})

  mark_as_advanced(COCOA_LIBRARY)
  mark_as_advanced(COCOA_INCLUDE_DIR)
else (TEXMACS_GUI STREQUAL "Qt")
  find_package (X11)

  set (TeXmacs_Include_Dirs
    ${TeXmacs_Include_Dirs}
    ${X11_INCLUDE_DIR}
  )
  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_Widkit_SRCS} ${TeXmacs_X11_SRCS})
  set (TeXmacs_Libraries ${TeXmacs_Libraries} ${X11_LIBRARIES})

  add_definitions ("-DX11TEXMACS")
  set (X11TEXMACS 1)

  set (CONFIG_X11 "X11 Widkit Ghostscript")
  set (CONFIG_GUI "X11")

  message (STATUS "Enabling standard X11 interface")
endif (TEXMACS_GUI MATCHES "Qt.*")

if (APPLE AND TEXMACS_GUI STREQUAL "X11")
  set (TeXmacs_MacOS_SRCS
    ${TeXmacs_MacOS_SRCS}
    ${TEXMACS_SOURCE_DIR}/src/Plugins/MacOS/mac_app.mm
  )
endif (APPLE AND TEXMACS_GUI STREQUAL "X11")

if (APPLE AND USE_SPARKLE)
  set (TeXmacs_MacOS_SRCS
    ${TeXmacs_MacOS_SRCS}
    ${TEXMACS_SOURCE_DIR}/src/Plugins/Updater/tm_sparkle.mm
  )
endif (APPLE AND USE_SPARKLE)

### --------------------------------------------------------------------
### OS System selection
### --------------------------------------------------------------------

if (WIN32)
  file (GLOB_RECURSE TeXmacs_System_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/Windows/*.cpp")
elseif (OS_WASM)
  file (GLOB_RECURSE TeXmacs_System_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/Wasm/*.cpp")
else (WIN32)
  file (GLOB_RECURSE TeXmacs_System_SRCS "${TEXMACS_SOURCE_DIR}/src/Plugins/Unix/*.cpp")
endif (WIN32)

set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_System_SRCS})

if (APPLE)
  set (TeXmacs_Libraries
    ${TeXmacs_Libraries}
    "-framework ApplicationServices"
    "-framework CoreFoundation"
    "-framework Security"
    "-framework Carbon"
    "-framework AppKit"
    "-framework Cocoa"
    "-framework IOKit"
    "-framework CoreGraphics"
    "-framework CoreServices"
    "-framework CoreText"
    "-framework Foundation"
    "-framework ImageIO"
  )
  set (TeXmacs_All_SRCS ${TeXmacs_All_SRCS} ${TeXmacs_MacOS_SRCS})

  set (GUI_TYPE MACOSX_BUNDLE)
  set (NO_FAST_ALLOC 1)
  set (AQUATEXMACS 1)
  set (COCOA_CFLAGS "")
  set (COCOA_LDFLAGS "-framework COCOA")
  set (CONFIG_GUI "COCOA")
  set (CONFIG_COCOA "Cocoa")

  message (STATUS "Enabling experimental Cocoa port")
else (APPLE)
endif (APPLE)

include_directories (${TeXmacs_Include_Dirs})

### --------------------------------------------------------------------
### Debugging options
### --------------------------------------------------------------------


### --------------------------------------------------------------------
### Warning options
### --------------------------------------------------------------------

### --------------------------------------------------------------------
### Optimization options
### --------------------------------------------------------------------

### --------------------------------------------------------------------
### Compile sources
### --------------------------------------------------------------------
if (OS_WASM)
  add_compile_options ("-Wall" "-Wextra")
  add_link_options("SHELL:--preload-file ${TEXMACS_SOURCE_DIR}/TeXmacs@TeXmacs") #preload files under TeXmacs/, @ symbol is used for mapping path
  set (TeXmacs_binary_name "Mogan")
  qt_add_executable(${TeXmacs_binary_name} ${TeXmacs_All_SRCS} ${TEXMACS_SOURCE_DIR}/src/Texmacs/Texmacs/texmacs.cpp)
  target_link_libraries(${TeXmacs_binary_name} PRIVATE ${QT_LIBRARIES})
  if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_options("SHELL:-s ASSERTIONS=1" "SHELL:-s DEMANGLE_SUPPORT=1" "-g3")
    add_link_options("SHELL:-s ASSERTIONS=1" "SHELL:-s DEMANGLE_SUPPORT=1"  "-g3" "-gsource-map")
  endif()
else (OS_WASM)
  add_subdirectory (src)
  add_subdirectory (misc)
  add_subdirectory (TeXmacs)
endif(OS_WASM)

### --------------------------------------------------------------------
### Installation
### --------------------------------------------------------------------
if (NOT OS_WASM)
if (APPLE)
  install (FILES packages/macos/Xmacs.icns DESTINATION ${CMAKE_INSTALL_PREFIX})
  install (FILES packages/macos/TeXmacs-document.icns DESTINATION ${CMAKE_INSTALL_PREFIX})
  install (DIRECTORY src/Plugins/Cocoa/English.lproj DESTINATION ${CMAKE_INSTALL_PREFIX})
  install (DIRECTORY src/Plugins/Cocoa/zh_CN.lproj DESTINATION ${CMAKE_INSTALL_PREFIX})
endif (APPLE)

if (APPLE)
  install (FILES ${TEXMACS_BINARY_DIR}/misc/scripts/mogan.sh
          DESTINATION bin
          PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                      GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
elseif (UNIX)
  install (FILES ${TEXMACS_BINARY_DIR}/misc/scripts/mogan
          DESTINATION bin
          PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                      GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
else (APPLE)
  message (STATUS "Scripts launcher are not required on Windows")
endif (APPLE)
endif (NOT OS_WASM)

### share/

if (NOT OS_WASM)
  if (WIN32)
    install (DIRECTORY TeXmacs/plugins DESTINATION ${CMAKE_INSTALL_PREFIX}
      FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
      PATTERN "bin" EXCLUDE
      PATTERN "CMakeLists.txt" EXCLUDE
      PATTERN ".gitignore" EXCLUDE)
  else (WIN32)
    install (DIRECTORY TeXmacs/plugins DESTINATION share/Xmacs
      FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ
      PATTERN "bin" EXCLUDE
      PATTERN "CMakeLists.txt" EXCLUDE
      PATTERN ".gitignore" EXCLUDE)
  endif (WIN32)
  
  install (FILES misc/scripts/tm_gs
           DESTINATION share/Xmacs/bin
           PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                       GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
  
  install (FILES TeXmacs/misc/mime/mogan.desktop DESTINATION share/applications)
  install (FILES TeXmacs/misc/images/text-x-mogan.svg
    DESTINATION share/icons/hicolor/scalable/mimetypes)
  install (FILES TeXmacs/misc/mime/mogan.xml DESTINATION share/mime/packages)
  install (FILES TeXmacs/misc/pixmaps/Xmacs.xpm DESTINATION share/pixmaps)
  foreach (size IN ITEMS 16 32 128 512)
    install (FILES TeXmacs/misc/images/new-mogan-${size}.png RENAME Xmacs.png
      DESTINATION share/icons/hicolor/${size}x${size}/apps/)
  endforeach ()
  
  if (WIN32)
    install (FILES ${TEXMACS_SOURCE_DIR}/packages/windows/TeXmacs-large.bmp DESTINATION ${CMAKE_INSTALL_PREFIX})
    install (FILES ${TEXMACS_SOURCE_DIR}/packages/windows/TeXmacs-small.bmp DESTINATION ${CMAKE_INSTALL_PREFIX})
    install (FILES ${TEXMACS_SOURCE_DIR}/packages/windows/Xmacs.ico DESTINATION ${CMAKE_INSTALL_PREFIX})
    install (FILES ${TEXMACS_SOURCE_DIR}/packages/windows/TeXmacs.ico DESTINATION ${CMAKE_INSTALL_PREFIX})
    install (FILES ${TEXMACS_BINARY_DIR}/Xmacs.iss DESTINATION ${CMAKE_INSTALL_PREFIX})
  endif (WIN32)
endif (NOT OS_WASM)
### --------------------------------------------------------------------
### Testing
### --------------------------------------------------------------------

option (BUILD_TESTS "Build unit tests" OFF)

if (BUILD_TESTS)
  include (CTest)
  find_package(QT NAMES Qt6 Qt5 COMPONENTS Test REQUIRED)
  find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Test REQUIRED)
  enable_testing ()
  add_subdirectory (tests)
  # add_subdirectory (misc/benchmark)
endif (BUILD_TESTS)

### ---------------------------------------------------------------------
### VSCode Support
### ---------------------------------------------------------------------
foreach (DIR ${TeXmacs_Include_Dirs})
  set (VSCODE_INCLUDE_DIRS "${VSCODE_INCLUDE_DIRS}\"${DIR}\",\n                ")
endforeach (DIR ${TeXmacs_Include_Dirs})

configure_file (${TEXMACS_SOURCE_DIR}/misc/vscode/c_cpp_properties.json.in
  ${TEXMACS_SOURCE_DIR}/.vscode/c_cpp_properties.json
  @ONLY
)

message (STATUS "TeXmacs_Libraries" ${TeXmacs_Libraries})
