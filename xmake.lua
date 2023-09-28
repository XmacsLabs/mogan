-------------------------------------------------------------------------------
--
-- MODULE      : xmake.lua
-- DESCRIPTION : Xmake config file for TeXmacs
-- COPYRIGHT   : (C) 2022  jingkaimori
--                   2022  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

includes("check_cxxtypes.lua")
includes("check_cxxincludes.lua")
includes("check_cxxfuncs.lua")
includes("check_cxxsnippets.lua")

set_project("Mogan Editor")

-- because this cpp project use variant length arrays which is not supported by
-- msvc, this project will not support windows env.
-- because some package is not ported to cygwin env, this project will not
-- support cygwin env.
set_allowedplats(
    -- these plat should be guaranteed
    "linux", "macosx", "mingw", "wasm",
    --this plat is not maintained
    "android", "appletvos", "bsd", "cross", "iphoneos", "msys", "watchos"
) 

-- add releasedbg, debug and release modes for different platforms.
-- debug mode cannot run on mingw with qt precompiled binary
if is_plat("mingw") then
    set_allowedmodes("releasedbg", "release")
    add_rules("mode.releasedbg", "mode.release")
else 
    set_allowedmodes("releasedbg", "release", "debug")
    add_rules("mode.releasedbg", "mode.release", "mode.debug")
end

if is_plat("linux") and (linuxos.name() == "debian" or linuxos.name() == "ubuntu" or linuxos.name() == "uos") then
    add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
    add_requires("apt::libsqlite3-dev", {alias="sqlite3"})
    add_requires("apt::libpng-dev", {alias="libpng"})
    -- config package name for libjpeg on Ubuntu
    if linuxos.name() == "ubuntu" then
        add_requires("apt::libjpeg-turbo8-dev", {alias="libjpeg"})
    else
        add_requires("apt::libjpeg62-turbo-dev", {alias="libjpeg"})
    end
    -- config package name for freetype on UOS
    if linuxos.name() == "uos" then
    	add_requires("apt::libfreetype6-dev", {alias="freetype"})
    else
        add_requires("apt::libfreetype-dev", {alias="freetype"})
    end
else
    if not is_plat("wasm") then
        add_requires("libiconv 1.17", {system=false})
        add_requires("libcurl 7.84.0", {system=false})
    end
    add_requires("libpng 1.6.37", {system=false})
    add_requires("libjpeg v9e", {system=false})
    add_requires("freetype 2.12.1", {system=false})
    if is_plat ("macosx") then
        add_requires("sqlite3", {system=true})
    else
        add_requires("sqlite3 3.39.0+200", {system=false})
    end
end

local PDFHUMMUS_VERSION = "4.5.10"
if not is_plat("wasm") then
    add_requires("pdfhummus "..PDFHUMMUS_VERSION, {system=false,configs={libpng=true,libjpeg=true}})
    add_requires("nowide_standalone 11.2.0", {system=false})
end

local XMACS_VERSION="1.1.6-rc3"
local INSTALL_DIR="build/package"

local TEXMACS_VERSION = "2.1.2"
local DEVEL_VERSION = TEXMACS_VERSION
local DEVEL_RELEASE = 1
local STABLE_VERSION = TEXMACS_VERSION
local STABLE_RELEASE = 1

set_configvar("DEVEL_VERSION", DEVEL_VERSION)

if is_plat("linux") then 
    set_configvar("CONFIG_OS", "GNU_LINUX")
elseif is_subhost("cygwin") then
    set_configvar("CONFIG_OS", "CYGWIN")
elseif is_plat("wasm") then
    set_configvar("CONFIG_OS", "WASM")
else 
    set_configvar("CONFIG_OS", "")
end

set_configvar("AQUATEXMACS", is_plat("macosx"))

set_configvar("QTTEXMACS", 1)
add_defines("QTTEXMACS")

if is_plat("wasm") then
    set_configvar("QTPIPES", false)
    set_configvar("USE_QT_PRINTER", false)
else
    set_configvar("QTPIPES", true)
    add_defines("QTPIPES")

    set_configvar("USE_QT_PRINTER", true)
    add_defines("USE_QT_PRINTER")
end

set_configvar("USE_CURL", not is_plat("wasm"))
set_configvar("USE_GS", not is_plat("wasm"))
set_configvar("USE_ICONV", not is_plat("wasm"))
set_configvar("USE_SQLITE3", not is_plat("wasm"))
set_configvar("LINKED_AXEL", false)
set_configvar("LINKED_CAIRO", false)
set_configvar("LINKED_SQLITE3", 1)
set_configvar("USE_FREETYPE", 1)
set_configvar("LINKED_IMLIB2", false)
set_configvar("PDF_RENDERER", not is_plat("wasm"))
set_configvar("PDFHUMMUS_NO_TIFF", true)
if is_plat("wasm") then
    set_configvar("PDFHUMMUS_VERSION", "not supported")
else
    set_configvar("PDFHUMMUS_VERSION", PDFHUMMUS_VERSION)
end

local USE_STACK_TRACE = not (is_plat("mingw") or is_plat("wasm"))
set_configvar("USE_STACK_TRACE", USE_STACK_TRACE)

if is_plat("mingw") then
    set_configvar("GS_EXE", "bin/gs.exe")
elseif is_plat("macosx") then
    set_configvar("GS_EXE", "bin/gs")
else
    set_configvar("GS_EXE", false)
end

set_configvar("STDC_HEADERS", true)
configvar_check_cxxincludes("HAVE_STDLIB_H", "stdlib.h")
configvar_check_cxxincludes("HAVE_STRINGS_H", "strings.h")
configvar_check_cxxincludes("HAVE_STRING_H", "string.h")
configvar_check_cxxincludes("HAVE_UNISTD_H", "unistd.h")
configvar_check_cxxtypes("HAVE_INTPTR_T", "intptr_t", {includes = {"memory"}})
configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")
configvar_check_cxxincludes("HAVE_MEMORY_H", "memory.h")
configvar_check_cxxincludes("HAVE_PTY_H", "pty.h")
configvar_check_cxxincludes("HAVE_STDINT_H", "stdint.h")
configvar_check_cxxincludes("HAVE_SYS_STAT_H", "sys/stat.h")
configvar_check_cxxincludes("HAVE_SYS_TYPES_H", "sys/types.h")
configvar_check_cxxtypes("HAVE_TIME_T", "time_t", {includes = {"memory"}})
configvar_check_cxxincludes("HAVE_UTIL_H", "util.h")
configvar_check_cxxfuncs("HAVE_GETTIMEOFDAY", "gettimeofday", {includes={"sys/time.h"}})
configvar_check_cxxsnippets(
    "CONFIG_LARGE_POINTER", [[
        #include <stdlib.h>
        static_assert(sizeof(void*) == 8, "");]])

set_version(TEXMACS_VERSION)

add_configfiles(
    "misc/doxygen/Doxyfile.in", {
        filename = "doxyfile",
        pattern = "@(.-)@",
        variables = {
            PACKAGE = "Mogan Editor",
            DOXYGEN_DIR = get_config("buildir")
            }})
set_configdir("src/System")
add_configfiles(
    "src/System/config.h.xmake", {
        filename = "config.h",
        variables = {
            GS_FONTS = "../share/ghostscript/fonts:/usr/share/fonts:",
            GS_LIB = "../share/ghostscript/9.06/lib:",
            OS_MACOS = is_plat("macosx"),
            MACOSX_EXTENSIONS = is_plat("macosx"),
            OS_MINGW = is_plat("mingw"),
            OS_WASM = is_plat("wasm"),
            SIZEOF_VOID_P = 8,
            USE_JEAIII = true
            }})

target("tm_shell") do
    if is_plat("macosx") or is_plat("linux") then
        set_enabled(true)
    else
        set_enabled(false)
    end

    set_kind("binary")

    set_languages("c++17")

    add_includedirs("src/System")
    add_files("plugins/shell/src/tm_shell.cpp")
    add_links("util")
end

target("libkernel") do
    set_kind("static")
    set_group("kernel")
    set_basename("kernel")

    set_policy("check.auto_ignore_flags", false)
    set_languages("c++17")

    if is_plat("mingw") then
        add_packages("nowide_standalone")
    end

    add_includedirs({
        "src/System",
        "src/System/Memory",
        "src/System/IO",
        "src/Plugins",
        "src/Kernel/Abstractions",
        "src/Kernel/Containers",
        "src/Kernel/Types",
        "src/Data/Drd",
    })

    add_files({
        "src/Plugins/Jeaiii/**.cpp",
        "src/Kernel/Abstractions/basic.cpp",
        "src/Kernel/Containers/**.cpp",
        "src/Kernel/Types/**.cpp",
        "src/Data/Drd/**.cpp",
        "src/System/IO/**.cpp",
        "src/System/Memory/**.cpp"
    })

    if is_plat("mingw") then
        add_includedirs({
            "src/Plugins/Windows"
        })
    end
end

for _, filepath in ipairs(os.files("tests/Kernel/**_test.cpp")) do
    local testname = path.basename(filepath) 
    target(testname) do 
        add_runenvs("TEXMACS_PATH", path.join(os.projectdir(), "TeXmacs"))
        set_group("kernel_tests")
        add_deps("libkernel")
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)
        add_rules("qt.console")
        add_frameworks("QtTest")

        if is_plat("mingw") then
            add_packages("nowide_standalone")
        end
        add_includedirs("tests/Base")
        add_includedirs(
            "src/Plugins",
            "src/System",
            "src/System/Memory",
            "src/System/IO",
            "src/Kernel/Abstractions",
            "src/Kernel/Containers",
            "src/Kernel/Types"
        )
        add_files("tests/Base/base.cpp")
        add_files(filepath) 
        add_files(filepath, {rules = "qt.moc"})
    end
end

target("libmogan") do
    set_basename("mogan")
    set_version(TEXMACS_VERSION)
    
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)
    if is_plat("linux") and is_mode("release") then
        add_rules("qt.shared")
    else
        add_rules("qt.static")
    end
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    if is_plat("macosx") then
        add_frameworks("QtMacExtras")
    end

    add_packages("libpng")
    add_packages("libjpeg")
    add_packages("freetype")
    add_packages("sqlite3")

    if not is_plat("wasm") then
        add_packages("nowide_standalone")
        add_packages("pdfhummus")
        add_packages("libiconv")
        add_packages("libcurl")
    end

    if is_plat("mingw") then
        add_syslinks("wsock32", "ws2_32", "crypt32","secur32", {public = true})
    end

    ---------------------------------------------------------------------------
    -- generate config files. see also:
    --    * https://github.com/xmake-io/xmake/issues/320
    --    * https://github.com/xmake-io/xmake/issues/342
    ---------------------------------------------------------------------------
    set_configdir("src/System")
    -- check for dl library
    -- configvar_check_cxxfuncs("TM_DYNAMIC_LINKING","dlopen")
    add_options("libdl")
    add_configfiles(
        "src/System/config.h.xmake", {
            filename = "config.h",
            variables = {
                GS_FONTS = "../share/ghostscript/fonts:/usr/share/fonts:",
                GS_LIB = "../share/ghostscript/9.06/lib:",
                OS_MACOS = is_plat("macosx"),
                MACOSX_EXTENSIONS = is_plat("macosx"),
                OS_MINGW = is_plat("mingw"),
                SIZEOF_VOID_P = 8,
                USE_JEAIII = true
                }})


    add_configfiles(
        "src/System/tm_configure.hpp.xmake", {
            filename = "tm_configure.hpp",
            pattern = "@(.-)@",
            variables = {
                XMACS_VERSION = XMACS_VERSION,
                CONFIG_USER = os.getenv("USER") or "unknown",
                CONFIG_DATE = os.time(),
                CONFIG_STD_SETENV = "#define STD_SETENV",
                tm_devel = "Texmacs-" .. DEVEL_VERSION,
                tm_devel_release = "Texmacs-" .. DEVEL_VERSION .. "-" .. DEVEL_RELEASE,
                tm_stable = "Texmacs-" .. STABLE_VERSION,
                tm_stable_release = "Texmacs-" .. STABLE_VERSION .. "-" .. STABLE_RELEASE,
                }})

    ---------------------------------------------------------------------------
    -- add source and header files
    ---------------------------------------------------------------------------
    add_includedirs({
            "src/Data/Convert",
            "src/Data/Document",
            "src/Data/Drd",
            "src/Data/History",
            "src/Data/Observers",
            "src/Data/Parser",
            "src/Data/String",
            "src/Data/Tree",
            "src/Edit",
            "src/Edit/Editor",
            "src/Edit/Interface",
            "src/Edit/Modify",
            "src/Edit/Process",
            "src/Edit/Replace",
            "src/Graphics/Bitmap_fonts",
            "src/Graphics/Colors",
            "src/Graphics/Fonts",
            "src/Graphics/Gui",
            "src/Graphics/Handwriting",
            "src/Graphics/Mathematics",
            "src/Graphics/Pictures",
            "src/Graphics/Renderer",
            "src/Graphics/Spacial",
            "src/Graphics/Types",
            "src/Kernel/Abstractions",
            "src/Kernel/Containers",
            "src/Kernel/Types",
            "src/Plugins",
            "src/Plugins/Pdf",
            "src/Plugins/Qt",
            "src/Plugins/UniversalStacktrace",
            "src/Scheme",
            "src/Scheme/S7",
            "src/Scheme/Scheme",
            "src/Style/Environment",
            "src/Style/Evaluate",
            "src/Style/Memorizer",
            "src/System",
            "src/System/Memory",
            "src/System/IO",
            "src/System/Boot",
            "src/System/Classes",
            "src/System/Files",
            "src/System/Language",
            "src/System/Link",
            "src/System/Misc",
            "src/Texmacs",
            "src/Texmacs/Data",
            "src/Typeset",
            "src/Typeset/Bridge",
            "src/Typeset/Concat",
            "src/Typeset/Page",
            "TeXmacs/include"
        }, {public = true})

    if is_plat("macosx") then
        add_includedirs("src/Plugins/MacOS", {public = true})
    else
        add_includedirs("src/Plugins/Unix", {public = true})
    end

    add_files({
            "src/Data/**.cpp",
            "src/Edit/**.cpp",
            "src/Graphics/**.cpp",
            "src/Kernel/**.cpp",
            "src/Scheme/Scheme/**.cpp",
            "src/Scheme/S7/**.cpp",
            "src/Scheme/S7/*.c",
            "src/System/**.cpp",
            "src/Texmacs/Data/**.cpp",
            "src/Texmacs/Server/**.cpp",
            "src/Texmacs/Window/**.cpp",
            "src/Typeset/**.cpp",
            "src/Plugins/Axel/**.cpp",
            "src/Plugins/Bibtex/**.cpp",
            "src/Plugins/Database/**.cpp",
            "src/Plugins/Freetype/**.cpp",
            "src/Plugins/Jeaiii/**.cpp",
            "src/Plugins/Ispell/**.cpp",
            "src/Plugins/Metafont/**.cpp",
            "src/Plugins/LaTeX_Preview/**.cpp",
            "src/Plugins/Openssl/**.cpp",
            "src/Plugins/Sqlite3/**.cpp",
            "src/Plugins/Updater/**.cpp",
            "src/Plugins/Curl/**.cpp"})
            
    if not is_plat("wasm") then
        add_files({
                "src/Plugins/Cairo/**.cpp",
                "src/Plugins/Imlib2/**.cpp",
                "src/Plugins/Pdf/pdf_hummus_renderer.cpp",
                "src/Plugins/Ghostscript/**.cpp"})
    end

    if is_plat("mingw") then
        add_files("src/Plugins/Windows/**.cpp")
    elseif is_plat("wasm") then
        add_files("src/Plugins/Wasm/**.cpp")
    else
        add_files("src/Plugins/Unix/**.cpp")
    end

    if is_plat("macosx") then
        add_files({
                "src/Plugins/MacOS/HIDRemote.m",
                "src/Plugins/MacOS/mac_spellservice.mm",
                "src/Plugins/MacOS/mac_utilities.mm",
                "src/Plugins/MacOS/mac_app.mm"})
    end

    add_files({
        "src/Plugins/Qt/**.cpp",
        "src/Plugins/Qt/**.hpp"})

    if is_plat("wasm") then
        add_cxxflags({"-Wall","-Wextra"})
    end
    add_mxflags("-fno-objc-arc")
    add_cxxflags("-include src/System/config.h")
end 

option("libdl") do
    add_links("dl")
end

target("mogan") do 
    if is_plat("macosx") then
        set_filename("Mogan")
    end

    if is_plat("macosx") or is_plat("linux") then
        add_rules("qt.widgetapp")
    else
        add_rules("qt.widgetapp_static")
    end

    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    if is_plat("macosx") then
        add_frameworks("QtMacExtras")
    end

    if is_plat("linux") then
        add_rpathdirs("@executable_path/../lib")
    end
    add_deps("libmogan")
    add_syslinks("pthread")

    add_files("src/Texmacs/Texmacs/texmacs.cpp")

    if is_plat("wasm") then
        add_cxxflags({"-Wall","-Wextra"})
        add_ldflags({"--preload-file $(scriptdir)/TeXmacs@TeXmacs"})
    elseif is_plat("mingw") then
        add_packages("nowide_standalone")
    end
    set_installdir(INSTALL_DIR)
    after_install(function(target)
        local install_dir = path.join(target:installdir(), "/bin/")
        os.cp(target:targetfile(), install_dir)
    end)
end 

target("mogan_install") do
    if is_plat("macosx") or is_plat("linux") then
        add_deps("tm_shell")
    end
    add_deps("mogan")
    set_kind("phony")
    set_configvar("XMACS_VERSION", XMACS_VERSION)
    set_configvar("WIN_BIT_SIZE", "64")
    set_configdir(".")

    if is_plat("mingw") then
        add_configfiles(
            "(packages/windows/Xmacs.iss.in)",{
                filename = "Xmacs.iss",
                pattern = "@(.-)@",
            }
        )
    end

    if is_plat("macosx") then
        set_configvar("APPCAST", "")
        set_configvar("OSXVERMIN", "")
        add_configfiles(
            "(packages/macos/Info.plist.in)",{
                filename = "Info.plist",
                pattern = "@(.-)@",
            }
        )
    end

    set_configvar("prefix", INSTALL_DIR)
    set_configvar("exec_prefix", INSTALL_DIR)
    set_configvar("datarootdir", INSTALL_DIR.."/share")
    set_configvar("datadir", INSTALL_DIR.."/share")
    set_configvar("tmdata", INSTALL_DIR.."/Xmacs")
    set_configvar("tmbin", INSTALL_DIR.."/lib/xmacs/Xmacs")
    set_configvar("CONFIG_LIB_PATH", "LD_LIBRARY_PATH")

    add_configfiles(
        "(misc/man/mogan.1.in)",{
            filename = "mogan.1",
            pattern = "@([^\n]-)@",
        }
    )
    
    set_installdir(INSTALL_DIR)
    if is_plat("macosx") then
        add_installfiles({
            "packages/macos/new-mogan.icns",
            "packages/macos/TeXmacs-document.icns",
            "src/Plugins/Cocoa/(English.lproj/**)",
            "src/Plugins/Cocoa/(zh_CN.lproj/**)",
        })
        add_installfiles({
            "misc/scripts/mogan.sh"
        })
    elseif is_plat("linux") then
        add_installfiles({
            "misc/scripts/mogan"
        })
    elseif is_plat("mingw") then
        add_installfiles("packages/windows/TeXmacs-large.bmp")
        add_installfiles("packages/windows/TeXmacs-small.bmp")
        add_installfiles("packages/windows/Xmacs.ico")
        add_installfiles("packages/windows/TeXmacs.ico")
        add_installfiles("packages/windows/Xmacs.iss")
    end

    
    -- share/

    if is_plat("mingw") then
        add_installfiles("(plugins/**)")
    else 
        add_installfiles("(plugins/**)", {prefixdir="share/Xmacs"})
    end

    add_installfiles("misc/scripts/tm_gs", {prefixdir="share/Xmacs/bin"})
  
    add_installfiles("TeXmacs/misc/images/text-x-mogan.svg", {prefixdir="share/icons/hicolor/scalable/mimetypes"})
    add_installfiles("TeXmacs/misc/mime/mogan.desktop", {prefixdir="share/applications"})
    add_installfiles("TeXmacs/misc/mime/mogan.xml", {prefixdir="share/mime/packages"})
    add_installfiles("TeXmacs/misc/pixmaps/Xmacs.xpm", {prefixdir="share/pixmaps"})
  

    if is_plat("mingw") then
        add_installfiles({
            "TeXmacs(/doc/**)",
            "TeXmacs(/examples/**)",
            "TeXmacs(/fonts/**)",
            "TeXmacs(/langs/**)",
            "TeXmacs(/misc/**)",
            "TeXmacs(/packages/**)",
            "TeXmacs(/progs/**)",
            "TeXmacs(/styles/**)",
            "TeXmacs(/texts/**)",
            "TeXmacs/COPYING", -- copying files are different
            "TeXmacs/INSTALL",
            "LICENSE", -- license files are same
            "TeXmacs/README",
            "TeXmacs/TEX_FONTS",
        })
    else
        add_installfiles({
            "TeXmacs(/doc/**)",
            "TeXmacs(/examples/**)",
            "TeXmacs(/fonts/**)",
            "TeXmacs(/langs/**)",
            "TeXmacs(/misc/**)",
            "TeXmacs(/packages/**)",
            "TeXmacs(/progs/**)",
            "TeXmacs(/styles/**)",
            "TeXmacs(/texts/**)",
            "TeXmacs/COPYING", -- copying files are different
            "TeXmacs/INSTALL",
            "LICENSE", -- license files are same
            "TeXmacs/README",
            "TeXmacs/TEX_FONTS",
        }, {prefixdir="share/Xmacs"})
    end

    after_install(
        function (target)
            if is_plat("linux") then
                for _,size in ipairs({16, 32, 128, 512}) do
                    os.cp (
                        "TeXmacs/misc/images/new-mogan-"..size..".png", 
                        path.join(target:installdir(), "share/icons/hicolor/", size .."x"..size, "/apps/Xmacs.png"))
                end
            end
            if is_plat("macosx") or is_plat("linux")  then
                os.mkdir (path.join(target:installdir(), "share/Xmacs/plugins/shell/bin"))
                os.mv (path.join(target:installdir(), "bin/tm_shell"),
                       path.join(target:installdir(), "share/Xmacs/plugins/shell/bin"))
            end
            if is_plat("macosx") then
                os.cp ("packages/macos/Info.plist", path.join(target:installdir(), "../Info.plist"))
                os.rm (path.join(target:installdir(), "bin"))
            end
        end)
end


for _, filepath in ipairs(os.files("tests/**_test.cpp")) do
    if string.sub(filepath, 1, string.len("tests/Kernel")) ~= "tests/Kernel" then
        local testname = path.basename(filepath) 
        target(testname) do 
            add_runenvs("TEXMACS_PATH", path.join(os.projectdir(), "TeXmacs"))
            set_group("tests")
            add_deps("libmogan")
            set_languages("c++17")
            set_policy("check.auto_ignore_flags", false)
            add_rules("qt.console")
            add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg", "QtTest")
            add_syslinks("pthread")

            add_includedirs("tests/Base")
            add_files("tests/Base/base.cpp")
            add_files(filepath) 
            add_files(filepath, {rules = "qt.moc"})

            if is_plat("wasm") then
                add_cxxflags({"-Wall","-Wextra"})
                add_ldflags({"--preload-file $(scriptdir)/TeXmacs@TeXmacs"})
            elseif is_plat("mingw") then
                add_packages("nowide_standalone")
            end
        end
    end
end
