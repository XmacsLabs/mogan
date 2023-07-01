-------------------------------------------------------------------------------
--
-- MODULE      : xmake.lua
-- DESCRIPTION : Xmake config file for TeXmacs
-- COPYRIGHT   : (C) 2022       jingkaimori
--                   2022-2023  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

-- Check CXX Types/Includes/Funcs/Snippets
includes("check_cxxtypes.lua")
includes("check_cxxincludes.lua")
includes("check_cxxfuncs.lua")
includes("check_cxxsnippets.lua")

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



---
--- Project: Mogan Editor
---
set_project("Mogan Editor")

set_allowedplats(
    -- these plat should be guaranteed
    "linux", "macosx", "mingw",
    --this plat is not maintained
    "android", "appletvos", "bsd", "cross", "iphoneos", "msys", "wasm", "watchos"
    -- because this cpp project use variant length arrays which is not supported by
    -- msvc, this project will not support windows env.
    -- because some package is not ported to cygwin env, this project will not
    -- support cygwin env.
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



--
-- Dependencies: Platform|Package Manager
--

-- GNU/Linux variants
-- [x] APT powered
-- [ ] pacman powered
-- [ ] portage powered
-- ...
if is_plat("linux") and (linuxos.name() == "ubuntu" or linuxos.name() == "uos") then
    add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
    add_requires("apt::libpng-dev", {alias="libpng"})
    add_requires("apt::zlib1g-dev", {alias="zlib"})
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
-- Let xrepo manage the dependencies for macOS and other GNU/Linux distros
    add_requires("libpng 1.6.37", {system=false})
    add_requires("libiconv 1.17", {system=false})
    add_requires("zlib 1.2.12", {system=false})
    add_requires("libjpeg v9e", {system=false})
    add_requires("libcurl 7.84.0", {system=false})
    add_requires("freetype 2.12.1", {system=false})
end

if is_plat("mingw") then
    add_requires("nowide_standalone 11.2.0", {system=false})
end

if is_plat("linux") then
    add_requires("fontconfig", {system = true})
end

local PDFHUMMUS_VERSION = "4.1"
add_requires("pdfhummus "..PDFHUMMUS_VERSION, {system=false,configs={libpng=true,libjpeg=true}})
add_requires("s7 2023.04.13", {system=false})


--
-- Library: L1 Kernel
--
set_configvar("QTTEXMACS", 1)

local l1_files = {
    "src/Kernel/Basic/**.cpp",
    "src/Kernel/Containers/**.cpp",
    "src/Kernel/Types/**.cpp",
    "src/Data/Drd/**.cpp",
    "src/System/IO/**.cpp",
    "src/System/Memory/**.cpp"
}
local l1_includedirs = {
    "src/Kernel/Basic",
    "src/Kernel/Abstractions",
    "src/Kernel/Containers",
    "src/Kernel/Types",
    "src/Data/Drd",
    "src/Data/String",
    "src/System/IO",
    "src/System/Memory",
    "src/Plugins"
}

target("libkernel_l1") do
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)

    set_kind("static")
    set_group("kernel_l1")
    set_basename("kernel_l1")

    if is_plat("mingw") then
        add_packages("nowide_standalone")
    end

    add_configfiles(
        "src/System/config_l1.h.xmake", {
            filename = "L1/config.h",
            variables = {
                OS_MINGW = is_plat("mingw")
            }
        }
    )
    add_configfiles(
        "src/System/tm_configure_l1.hpp.xmake", {
            filename = "L1/tm_configure.hpp",
            pattern = "@(.-)@"
        }
    )

    add_includedirs("$(buildir)/L1")
    add_includedirs(l1_includedirs, {public = true})
    add_files(l1_files)

    if is_plat("mingw") then
        add_includedirs({
            "src/Plugins/Windows"
        })
    end
end

for _, filepath in ipairs(os.files("tests/Kernel/**_test.cpp")) do
    local testname = path.basename(filepath)
    target(testname) do
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)

        set_group("kernel_l1_tests")
        add_deps("libkernel_l1")
        add_rules("qt.console")
        add_frameworks("QtTest")

        if is_plat("mingw") then
            add_packages("nowide_standalone")
        end
        add_includedirs("tests/Base")
        add_includedirs("$(buildir)/L1")
        add_includedirs(l1_includedirs)
        add_files("tests/Base/base.cpp")
        add_files(filepath)
        add_files(filepath, {rules = "qt.moc"})
    end
end



--
-- Library: L2 Kernel
--
local CONFIG_USER = "MOGAN_DEVELOPERS"
local CONFIG_DATE = "1970-01-01"
local TEXMACS_VERSION = "2.1.2"

local l2_files = {
    "src/System/Classes/**.cpp",
    "src/System/Files/**.cpp",
    "src/System/Misc/**.cpp",
    "src/Plugins/Curl/**.cpp",
    "src/Texmacs/Server/tm_debug.cpp",
}
local l2_includedirs = {
    "src/Kernel/Algorithms",
    "src/System/Files",
    "src/System/Classes",
    "src/System/Misc",
    "src/Plugins",
    "src/Texmacs",
}

target("libkernel_l2") do
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)

    set_kind("static")
    set_group("kernel_l2")
    set_basename("kernel_l2")

    add_packages("libcurl")
    if is_plat("mingw") then
        add_packages("nowide_standalone")
    end
    add_deps("libkernel_l1")

    add_configfiles(
        "src/System/config_l2.h.xmake", {
            filename = "L2/config.h",
            variables = {
                OS_MINGW = is_plat("mingw"),
                QTTEXMACS = false,
            }
        }
    )
    add_configfiles(
        "src/System/tm_configure_l2.hpp.xmake", {
            filename = "L2/tm_configure.hpp",
            pattern = "@(.-)@",
            variables = {
                CONFIG_USER = CONFIG_USER,
                CONFIG_DATE = CONFIG_DATE,
                CONFIG_OS = CONFIG_OS,
                VERSION = TEXMACS_VERSION,
            }
        }
    )

    add_headerfiles({
        "src/System/Classes/tm_timer.hpp",
        "src/Texmacs/tm_debug.hpp",
    })
    add_includedirs("$(buildir)/L2")
    add_includedirs(l2_includedirs, {public = true})
    add_files(l2_files)

    if is_plat("linux") or is_plat("macosx") then
        add_includedirs("src/Plugins/Unix")
        add_files("src/Plugins/Unix/**.cpp")
    end

    if is_plat("mingw") then
        add_includedirs("src/Plugins/Windows")
        add_files("src/Plugins/Windows/**.cpp")
    end
end

--
-- Library: L3 Kernel
--
local l3_files = {
    "src/Kernel/Abstractions/**.cpp",
    "src/Data/History/**.cpp",
    "src/Data/Observers/**.cpp",
    "src/Data/Scheme/**.cpp",
    "src/Data/String/**.cpp",
    "src/Data/Document/new_document.cpp",
    "src/Scheme/L1/**.cpp",
    "src/Scheme/L2/**.cpp",
    "src/Scheme/L3/**.cpp",
    "src/Scheme/S7/**.cpp",
    "src/Scheme/Scheme/object.cpp",
    "src/System/Config/**.cpp",
    "src/System/Language/locale.cpp",
    "src/System/Classes/**.cpp",
    "src/System/Files/**.cpp",
    "src/System/Misc/**.cpp",
    "src/Plugins/Curl/**.cpp",
    "src/Texmacs/Server/tm_debug.cpp",
}
local l3_includedirs = {
    "src/Kernel/Abstractions",
    "src/Data/Document",
    "src/Data/History",
    "src/Data/Observers",
    "src/Data/Scheme",
    "src/Data/String",
    "src/Kernel/Abstractions",
    "src/Kernel/Algorithms",
    "src/Scheme",
    "src/Scheme/Scheme",
    "src/Scheme/S7",
    "src/Scheme/L1",
    "src/Scheme/L2",
    "src/Scheme/L3",
    "src/System/Config",
    "src/System/Language",
    "src/System/Files",
    "src/System/Classes",
    "src/System/Misc",
    "src/Plugins",
    "src/Texmacs",
}
target("libkernel_l3") do
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)

    set_kind("static")
    set_group("kernel_l3")
    set_basename("kernel_l3")

    add_packages("libcurl")
    add_packages("s7")
    if is_plat("mingw") then
        add_packages("nowide_standalone")
    end
    add_deps("libkernel_l1")

    add_configfiles(
        "src/System/config_l3.h.xmake", {
            filename = "L3/config.h",
            variables = {
                OS_MINGW = is_plat("mingw"),
                QTTEXMACS = false,
            }
        }
    )
    add_configfiles(
        "src/System/tm_configure_l3.hpp.xmake", {
            filename = "L3/tm_configure.hpp",
            pattern = "@(.-)@",
            variables = {
                CONFIG_USER = CONFIG_USER,
                CONFIG_DATE = CONFIG_DATE,
                CONFIG_OS = CONFIG_OS,
                VERSION = TEXMACS_VERSION,
            }
        }
    )

    add_includedirs("$(buildir)/L3")
    add_includedirs("$(buildir)")
    add_includedirs(l3_includedirs, {public = true})
    add_files(l3_files)

    if is_plat("linux") or is_plat("macosx") then
        add_includedirs("src/Plugins/Unix")
        add_files("src/Plugins/Unix/**.cpp")
    end

    if is_plat("mingw") then
        add_includedirs("src/Plugins/Windows")
        add_files("src/Plugins/Windows/**.cpp")
    end
end

for _, filepath in ipairs(os.files("tests/System/Classes/**_test.cpp")) do
    local testname = path.basename(filepath)
    target(testname) do
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)

        set_group("kernel_l3_tests")
        add_deps("libkernel_l3")
        add_syslinks("pthread")
        add_rules("qt.console")
        add_frameworks("QtTest")

        add_packages("libcurl")
        if is_plat("mingw") then
            add_packages("nowide_standalone")
        end
        add_includedirs("tests/Base")
        add_includedirs("$(buildir)/L3")
        add_includedirs(l1_includedirs)
        add_includedirs(l3_includedirs)
        add_files("tests/Base/base.cpp")
        add_files(filepath)
        add_files(filepath, {rules = "qt.moc"})
    end
end


local XMACS_VERSION="1.2.0"
local INSTALL_RELATIVE_DIR="packages/org.xmacslabs.mogan/data/"
local INSTALL_DIR=path.join("$(buildir)", INSTALL_RELATIVE_DIR)

local DEVEL_VERSION = TEXMACS_VERSION
local DEVEL_RELEASE = 1
local STABLE_VERSION = TEXMACS_VERSION
local STABLE_RELEASE = 1


set_configvar("QTPIPES", 1)
add_defines("QTPIPES")

set_configvar("USE_QT_PRINTER", 1)
add_defines("USE_QT_PRINTER")

set_configvar("USE_ICONV", 1)
set_configvar("USE_CURL", 1)
set_configvar("USE_FREETYPE", 1)
set_configvar("USE_GS", 1)
set_configvar("PDF_RENDERER", 1)
set_configvar("PDFHUMMUS_NO_TIFF", true)
set_configvar("PDFHUMMUS_VERSION", PDFHUMMUS_VERSION)

if is_plat("mingw") then
    set_configvar("GS_EXE", "bin/gs.exe")
else
    set_configvar("GS_EXE", "/usr/bin/gs")
end

if is_plat("mingw") then
else if is_plat("macosx") then
    set_configvar("USE_STACK_TRACE", true)
    set_configvar("AQUATEXMACS", true)
end
    set_configvar("USE_STACK_TRACE", true)
end

set_configvar("STDC_HEADERS", true)

set_version(TEXMACS_VERSION)


add_configfiles(
    "src/System/config.h.xmake", {
        filename = "config.h",
        variables = {
            GS_FONTS = "../share/ghostscript/fonts:/usr/share/fonts:",
            GS_LIB = "../share/ghostscript/9.06/lib:",
            OS_GNU_LINUX = is_plat("linux"),
            OS_MACOS = is_plat("macosx"),
            MACOSX_EXTENSIONS = is_plat("macosx"),
            OS_MINGW = is_plat("mingw"),
            SIZEOF_VOID_P = 8,
            USE_FONTCONFIG = is_plat("linux"),
            USE_STACK_TRACE = not is_plat("mingw"),
            USE_GS = true,
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



target("libmogan") do
    set_basename("mogan")
    set_version(TEXMACS_VERSION)
    
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)
    if is_plat("linux") then
        add_rules("qt.shared")
    else
        add_rules("qt.static")
    end
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    if is_plat("macosx") then
        add_frameworks("QtMacExtras")
    end

    add_packages("libpng")
    add_packages("libiconv")
    add_packages("zlib")
    add_packages("libjpeg")
    add_packages("libcurl")
    add_packages("freetype")
    add_packages("pdfhummus")
    add_packages("nowide_standalone")
    add_packages("s7")

    if is_plat("mingw") then
        add_syslinks("wsock32", "ws2_32", "crypt32","secur32", {public = true})
    end
    
    if is_plat("linux") then
        add_packages("fontconfig")
    end

    ---------------------------------------------------------------------------
    -- generate config files. see also:
    --    * https://github.com/xmake-io/xmake/issues/320
    --    * https://github.com/xmake-io/xmake/issues/342
    ---------------------------------------------------------------------------
    -- check for dl library
    -- configvar_check_cxxfuncs("TM_DYNAMIC_LINKING","dlopen")
    add_options("libdl")

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
            "src/Data/Scheme",
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
            "src/Kernel/Algorithms",
            "src/Kernel/Basic",
            "src/Kernel/Containers",
            "src/Kernel/Types",
            "src/Plugins",
            "src/Plugins/Pdf",
            "src/Plugins/Qt",
            "src/Scheme",
            "src/Scheme/S7",
            "src/Scheme/L1",
            "src/Scheme/L2",
            "src/Scheme/L3",
            "src/Scheme/L4",
            "src/Scheme/L5",
            "src/Scheme/Plugins",
            "src/Scheme/Scheme",
            "src/Style/Environment",
            "src/Style/Evaluate",
            "src/Style/Memorizer",
            "src/System",
            "src/System/Memory",
            "src/System/IO",
            "src/System/Boot",
            "src/System/Classes",
            "src/System/Config",
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
            "$(buildir)",
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
            "src/Scheme/L1/**.cpp",
            "src/Scheme/L2/**.cpp",
            "src/Scheme/L3/**.cpp",
            "src/Scheme/L4/**.cpp",
            "src/Scheme/L5/**.cpp",
            "src/Scheme/Plugins/**.cpp",
            "src/System/**.cpp",
            "src/Texmacs/Data/**.cpp",
            "src/Texmacs/Server/**.cpp",
            "src/Texmacs/Window/**.cpp",
            "src/Typeset/**.cpp",
            "src/Plugins/Bibtex/**.cpp",
            "src/Plugins/Curl/**.cpp",
            "src/Plugins/Database/**.cpp",
            "src/Plugins/Freetype/**.cpp",
            "src/Plugins/Pdf/**.cpp",
            "src/Plugins/Ghostscript/**.cpp",
            "src/Plugins/Ispell/**.cpp",
            "src/Plugins/Metafont/**.cpp",
            "src/Plugins/LaTeX_Preview/**.cpp",
            "src/Plugins/Openssl/**.cpp",
            "src/Plugins/Tex/**.cpp",
            "src/Plugins/Updater/**.cpp",
            "src/Plugins/Xml/**.cpp"})

    if is_plat("mingw") then
        add_files("src/Plugins/Windows/**.cpp")
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

    add_mxflags("-fno-objc-arc")
    add_cxxflags("-include $(buildir)/config.h")
end 

option("libdl") do
    add_links("dl")
end

target("mogan") do 
    set_version(XMACS_VERSION)
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

    if is_plat("mingw") then
        add_packages("nowide_standalone")
    end

    if is_plat("linux") then
        add_rpathdirs("@executable_path/../lib")
    end
    add_deps("libmogan")
    add_syslinks("pthread")

    add_files("src/Texmacs/Texmacs/texmacs.cpp")
    if is_plat("mingw") then
        add_configfiles("packages/windows/resource.rc.in", {
            filename = "resource.rc"
        })
        add_configfiles("packages/windows/TeXmacs.ico", {
            onlycopy = true
        })
        add_files("$(buildir)/resource.rc")
    end

    on_config(function (target) 
        -- use relative path here to avoid import failure on windows
        local scheme_path = path.join("src", "Scheme")
        local build_glue_path = path.join("src", "Scheme", "Glue")
        local build_glue = import("build_glue", {rootdir = build_glue_path})
        for _, filepath in ipairs(os.filedirs(path.join(scheme_path, "**/glue_*.lua"))) do
            local glue_name = path.basename(filepath)
            local glue_dir = path.directory(filepath)
            local glue_table = import(glue_name, {rootdir = glue_dir})()
            io.writefile(
                path.join("$(buildir)", glue_name .. ".cpp"),
                build_glue(glue_table, glue_name))
            cprint("generating scheme glue %s ... %s", glue_name, "${color.success}${text.success}")
        end
    end)

    set_installdir(INSTALL_DIR)
end 

target("mogan_install") do
    add_deps("mogan")
    set_kind("phony")
    set_installdir(INSTALL_DIR)
    set_configdir(INSTALL_DIR)
    set_configvar("DEVEL_VERSION", DEVEL_VERSION)
    set_configvar("PACKAGE", "Mogan Editor")
    set_configvar("XMACS_VERSION", XMACS_VERSION)

    -- install man.1 manual file
    add_configfiles(
        "(misc/man/texmacs.1.in)",{
            filename = "texmacs.1",
            pattern = "@([^\n]-)@",
        }
    )

    -- package metadata
    if is_plat("macosx") then
        set_configvar("APPCAST", "")
        set_configvar("OSXVERMIN", "")
        add_configfiles(
            "(packages/macos/Info.plist.in)",{
                filename = "Info.plist",
                pattern = "@(.-)@",
            }
        )
        add_installfiles({
            "packages/macos/Xmacs.icns",
            "packages/macos/TeXmacs-document.icns",
            "src/Plugins/Cocoa/(English.lproj/**)",
            "src/Plugins/Cocoa/(zh_CN.lproj/**)",
            "misc/scripts/mogan.sh"
        })
    elseif is_plat("linux") then
        add_installfiles({
            "misc/scripts/mogan"
        })
    end
  
    -- install icons
    add_installfiles("TeXmacs/misc/images/text-x-mogan.svg", {prefixdir="share/icons/hicolor/scalable/mimetypes"})
    add_installfiles("TeXmacs/misc/mime/mogan.desktop", {prefixdir="share/applications"})
    add_installfiles("TeXmacs/misc/mime/mogan.xml", {prefixdir="share/mime/packages"})
    add_installfiles("TeXmacs/misc/pixmaps/Xmacs.xpm", {prefixdir="share/pixmaps"})
  
    -- install texmacs runtime files
    local TeXmacs_files = {
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
        "(plugins/**)" -- plugin files
    }
    if is_plat("mingw") then
        add_installfiles(TeXmacs_files)
    else
        add_installfiles("misc/scripts/tm_gs", {prefixdir="share/Xmacs/bin"})
        add_installfiles(TeXmacs_files, {prefixdir="share/Xmacs"})
    end

    -- install tm files for testing purpose
    add_installfiles({
        "TeXmacs(/tests/*.tm)",
        "TeXmacs(/tests/*.bib)",
    }, {prefixdir="share/Xmacs"})

    after_install(
        function (target)
            os.cp (
                "TeXmacs/misc/images/texmacs.svg", 
                path.join(target:installdir(), "share/icons/hicolor/scalable/apps", "Mogan.svg"))
            for _,size in ipairs({32, 48, 64, 128, 256, 512}) do
                os.cp (
                    "TeXmacs/misc/images/texmacs-"..size..".png",
                    path.join(target:installdir(), "share/icons/hicolor/", size .."x"..size, "/apps/Xmacs.png"))
            end
            if is_plat("macosx") then
                os.cp ("$(buildir)/packages/macos/Info.plist", path.join(target:installdir(), "../Info.plist"))
            end
            if is_plat("macosx") and is_arch("arm64") then
                os.execv("codesign", {"--force", "--deep", "--sign", "-", target:installdir().."../.."})
            end

            if is_plat("mingw") then
                import("detect.sdks.find_qt")
                import("core.base.option")
                import("core.project.config")

                -- get qt sdk
                local qt = assert(find_qt(), "Qt SDK not found!")

                -- get windeployqt
                local windeployqt = path.join(qt.bindir, "windeployqt.exe")
                assert(os.isexec(windeployqt), "windeployqt.exe not found!\n if you are using msys, package mingw-w64-<arch>-qt5-translations and mingw-w64-<arch>-qt5-tools is needed!")
                
                -- deploy necessary dll
                local buildir = config.buildir()
                local deploy_argv = {"--compiler-runtime", "-printsupport"}
                if option.get("diagnosis") then
                    table.insert(deploy_argv, "--verbose=2")
                elseif option.get("verbose") then
                    table.insert(deploy_argv, "--verbose=1")
                else
                    table.insert(deploy_argv, "--verbose=0")
                end
                local install_file = path.join(buildir, INSTALL_RELATIVE_DIR, "bin")
                table.insert(deploy_argv, install_file)
                os.iorunv(windeployqt, deploy_argv)
            end
        end)
end

target("windows_installer") do
    set_kind("phony")
    set_enabled(is_plat("mingw"))
    add_deps("mogan_install")
    set_configvar("PACKAGE_DATE", os.date("%Y-%m-%d"))
    set_configvar("XMACS_VERSION", XMACS_VERSION)
    set_installdir("$(buildir)")
    add_configfiles(
        "packages/windows/(config/config.in.xml)",{
            filename = "config.xml",
        }
    )
    add_installfiles({
        "packages/windows/config/TeXmacs-small.png",
        "packages/windows/config/TeXmacs-large.png",
        "packages/windows/TeXmacs.ico",
    }, {prefixdir = "config/"})
    add_configfiles(
        "packages/windows/(packages/org.xmacslabs.mogan/meta/package.in.xml)",{
            filename = "package.xml",
        }
    )
    add_installfiles({
        "packages/windows/packages/org.xmacslabs.mogan/meta/installscript.qs",
        "LICENSE"}, {prefixdir = "packages/org.xmacslabs.mogan/meta/"})
    after_install(function (target, opt)
        import("core.base.option")
        import("core.project.config")
        import("detect.sdks.find_qt")
        import("lib.detect.find_program")

        -- get qt sdk
        local qt = assert(find_qt(), "Qt SDK not found!")

        -- get binarycreator
        local binarycreator_missing_prompt = [[
binarycreator.exe not found!
if you are using msys, package mingw-w64-<arch>-qt-installer-framework is needed!"
]]
        local binarycreator_path = find_program("binarycreator", {
            check = "--help",
            paths = {
                path.join(os.getenv("IQTA_TOOLS"), "/QtInstallerFramework/4.6/bin"),
                "$(env PATH)"}})
        if not binarycreator_path then
            binarycreator_path = path.translate(path.join(qt.bindir, "binarycreator.exe"))
            assert(os.isexec(binarycreator_path), binarycreator_missing_prompt)
        end
        assert(binarycreator_path, binarycreator_missing_prompt)

        -- generate windows package
        local buildir = config.buildir()
        local package_argv = {
            "--config", path.join(buildir, "config/config.xml"),
            "--packages", path.join(buildir, "packages"),
            path.join(buildir, "Mogan-v"..XMACS_VERSION.."-64bit-installer.exe")
        }
        os.iorunv(binarycreator_path, package_argv)
    end)
end

for _, filepath in ipairs(os.files("tests/**_test.cpp")) do
    if string.sub(filepath, 1, string.len("tests/Kernel")) ~= "tests/Kernel"
       and string.sub(filepath, 1, string.len("tests/System/Classes")) ~= "tests/System/Classes" then
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
            add_packages("s7")

            add_includedirs({"$(buildir)", "tests/Base"})
            add_files("tests/Base/base.cpp")
            add_files(filepath) 
            add_files(filepath, {rules = "qt.moc"})
        end
    end
end

for _, filepath in ipairs(os.files("TeXmacs/tests/*.scm")) do
    local testname = path.basename(filepath)
    target(testname) do
        set_kind("phony")
        set_group("integration_tests")
        on_run(function (target)
            name = target:name()
            params = {"-headless", "-b", "TeXmacs/tests/"..name..".scm", "-x", "(test_"..name..")", "-q"}
            if is_plat("macosx") then
                os.execv("$(buildir)/macosx/$(arch)/release/Mogan.app/Contents/MacOS/Mogan", params)
            else
                os.execv("$(buildir)/package/bin/mogan", params)
            end
        end)
    end
end

-- xmake plugins
add_configfiles(
    "misc/doxygen/Doxyfile.in", {
        filename = "doxyfile",
        pattern = "@(.-)@",
        variables = {
            PACKAGE = "Mogan Editor",
            DOXYGEN_DIR = get_config("buildir"),
            DEVEL_VERSION = DEVEL_VERSION,
        }
    }
)
