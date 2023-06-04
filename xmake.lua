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
    "src/Kernel/Abstractions/basic.cpp",
    "src/Kernel/Containers/**.cpp",
    "src/Kernel/Types/**.cpp",
    "src/Data/Drd/**.cpp",
    "src/System/IO/**.cpp",
    "src/System/Memory/**.cpp"
}
local l1_includedirs = {
    "src/System/Memory",
    "src/System/IO",
    "src/Plugins",
    "src/Kernel/Abstractions",
    "src/Kernel/Containers",
    "src/Kernel/Types",
    "src/Data/Drd"
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

for _, filepath in ipairs(os.files("tests/System/**_test.cpp")) do
    local testname = path.basename(filepath)
    target(testname) do
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)

        set_group("kernel_l2_tests")
        add_deps("libkernel_l2")
        add_syslinks("pthread")
        add_rules("qt.console")
        add_frameworks("QtTest")

        add_packages("libcurl")
        if is_plat("mingw") then
            add_packages("nowide_standalone")
        end
        add_includedirs("tests/Base")
        add_includedirs("$(buildir)/L2")
        add_includedirs(l1_includedirs)
        add_includedirs(l2_includedirs)
        add_files("tests/Base/base.cpp")
        add_files(filepath)
        add_files(filepath, {rules = "qt.moc"})
    end
end



local XMACS_VERSION="1.2.0"
local INSTALL_DIR="$(buildir)/package"

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
            "src/Kernel/Containers",
            "src/Kernel/Types",
            "src/Plugins",
            "src/Plugins/Pdf",
            "src/Plugins/Qt",
            "src/Scheme",
            "src/Scheme/S7",
            "src/Scheme/L1",
            "src/Scheme/L2",
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
            "src/Plugins/Updater/**.cpp"})

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

    set_installdir(INSTALL_DIR)
    after_install(function(target)
        local install_dir = path.join(target:installdir(), "/bin/")
        os.cp(target:targetfile(), install_dir)
        local bin_dir = path.directory(target:targetfile())
    end)
end 

target("mogan_install") do
    add_deps("mogan")
    set_kind("phony")
    set_configvar("XMACS_VERSION", XMACS_VERSION)
    set_configvar("WIN_BIT_SIZE", "64")

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
            "packages/macos/Xmacs.icns",
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
            "TeXmacs(/doc/main/**)",
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
        end)
end


for _, filepath in ipairs(os.files("tests/**_test.cpp")) do
    if string.sub(filepath, 1, string.len("tests/Kernel")) ~= "tests/Kernel"
       and string.sub(filepath, 1, string.len("tests/System")) ~= "tests/System" then
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
        set_group("doc_tests")
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
