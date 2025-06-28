-------------------------------------------------------------------------------
--
-- MODULE      : xmake.lua
-- DESCRIPTION : Xmake config file for TeXmacs
-- COPYRIGHT   : (C) 2022       jingkaimori
--                   2022-2024  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

set_project("Liii STEM Suite")
local XMACS_VERSION="2025.0.11"

-- because this cpp project use variant length arrays which is not supported by
-- msvc, this project will not support windows env.
-- because some package is not ported to cygwin env, this project will not
-- support cygwin env.
set_allowedplats("linux", "macosx", "windows")

if is_plat("linux") then
    set_configvar("OS_GNU_LINUX", true)
else
    set_configvar("OS_GNU_LINUX", false)
end
if is_plat("macosx") then
    set_configvar("OS_MACOS", true)
else
    set_configvar("OS_MACOS", false)
end
if is_plat("windows") then
    set_configvar("OS_WIN", true)
else
    set_configvar("OS_WIN", false)
end

-- add releasedbg, debug and release modes for different platforms.
-- debug mode cannot run on mingw with qt precompiled binary
set_allowedmodes("releasedbg", "release", "debug")
add_rules("mode.releasedbg", "mode.release", "mode.debug")

add_repositories("liii-repo xmake")

TBOX_VERSION= "1.7.5"
LOLLY_VERSION= "1.4.26"
S7_VERSION = "20240816"

package("liii-libaesgm")
    set_homepage("https://github.com/xmake-mirror/libaesgm")
    set_description("https://repology.org/project/libaesgm/packages")

    set_sourcedir(path.join(os.scriptdir(), "3rdparty/libaesgm"))

    on_install("linux", "macosx", "windows", "mingw", function (package)
        if package:is_plat("windows", "mingw") and package:is_arch("arm", "arm64") then
            -- Windows is always little endian
            io.replace("brg_endian.h", [[
#elif 0     /* **** EDIT HERE IF NECESSARY **** */
#  define PLATFORM_BYTE_ORDER IS_LITTLE_ENDIAN]], [[
#elif 1     /* Edited: Windows ARM is little endian */
#  define PLATFORM_BYTE_ORDER IS_LITTLE_ENDIAN]], { plain = true })
        end
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)

    on_test(function (package)
        assert(package:has_cfuncs("aes_init", {includes = "aes.h"}))
    end)
package_end()

PDFHUMMUS_VERSION = "4.6.2"
package("liii-pdfhummus")
    set_homepage("https://www.pdfhummus.com/")
    set_description("High performance library for creating, modiyfing and parsing PDF files in C++ ")
    set_license("Apache-2.0")

    set_sourcedir(path.join(os.scriptdir(), "3rdparty/pdfhummus"))

    add_deps("zlib", "freetype", "liii-libaesgm")

    add_configs("libtiff", {description = "Supporting tiff image", default = false, type = "boolean"})
    add_configs("libjpeg", {description = "Support DCT encoding", default = false, type = "boolean"})
    add_configs("libpng", {description = "Support png image", default = false, type = "boolean"})

    if is_plat("linux") then
        add_syslinks("m")
    end

    on_load(function (package)
        for _, dep in ipairs({"libtiff", "libpng", "libjpeg"}) do
            if package:config(dep) then
                package:add("deps", dep)
            end
        end
    end)
    on_install("linux", "windows", "mingw", "macosx", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        for _, dep in ipairs({"libtiff", "libpng", "libjpeg"}) do
            if package:config(dep) then
                configs[dep] = true
            end
        end
        import("package.tools.xmake").install(package, configs)
    end)

    on_test(function (package)
        assert(package:check_cxxsnippets({test = [[
            #include "PDFWriter/PDFWriter.h"
            #include <iostream>
            using namespace std;
            using namespace PDFHummus;
            void test() {
                PDFWriter pdfWriter;
                pdfWriter.Reset();
            }
        ]]}, {configs = {languages = "c++11"}}))
    end)
package_end()

includes("@builtin/check")
configvar_check_cxxtypes("HAVE_INTPTR_T", "intptr_t", {includes = {"memory"}})
configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")
configvar_check_cxxincludes("HAVE_STDINT_H", "stdint.h")

function using_apt ()
    return linuxos.name() == "debian"
           or linuxos.name() == "ubuntu"
           or linuxos.name() == "uos"
end

function using_legacy_apt ()
    return (linuxos.name() == "uos") or (linuxos.name () == "ubuntu" and linuxos.version():major() == 20)
end

local FREETYPE_VERSION = "2.12.1"
local LIBGIT2_VERSION = "1.7.1"
local LIBICONV_VERSION = "1.17"

-- package: s7
add_requires("s7", {system=false})
add_requires("tbox", {system=false})
add_requires("lolly", {system=false})
add_requires("cpr", {system=false})
if is_plat ("windows") then
    add_requires("libiconv "..LIBICONV_VERSION, {system=false})
end

add_requires("libjpeg")
if is_plat("linux") then
    add_requires("apt::libpng-dev", {alias="libpng"})
    add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
end

add_requires("liii-pdfhummus", {system=false,configs={libpng=true,libjpeg=true}})
if using_legacy_apt() then
    add_requires("freetype "..FREETYPE_VERSION, {system=false})
    add_requireconfs("liii-pdfhummus.freetype", {version = FREETYPE_VERSION, system = false, override=true})
else
    if is_plat("linux") then
        add_requires("apt::libfreetype-dev", {alias="freetype"})
    end
end

-- package: libgit2
if is_plat ("linux") and using_apt() then
    add_requires("apt::libgit2-dev", {alias="libgit2"})
elseif not is_plat("wasm") then
    add_requires("libgit2 "..LIBGIT2_VERSION, {system=false})
end

-- package: qt6widgets
QT6_VERSION="6.5.3"
if is_plat("windows") then
    add_requires("qt6widgets "..QT6_VERSION)
end


set_configvar("USE_FREETYPE", 1)

target ("goldfish") do
    set_languages("c++17")
    set_targetdir("$(projectdir)/TeXmacs/plugins/goldfish/bin/")
    add_files ("$(projectdir)/TeXmacs/plugins/goldfish/src/goldfish.cpp")
    add_packages("s7")
    add_packages("tbox")
    add_packages("cpr")
    on_install(function (target)
    end)
end

function build_glue_on_config()
    on_config(function (target)
        import("core.project.depend")
        -- use relative path here to avoid import failure on windows
        local scheme_path = path.join("src", "Scheme")
        local build_glue_path = path.join("src", "Scheme", "Glue")
        local build_glue = import("build_glue", {rootdir = build_glue_path})
        for _, filepath in ipairs(os.filedirs(path.join(scheme_path, "**/glue_*.lua"))) do
            depend.on_changed(function ()
                local glue_name = path.basename(filepath)
                local glue_dir = path.directory(filepath)
                local glue_table = import(glue_name, {rootdir = glue_dir})()
                io.writefile(
                    path.join("$(buildir)/glue", glue_name .. ".cpp"),
                    build_glue(glue_table, glue_name))
                cprint("generating scheme glue %s ... %s", glue_name, "${color.success}${text.success}")
            end, {
                values = {true},
                files = {filepath, path.join(build_glue_path, "build_glue.lua")},
                always_changed = false
            })
        end
    end)
end

local moe_files = {
    "3rdparty/moebius/Data/History/**.cpp",
    "3rdparty/moebius/Data/String/**.cpp",
    "3rdparty/moebius/Data/Tree/**.cpp",
    "3rdparty/moebius/Kernel/Types/**.cpp",
    "3rdparty/moebius/Kernel/Abstractions/**.cpp",
    "3rdparty/moebius/Scheme/**.cpp",
    "3rdparty/moebius/moebius/**.cpp",
}
local moe_includedirs = {
    "3rdparty/moebius/Data/History",
    "3rdparty/moebius/Data/String",
    "3rdparty/moebius/Data/Tree",
    "3rdparty/moebius/Kernel/Types",
    "3rdparty/moebius/Kernel/Abstractions",
    "3rdparty/moebius/Scheme",
    "3rdparty/moebius/Scheme/L1",
    "3rdparty/moebius/Scheme/L2",
    "3rdparty/moebius/Scheme/L3",
    "3rdparty/moebius/Scheme/S7",
    "3rdparty/moebius/Scheme/Scheme",
    "3rdparty/moebius/",
}

target("libmoebius") do
    set_kind ("static")
    set_languages("c++17")
    set_encodings("utf-8")
    set_basename("moebius")

    add_includedirs(moe_includedirs)
    add_files(moe_files)

    add_packages("lolly")
    add_packages("s7")

    on_install(function (target)
    end)
end


target("libmogan") do
    set_basename("mogan")
    local TEXMACS_VERSION = "2.1.4"
    local DEVEL_VERSION = TEXMACS_VERSION
    local DEVEL_RELEASE = 1
    local STABLE_VERSION = TEXMACS_VERSION
    local STABLE_RELEASE = 1
    set_version(TEXMACS_VERSION)

    if is_plat("windows") then
        set_runtimes("MT")
    end
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)
    set_encodings("utf-8")

    add_deps("libmoebius")
    
    set_policy("check.auto_ignore_flags", false)
    add_rules("qt.static")
    on_install(function (target)
        print("No need to install libmogan")
    end)
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")

    build_glue_on_config()
    set_configvar("QTTEXMACS", 1)
    add_defines("QTTEXMACS")
    set_configvar("QTPIPES", 1)
    add_defines("QTPIPES")
    set_configvar("USE_QT_PRINTER", 1)
    add_defines("USE_QT_PRINTER")

    add_packages("lolly")
    add_packages("liii-pdfhummus")
    add_packages("freetype")
    add_packages("s7")
    add_packages("libgit2")
    if not is_plat("macosx") then
        add_packages("libiconv")
    end

    if is_plat("windows") then
        add_syslinks("secur32", "shell32", "winhttp", "rpcrt4", {public = true})
    elseif is_plat("macosx") then
        add_syslinks("iconv")
    end

    ---------------------------------------------------------------------------
    -- generate config files. see also:
    --    * https://github.com/xmake-io/xmake/issues/320
    --    * https://github.com/xmake-io/xmake/issues/342
    ---------------------------------------------------------------------------
    set_configdir("src/System")
    -- check for dl library
    -- configvar_check_cxxfuncs("TM_DYNAMIC_LINKING","dlopen")
    configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")

    set_configvar("STDC_HEADERS", true)

    set_configvar("GS_EXE", "/usr/bin/gs")

    set_configvar("PDFHUMMUS_NO_TIFF", true)
    add_configfiles(
        "src/System/config.h.xmake", {
            filename = "config.h",
            variables = {
                GS_FONTS = "../share/ghostscript/fonts:/usr/share/fonts:",
                GS_LIB = "../share/ghostscript/9.06/lib:",
                OS_MACOS = is_plat("macosx"),
                MACOSX_EXTENSIONS = is_plat("macosx"),
                SIZEOF_VOID_P = 8,
                USE_ICONV = true,
                USE_PLUGIN_GS = true,
                USE_PLUGIN_BIBTEX = true,
                USE_PLUGIN_LATEX_PREVIEW = true,
                USE_PLUGIN_TEX = true,
                USE_PLUGIN_ISPELL = true,
                USE_PLUGIN_PDF = true,
                USE_PLUGIN_SPARKLE = false,
                USE_PLUGIN_HTML = true,
                USE_PLUGIN_GIT = not is_plat("wasm")
                }})

    if is_plat("linux") then 
        set_configvar("CONFIG_OS", "GNU_LINUX")
    elseif is_subhost("cygwin") then
        set_configvar("CONFIG_OS", "CYGWIN")
    else 
        set_configvar("CONFIG_OS", "")
    end

    configvar_check_cxxsnippets(
        "CONFIG_LARGE_POINTER", [[
            #include <stdlib.h>
            static_assert(sizeof(void*) == 8, "");]])
    add_configfiles(
        "src/System/tm_configure.hpp.xmake", {
            filename = "tm_configure.hpp",
            pattern = "@(.-)@",
            variables = {
                TEXMACS_VERSION = TEXMACS_VERSION,
                XMACS_VERSION = XMACS_VERSION,
                CONFIG_USER = os.getenv("USER") or "unknown",
                CONFIG_DATE = os.time(),
                CONFIG_STD_SETENV = "#define STD_SETENV",
                tm_devel = "Texmacs-" .. DEVEL_VERSION,
                tm_devel_release = "Texmacs-" .. DEVEL_VERSION .. "-" .. DEVEL_RELEASE,
                tm_stable = "Texmacs-" .. STABLE_VERSION,
                tm_stable_release = "Texmacs-" .. STABLE_VERSION .. "-" .. STABLE_RELEASE,
                PDFHUMMUS_VERSION = PDFHUMMUS_VERSION,
                LOLLY_VERSION = LOLLY_VERSION,
                }})

    ---------------------------------------------------------------------------
    -- add source and header files
    ---------------------------------------------------------------------------
    add_includedirs(moe_includedirs)
    add_includedirs({
            "src/Data/Convert",
            "src/Data/Document",
            "src/Data/History",
            "src/Data/Observers",
            "src/Data/Parser",
            "src/Data/String",
            "src/Data/Tree",
            "src/Data/Scheme",
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
            "src/Plugins/UniversalStacktrace",
            "src/Plugins/Html",
            "src/Plugins/Git",
            "src/Scheme",
            "src/Scheme/L1",
            "src/Scheme/L2",
            "src/Scheme/L3",
            "src/Scheme/L4",
            "src/Scheme/L5",
            "src/Scheme/Plugins",
            "src/Scheme/S7",
            "src/Scheme/Scheme",
            "src/Style/Environment",
            "src/Style/Evaluate",
            "src/Style/Memorizer",
            "src/System",
            "src/System/Boot",
            "src/System/Classes",
            "src/System/Config",
            "src/System/Files",
            "src/System/IO",
            "src/System/Language",
            "src/System/Link",
            "src/System/Memory",
            "src/System/Misc",
            "src/Texmacs",
            "src/Texmacs/Data",
            "src/Typeset",
            "src/Typeset/Bridge",
            "src/Typeset/Concat",
            "src/Typeset/Page",
            "TeXmacs/include",
            "$(buildir)/glue",
            "$(projectdir)/TeXmacs/plugins/goldfish/src/"
        }, {public = true})

    add_files({
            "src/Data/**.cpp",
            "src/Edit/**.cpp",
            "src/Graphics/**.cpp",
            "src/Kernel/**.cpp",
            "src/Scheme/Scheme/**.cpp",
            "src/Scheme/S7/**.cpp",
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
            "src/Plugins/Database/**.cpp",
            "src/Plugins/Freetype/**.cpp",
            "src/Plugins/Pdf/**.cpp",
            "src/Plugins/Ghostscript/**.cpp",
            "src/Plugins/Ispell/**.cpp",
            "src/Plugins/Metafont/**.cpp",
            "src/Plugins/LaTeX_Preview/**.cpp",
            "src/Plugins/Openssl/**.cpp",
            "src/Plugins/Tex/**.cpp",
            "src/Plugins/Xml/**.cpp",
            "src/Plugins/Html/**.cpp",
            "src/Plugins/Git/**.cpp",
            "src/Plugins/Updater/**.cpp",
            "$(projectdir)/TeXmacs/plugins/goldfish/src/**.cpp"})

    add_files({
        "src/Plugins/Qt/**.cpp",
        "src/Plugins/Qt/**.hpp"})

    if is_plat("macosx") then
        plugin_macos_srcs = {
            "$(projectdir)/src/Plugins/MacOS/HIDRemote.m",
            "$(projectdir)/src/Plugins/MacOS/mac_spellservice.mm",
            "$(projectdir)/src/Plugins/MacOS/mac_utilities.mm",
            "$(projectdir)/src/Plugins/MacOS/mac_app.mm"
        }
        add_includedirs("src/Plugins/MacOS", {public = true})
        add_files(plugin_macos_srcs)
    end

    add_mxflags("-fno-objc-arc")
    before_build(function (target)
        target:add("forceincludes", path.absolute("src/System/config.h"))
        target:add("forceincludes", path.absolute("src/System/tm_configure.hpp"))
    end)
end 

local stem_files = {
    "$(projectdir)/TeXmacs(/doc/**)",
    "$(projectdir)/TeXmacs(/langs/**)",
    "$(projectdir)/TeXmacs(/misc/**)",
    "$(projectdir)/TeXmacs(/packages/**)",
    "$(projectdir)/TeXmacs(/progs/**)",
    "$(projectdir)/TeXmacs(/styles/**)",
    "$(projectdir)/TeXmacs(/texts/**)",
    "$(projectdir)/TeXmacs/COPYING", -- copying files are different
    "$(projectdir)/TeXmacs/INSTALL",
    "$(projectdir)/LICENSE", -- license files are same
    "$(projectdir)/TeXmacs/README",
    "$(projectdir)/TeXmacs/TEX_FONTS",
    "$(projectdir)/TeXmacs(/plugins/**)" -- plugin files
}

if is_plat("windows") then
    target("liii_windows_icon") do
        set_version(XMACS_VERSION)
        set_kind("object")
        add_configfiles("$(projectdir)/packages/windows/resource.rc.in", {
            filename = "resource.rc"
        })
        add_configfiles("$(projectdir)/packages/windows/Xmacs.ico", {
            onlycopy = true
        })
        add_files("$(buildir)/resource.rc")
    end
end

target("liii") do 
    add_deps("goldfish")
    if is_plat("windows") and is_mode("release") then
        add_deps("liii_windows_icon")
    end
    if is_plat("linux") then
        set_filename("liiistem")
    elseif is_plat("macosx") then
        set_filename("LiiiSTEM")
    else
        set_filename("LiiiSTEM.exe")
    end

    local install_dir = "$(buildir)"
    if is_plat("windows") then
        install_dir = path.join("$(buildir)", "packages/liii/data/")
    elseif is_plat("macosx") then
        install_dir = path.join("$(buildir)", "macosx/$(arch)/$(mode)/LiiiSTEM.app/Contents/Resources/")
    else
        if os.getenv("INSTALL_DIR") == nil then
            install_dir = path.join("$(buildir)", "packages/liii/")
        else
            install_dir = os.getenv("INSTALL_DIR")
        end
    end
    set_installdir(install_dir)

    if is_plat("windows") then
        add_installfiles(stem_files)
    else
        add_installfiles(stem_files, {prefixdir="share/liiilabs"})
    end

    if is_plat("windows") then
        add_installfiles("$(projectdir)/TeXmacs(/fonts/**)")
    else
        add_installfiles("$(projectdir)/TeXmacs(/fonts/**)", {prefixdir="share/liiilabs"})
    end

    if is_plat("linux") then
        -- add_installfiles("$(projectdir)/TeXmacs/misc/images/text-x-mogan.svg", {prefixdir="share/icons/hicolor/scalable/mimetypes"})
        add_installfiles("$(projectdir)/TeXmacs/misc/mime/LiiiSTEM.desktop", {prefixdir="share/applications"})
        add_installfiles("$(projectdir)/TeXmacs/misc/images/liiistem.png", {prefixdir="share/icons/hicolor/512x512/apps"})
        -- add_installfiles("$(projectdir)/TeXmacs/misc/mime/mogan.xml", {prefixdir="share/mime/packages"})
    end


    -- package metadata
    if is_plat("macosx") then
        add_installfiles({
            "$(projectdir)/packages/macos/new-mogan.icns",
            "$(projectdir)/packages/macos/TeXmacs-document.icns",
            "$(projectdir)/src/Plugins/Cocoa/(en.lproj/**)",
            "$(projectdir)/src/Plugins/Cocoa/(zh-Hans.lproj/**)"
        })
    end

    if is_plat("windows") then
        set_optimize("smallest")
        set_runtimes("MT")
        add_ldflags("/STACK:16777216")
    end

    if is_mode("debug", "releasedbg") and is_plat("windows") then
        add_rules("qt.console")
    else
        add_rules("qt.widgetapp")
    end

    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    add_packages("s7")
    add_packages("lolly")
    add_deps("libmogan")
    if not is_plat("windows") then
        add_syslinks("pthread", "dl", "m")
    end

    add_includedirs(moe_includedirs)
    add_files("src/Mogan/Research/research.cpp")

    -- install tm files for testing purpose
    if is_mode("releasedbg") then
        if is_plat("mingw", "windows") then
            add_installfiles({
                "$(projectdir)/TeXmacs(/tests/tm/*.tm)",
                "$(projectdir)/TeXmacs(/tests/tex/*.tex)",
                "$(projectdir)/TeXmacs(/tests/bib/*.bib)",
            })
        else
            add_installfiles({
                "$(projectdir)/TeXmacs(/tests/*.tm)",
                "$(projectdir)/TeXmacs(/tests/*.bib)",
            }, {prefixdir="share/liiilabs"})
        end
    end

    -- deploy necessary dll
    if is_plat("windows") then
        set_values("qt.deploy.flags", {"-printsupport", "--no-opengl-sw", "--no-translations", "--release"})
    end

    on_run(function (target)
        name = target:name()
        if is_plat("windows") then
            os.execv(target:installdir().."/bin/LiiiSTEM.exe")
        elseif is_plat("linux", "macosx") then
            print("Launching " .. target:targetfile())
            os.execv(target:targetfile(), {"-d"}, {envs={TEXMACS_PATH= path.join(os.projectdir(), "TeXmacs")}})
        else
            print("Unsupported plat $(plat)")
        end
    end)

    before_build(function (target)
        target:add("forceincludes", path.absolute("src/System/config.h"))
        target:add("forceincludes", path.absolute("src/System/tm_configure.hpp"))
    end)
end 

function add_target_integration_test(filepath, INSTALL_DIR, RUN_ENVS)
    local testname = path.basename(filepath)
    target(testname) do
        set_enabled(not is_plat("wasm"))
        set_kind("phony")
        set_group("integration_tests")
        add_deps("liii")
        on_run(function (target)
            name = target:name()
            test_name = "(test_"..name..")"
            print("------------------------------------------------------")
            print("Executing: " .. test_name)
            params = {
                "-headless",
                "-b", path.join("TeXmacs","tests",name..".scm"),
                "-x", test_name,
                "-q"
            }
            if is_plat("macosx", "linux") then
                binary = target:deps()["liii"]:targetfile()
            elseif is_plat("mingw", "windows") then
                binary = path.join(INSTALL_DIR,"bin","MoganResearch.exe")
            else
                print("Unsupported plat $(plat)")
            end
            cmd = binary
            if is_plat("macosx", "linux") then
                os.execv(cmd, params, {envs=RUN_ENVS})
            else
                os.execv(cmd, params)
            end
        end)
    end
end

-- Integration tests
RUN_ENVS = {TEXMACS_PATH=path.join(os.projectdir(), "TeXmacs")}
for _, filepath in ipairs(os.files("TeXmacs/tests/*.scm")) do
    add_target_integration_test(filepath, INSTALL_DIR, RUN_ENVS)
end

target("liii_packager") do
    set_enabled(is_plat("macosx") and is_mode("release"))
    set_kind("phony")

    add_deps("liii")

    set_configvar("XMACS_VERSION", XMACS_VERSION)
    set_configvar("APPCAST", "")
    set_configvar("OSXVERMIN", "")
    add_configfiles("$(projectdir)/packages/macos/Info.plist.in", {
        filename = "Info.plist",
        pattern = "@(.-)@",
    })

    set_installdir(path.join("$(buildir)", "macosx/$(arch)/$(mode)/LiiiSTEM.app/Contents/Resources/"))

    local dmg_name= "LiiiSTEM-v" .. XMACS_VERSION .. ".dmg"
    if is_arch("arm64") then
        dmg_name= "LiiiSTEM-v" .. XMACS_VERSION .. "-arm.dmg"
    end

    after_install(function (target, opt)
        local app_dir = target:installdir() .. "/../../"
        os.cp("$(buildir)/Info.plist", app_dir .. "/Contents")
        os.execv("codesign", {"--force", "--deep", "--sign", "-", app_dir})

        local hdiutil_command= "/usr/bin/sudo /usr/bin/hdiutil create $(buildir)/" .. dmg_name .. " -fs HFS+ -srcfolder " .. app_dir
        io.write("Execute: ")
        print(hdiutil_command)
        print("Remove /usr/bin/sudo if you want to package it by your own")

        local maxRetries= 5
        local retries = 0
        while retries <= maxRetries do
            try {
                function ()
                    os.execv(hdiutil_command)
                    os.exit(0)
                end,
                catch {
                    function (errors)
                        retries = retries + 1
                        io.write("Retrying, attempt ")
                        print(retries)
                        if retries > maxRetries then
                            os.raise("Command failed after " .. maxRetries .. " retries")
                        end
                    end
                }
            }
        end
    end)
end

if is_mode("release") then
includes("@builtin/xpack")
xpack("liii") do
    set_formats("nsis")
    set_author("Darcy Shen <da@liii.pro>")
    set_license("GPLv3")
    set_licensefile(path.join(os.projectdir(), "LICENSE"))
    set_title("Liii STEM")
    set_description("A one-stop solution that meets all your STEM writing needs")
    set_homepage("https://liiistem.cn")

    _, pos = string.find(XMACS_VERSION, "-")
    local XMACS_VERSION_XYZ= XMACS_VERSION
    if not (pos == nil) then
        XMACS_VERSION_XYZ= string.sub(XMACS_VERSION, 1, pos-1)
    end
    set_version(XMACS_VERSION_XYZ..".0")

    if is_plat ("windows") then
        set_specfile(path.join(os.projectdir(), "packages/windows/research.nsis"))
        set_specvar("PACKAGE_INSTALL_DIR", "LiiiLabs\\LiiiSTEM-"..XMACS_VERSION)
        set_specvar("PACKAGE_NAME", "LiiiSTEM")
        set_specvar("PACKAGE_SHORTCUT_NAME", "Liii STEM")
        set_iconfile(path.join(os.projectdir(), "packages/windows/Xmacs.ico"))
        set_bindir("bin")
        add_installfiles(path.join(os.projectdir(), "build/packages/liii/data/bin/(**)|LiiiSTEM.exe"), {prefixdir = "bin"})
    end

    add_targets("liii")

    if is_plat("windows") then
        on_load(function (package)
            local format = package:format()
            if format == "nsis" then
                package:set("basename", "LiiiSTEM-v" .. package:version() .. "-64bit-installer")
            else
                package:set("basename", "LiiiSTEM-v" .. package:version() .. "-64bit-portable")
            end
        end)
    end
end
end
