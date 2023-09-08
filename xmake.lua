-------------------------------------------------------------------------------
--
-- MODULE      : xmake.lua
-- DESCRIPTION : Xmake config file for Mogan Applications
-- COPYRIGHT   : (C) 2022-2023  jingkaimori
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
--- Project: Mogan Applications
---
set_project("Mogan Applications")

set_allowedplats(
    "wasm", "linux", "macosx", "mingw", "windows"
) 

if is_plat("mingw") and is_host("windows") then
    add_requires("mingw-w64 8.1.0")
    set_toolchains("mingw@mingw-w64")
end

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
-- global variables in configurations domain
--
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

--
-- Add packages from xrepo or platform package manager
--
includes("misc/xmake/packages.lua")
add_requires_of_mogan()

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
                    path.join("$(buildir)", glue_name .. ".cpp"),
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


--
-- Experimental options of Mogan
--
option("experimental")
    set_description("Enable experimental style rewriting code")
    set_default(false)
    set_values(false, true)
option_end()
if has_config("experimental") then
    set_configvar("EXPERIMENTAL", true)
end
option("sanity-checks")
    set_description("Enable sanity checks")
    set_default(false)
    set_values(false, true)
option_end()
if has_config("sanity-checks") then
    set_configvar("SANITY_CHECKS", true)
end
option("use-exceptions")
    set_description("Use C++ exception mechanism")
    set_default(false)
    set_values(false, true)
option_end()
if has_config("use-exceptions") then
    set_configvar("USE_EXCEPTIONS", true)
end

--
-- Library: L3 Kernel
--
set_configvar("QTTEXMACS", 1)
local CONFIG_USER = "XmacsLabs"
local TEXMACS_VERSION = "2.1.2"

local l3_files = {
    "src/Kernel/**.cpp",
    "src/Data/History/**.cpp",
    "src/Data/Observers/**.cpp",
    "src/Data/Scheme/**.cpp",
    "src/Data/String/**.cpp",
    "src/Data/Document/new_document.cpp",
    "src/Data/Drd/**.cpp",
    "src/Data/Tree/tree_helper.cpp",
    "src/Data/Tree/tree_label.cpp",
    "src/Data/Tree/tree_cursor.cpp",
    "src/Data/Tree/tree_observer.cpp",
    "src/Scheme/L1/**.cpp",
    "src/Scheme/L2/**.cpp",
    "src/Scheme/L3/**.cpp",
    "src/Scheme/S7/**.cpp",
    "src/Scheme/Scheme/object.cpp",
    "src/System/Config/**.cpp",
    "src/System/Language/locale.cpp",
    "src/System/Classes/**.cpp",
    "src/System/Files/**files.cpp",
    "src/System/Misc/data_cache.cpp",
    "src/System/Misc/persistent.cpp",
    "src/System/Misc/stack_trace.cpp",
    "src/Texmacs/Server/tm_debug.cpp",
}
local l3_includedirs = {
    "src/Kernel/Types",
    "src/Kernel/Abstractions",
    "src/Data/Drd",
    "src/Data/Document",
    "src/Data/History",
    "src/Data/Observers",
    "src/Data/Scheme",
    "src/Data/String",
    "src/Data/Tree",
    "src/Kernel/Abstractions",
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
    set_version(TEXMACS_VERSION, {build = "%Y-%m-%d"})

    add_packages("s7")
    add_packages("lolly")

    add_configfiles(
        "src/System/config_l3.h.xmake", {
            filename = "L3/config.h",
            variables = {
                OS_MINGW = is_plat("mingw"),
                OS_MACOS = is_plat("macosx"),
                OS_WIN = is_plat("windows"),
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
                CONFIG_OS = CONFIG_OS,
                VERSION = TEXMACS_VERSION,
            }
        }
    )

    add_includedirs("$(buildir)/L3")
    add_includedirs("$(buildir)")
    add_includedirs(l3_includedirs, {public = true})
    add_files(l3_files)

    if is_plat("windows") then
        add_cxxflags("-FI " .. path.absolute("$(buildir)\\L3\\config.h"))
        add_cxxflags("-FI " .. path.absolute("$(buildir)\\L3\\tm_configure.hpp"))
    else
        add_cxxflags("-include $(buildir)/L3/config.h")
        add_cxxflags("-include $(buildir)/L3/tm_configure.hpp")
    end
end


local XMACS_VERSION="1.2.0"

local INSTALL_DIR = "$(buildir)"
if is_plat("mingw") then 
    INSTALL_DIR = path.join("$(buildir)", "packages/app.mogan/data/")
elseif is_plat("macosx") then 
    INSTALL_DIR = path.join("$(buildir)", "macosx/$(arch)/$(mode)/Mogan.app/Contents/Resources/")
else 
    if os.getenv("INSTALL_DIR") == nil then 
      INSTALL_DIR = path.join("$(buildir)", "packages/app.mogan/")
    else 
      INSTALL_DIR = os.getenv("INSTALL_DIR")
    end
end


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


set_version(TEXMACS_VERSION, {build = "%Y-%m-%d"})

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
            CONFIG_USER = CONFIG_USER,
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
        set_installdir(INSTALL_DIR)
    elseif is_plat("macosx") then
        on_install(function (target)
            print("No need to install libmogan on macOS")
        end)
        add_rules("qt.static")
    else
        add_rules("qt.static")
    end
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    if is_plat("macosx") then
        add_frameworks("QtMacExtras")
    end

    build_glue_on_config()

    add_packages("lolly")
    add_packages("libiconv")
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
            "TeXmacs/include",
            "src/Mogan"
        }, {public = true})

    if is_plat("macosx") then
        add_includedirs("src/Plugins/MacOS", {public = true})
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
    add_cxxflags("-include $(buildir)/tm_configure.hpp")
end 

option("libdl") do
    add_links("dl")
end

if is_plat("mingw") then
    target("windows_icon") do
        set_version(XMACS_VERSION)
        set_kind("object")
        add_configfiles("packages/windows/resource.rc.in", {
            filename = "resource.rc"
        })
        add_configfiles("packages/windows/TeXmacs.ico", {
            onlycopy = true
        })
        add_files("$(buildir)/resource.rc")
    end
end

target("draw") do
    set_enabled(is_plat("linux") or is_plat("wasm"))
    set_version(XMACS_VERSION)
    set_installdir(INSTALL_DIR)

    set_filename("mogan_draw")
    add_rules("qt.widgetapp")
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    add_packages("lolly")
    add_rpathdirs("@executable_path/../lib")
    add_deps("libmogan")
    add_syslinks("pthread")

    add_includedirs({
        "$(buildir)",
    })
    add_files("src/Mogan/Draw/draw.cpp")

    

    set_configdir(INSTALL_DIR)
    set_configvar("DEVEL_VERSION", DEVEL_VERSION)
    set_configvar("PACKAGE", "Mogan Draw")
    set_configvar("XMACS_VERSION", XMACS_VERSION)

    -- install man.1 manual file
    add_configfiles(
        "(misc/man/texmacs.1.in)",{
            filename = "texmacs.1",
            pattern = "@([^\n]-)@",
        }
    )

    -- install texmacs runtime files
      add_installfiles("misc/scripts/tm_gs", {prefixdir="share/Xmacs/bin"})
      add_installfiles(TeXmacs_files, {prefixdir="share/Xmacs"})

    -- install tm files for testing purpose
    if is_mode("releasedbg") then
            add_installfiles({
                "TeXmacs(/tests/*.tm)",
                "TeXmacs(/tests/*.bib)",
            }, {prefixdir="share/Xmacs"})
    end

    after_install(function (target)
        print("after_install of target draw")
        import("misc.xmake.global")
        global.copy_icons(target)
    end)

    on_run(function (target)
        name = target:name()
        os.execv(target:installdir().."/bin/mogan_draw")
    end)
end


target("research") do 
    set_version(XMACS_VERSION)
    set_installdir(INSTALL_DIR)

    if is_plat("macosx") then
        set_filename("Mogan")
    elseif is_plat("mingw") then
        set_filename("mogan.exe")
    else
        set_filename("mogan")
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

    add_packages("lolly")
    if is_plat("mingw") then
        add_packages("nowide_standalone")
        add_packages("qt5widgets")
    end

    if is_plat("mingw") and is_mode("releasedbg") then
        set_policy("check.auto_ignore_flags", false)
        add_ldflags("-mconsole")
    end

    if is_plat("linux") then
        add_rpathdirs("@executable_path/../lib")
    end
    add_deps("libmogan")
    add_syslinks("pthread")

    add_includedirs({
        "$(buildir)",
    })
    add_files("src/Mogan/Research/research.cpp")
    if is_plat("mingw") and is_mode("release") then
        add_deps("windows_icon")
    end

    set_configdir(INSTALL_DIR)
    set_configvar("DEVEL_VERSION", DEVEL_VERSION)
    set_configvar("PACKAGE", "Mogan Research")
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
  
    if is_plat("mingw") then
        add_installfiles(TeXmacs_files)
    else
        add_installfiles("misc/scripts/tm_gs", {prefixdir="share/Xmacs/bin"})
        add_installfiles(TeXmacs_files, {prefixdir="share/Xmacs"})
    end

    -- install tm files for testing purpose
    if is_mode("releasedbg") then
        if is_plat("mingw") then
            add_installfiles({
                "TeXmacs(/tests/*.tm)",
                "TeXmacs(/tests/*.bib)",
            })
        else
            add_installfiles({
                "TeXmacs(/tests/*.tm)",
                "TeXmacs(/tests/*.bib)",
            }, {prefixdir="share/Xmacs"})
        end
    end

    after_install(function (target)
        print("after_install of target research")
        import("misc.xmake.global")
        global.copy_icons(target)

        if is_plat("macosx") and is_arch("arm64") then
            local app_dir = target:installdir() .. "/../../"
            os.rm(app_dir .. "Contents/Resources/include")
            os.rm(app_dir .. "Contents/Frameworks/QtQmlModels.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQml.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQuick.framework")
            os.execv("codesign", {"--force", "--deep", "--sign", "-", app_dir})
        end

        if is_plat("mingw") then
            import("detect.sdks.find_qt")
            import("core.base.option")
            import("core.project.config")
            import("lib.detect.find_tool")

            -- get qt sdk
            local qt = assert(find_qt(), "Qt SDK not found!")

            -- get windeployqt
            local windeployqt_tool = assert(
                find_tool("windeployqt", {check = "--help"}),
                "windeployqt.exe not found!")
            local windeployqt = windeployqt_tool.program
            
            -- deploy necessary dll
            -- since version of mingw used to build mogan may differs from
            --   version of Qt, tell windeployqt to use lib from mingw may
            --   break ABI compability, but major version of mingw must be
            --   same.
            local deploy_argv = {"--no-compiler-runtime", "-printsupport"}
            if option.get("diagnosis") then
                table.insert(deploy_argv, "--verbose=2")
            elseif option.get("verbose") then
                table.insert(deploy_argv, "--verbose=1")
            else
                table.insert(deploy_argv, "--verbose=0")
            end
            local install_bindir = path.join(target:installdir(), "bin")
            table.insert(deploy_argv, install_bindir)
            os.iorunv(windeployqt, deploy_argv, {envs = {PATH = qt.bindir}})
            os.cp(path.join(qt.bindir, "libstdc++*.dll"), install_bindir)
            os.cp(path.join(qt.bindir, "libgcc*.dll"), install_bindir)
            os.cp(path.join(qt.bindir, "libwinpthread*.dll"), install_bindir)
        end
    end)

    on_run(function (target)
        name = target:name()
        if is_plat("mingw") then
            os.execv(target:installdir().."/bin/mogan.exe")
        elseif is_plat("linux") then
            os.execv(target:installdir().."/bin/mogan")
        else
            os.execv(target:installdir().."/../MacOS/Mogan")
        end
    end)
end

target("macos_installer") do
    set_enabled(is_plat("macosx") and is_mode("release"))
    set_kind("phony")
    set_installdir(INSTALL_DIR)
    add_deps("research")

    after_install(function (target, opt)
        local app_dir = target:installdir() .. "/../../"
        os.execv("hdiutil create Mogan-v" .. XMACS_VERSION .. ".dmg -fs HFS+ -srcfolder " .. app_dir)
    end)
end

target("windows_installer") do
    set_kind("phony")
    set_enabled(is_plat("mingw") and is_mode("release"))
    add_packages("qtifw")
    add_deps("research")
    set_configvar("PACKAGE_DATE", os.date("%Y-%m-%d"))
    set_configvar("XMACS_VERSION", XMACS_VERSION)
    set_installdir("$(buildir)")

    if is_plat("mingw") then
        add_configfiles(
            "packages/windows/(config/config.in.xml)",{
                filename = "config.xml",
            }
        )
        add_configfiles(
            "packages/windows/(packages/app.mogan/meta/package.in.xml)",{
                filename = "package.xml",
            }
        )
        add_installfiles({
            "packages/windows/packages/app.mogan/meta/installscript.qs",
            "LICENSE"}, {prefixdir = "packages/app.mogan/meta/"})
        add_installfiles({
            "packages/windows/config/TeXmacs-small.png",
            "packages/windows/config/TeXmacs-large.png",
            "packages/windows/TeXmacs.ico",
        }, {prefixdir = "config/"})
    end

    after_install(function (target, opt)
        print("after_install of target windows_installer")
        import("core.project.config")
        import("lib.detect.find_tool")
        local qtifw_dir = target:pkg("qtifw"):installdir()
        local binarycreator_path = path.join(qtifw_dir, "/bin/binarycreator.exe")
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

function add_test_target (filepath, dep)
    local testname = path.basename(filepath)
    target(testname) do
        add_runenvs("TEXMACS_PATH", path.join(os.projectdir(), "TeXmacs"))
        set_group("tests")
        add_deps(dep)
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)
        add_rules("qt.console")
        add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg", "QtTest")
        add_syslinks("pthread")
        if is_plat ("mingw") then
            add_packages("mingw-w64")
        end
        add_packages("s7")
        add_packages("lolly")

        add_includedirs({"$(buildir)", "tests/Base"})
        build_glue_on_config()
        add_files("tests/Base/base.cpp")
        add_files(filepath)
        add_files(filepath, {rules = "qt.moc"})
        add_cxxflags("-include $(buildir)/config.h")
    end
end

for _, filepath in ipairs(os.files("tests/Data/History/**_test.cpp")) do
    add_test_target (filepath, "libkernel_l3")
end

for _, filepath in ipairs(os.files("tests/Data/Parser/**_test.cpp")) do
    add_test_target (filepath, "libmogan")
end

for _, filepath in ipairs(os.files("tests/Data/String/**_test.cpp")) do
    add_test_target (filepath, "libmogan")
end

for _, filepath in ipairs(os.files("tests/Graphics/**_test.cpp")) do
    add_test_target (filepath, "libmogan")
end

for _, filepath in ipairs(os.files("tests/System/**_test.cpp")) do
    add_test_target (filepath, "libmogan")
end

for _, filepath in ipairs(os.files("tests/Typeset/**_test.cpp")) do
    add_test_target (filepath, "libmogan")
end

for _, filepath in ipairs(os.files("tests/Plugins/**_test.cpp")) do
    add_test_target (filepath, "libmogan")
end

for _, filepath in ipairs(os.files("TeXmacs/tests/*.scm")) do
    local testname = path.basename(filepath)
    target(testname) do
        set_kind("phony")
        set_group("integration_tests")
        on_run(function (target)
            name = target:name()
            params = {
                "-headless",
                "-b", path.join("TeXmacs","tests",name..".scm"),
                "-x", "\"(test_"..name..")\"",
                "-q"}
            if is_plat("macosx") then
                binary = INSTALL_DIR.."/../MacOS/Mogan"
            elseif is_plat("mingw") then
                binary = path.join(INSTALL_DIR,"bin","mogan.exe")
            else
                binary = INSTALL_DIR.."/bin/mogan"
            end
            cmd = binary
            for _, param in ipairs(params) do
                cmd = cmd .. " " .. param
            end
            print ("> " .. cmd)
            os.execv(cmd)
        end)
    end
end

-- xmake plugins
add_configfiles(
    "misc/doxygen/Doxyfile.in", {
        filename = "doxyfile",
        pattern = "@(.-)@",
        variables = {
            PACKAGE = "Mogan Applications",
            DOXYGEN_DIR = get_config("buildir"),
            DEVEL_VERSION = DEVEL_VERSION,
        }
    }
)
