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

includes("@builtin/check")
configvar_check_cxxtypes("HAVE_INTPTR_T", "intptr_t", {includes = {"memory"}})
configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")
configvar_check_cxxincludes("HAVE_STDINT_H", "stdint.h")


set_project("Liii STEM Suite")

-- because this cpp project use variant length arrays which is not supported by
-- msvc, this project will not support windows env.
-- because some package is not ported to cygwin env, this project will not
-- support cygwin env.
set_allowedplats("linux") 

-- add releasedbg, debug and release modes for different platforms.
-- debug mode cannot run on mingw with qt precompiled binary
set_allowedmodes("releasedbg", "release", "debug")
add_rules("mode.releasedbg", "mode.release", "mode.debug")

add_repositories("liii-repo xmake")

function using_legacy_apt ()
    return (linuxos.name() == "uos") or (linuxos.name () == "ubuntu" and linuxos.version():major() == 20)
end

PDFHUMMUS_VERSION = "4.6.2"
S7_VERSION = "20240516"
local FREETYPE_VERSION = "2.12.1"

-- package: s7
add_requires("s7 "..S7_VERSION, {system=false})
add_requires("lolly 1.1.0", {system=false})

add_requires("libjpeg")
add_requires("apt::libpng-dev", {alias="libpng"})
add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
add_requires("pdfhummus "..PDFHUMMUS_VERSION, {system=false,configs={libpng=true,libjpeg=true}})
if using_legacy_apt() then
    add_requires("freetype "..FREETYPE_VERSION, {system=false})
    add_requireconfs("pdfhummus.freetype", {version = FREETYPE_VERSION, system = false, override=true})
else
    add_requires("apt::libfreetype-dev", {alias="freetype"})
end

local XMACS_VERSION="2025.1.0"

set_configvar("USE_FREETYPE", 1)

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

target("libmogan") do
    set_basename("mogan")
    local TEXMACS_VERSION = "2.1.4"
    local DEVEL_VERSION = TEXMACS_VERSION
    local DEVEL_RELEASE = 1
    local STABLE_VERSION = TEXMACS_VERSION
    local STABLE_RELEASE = 1
    set_version(TEXMACS_VERSION)
    
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)
    add_rules("qt.static")
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")

    build_glue_on_config()
    set_configvar("QTTEXMACS", 1)
    add_defines("QTTEXMACS")
    set_configvar("QTPIPES", 1)
    add_defines("QTPIPES")
    set_configvar("USE_QT_PRINTER", 1)
    add_defines("USE_QT_PRINTER")
    set_configvar("USE_CURL", 1)
    set_configvar("USE_SQLITE3", 1)

    set_configvar("LINKED_AXEL", false)
    set_configvar("LINKED_CAIRO", false)
    set_configvar("LINKED_IMLIB2", false)

    add_packages("lolly")
    add_packages("pdfhummus")
    add_packages("freetype")
    add_packages("s7")

    ---------------------------------------------------------------------------
    -- generate config files. see also:
    --    * https://github.com/xmake-io/xmake/issues/320
    --    * https://github.com/xmake-io/xmake/issues/342
    ---------------------------------------------------------------------------
    set_configdir("src/System")
    -- check for dl library
    -- configvar_check_cxxfuncs("TM_DYNAMIC_LINKING","dlopen")
    configvar_check_cxxincludes("HAVE_STDLIB_H", "stdlib.h")
    configvar_check_cxxincludes("HAVE_STRINGS_H", "strings.h")
    configvar_check_cxxincludes("HAVE_STRING_H", "string.h")
    configvar_check_cxxincludes("HAVE_UNISTD_H", "unistd.h")
    configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")
    configvar_check_cxxincludes("HAVE_MEMORY_H", "memory.h")
    configvar_check_cxxincludes("HAVE_PTY_H", "pty.h")
    configvar_check_cxxincludes("HAVE_SYS_STAT_H", "sys/stat.h")
    configvar_check_cxxincludes("HAVE_SYS_TYPES_H", "sys/types.h")
    configvar_check_cxxtypes("HAVE_TIME_T", "time_t", {includes = {"memory"}})
    configvar_check_cxxincludes("HAVE_UTIL_H", "util.h")
    configvar_check_cxxfuncs("HAVE_GETTIMEOFDAY", "gettimeofday", {includes={"sys/time.h"}})

    set_configvar("STDC_HEADERS", true)

    set_configvar("GS_EXE", "/usr/bin/gs")

    set_configvar("USE_STACK_TRACE", true)

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
                USE_GS = true,
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
            "$(buildir)/glue"
        }, {public = true})

    add_includedirs("src/Plugins/Unix", {public = true})

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
            "src/Plugins/Xml/**.cpp",
            "src/Plugins/Updater/**.cpp"})

    add_files("src/Plugins/Unix/**.cpp")

    add_files({
        "src/Plugins/Qt/**.cpp",
        "src/Plugins/Qt/**.hpp"})

    add_mxflags("-fno-objc-arc")
    add_cxxflags("-include src/System/tm_configure.hpp")
    add_cxxflags("-include src/System/config.h")
end 

target("liii") do 
    set_filename("LiiiSTEM")

    add_rules("qt.widgetapp")
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    add_packages("lolly")
    if is_plat("linux") then
        add_rpathdirs("@executable_path/../lib")
    end
    add_deps("libmogan")
    add_syslinks("pthread")
    add_files("src/Mogan/Research/research.cpp")

    on_run(function (target)
        name = target:name()
        if is_plat("linux", "macosx") then
            print("Launching " .. target:targetfile())
            os.execv(target:targetfile(), {}, {envs={TEXMACS_PATH= path.join(os.projectdir(), "TeXmacs")}})
        else
            print("Unsupported plat $(plat)")
        end
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
