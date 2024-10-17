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

-- because this cpp project use variant length arrays which is not supported by
-- msvc, this project will not support windows env.
-- because some package is not ported to cygwin env, this project will not
-- support cygwin env.
set_allowedplats("linux")

if is_plat("linux") then
    set_configvar("OS_GNU_LINUX", true)
else
    set_configvar("OS_GNU_LINUX", false)
end

-- add releasedbg, debug and release modes for different platforms.
-- debug mode cannot run on mingw with qt precompiled binary
set_allowedmodes("releasedbg", "release", "debug")
add_rules("mode.releasedbg", "mode.release", "mode.debug")

add_repositories("liii-repo xmake")

TBOX_VERSION= "1.7.5"
LOLLY_VERSION= "1.3.25"
package("liii-lolly")
    set_homepage("https://github.com/XmacsLabs/lolly")
    set_description("Lolly is a C++ library")

    set_sourcedir(path.join(os.scriptdir(), "3rdparty/lolly"))
    if not is_plat("wasm") then
        add_deps("libcurl", "tbox", "cpr")
    end

    on_install("linux", "macosx", "mingw", "wasm", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()

S7_VERSION = "20240816"
package("liii-s7")
    set_homepage("https://ccrma.stanford.edu/software/snd/snd/s7.html")
    set_description("s7 is a Scheme interpreter intended as an extension language for other applications.")

    set_sourcedir(path.join(os.scriptdir(), "3rdparty/s7"))

    add_configs("gmp", {description = "enable gmp support", default = false, type = "boolean"})

    on_load(function (package)
        package:addenv("PATH", "bin")
        if package:config("gmp") then
            package:add("deps", "gmp")
        end
    end)

    if is_plat("linux") then
        add_syslinks("pthread", "dl", "m")
    end

    on_install("bsd", "cross", "cygwin", "linux", "macosx", "mingw", "msys", "wasm", "windows", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)

    on_test(function(package)
        assert(package:check_csnippets([[
            static s7_pointer old_add;           /* the original "+" function for non-string cases */
            static s7_pointer old_string_append; /* same, for "string-append" */

            static s7_pointer our_add(s7_scheme *sc, s7_pointer args)
            {
                /* this will replace the built-in "+" operator, extending it to include strings:
                *   (+ "hi" "ho") -> "hiho" and  (+ 3 4) -> 7
                */
                if ((s7_is_pair(args)) &&
                    (s7_is_string(s7_car(args))))
                    return(s7_apply_function(sc, old_string_append, args));
                return(s7_apply_function(sc, old_add, args));
            }
        ]], {includes = "s7.h"}))
    end)
package_end()


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



function using_legacy_apt ()
    return (linuxos.name() == "uos") or (linuxos.name () == "ubuntu" and linuxos.version():major() == 20)
end

local FREETYPE_VERSION = "2.12.1"
local LIBGIT2_VERSION = "1.7.1"

-- package: s7
add_requires("liii-s7", {system=false})
add_requires("liii-lolly", {system=false})

add_requires("libjpeg")
add_requires("apt::libpng-dev", {alias="libpng"})
add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
add_requires("liii-pdfhummus", {system=false,configs={libpng=true,libjpeg=true}})
if using_legacy_apt() then
    add_requires("freetype "..FREETYPE_VERSION, {system=false})
    add_requireconfs("liii-pdfhummus.freetype", {version = FREETYPE_VERSION, system = false, override=true})
else
    add_requires("apt::libfreetype-dev", {alias="freetype"})
end

-- package: libgit2
if not is_plat("wasm") then
    add_requires("libgit2 "..LIBGIT2_VERSION, {system=false})
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

    add_packages("liii-lolly")
    add_packages("liii-pdfhummus")
    add_packages("freetype")
    add_packages("liii-s7")
    add_packages("libgit2")

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
                USE_PLUGIN_GS = true,
                USE_PLUGIN_BIBTEX = true,
                USE_PLUGIN_LATEX_PREVIEW = true,
                USE_PLUGIN_TEX = true,
                USE_PLUGIN_ISPELL = true,
                USE_PLUGIN_PDF = true,
                USE_PLUGIN_SPARKLE = false,
                USE_PLUGIN_HTML = true,
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
            "$(buildir)/glue"
        }, {public = true})

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
            "src/Plugins/Html/**.cpp",
            "src/Plugins/Git/**.cpp",
            "src/Plugins/Updater/**.cpp"})

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
    add_packages("liii-lolly")
    add_deps("libmogan")
    add_syslinks("pthread", "dl", "m")
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
