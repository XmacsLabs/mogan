-------------------------------------------------------------------------------
--
-- MODULE      : xmake.lua
-- DESCRIPTION : Xmake config file for Mogan STEM Suite
-- COPYRIGHT   : (C) 2022-2023  jingkaimori
--                   2022-2024  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

set_xmakever("2.8.7")

-- mode
set_allowedmodes("releasedbg", "release", "debug", "profile")
add_rules("mode.releasedbg", "mode.release", "mode.debug", "mode.profile")

-- plat
set_allowedplats("wasm", "linux", "macosx", "windows") 
if is_plat("wasm") then
    set_configvar("OS_WASM", true)
    add_requires("emscripten 3.1.25")
    set_toolchains("emcc@emscripten")
else
    set_configvar("OS_WASM", false)
end
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

-- check compilers and standard libraries
includes("@builtin/check")
configvar_check_cxxtypes("HAVE_INTPTR_T", "intptr_t", {includes = {"memory"}})
configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")
configvar_check_cxxincludes("HAVE_STDINT_H", "stdint.h")

if is_mode("release") then
    includes("@builtin/xpack")
end

includes("xmake/vars.lua")
includes("xmake/packages.lua")

---
--- Project: Mogan STEM Suite
---
set_project("Mogan STEM Suite")

--
-- Experimental options of Mogan
--
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

-- only lock it in rc releases
-- if is_plat ("macosx", "windows") then
--     set_policy("package.requires_lock", true)
-- end
add_repositories("mogan-repo xmake")
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

function add_tm_configure(target_name, variables)
    if target_name == "libmogan" then
        add_configfiles("src/System/tm_configure.hpp.xmake", {
            filename = "tm_configure.hpp",
            variables = variables
        })
    else
        add_configfiles("src/System/tm_configure.hpp.xmake", {
            filename = target_name .. "/tm_configure.hpp",
            variables = variables
        })
    end
end

--
-- Library: L3 Kernel
--
includes ("xmake/L3.lua")
target("libkernel_l3") do
    add_target_L3()
end

set_configvar("QTTEXMACS", 1)

local INSTALL_DIR = "$(buildir)"
if is_plat("windows") then 
    INSTALL_DIR = path.join("$(buildir)", "packages/app.mogan/data/")
elseif is_plat("macosx") then 
    INSTALL_DIR = path.join("$(buildir)", "macosx/$(arch)/$(mode)/MoganResearch.app/Contents/Resources/")
else 
    if os.getenv("INSTALL_DIR") == nil then 
      INSTALL_DIR = path.join("$(buildir)", "packages/app.mogan/")
    else 
      INSTALL_DIR = os.getenv("INSTALL_DIR")
    end
end

if not is_plat("wasm") then
    set_configvar("QTPIPES", 1)
    set_configvar("USE_QT_PRINTER", 1)
end

set_configvar("USE_ICONV", 1)
set_configvar("USE_FREETYPE", 1)
set_configvar("USE_PLUGIN_PDF", true)
set_configvar("PDFHUMMUS_NO_TIFF", true)
set_configvar("USE_PLUGIN_BIBTEX", true)
set_configvar("USE_PLUGIN_LATEX_PREVIEW", true)
set_configvar("USE_PLUGIN_TEX", true)
set_configvar("USE_PLUGIN_ISPELL", true)
set_configvar("USE_PLUGIN_SPARKLE", false)
set_configvar("USE_PLUGIN_HTML", true)
set_configvar("TM_DYNAMIC_LINKING", false)

if is_plat("macosx") then
    set_configvar("AQUATEXMACS", true)
end

set_version(XMACS_VERSION, {build = "%Y-%m-%d"})

add_configfiles("src/System/config.h.xmake", {
    filename = "config.h",
    variables = {
        NOMINMAX = is_plat("windows"),
        MACOSX_EXTENSIONS = is_plat("macosx"),
        SIZEOF_VOID_P = 8,
        USE_FONTCONFIG = is_plat("linux"),
        USE_STACK_TRACE = (not is_plat("wasm")) and (not is_plat("windows")),
        USE_PLUGIN_GS = not is_plat("wasm"),
        USE_PLUGIN_GIT = not is_plat("wasm"),
        APP_MOGAN_RESEARCH = true,
    }
})

target("libmogan") do
    set_enabled(not is_plat ("wasm"))
    set_basename("mogan")
    set_version(TEXMACS_VERSION, {build = "%Y-%m-%d"})
    
    if is_plat("windows") then
        set_runtimes("MT")
    end
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)
    set_encodings("utf-8")

    add_rules("qt.static")
    on_install(function (target)
        print("No need to install libmogan")
    end)
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")

    build_glue_on_config()
    add_tm_configure("libmogan", TM_CONFIGURE_VARS)

    if not is_plat("wasm") then
        add_packages("cpptrace")
    end
    add_packages("moebius")
    if not is_plat("macosx") then
        add_packages("libiconv")
    end
    add_packages("freetype")
    add_packages("pdfhummus")
    add_packages("s7")
    add_packages("tree-sitter")
    add_packages("tree-sitter-cpp")
    add_packages("tree-sitter-scheme")
    
    add_packages("libgit2")
    if is_plat("linux") and not using_legacy_apt() then
        add_packages("fontconfig")
    end

    if is_plat("windows") then
        add_syslinks("secur32", "shell32", "winhttp", "rpcrt4", {public = true})
    elseif is_plat("macosx") then
        add_syslinks("iconv")
    end
    
    ---------------------------------------------------------------------------
    -- add source and header files
    ---------------------------------------------------------------------------
    add_includedirs(libmogan_headers, {public = true})
    add_includedirs("$(buildir)")
    add_files(libmogan_srcs)

    if is_plat("macosx") then
        add_includedirs("src/Plugins/MacOS", {public = true})
        add_files(plugin_macos_srcs)
    end
    add_files(plugin_qt_srcs)
    add_files(plugin_bibtex_srcs)
    add_files(plugin_freetype_srcs)
    add_files(plugin_database_srcs)
    add_files(plugin_ghostscript_srcs)
    add_files(plugin_ispell_srcs)
    add_files(plugin_metafont_srcs)
    add_files(plugin_tex_srcs)
    add_files(plugin_latex_preview_srcs)
    add_files(plugin_openssl_srcs)
    add_files(plugin_updater_srcs)
    add_files(plugin_xml_srcs)
    add_files(plugin_html_srcs)
    add_files(plugin_treesitter_srcs)
    add_files(plugin_pdf_srcs)
    add_files(plugin_git_srcs)

    add_mxflags("-fno-objc-arc")
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/tm_configure.hpp"))
    end)
end 


if is_plat("wasm", "linux") then
    includes("xmake/draw.lua")
    target("draw") do
        set_version(XMACS_VERSION, {build = "%Y-%m-%d"})
        add_tm_configure("draw", TM_CONFIGURE_VARS)
        add_target_draw()
    end
end


-- Mogan Research
includes("xmake/research.lua")


-- Mogan Beamer
if is_plat("macosx", "windows") then
    includes("xmake/beamer.lua")
end


-- Mogan Code
if is_plat("macosx", "windows") then
    includes("xmake/code.lua")
end


includes("xmake/tests.lua")
-- Tests in C++
l3_cpp_tests = os.files("tests/L3/**_test.cpp")
all_cpp_tests = os.files("tests/**_test.cpp")
cpp_benches = os.files("bench/**_bench.cpp")

for _, filepath in ipairs(l3_cpp_tests) do
    add_target_cpp_test(filepath, "libkernel_l3")
end

for _, filepath in ipairs(cpp_benches) do
    add_target_cpp_bench(filepath, "libmogan")
end

if not (is_plat("linux") and (linuxos.name () == "ubuntu" and linuxos.version():major() == 20)) then
    for _, filepath in ipairs(all_cpp_tests) do
        if not table.contains(l3_cpp_tests, filepath) then
            add_target_cpp_test(filepath, "libmogan")
        end
    end
end

-- Tests in Scheme
for _, filepath in ipairs(os.files("TeXmacs/progs/**/*-test.scm")) do
    add_target_scheme_test(filepath, INSTALL_DIR, RUN_ENVS)
end

for _, filepath in ipairs(os.files("TeXmacs/progs/kernel/**/*-test.scm")) do
    add_target_scheme_test(filepath, INSTALL_DIR, RUN_ENVS)
end

-- Integration tests
for _, filepath in ipairs(os.files("TeXmacs/tests/*.scm")) do
    add_target_integration_test(filepath, INSTALL_DIR, RUN_ENVS)
end


-- xmake plugins
add_configfiles(
    "misc/doxygen/Doxyfile.in", {
        filename = "doxyfile",
        pattern = "@(.-)@",
        variables = {
            PACKAGE = "Mogan STEM Suite",
            DOXYGEN_DIR = get_config("buildir"),
            DEVEL_VERSION = DEVEL_VERSION,
        }
    }
)
