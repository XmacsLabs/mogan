-------------------------------------------------------------------------------
--
-- MODULE      : xmake.lua
-- DESCRIPTION : Xmake config file for Mogan STEM Suite
-- COPYRIGHT   : (C) 2022-2023  jingkaimori
--                   2022-2023  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

set_xmakever("2.8.5")

-- Check CXX Types/Includes/Funcs
includes("@builtin/check")
configvar_check_cxxincludes("HAVE_UNISTD_H", "unistd.h")
configvar_check_cxxtypes("HAVE_INTPTR_T", "intptr_t", {includes = {"memory"}})
configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")
configvar_check_cxxincludes("HAVE_STDINT_H", "stdint.h")
configvar_check_cxxincludes("HAVE_SYS_STAT_H", "sys/stat.h")
configvar_check_cxxincludes("HAVE_SYS_TYPES_H", "sys/types.h")
configvar_check_cxxincludes("HAVE_UTIL_H", "util.h")

---
--- Project: Mogan STEM Suite
---
set_project("Mogan STEM Suite")

-- only lock it in rc releases
if is_plat ("macosx", "windows") then
    set_policy("package.requires_lock", true)
end

local TEXMACS_VERSION = "2.1.2"
local XMACS_VERSION="1.2.5.5"
local CONFIG_USER = "XmacsLabs"
local DEVEL_VERSION = TEXMACS_VERSION
local DEVEL_RELEASE = 1
local STABLE_VERSION = TEXMACS_VERSION
local STABLE_RELEASE = 1

local TM_CONFIGURE_VARS = {
    CONFIG_USER = CONFIG_USER,
    TEXMACS_VERSION = TEXMACS_VERSION,
    XMACS_VERSION = XMACS_VERSION,
    tm_devel = "Texmacs-" .. DEVEL_VERSION,
    tm_devel_release = "Texmacs-" .. DEVEL_VERSION .. "-" .. DEVEL_RELEASE,
    tm_stable = "Texmacs-" .. STABLE_VERSION,
    tm_stable_release = "Texmacs-" .. STABLE_VERSION .. "-" .. STABLE_RELEASE,
    LOLLY_VERSION = LOLLY_VERSION,
}


set_allowedplats("wasm", "linux", "macosx", "mingw", "windows") 

if is_plat("wasm") then
    set_configvar("OS_WASM", true)
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
if is_plat("mingw") then
    set_configvar("OS_MINGW", true)
else
    set_configvar("OS_MINGW", false)
end
if is_plat("windows") then
    set_configvar("OS_WIN", true)
else
    set_configvar("OS_WIN", false)
end


if is_plat("mingw") and is_host("windows") then
    add_requires("mingw-w64 11.2.0")
    set_toolchains("mingw@mingw-w64")
end

if is_plat("wasm") then
    add_requires("emscripten 3.1.25")
    set_toolchains("emcc@emscripten")
end

-- add releasedbg, debug and release modes.
set_allowedmodes("releasedbg", "release", "debug")
add_rules("mode.releasedbg", "mode.release", "mode.debug")

if is_mode("release") then
  includes("@builtin/xpack")
end

add_repositories("mogan-repo xmake")
includes("xmake/packages.lua")
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

--
-- Library: L3 Kernel
--
includes ("xmake/L3.lua")
target("libkernel_l3") do
    ---------------------------------------------------------------------------
    -- generate config files. see also:
    --    * https://github.com/xmake-io/xmake/issues/320
    --    * https://github.com/xmake-io/xmake/issues/342
    ---------------------------------------------------------------------------
    add_configfiles("src/System/config_l3.h.xmake", {
        filename = "L3/config.h",
        variables = {
            QTTEXMACS = false,
            USE_FREETYPE = true,
            USE_FONTCONFIG = is_plat("linux") and (not using_legacy_apt()),
        }
    })
    add_configfiles("src/System/tm_configure_l3.hpp.xmake", {
        filename = "L3/tm_configure.hpp",
        variables = {
            CONFIG_USER = CONFIG_USER,
            CONFIG_OS = CONFIG_OS,
            VERSION = TEXMACS_VERSION,
            LOLLY_VERSION = LOLLY_VERSION,
            XMACS_VERSION = XMACS_VERSION,
            TEXMACS_VERSION = TEXMACS_VERSION,
        }
    })

    add_target_L3()
end



set_configvar("QTTEXMACS", 1)

local INSTALL_DIR = "$(buildir)"
if is_plat("mingw", "windows") then 
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
local RUN_ENVS = {TEXMACS_PATH=path.join(os.projectdir(), "TeXmacs")}

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
        USE_FONTCONFIG = is_plat("linux") and (not linuxos.name() == "uos"),
        USE_STACK_TRACE = (not is_plat("mingw")) and (not is_plat("wasm")) and (not is_plat("windows")),
        USE_PLUGIN_GS = not is_plat("wasm"),
        USE_PLUGIN_GIT = not is_plat("wasm"),
    }
})

libmogan_srcs = {
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
}
libmogan_headers = {
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
    "src/Scheme",
    "src/Scheme/S7",
    "src/Scheme/L1",
    "src/Scheme/L2",
    "src/Scheme/L3",
    "src/Scheme/L4",
    "src/Scheme/L5",
    "src/Scheme/Plugins",
    "src/Scheme/Scheme",
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
    "$(buildir)/glue",
    "TeXmacs/include",
    "src/Mogan"
}

plugin_qt_srcs_on_wasm = {
    "src/Plugins/Qt/*.cpp|QTMPipeLink.cpp|QTMPrintDialog.cpp|QTMPrinterSettings.cpp|qt_printer_widget.cpp",
    "src/Plugins/Qt/*.hpp|QTMPipeLink.hpp|QTMPrintDialog.hpp|QTMPrinterSettings.hpp",
}
plugin_qt_srcs = {
    "src/Plugins/Qt/**.cpp",
    "src/Plugins/Qt/**.hpp"
}
plugin_macos_srcs = {
    "src/Plugins/MacOS/HIDRemote.m",
    "src/Plugins/MacOS/mac_spellservice.mm",
    "src/Plugins/MacOS/mac_utilities.mm",
    "src/Plugins/MacOS/mac_app.mm"
}
plugin_pdf_srcs = { "src/Plugins/Pdf/**.cpp" }
plugin_xml_srcs = { "src/Plugins/Xml/**.cpp" }
plugin_html_srcs = { "src/Plugins/Html/**.cpp" }
plugin_database_srcs = { "src/Plugins/Database/**.cpp" }
plugin_freetype_srcs = { "src/Plugins/Freetype/**.cpp" }
plugin_metafont_srcs = { "src/Plugins/Metafont/**.cpp" }
plugin_ghostscript_srcs = { "src/Plugins/Ghostscript/**.cpp" }
plugin_ispell_srcs = { "src/Plugins/Ispell/**.cpp" }
plugin_tex_srcs = {"src/Plugins/Tex/**.cpp"}
plugin_latex_preview_srcs = {"src/Plugins/LaTeX_Preview/**.cpp"}
plugin_bibtex_srcs = { "src/Plugins/Bibtex/**.cpp" }
plugin_openssl_srcs = { "src/Plugins/Openssl/**.cpp" }
plugin_updater_srcs = { "src/Plugins/Updater/**.cpp" }
plugin_git_srcs = { "src/Plugins/Git/**.cpp" }


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

    add_packages("lolly")
    if not is_plat("macosx") then
        add_packages("libiconv")
    end
    add_packages("freetype")
    add_packages("pdfhummus")
    add_packages("s7")
    add_packages("libgit2")
    if is_plat("linux") and not using_legacy_apt() then
        add_packages("fontconfig")
    end

    if is_plat("mingw") then
        add_syslinks("wsock32", "ws2_32", "crypt32","secur32", {public = true})
    elseif is_plat("windows") then
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
    add_files(plugin_pdf_srcs)
    add_files(plugin_git_srcs)

    add_mxflags("-fno-objc-arc")
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/tm_configure.hpp"))
    end)
end 


includes("xmake/tm2html.lua")
if not is_plat("wasm") then
    target ("tm2html") do
        set_version(XMACS_VERSION)
        set_installdir(INSTALL_DIR)
        set_configdir(INSTALL_DIR)
        set_configvar("DEVEL_VERSION", DEVEL_VERSION)
        set_configvar("PACKAGE", "Mogan Research")
        set_configvar("XMACS_VERSION", XMACS_VERSION)
        add_target_tm2html()
    end
end


includes("xmake/draw.lua")
if is_plat("wasm", "linux") then
    target("draw") do
        set_version(XMACS_VERSION, {build = "%Y-%m-%d"})
        add_tm_configure("draw", TM_CONFIGURE_VARS)
        add_target_draw()
    end
end


includes("xmake/code.lua")
if is_plat("wasm", "linux") then
    target("code") do
        set_version(XMACS_VERSION, {build = "%Y-%m-%d"})
        add_tm_configure("code", TM_CONFIGURE_VARS)
        add_target_code()
    end
end


if is_plat("mingw", "windows") then
    target("research_windows_icon") do
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

includes("xmake/research.lua")
target("research") do
    set_version(XMACS_VERSION, {build = "%Y-%m-%d"})
    if is_plat("wasm") then
        add_tm_configure("research", TM_CONFIGURE_VARS)
        add_target_research_on_wasm()
    else
        set_installdir(INSTALL_DIR)
        set_configdir(INSTALL_DIR)
        set_configvar("DEVEL_VERSION", DEVEL_VERSION)
        set_configvar("XMACS_VERSION", XMACS_VERSION)
        add_target_research_on_others()
        on_run(function (target)
            name = target:name()
            if is_plat("mingw", "windows") then
                os.execv(target:installdir().."/bin/MoganResearch.exe")
            elseif is_plat("linux", "macosx") then
                print("Launching " .. target:targetfile())
                os.execv(target:targetfile(), {}, {envs=RUN_ENVS})
            else
                print("Unsupported plat $(plat)")
            end
        end)
    end
end

target("research_packager") do
    set_enabled(is_plat("macosx") and is_mode("release"))
    set_kind("phony")
    if is_plat("macosx") then
        set_configvar("XMACS_VERSION", XMACS_VERSION)
        set_configvar("APPCAST", "")
        set_configvar("OSXVERMIN", "")
        add_configfiles("$(projectdir)/packages/macos/Info.plist.in", {
            filename = "Info.plist",
            pattern = "@(.-)@",
        })
    end
    set_installdir(INSTALL_DIR)
    if is_plat("macosx") then
        add_deps("research")
    end

    after_install(function (target, opt)
        local app_dir = target:installdir() .. "/../../"
        os.cp("$(buildir)/Info.plist", app_dir .. "/Contents")
        local dmg_name= "MoganResearch-v" .. XMACS_VERSION .. ".dmg"
        if is_arch("arm64") then
            dmg_name= "MoganResearch-v" .. XMACS_VERSION .. "-arm.dmg"
        end
        os.execv("codesign", {"--force", "--deep", "--sign", "-", app_dir})
        os.execv("hdiutil create $(buildir)/" .. dmg_name .. " -fs HFS+ -srcfolder " .. app_dir)
    end)
end

if is_mode ("release") then
    xpack("research") do
        add_xpack_research(XMACS_VERSION)
    end
end

includes("xmake/tests.lua")
-- Tests in C++
l3_cpp_tests = os.files("tests/L3/**_test.cpp")
all_cpp_tests = os.files("tests/**_test.cpp")

for _, filepath in ipairs(l3_cpp_tests) do
    add_target_cpp_test(filepath, "libkernel_l3")
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
