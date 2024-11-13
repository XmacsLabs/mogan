-------------------------------------------------------------------------------
--
-- MODULE      : research.lua
-- DESCRIPTION : Xmake config file for Mogan Research
-- COPYRIGHT   : (C) 2022-2023  jingkaimori
--                   2022-2024  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

includes ("vars.lua")
if not is_plat("wasm") then
    includes ("goldfish.lua")
end

local research_files = {
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

function add_target_research_on_wasm()
    set_languages("c++17")

    add_configfiles("$(projectdir)/src/System/tm_configure.hpp.xmake", {
        filename = "research/tm_configure.hpp",
        variables = TM_CONFIGURE_VARS
    })

    build_glue_on_config()
    add_configfiles("$(projectdir)/src/System/config.h.xmake", {
        filename = "research/config.h",
        variables = {
            MACOSX_EXTENSIONS = is_plat("macosx"),
            USE_PLUGIN_PDF = false,
            USE_PLUGIN_BIBTEX = true,
            USE_PLUGIN_LATEX_PREVIEW = false,
            USE_PLUGIN_TEX = true,
            USE_PLUGIN_ISPELL = true,
            USE_PLUGIN_HTML = true,
            NOMINMAX = is_plat("windows"),
            SIZEOF_VOID_P = 8,
            USE_FONTCONFIG = false,
            USE_STACK_TRACE = (not is_plat("mingw")) and (not is_plat("wasm")) and (not is_plat("windows")),
            USE_PLUGIN_GS = not is_plat("wasm"),
            USE_FREETYPE = true,
            USE_ICONV = true,
            QTTEXMACS = true,
            APP_MOGAN_RESEARCH = true,
        }
    })
    
    add_packages("moebius")
    add_packages("freetype")
    add_packages("s7")
    add_packages("tree-sitter")
    add_packages("tree-sitter-cpp")
    add_packages("tree-sitter-scheme")

    add_rules("qt.widgetapp_static")
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtSvg", "QWasmIntegrationPlugin")
    
    add_includedirs(libmogan_headers, {public = true})
    add_includedirs("$(buildir)/research")
    add_files(libmogan_srcs)
    add_files(plugin_qt_srcs_on_wasm)
    add_files(plugin_bibtex_srcs)
    add_files(plugin_freetype_srcs)
    add_files(plugin_database_srcs)
    add_files(plugin_ispell_srcs)
    add_files(plugin_metafont_srcs)
    add_files(plugin_tex_srcs)
    add_files(plugin_latex_preview_srcs)
    add_files(plugin_openssl_srcs)
    add_files(plugin_xml_srcs)
    add_files(plugin_html_srcs)
    add_files(plugin_treesitter_srcs)
    add_files("$(projectdir)/src/Mogan/Research/research.cpp")
    
    add_ldflags("-s --preload-file $(projectdir)/TeXmacs@TeXmacs", {force = true})
    
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/research/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/research/tm_configure.hpp"))
    end)
end


function add_target_research_on_others()
    set_basename("MoganResearch")

    local install_dir = "$(buildir)"
    if is_plat("windows") then
        install_dir = path.join("$(buildir)", "packages/app.mogan/data/")
    elseif is_plat("macosx") then
        install_dir = path.join("$(buildir)", "macosx/$(arch)/$(mode)/MoganResearch.app/Contents/Resources/")
    else
        if os.getenv("INSTALL_DIR") == nil then
            install_dir = path.join("$(buildir)", "packages/app.mogan/")
        else
            install_dir = os.getenv("INSTALL_DIR")
        end
    end
    set_installdir(install_dir)
    set_configdir(install_dir)

    set_configvar("DEVEL_VERSION", DEVEL_VERSION)
    set_configvar("XMACS_VERSION", XMACS_VERSION)

    if is_plat("windows") then
        set_optimize("smallest")
        set_runtimes("MT")
    end

    if is_mode("debug", "releasedbg") and is_plat("mingw", "windows") then
        add_rules("qt.console")
    else
        add_rules("qt.widgetapp")
    end

    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")

    add_packages("cpptrace")
    add_packages("moebius")
    if is_plat("mingw", "windows") then
        add_packages("qt6widgets")
    end
    add_packages("s7")
    add_packages("tree-sitter")
    add_packages("tree-sitter-cpp")
    add_packages("tree-sitter-scheme")

    add_deps("libmogan")
    if is_plat("linux") and (linuxos.name () == "ubuntu" and linuxos.version():major() == 20) then
        add_packages("freetype", {links={}})
        add_packages("pdfhummus", {links={}})
        on_config(function (target)
            local pkg= target:pkg("freetype")
            local pkg2= target:pkg("pdfhummus")
            target:add("links", "$(buildir)/$(plat)/$(arch)/$(mode)/libmogan.a")
            target:add("links", pkg2:installdir() .. "/lib/libpdfhummus.a")
            target:add("links", pkg:installdir() .. "/lib/libfreetype.a")
        end)
    end
    add_includedirs({
        "$(buildir)",
    })
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/tm_configure.hpp"))
    end)
    add_files("$(projectdir)/src/Mogan/Research/research.cpp")

    if is_plat("linux") then
        add_rpathdirs("@executable_path/../lib")
    end
    if not is_plat("windows") then
        add_syslinks("pthread")
    end

    if is_plat("mingw", "windows") and is_mode("release") then
        add_deps("research_windows_icon")
    end

    set_configvar("PACKAGE", "Mogan Research")

    -- install man.1 manual file
    if is_plat("linux") then
        add_configfiles("(misc/man/texmacs.1.in)", {
            filename = "texmacs.1",
            pattern = "@([^\n]-)@",
        })
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
  
    -- install icons
    if is_plat("linux") then
        add_installfiles("$(projectdir)/TeXmacs/misc/images/text-x-mogan.svg", {prefixdir="share/icons/hicolor/scalable/mimetypes"})
        add_installfiles("$(projectdir)/TeXmacs/misc/mime/MoganResearch.desktop", {prefixdir="share/applications"})
        add_installfiles("$(projectdir)/TeXmacs/misc/mime/mogan.xml", {prefixdir="share/mime/packages"})
    end
  
    if is_plat("mingw", "windows") then
        add_installfiles(research_files)
    else
        add_installfiles(research_files, {prefixdir="share/Xmacs"})
    end

    if is_plat("linux") then
        add_installfiles("$(projectdir)/TeXmacs(/fonts/enc/**)", {prefixdir="share/Xmacs"})
        add_installfiles("$(projectdir)/TeXmacs(/fonts/tfm/**)", {prefixdir="share/Xmacs"})
        add_installfiles("$(projectdir)/TeXmacs(/fonts/type1/**)", {prefixdir="share/Xmacs"})
        add_installfiles("$(projectdir)/TeXmacs(/fonts/virtual/**)", {prefixdir="share/Xmacs"})
        add_installfiles("$(projectdir)/TeXmacs(/fonts/*scm)", {prefixdir="share/Xmacs"})
        add_installfiles("$(projectdir)/TeXmacs(/fonts/*LICENSE)", {prefixdir="share/Xmacs"})
    elseif is_plat("macosx") then
        add_installfiles("$(projectdir)/TeXmacs(/fonts/**)", {prefixdir="share/Xmacs"})
    else
        add_installfiles("$(projectdir)/TeXmacs(/fonts/**)")
    end

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
            }, {prefixdir="share/Xmacs"})
        end
    end

    -- deploy necessary dll
    if is_plat("windows") then
        set_values("qt.deploy.flags", {"-printsupport", "--no-opengl-sw", "--no-translations"})
    elseif is_plat("mingw") then
        -- qt on mingw provides debug dll only, without "d" suffix.
        set_values("qt.deploy.flags", {"-printsupport", "--no-opengl-sw", "--no-translations", "--debug"})
    end

    after_install(function (target)
        print("after_install of target research")
        if is_plat("linux") then
            import("global")
            global.copy_icons(target)
        end

        if is_plat("macosx") and is_arch("arm64") then
            local app_dir = target:installdir() .. "/../../"
            os.rm(app_dir .. "Contents/Resources/include")
            os.rm(app_dir .. "Contents/Frameworks/QtQmlModels.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQml.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQuick.framework")
        end
    end)

    on_run(function (target)
        name = target:name()
        if is_plat("windows") then
            os.execv(target:installdir().."/bin/MoganResearch.exe")
        elseif is_plat("linux", "macosx") then
            print("Launching " .. target:targetfile())
            os.execv(target:targetfile(), {"-d"}, {envs={TEXMACS_PATH= path.join(os.projectdir(), "TeXmacs")}})
        else
            print("Unsupported plat $(plat)")
        end
    end)
end


target("research") do
    if not is_plat("wasm") then
        add_deps("goldfish")
    end
    set_version(XMACS_VERSION, {build = "%Y-%m-%d"})
    if is_plat("wasm") then
        add_target_research_on_wasm()
    else
        add_target_research_on_others()
    end
end


if is_mode("release") then
xpack("research") do
    set_formats("nsis", "zip")
    set_specfile(path.join(os.projectdir(), "packages/windows/research.nsis"))
    set_specvar("PACKAGE_INSTALL_DIR", "XmacsLabs\\MoganResearch-"..XMACS_VERSION)
    set_specvar("PACKAGE_NAME", "MoganResearch")
    set_specvar("PACKAGE_SHORTCUT_NAME", "Mogan Research")
    _, pos = string.find(XMACS_VERSION, "-")
    local XMACS_VERSION_XYZ= XMACS_VERSION
    if not (pos == nil) then
        XMACS_VERSION_XYZ= string.sub(XMACS_VERSION, 1, pos-1)
    end
    set_version(XMACS_VERSION_XYZ)
    set_title("Mogan Research")
    set_author("XmacsLabs")
    set_description("user friendly distribution of GNU TeXmacs")
    set_homepage("https://mogan.app")
    set_license("GPLv3")
    set_licensefile(path.join(os.projectdir(), "LICENSE"))
    add_targets("research")
    set_iconfile(path.join(os.projectdir(), "packages/windows/Xmacs.ico"))
    set_bindir("bin")
    add_installfiles(path.join(os.projectdir(), "build/packages/app.mogan/data/bin/(**)|MoganResearch.exe"), {prefixdir = "bin"})
    on_load(function (package)
        local format = package:format()
        if format == "nsis" then
            package:set("basename", "MoganResearch-v" .. package:version() .. "-64bit-installer")
        else
            package:set("basename", "MoganResearch-v" .. package:version() .. "-64bit-portable")
        end
    end)
end
end


target("research_packager") do
    set_enabled(is_plat("macosx") and is_mode("release"))
    set_kind("phony")

    add_deps("research")

    set_configvar("XMACS_VERSION", XMACS_VERSION)
    set_configvar("APPCAST", "")
    set_configvar("OSXVERMIN", "")
    add_configfiles("$(projectdir)/packages/macos/Info.plist.in", {
        filename = "Info.plist",
        pattern = "@(.-)@",
    })

    set_installdir(path.join("$(buildir)", "macosx/$(arch)/$(mode)/MoganResearch.app/Contents/Resources/"))

    local dmg_name= "MoganResearch-v" .. XMACS_VERSION .. ".dmg"
    if is_arch("arm64") then
        dmg_name= "MoganResearch-v" .. XMACS_VERSION .. "-arm.dmg"
    end

    after_install(function (target, opt)
        local app_dir = target:installdir() .. "/../../"
        os.cp("$(buildir)/Info.plist", app_dir .. "/Contents")
        os.execv("codesign", {"--force", "--deep", "--sign", "-", app_dir})
        os.execv("hdiutil create $(buildir)/" .. dmg_name .. " -fs HFS+ -srcfolder " .. app_dir)
    end)
end
