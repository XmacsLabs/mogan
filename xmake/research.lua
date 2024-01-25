-------------------------------------------------------------------------------
--
-- MODULE      : research.lua
-- DESCRIPTION : Xmake config file for Mogan Research
-- COPYRIGHT   : (C) 2022-2023  jingkaimori
--                   2022-2023  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


local TeXmacs_files = {
        "TeXmacs(/doc/**)",
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
        "TeXmacs(/plugins/**)" -- plugin files
}

function add_target_research_on_wasm()
    set_languages("c++17")

    build_glue_on_config()
    add_configfiles("src/System/config.h.xmake", {
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
            GS_FONTS = "../share/ghostscript/fonts:/usr/share/fonts:",
            GS_LIB = "../share/ghostscript/9.06/lib:",
            GS_EXE = "",
            USE_FREETYPE = true,
            USE_ICONV = true,
            QTTEXMACS = true,
        }
    })
    
    add_packages("lolly")
    add_packages("freetype")
    add_packages("s7")
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
    add_files("src/Mogan/Research/research.cpp")
    
    add_ldflags("-s --preload-file $(projectdir)/TeXmacs@TeXmacs", {force = true})
    
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/research/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/research/tm_configure.hpp"))
    end)
end


function add_target_research_on_others()
    set_basename("MoganResearch")

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

    add_packages("lolly")
    if is_plat("mingw", "windows") then
        add_packages("qt6widgets")
    end

    add_deps("libmogan")
    add_includedirs({
        "$(buildir)",
    })
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/tm_configure.hpp"))
    end)
    add_files("src/Mogan/Research/research.cpp")

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
    add_configfiles("(misc/man/texmacs.1.in)", {
        filename = "texmacs.1",
        pattern = "@([^\n]-)@",
    })

    -- package metadata
    if is_plat("macosx") then
        add_installfiles({
            "packages/macos/new-mogan.icns",
            "packages/macos/TeXmacs-document.icns",
            "src/Plugins/Cocoa/(English.lproj/**)",
            "src/Plugins/Cocoa/(zh_CN.lproj/**)"
        })
    end
  
    -- install icons
    if is_plat("linux") then
        add_installfiles("TeXmacs/misc/images/text-x-mogan.svg", {prefixdir="share/icons/hicolor/scalable/mimetypes"})
        add_installfiles("TeXmacs/misc/mime/MoganResearch.desktop", {prefixdir="share/applications"})
        add_installfiles("TeXmacs/misc/mime/mogan.xml", {prefixdir="share/mime/packages"})
    end
  
    if is_plat("mingw", "windows") then
        add_installfiles(TeXmacs_files)
    else
        add_installfiles("misc/scripts/tm_gs", {prefixdir="share/Xmacs/bin"})
        add_installfiles(TeXmacs_files, {prefixdir="share/Xmacs"})
    end

    -- install tm files for testing purpose
    if is_mode("releasedbg") then
        if is_plat("mingw", "windows") then
            add_installfiles({
                "TeXmacs(/tests/tm/*.tm)",
                "TeXmacs(/tests/tex/*.tex)",
                "TeXmacs(/tests/bib/*.bib)",
            })
        else
            add_installfiles({
                "TeXmacs(/tests/*.tm)",
                "TeXmacs(/tests/*.bib)",
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
        import("xmake.global")
        if is_plat("linux") then
            global.copy_icons(target)
        end

        if is_plat("macosx") and is_arch("arm64") then
            local app_dir = target:installdir() .. "/../../"
            os.rm(app_dir .. "Contents/Resources/include")
            os.rm(app_dir .. "Contents/Frameworks/QtQmlModels.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQml.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQuick.framework")
            os.execv("codesign", {"--force", "--deep", "--sign", "-", app_dir})
        end
    end)
end

function add_xpack_research(XMACS_VERSION)
    set_formats("nsis")
    set_specfile("packages/windows/research.nsis")
    set_specvar("PACKAGE_INSTALL_DIR", "XmacsLabs\\MoganResearch-"..XMACS_VERSION)
    set_specvar("PACKAGE_NAME", "MoganResearch")
    set_version("1.2.3")
    set_title("Mogan Research")
    set_author("XmacsLabs")
    set_description("user friendly distribution of GNU TeXmacs")
    set_homepage("https://mogan.app")
    set_license("GPLv3")
    set_licensefile("LICENSE")
    add_targets("research")
    set_iconfile("packages/windows/Xmacs.ico")
    set_bindir("bin")
    add_installfiles("build/packages/app.mogan/data/bin/(**)|MoganResearch.exe", {prefixdir = "bin"})

    on_load(function (package)
        if is_plat("windows") then
            package:set("basename", "MoganResearch-v" .. XMACS_VERSION .. "-64bit-installer")
        end
    end)
end
