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
            NOMINMAX = is_plat("windows"),
            SIZEOF_VOID_P = 8,
            USE_FONTCONFIG = is_plat("linux"),
            USE_STACK_TRACE = (not is_plat("mingw")) and (not is_plat("wasm")) and (not is_plat("windows")),
            USE_GS = not is_plat("wasm"),
            GS_FONTS = "../share/ghostscript/fonts:/usr/share/fonts:",
            GS_LIB = "../share/ghostscript/9.06/lib:",
            GS_EXE = "",
            USE_FREETYPE = true,
            USE_ICONV = true,
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
    add_files("src/Mogan/Research/research.cpp")
    
    add_ldflags("-s --preload-file $(projectdir)/TeXmacs@TeXmacs", {force = true})
    
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/research/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/research/tm_configure.hpp"))
    end)
end


function add_target_research_on_others()
    set_installdir(INSTALL_DIR)

    if is_plat("macosx") then
        set_filename("Mogan")
    elseif is_plat("mingw", "windows") then
        set_filename("mogan.exe")
    else
        set_filename("mogan")
    end

    if is_plat("windows") then
        set_optimize("smallest");
    end

    if is_mode("debug", "releasedbg") and is_plat("mingw", "windows") then
        add_rules("qt.console")
    else
        add_rules("qt.widgetapp")
    end

    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    if is_plat("macosx") then
        add_frameworks("QtMacExtras")
    end

    add_packages("lolly")
    if is_plat("mingw", "windows") then
        add_packages("qt5widgets")
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

    if is_plat("mingw") and is_mode("release") then
        add_deps("windows_icon")
    end

    set_configvar("PACKAGE", "Mogan Research")

    -- install man.1 manual file
    add_configfiles("(misc/man/texmacs.1.in)", {
        filename = "texmacs.1",
        pattern = "@([^\n]-)@",
    })

    -- package metadata
    if is_plat("macosx") then
        set_configvar("APPCAST", "")
        set_configvar("OSXVERMIN", "")
        add_configfiles("(packages/macos/Info.plist.in)", {
            filename = "Info.plist",
            pattern = "@(.-)@",
        })
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
    add_installfiles("TeXmacs/misc/images/text-x-texmacs.svg", {prefixdir="share/icons/hicolor/scalable/mimetypes"})
    add_installfiles("TeXmacs/misc/mime/texmacs.desktop", {prefixdir="share/applications"})
    add_installfiles("TeXmacs/misc/mime/texmacs.xml", {prefixdir="share/mime/packages"})
    add_installfiles("TeXmacs/misc/pixmaps/TeXmacs.xpm", {prefixdir="share/pixmaps"})
  
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

    -- deploy necessary dll
    if is_plat("windows") then
        set_values("qt.deploy.flags", {"-printsupport"})
    elseif is_plat("mingw") then
        -- qt on mingw provides debug dll only, without "d" suffix.
        set_values("qt.deploy.flags", {"-printsupport", "--debug"})
    end

    after_install(function (target)
        print("after_install of target research")
        import("xmake.global")
        global.copy_icons(target)

        if is_plat("macosx") and is_arch("arm64") then
            local app_dir = target:installdir() .. "/../../"
            os.rm(app_dir .. "Contents/Resources/include")
            os.rm(app_dir .. "Contents/Frameworks/QtQmlModels.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQml.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQuick.framework")
            os.execv("codesign", {"--force", "--deep", "--sign", "-", app_dir})
        end
    end)

    on_run(function (target)
        name = target:name()
        if is_plat("mingw", "windows") then
            os.execv(target:installdir().."/bin/mogan.exe")
        elseif is_plat("linux", "macosx") then
            print("Launching " .. target:targetfile())
            os.execv(target:targetfile(), {}, {envs=RUN_ENVS})
        else
            print("Unsupported plat $(plat)")
        end
    end)
end
