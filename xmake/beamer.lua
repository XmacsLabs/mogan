-------------------------------------------------------------------------------
--
-- MODULE      : beamer.lua
-- DESCRIPTION : Xmake config file for Mogan Beamer
-- COPYRIGHT   : (C) 2024  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

includes ("vars.lua")

local beamer_files = {
    "$(projectdir)/TeXmacs(/doc/**)",
    "$(projectdir)/TeXmacs(/fonts/**)",
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
    target("beamer_windows_icon") do
        set_version(XMACS_VERSION)
        set_kind("object")
        add_configfiles("$(projectdir)/packages/windows/beamer.rc.in", {
            filename = "beamer/resource.rc"
        })
        add_configfiles("$(projectdir)/packages/windows/beamer.ico", {
            filename = "beamer/beamer.ico",
            onlycopy = true
        })
        add_files("$(buildir)/beamer/resource.rc")
    end
end

target("beamer") do
    if is_plat("windows") and is_mode("release") then
        add_deps("beamer_windows_icon")
    end

    if is_plat("windows") then 
        set_installdir("$(buildir)/packages/beamer/data/")
    elseif is_plat("macosx") then 
        set_installdir("$(buildir)/$(plat)/$(arch)/$(mode)/MoganBeamer.app/Contents/Resources/")
    end

    set_version(XMACS_VERSION, {build = "%Y-%m-%d"})
    set_basename("MoganBeamer")
    set_encodings("utf-8")

    add_configfiles("$(projectdir)/src/System/tm_configure.hpp.xmake", {
        filename = "beamer/tm_configure.hpp",
        variables = TM_CONFIGURE_VARS
    })

    if is_plat("windows") then
        set_optimize("smallest")
        set_runtimes("MT")
    end

    build_glue_on_config()
    add_configfiles("$(projectdir)/src/System/config.h.xmake", {
        filename = "beamer/config.h",
        variables = {
            MACOSX_EXTENSIONS = is_plat("macosx"),
            USE_PLUGIN_PDF = true,
            USE_PLUGIN_BIBTEX = false,
            USE_PLUGIN_LATEX_PREVIEW = true,
            USE_PLUGIN_TEX = true,
            USE_PLUGIN_ISPELL = true,
            USE_PLUGIN_HTML = true,
            QTPIPES = not is_plat("wasm"),
            USE_QT_PRINTER = not is_plat("wasm"),
            NOMINMAX = is_plat("windows"),
            SIZEOF_VOID_P = 8,
            USE_FONTCONFIG = is_plat("linux"),
            USE_STACK_TRACE = (not is_plat("mingw")) and (not is_plat("wasm")) and (not is_plat("windows")),
            USE_PLUGIN_GS = true,
            USE_FREETYPE = true,
            USE_ICONV = is_plat("wasm"),
            QTTEXMACS = true,
            APP_MOGAN_BEAMER = true,
        }
    })

    if is_mode("debug", "releasedbg") and is_plat("windows") then
        add_rules("qt.console")
    else
        add_rules("qt.widgetapp")
    end

    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")

    add_packages("moebius")
    add_packages("freetype")
    add_packages("s7")
    add_packages("pdfhummus")
    add_packages("libgit2")
    if not is_plat("macosx") then
        add_packages("libiconv")
    end
    if is_plat("windows") then
        add_syslinks("secur32", "shell32", {public = true})
        add_packages("qt6widgets")
    end

    add_includedirs(libmogan_headers, {public = true})
    add_includedirs("$(buildir)/beamer")
    add_files(libmogan_srcs)

    if is_plat("macosx") then
        add_includedirs("$(projectdir)/src/Plugins/MacOS", {public = true})
        add_files(plugin_macos_srcs)
    end
    add_files(plugin_database_srcs)
    add_files(plugin_qt_srcs)
    add_files(plugin_freetype_srcs)
    add_files(plugin_ghostscript_srcs)
    add_files(plugin_ispell_srcs)
    add_files(plugin_metafont_srcs)
    add_files(plugin_tex_srcs)
    add_files(plugin_latex_preview_srcs)
    add_files(plugin_openssl_srcs)
    add_files(plugin_xml_srcs)
    add_files(plugin_html_srcs)
    add_files(plugin_pdf_srcs)
    add_files(plugin_git_srcs)

    add_mxflags("-fno-objc-arc")

    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/beamer/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/beamer/tm_configure.hpp"))
    end)
    add_files("$(projectdir)/src/Mogan/Beamer/beamer.cpp")

    if not is_plat("windows") then
        add_syslinks("pthread")
    end

    -- if is_plat("mingw", "windows") and is_mode("release") then
    --     add_deps("research_windows_icon")
    -- end

    set_configvar("PACKAGE", "Mogan Beamer")

    -- package metadata
    if is_plat("macosx") then
        add_installfiles({
            "$(projectdir)/packages/macos/beamer.icns",
            "$(projectdir)/packages/macos/TeXmacs-document.icns",
        })
    end
  
    if is_plat("windows") then
        add_installfiles(beamer_files)
    else
        add_installfiles(beamer_files, {prefixdir="share/Xmacs"})
    end

    -- deploy necessary dll
    if is_plat("windows") then
        set_values("qt.deploy.flags", {"-printsupport", "--no-opengl-sw", "--no-translations"})
    end

    after_install(function (target)
        print("after_install of target beamer")

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
        if is_plat("windows") then
            os.execv(target:installdir().."/bin/MoganBeamer.exe")
        elseif is_plat("macosx") then
            print("Launching " .. target:targetfile())
            os.execv(target:targetfile(), {}, {envs={TEXMACS_PATH= path.join(os.projectdir(), "TeXmacs")}})
        else
            print("Unsupported plat $(plat)")
        end
    end)
end
