-------------------------------------------------------------------------------
--
-- MODULE      : draw.lua
-- DESCRIPTION : Xmake config file for Mogan Draw
-- COPYRIGHT   : (C) 2022-2023  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function add_target_draw()
    set_languages("c++17")

    build_glue_on_config()
    add_configfiles("src/System/config.h.xmake", {
        filename = "draw/config.h",
        variables = {
            MACOSX_EXTENSIONS = is_plat("macosx"),
            USE_PLUGIN_PDF = false,
            USE_PLUGIN_BIBTEX = false,
            USE_PLUGIN_LATEX_PREVIEW = false,
            USE_PLUGIN_TEX = false,
            USE_PLUGIN_ISPELL = false,
            USE_PLUGIN_SPARKLE = false,
            USE_PLUGIN_HTML = false,
            QTPIPES = is_plat("linux"),
            USE_QT_PRINTER = is_plat("linux"),
            NOMINMAX = is_plat("windows"),
            SIZEOF_VOID_P = 8,
            USE_FONTCONFIG = is_plat("linux"),
            USE_STACK_TRACE = (not is_plat("mingw")) and (not is_plat("wasm")) and (not is_plat("windows")),
            USE_PLUGIN_GS = false,
            USE_FREETYPE = true,
            USE_ICONV = true,
            QTTEXMACS = true,
            APP_MOGAN_DRAW = true,
        }
    })
    add_packages("lolly")
    add_packages("freetype")
    add_packages("s7")
    if is_plat("wasm") then
        add_rules("qt.widgetapp_static")
    else
        add_rules("qt.widgetapp")
    end
    if is_plat("wasm") then
        add_frameworks("QtGui", "QtWidgets", "QtCore", "QtSvg", "QWasmIntegrationPlugin")
    else
        add_frameworks("QtGui", "QtWidgets", "QtCore", "QtSvg", "QtPrintSupport")
    end
    if is_plat("linux") then
        add_packages("fontconfig")
    end

    add_includedirs(libmogan_headers, {public = true})
    add_includedirs("$(buildir)/draw")
    add_files(libmogan_srcs)
    if is_plat("wasm") then
        add_files(plugin_qt_srcs_on_wasm)
    else
        add_files(plugin_qt_srcs)
    end
    add_files(plugin_freetype_srcs)
    add_files(plugin_database_srcs)
    add_files(plugin_metafont_srcs)
    add_files(plugin_openssl_srcs)
    add_files(plugin_xml_srcs)
    add_files("src/Mogan/Draw/draw.cpp")

    if is_plat("wasm") then
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/fonts@TeXmacs/fonts", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/packages@TeXmacs/packages", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/progs@TeXmacs/progs", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/doc@TeXmacs/doc", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/include@TeXmacs/include", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/langs@TeXmacs/langs", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/styles@TeXmacs/styles", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/texts@TeXmacs/texts", {force = true})
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs/misc@TeXmacs/misc", {force = true})
    end

    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/draw/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/draw/tm_configure.hpp"))
    end)
end

