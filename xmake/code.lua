-------------------------------------------------------------------------------
--
-- MODULE      : code.lua
-- DESCRIPTION : Xmake config file for Mogan Code
-- COPYRIGHT   : (C) 2022-2023  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function add_target_code()
    set_languages("c++17")
    set_encodings("utf-8")
    if is_plat("windows") then
        set_runtimes("MT")
    end

    build_glue_on_config()
    add_configfiles("src/System/config.h.xmake", {
        filename = "code/config.h",
        variables = {
            MACOSX_EXTENSIONS = is_plat("macosx"),
            USE_PLUGIN_PDF = false,
            USE_PLUGIN_BIBTEX = false,
            USE_PLUGIN_LATEX_PREVIEW = false,
            USE_PLUGIN_TEX = false,
            USE_PLUGIN_ISPELL = false,
            USE_PLUGIN_HTML = false,
            QTPIPES = not is_plat("wasm"),
            USE_QT_PRINTER = not is_plat("wasm"),
            NOMINMAX = is_plat("windows"),
            SIZEOF_VOID_P = 8,
            USE_FONTCONFIG = is_plat("linux"),
            USE_STACK_TRACE = (not is_plat("mingw")) and (not is_plat("wasm")) and (not is_plat("windows")),
            USE_PLUGIN_GS = false,
            USE_FREETYPE = true,
            USE_ICONV = is_plat("wasm"),
            QTTEXMACS = true,
            APP_MOGAN_CODE = true,
        }
    })

    add_packages("moebius")
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
    add_includedirs("$(buildir)/code")
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
    add_files("src/Mogan/Code/code.cpp")

    if is_plat("wasm") then
        add_ldflags("-s --preload-file $(projectdir)/TeXmacs@TeXmacs", {force = true})
    elseif is_plat("windows") then
        add_ldflags("/LTCG")
        add_syslinks("secur32", "shell32", {public = true})
    end

    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/code/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/code/tm_configure.hpp"))
    end)
end
