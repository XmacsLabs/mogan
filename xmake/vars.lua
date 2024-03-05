
-------------------------------------------------------------------------------
--
-- MODULE      : vars.lua
-- DESCRIPTION : Xmake config file for variables
-- COPYRIGHT   : (C) 2022-2024  jingkaimori
--                   2022-2024  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

TEXMACS_VERSION = "2.1.2"
XMACS_VERSION="1.2.6-alpha"
CONFIG_USER = "XmacsLabs"
DEVEL_VERSION = TEXMACS_VERSION
DEVEL_RELEASE = 1
STABLE_VERSION = TEXMACS_VERSION
STABLE_RELEASE = 1

-- XmacsLabs dependencies
LOLLY_VERSION = "1.3.25"

-- Third-party dependencies
S7_VERSION = "20230413"
QT6_VERSION = "6.5.3"
QTIFW_VERSION = "4.6.0"
TBOX_VERSION = "1.7.5"
CPR_VERSION = "1.10.5"
CURL_VERSION = "8.4.0"
PDFHUMMUS_VERSION = "4.6.2"
FREETYPE_VERSION = "2.12.1"
LIBPNG_VERSION = "1.6.37"
LIBJPEG_VERSION = "v9e"
LIBICONV_VERSION = "1.17"
LIBGIT2_VERSION = "1.7.1"


TM_CONFIGURE_VARS = {
    CONFIG_USER = CONFIG_USER,
    TEXMACS_VERSION = TEXMACS_VERSION,
    XMACS_VERSION = XMACS_VERSION,
    tm_devel = "Texmacs-" .. DEVEL_VERSION,
    tm_devel_release = "Texmacs-" .. DEVEL_VERSION .. "-" .. DEVEL_RELEASE,
    tm_stable = "Texmacs-" .. STABLE_VERSION,
    tm_stable_release = "Texmacs-" .. STABLE_VERSION .. "-" .. STABLE_RELEASE,
    LOLLY_VERSION = LOLLY_VERSION,
}

RUN_ENVS = {TEXMACS_PATH=path.join(os.projectdir(), "TeXmacs")}

-- Source Code
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
