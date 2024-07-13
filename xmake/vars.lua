
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
XMACS_VERSION="1.2.8-alpha2"
CONFIG_USER = "XmacsLabs"
DEVEL_VERSION = TEXMACS_VERSION
DEVEL_RELEASE = 1
STABLE_VERSION = TEXMACS_VERSION
STABLE_RELEASE = 1

-- XmacsLabs dependencies
LOLLY_VERSION = "1.4.26"

-- Third-party dependencies
S7_VERSION = "20231024"
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
libmogan_headers = {
    "$(projectdir)/src/Data/Convert",
    "$(projectdir)/src/Data/Document",
    "$(projectdir)/src/Data/History",
    "$(projectdir)/src/Data/Observers",
    "$(projectdir)/src/Data/Parser",
    "$(projectdir)/src/Data/String",
    "$(projectdir)/src/Data/Tree",
    "$(projectdir)/src/Edit",
    "$(projectdir)/src/Edit/Editor",
    "$(projectdir)/src/Edit/Interface",
    "$(projectdir)/src/Edit/Modify",
    "$(projectdir)/src/Edit/Process",
    "$(projectdir)/src/Edit/Replace",
    "$(projectdir)/src/Graphics/Bitmap_fonts",
    "$(projectdir)/src/Graphics/Colors",
    "$(projectdir)/src/Graphics/Fonts",
    "$(projectdir)/src/Graphics/Gui",
    "$(projectdir)/src/Graphics/Handwriting",
    "$(projectdir)/src/Graphics/Mathematics",
    "$(projectdir)/src/Graphics/Pictures",
    "$(projectdir)/src/Graphics/Renderer",
    "$(projectdir)/src/Graphics/Spacial",
    "$(projectdir)/src/Graphics/Types",
    "$(projectdir)/src/Kernel/Abstractions",
    "$(projectdir)/src/Kernel/Types",
    "$(projectdir)/src/Plugins",
    "$(projectdir)/src/Scheme",
    "$(projectdir)/src/Scheme/S7",
    "$(projectdir)/src/Scheme/L2",
    "$(projectdir)/src/Scheme/L3",
    "$(projectdir)/src/Scheme/L4",
    "$(projectdir)/src/Scheme/L5",
    "$(projectdir)/src/Scheme/Plugins",
    "$(projectdir)/src/Scheme/Scheme",
    "$(projectdir)/src/System",
    "$(projectdir)/src/System/Boot",
    "$(projectdir)/src/System/Classes",
    "$(projectdir)/src/System/Config",
    "$(projectdir)/src/System/Files",
    "$(projectdir)/src/System/Language",
    "$(projectdir)/src/System/Link",
    "$(projectdir)/src/System/Misc",
    "$(projectdir)/src/Texmacs",
    "$(projectdir)/src/Texmacs/Data",
    "$(projectdir)/src/Typeset",
    "$(projectdir)/src/Typeset/Bridge",
    "$(projectdir)/src/Typeset/Concat",
    "$(projectdir)/src/Typeset/Page",
    "$(buildir)/glue",
    "$(projectdir)/TeXmacs/include",
}

libmogan_srcs = {
    "$(projectdir)/src/Data/**.cpp",
    "$(projectdir)/src/Edit/**.cpp",
    "$(projectdir)/src/Graphics/**.cpp",
    "$(projectdir)/src/Kernel/**.cpp",
    "$(projectdir)/src/Scheme/Scheme/**.cpp",
    "$(projectdir)/src/Scheme/S7/**.cpp",
    "$(projectdir)/src/Scheme/L2/**.cpp",
    "$(projectdir)/src/Scheme/L3/**.cpp",
    "$(projectdir)/src/Scheme/L4/**.cpp",
    "$(projectdir)/src/Scheme/L5/**.cpp",
    "$(projectdir)/src/Scheme/Plugins/**.cpp",
    "$(projectdir)/src/System/**.cpp",
    "$(projectdir)/src/Texmacs/Data/**.cpp",
    "$(projectdir)/src/Texmacs/Server/**.cpp",
    "$(projectdir)/src/Texmacs/Window/**.cpp",
    "$(projectdir)/src/Typeset/**.cpp",
}

plugin_qt_srcs_on_wasm = {
    "$(projectdir)/src/Plugins/Qt/*.cpp|QTMPipeLink.cpp|QTMPrintDialog.cpp|QTMPrinterSettings.cpp|qt_printer_widget.cpp",
    "$(projectdir)/src/Plugins/Qt/*.hpp|QTMPipeLink.hpp|QTMPrintDialog.hpp|QTMPrinterSettings.hpp",
}
plugin_qt_srcs = {
    "$(projectdir)/src/Plugins/Qt/**.cpp",
    "$(projectdir)/src/Plugins/Qt/**.hpp"
}
plugin_macos_srcs = {
    "$(projectdir)/src/Plugins/MacOS/HIDRemote.m",
    "$(projectdir)/src/Plugins/MacOS/mac_spellservice.mm",
    "$(projectdir)/src/Plugins/MacOS/mac_utilities.mm",
    "$(projectdir)/src/Plugins/MacOS/mac_app.mm"
}
plugin_pdf_srcs = { "$(projectdir)/src/Plugins/Pdf/**.cpp" }
plugin_xml_srcs = { "$(projectdir)/src/Plugins/Xml/**.cpp" }
plugin_html_srcs = { "$(projectdir)/src/Plugins/Html/**.cpp" }
plugin_database_srcs = { "$(projectdir)/src/Plugins/Database/**.cpp" }
plugin_freetype_srcs = { "$(projectdir)/src/Plugins/Freetype/**.cpp" }
plugin_metafont_srcs = { "$(projectdir)/src/Plugins/Metafont/**.cpp" }
plugin_ghostscript_srcs = { "$(projectdir)/src/Plugins/Ghostscript/**.cpp" }
plugin_ispell_srcs = { "$(projectdir)/src/Plugins/Ispell/**.cpp" }
plugin_tex_srcs = { "$(projectdir)/src/Plugins/Tex/**.cpp" }
plugin_latex_preview_srcs = { "$(projectdir)/src/Plugins/LaTeX_Preview/**.cpp" }
plugin_bibtex_srcs = { "$(projectdir)/src/Plugins/Bibtex/**.cpp" }
plugin_openssl_srcs = { "$(projectdir)/src/Plugins/Openssl/**.cpp" }
plugin_updater_srcs = { "$(projectdir)/src/Plugins/Updater/**.cpp" }
plugin_git_srcs = { "$(projectdir)/src/Plugins/Git/**.cpp" }
