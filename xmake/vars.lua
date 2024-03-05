
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
