-------------------------------------------------------------------------------
--
-- MODULE      : packages.lua
-- DESCRIPTION : Xmake package config file for TeXmacs
-- COPYRIGHT   : (C) 2023       jingkaimori
--                   2022-2023  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

--
-- Dependencies: Platform|Package Manager
--

-- The following versions are adopted on macOS/Windows/ArchLinux
-- We will use the system provided packages on Ubuntu 22.04/Debian 12/...
local LOLLY_VERSION = "1.3.10"
local TBOX_VERSION = "1.7.5"
local CPR_VERSION = "1.10.5"
local CURL_VERSION = "8.4.0"
local PDFHUMMUS_VERSION = "4.6.1"
local FREETYPE_VERSION = "2.12.1"
local QT6_VERSION = "6.5.3"
local QTIFW_VERSION = "4.6.0"
local S7_VERSION = "2023.04.13"
local LIBPNG_VERSION = "1.6.37"
local LIBJPEG_VERSION = "v9e"
local LIBICONV_VERSION = "1.17"

-- https://xmake.io/#/manual/package_dependencies?id=inherit-package-configuration
package("lolly")
    set_homepage("https://github.com/XmacsLabs/lolly")
    set_description("Lolly is a C++ library")

    add_urls("https://github.com/XmacsLabs/lolly.git")
    add_urls("https://gitee.com/XmacsLabs/lolly.git")

    add_deps("tbox")
    if not is_plat("wasm") then
        add_deps("cpr")
    end

    add_versions("v" .. LOLLY_VERSION, "da5fd221c00e348579106ca8c21110aaf00f763a")

    on_install("linux", "macosx", "mingw", "wasm", "windows", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()

function using_apt ()
    return linuxos.name() == "debian"
           or linuxos.name() == "ubuntu"
           or linuxos.name() == "uos"
end


function add_requires_of_mogan()
    -- package: lolly
    set_configvar("LOLLY_VERSION", LOLLY_VERSION)
    add_requires("lolly", {system=false})
    tbox_configs = {hash=true, ["force-utf8"]=true}
    add_requireconfs("lolly.tbox", {version = TBOX_VERSION, configs=tbox_configs, system = false, override=true})
    add_requireconfs("lolly.cpr", {version = CPR_VERSION, system = false, override=true})

    -- package: libcurl
    if is_plat("linux") and using_apt() then
        add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
        add_requireconfs("lolly.cpr.libcurl", {system = true, override=true})
    else
        add_requireconfs("lolly.cpr.libcurl", {version = CURL_VERSION, system = false, override=true})
    end

    -- package: s7
    add_requires("s7 "..S7_VERSION, {system=false})

    -- package: qt6widgets
    if is_plat("mingw") or is_plat("windows") then
        add_requires("qt6widgets "..QT6_VERSION)
        if is_mode("release") then
            add_requires("qtifw "..QTIFW_VERSION)
        end
    end

    -- package: pdfhummus
    set_configvar("PDFHUMMUS_VERSION", PDFHUMMUS_VERSION)
    if not is_plat("wasm") then
        add_requires("pdfhummus "..PDFHUMMUS_VERSION, {system=false,configs={libpng=true,libjpeg=true}})
    end

    -- package: libpng
    if is_plat("linux") and using_apt() then
        add_requires("apt::libpng-dev", {alias="libpng"})
        add_requireconfs("pdfhummus.libpng", {system = true, override=true})
    else
        add_requireconfs("pdfhummus.libpng", {version = LIBPNG_VERSION, system = false, override=true})
    end

    -- package: libjpeg
    if is_plat("linux") and using_apt() then
        if linuxos.name() == "ubuntu" then
            add_requires("apt::libjpeg-turbo8-dev", {alias="libjpeg"})
        else
            add_requires("apt::libjpeg62-turbo-dev", {alias="libjpeg"})
        end
        add_requireconfs("pdfhummus.libjpeg", {system = true, override=true})
    else
        add_requireconfs("pdfhummus.libjpeg", {version = LIBJPEG_VERSION, system = false, override=true})
    end

    -- package: freetype
    if is_plat("linux") and using_apt() then
        -- config package name for freetype on UOS
        if linuxos.name() == "uos" then
            add_requires("apt::libfreetype6-dev", {alias="freetype"})
        else
            add_requires("apt::libfreetype-dev", {alias="freetype"})
        end
    else
    -- Let xrepo manage the dependencies for macOS and other GNU/Linux distros
        add_requires("libiconv "..LIBICONV_VERSION, {system=false})
        add_requires("freetype "..FREETYPE_VERSION, {system=false})
        add_requireconfs("pdfhummus.freetype", {version = FREETYPE_VERSION, system = false, override=true})
    end

    if is_plat("linux") then
        add_requires("fontconfig", {system = true})
    end
end
