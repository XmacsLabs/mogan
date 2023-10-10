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

-- GNU/Linux variants
-- [x] APT powered
-- [ ] pacman powered
-- [ ] portage powered
-- ...

local LOLLY_VERSION = "1.2.9"
local TBOX_VERSION = "1.7.4"

-- https://xmake.io/#/manual/package_dependencies?id=inherit-package-configuration
package("lolly")
    set_homepage("https://github.com/XmacsLabs/lolly")
    set_description("Lolly is a C++ library")

    add_urls("https://github.com/XmacsLabs/lolly.git")
    add_urls("https://gitee.com/XmacsLabs/lolly.git")

    add_deps("tbox")
    if not is_plat("wasm") then
        add_deps("libcurl")
    end

    add_versions("v" .. LOLLY_VERSION, "c4173095d8a1afdeef37cdb48ee6453d3e5960eb")

    on_install("linux", "macosx", "mingw", "wasm", "windows", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()


function add_requires_of_mogan()
    local CURL_VERSION = "7.84.0"
    local FREETYPE_VERSION = "2.12.1"
    local PDFHUMMUS_VERSION = "4.5.10"

    if is_plat("linux") and (linuxos.name() == "ubuntu" or linuxos.name() == "uos") then
        -- config package name for freetype on UOS
        if linuxos.name() == "uos" then
            add_requires("apt::libfreetype6-dev", {alias="freetype"})
        else
            add_requires("apt::libfreetype-dev", {alias="freetype"})
        end
    else
    -- Let xrepo manage the dependencies for macOS and other GNU/Linux distros
        add_requires("libiconv 1.17", {system=false})
        add_requires("freetype "..FREETYPE_VERSION, {system=false})
    end

    if is_plat("mingw") or is_plat("windows") then
        add_requires("qt6widgets 6.5.3")
        if is_mode("release") then
            add_requires("qtifw 4.6.0")
        end
    end

    if is_plat("linux") then
        add_requires("fontconfig", {system = true})
    end

    set_configvar("LOLLY_VERSION", LOLLY_VERSION)
    add_requires("lolly", {system=false})
    tbox_configs = {hash=true, ["force-utf8"]=true}
    if is_plat("wasm") then
        tbox_version = "dev"
    else
        tbox_version = "v" .. TBOX_VERSION
    end
    add_requireconfs("lolly.tbox", {version = tbox_version, configs=tbox_configs, system = false, override=true})
    if is_plat("macosx") or is_plat("mingw") then
        add_requireconfs("lolly.libcurl", {version = CURL_VERSION, system = false, override=true})
    end

    set_configvar("PDFHUMMUS_VERSION", PDFHUMMUS_VERSION)
    if not is_plat("wasm") then
        add_requires("pdfhummus "..PDFHUMMUS_VERSION, {system=false,configs={libpng=true,libjpeg=true}})
        add_requireconfs("pdfhummus.freetype", {version = FREETYPE_VERSION, system = false, override=true})
        add_requireconfs("pdfhummus.libpng", {version = "1.6.37", system = false, override=true})
        add_requireconfs("pdfhummus.libjpeg", {version = "v9e", system = false, override=true})
    end

    add_requires("s7 2023.04.13", {system=false})
end
