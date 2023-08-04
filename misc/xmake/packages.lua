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

-- https://xmake.io/#/manual/package_dependencies?id=inherit-package-configuration
package("lolly")
    set_homepage("https://github.com/XmacsLabs/lolly")
    set_description("Lolly is a C++ library")

    add_urls("https://github.com/XmacsLabs/lolly.git")
    add_urls("https://gitee.com/XmacsLabs/lolly.git")

    add_versions("v0.99.6", "3fb04e4c1c9b24cd251b64224ba61cb63aa31c7d")

    on_install("linux", "macosx", "mingw", "wasm", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()


function add_requires_of_mogan()
    if is_plat("linux") and (linuxos.name() == "ubuntu" or linuxos.name() == "uos") then
        add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
        add_requires("apt::libpng-dev", {alias="libpng"})
        add_requires("apt::zlib1g-dev", {alias="zlib"})
        -- config package name for libjpeg on Ubuntu
        if linuxos.name() == "ubuntu" then
            add_requires("apt::libjpeg-turbo8-dev", {alias="libjpeg"})
        else
            add_requires("apt::libjpeg62-turbo-dev", {alias="libjpeg"})
        end
        -- config package name for freetype on UOS
        if linuxos.name() == "uos" then
            add_requires("apt::libfreetype6-dev", {alias="freetype"})
        else
            add_requires("apt::libfreetype-dev", {alias="freetype"})
        end
    else
    -- Let xrepo manage the dependencies for macOS and other GNU/Linux distros
        add_requires("libpng 1.6.37", {system=false})
        add_requires("libiconv 1.17", {system=false})
        add_requires("zlib 1.2.12", {system=false})
        add_requires("libjpeg v9e", {system=false})
        add_requires("libcurl 7.84.0", {system=false})
        add_requires("freetype 2.12.1", {system=false})
    end

    if is_plat("mingw") then
        add_requires("nowide_standalone 11.2.0", {system=false})
        add_requires("qt5widgets 5.15.2")
        if is_mode("release") then
            add_requires("qtifw 4.6.0")
        end
    end

    if is_plat("linux") then
        add_requires("fontconfig", {system = true})
    end

    local PDFHUMMUS_VERSION = "4.5.10"
    set_configvar("PDFHUMMUS_VERSION", PDFHUMMUS_VERSION)
    add_requires("pdfhummus "..PDFHUMMUS_VERSION, {system=false,configs={libpng=true,libjpeg=true}})
    add_requires("s7 2023.04.13", {system=false})
    add_requires("lolly", {system=false})
end
