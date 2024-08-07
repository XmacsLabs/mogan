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

includes("vars.lua")

--
-- Dependencies: Platform|Package Manager
--
function using_apt ()
    return linuxos.name() == "debian"
           or linuxos.name() == "ubuntu"
           or linuxos.name() == "uos"
end

function using_legacy_apt ()
    return (linuxos.name() == "uos") or (linuxos.name () == "ubuntu" and linuxos.version():major() == 20)
end

function using_pacman ()
    return linuxos.name() == "archlinux"
end

function add_requires_of_mogan()
    tbox_configs = {hash=true, ["force-utf8"]=true, charset=true}
    add_requires("tbox " .. TBOX_VERSION, {system=false, configs=tbox_configs})

    -- package: s7
    add_requires("s7 "..S7_VERSION, {system=false})
    
    -- package: tree-sitter
    add_requires("tree-sitter "..TREESITTER_VERSION, {system=false})

    -- package: tree-sitter-cpp
    add_requires("tree-sitter-cpp "..TREESITTER_CPP_VERSION, {configs = {shared = false}, system=false})

    -- package: qt6widgets
    if is_plat("mingw") or is_plat("windows") then
        add_requires("qt6widgets "..QT6_VERSION)
    end

    -- package: lolly
    set_configvar("LOLLY_VERSION", LOLLY_VERSION)
    add_requires("lolly", {system=false})
    tbox_configs = {hash=true, ["force-utf8"]=true, charset=true}
    add_requireconfs("lolly.tbox", {version = TBOX_VERSION, configs=tbox_configs, system = false, override=true})
    if is_plat("linux") and linuxos.name() == "uos" then
       add_requireconfs("lolly.cpr", {version = "1.8.3", system = false, override=true})
    else
       add_requireconfs("lolly.cpr", {version = CPR_VERSION, system = false, override=true})
    end
    add_requires("moebius", {system=false})
    add_requireconfs("moebius.lolly", {version = LOLLY_VERSION, system = false, override=true})

    -- package: libcurl
    if is_plat("linux") and using_apt() then
        add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
        add_requireconfs("lolly.cpr.libcurl", {system = true, override=true})
    elseif is_plat("linux") and using_pacman () then
        add_requires("pacman::curl", {alias="libcurl"})
        add_requireconfs("lolly.cpr.libcurl", {system = true, override=true})
    else
        add_requireconfs("lolly.cpr.libcurl", {version = CURL_VERSION, system = false, override=true})
    end

    -- package: fontconfig
    if is_plat("linux") then
        if using_legacy_apt() then
        else
            add_requires("fontconfig", {system = true})
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
    elseif is_plat("linux") and using_pacman () then
        add_requires("pacman::libpng", {alias="libpng"})
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
        if using_legacy_apt() then
            add_requires("freetype "..FREETYPE_VERSION, {system=false})
            add_requireconfs("pdfhummus.freetype", {version = FREETYPE_VERSION, system = false, override=true})
        else
            add_requires("apt::libfreetype-dev", {alias="freetype"})
        end
    elseif is_plat("linux") and using_pacman () then
        add_requires("pacman::freetype2", {alias="freetype"})
    else
    -- Let xrepo manage the dependencies for macOS and other GNU/Linux distros
        if not is_plat ("macosx") then
            add_requires("libiconv "..LIBICONV_VERSION, {system=false})
        end
        add_requires("freetype "..FREETYPE_VERSION, {system=false})
        add_requireconfs("pdfhummus.freetype", {version = FREETYPE_VERSION, system = false, override=true})
    end

    -- package: libgit2
    if is_plat("linux") then
        add_requires("libgit2", {system=true})
    elseif not is_plat("wasm") then
        add_requires("libgit2 "..LIBGIT2_VERSION)
    end

    if is_plat ("linux") and using_apt() then
        add_requires ("apt::libssl-dev", {alias="openssl"})
    end
end
