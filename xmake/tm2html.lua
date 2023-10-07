-------------------------------------------------------------------------------
--
-- MODULE      : tm2html.lua
-- DESCRIPTION : Xmake config file for tm2html
-- COPYRIGHT   : (C) 2022-2023  Pluto Ye
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function add_target_tm2html()
    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")
    add_packages("lolly")
    add_deps("libmogan")
    add_includedirs({
        "$(buildir)",
    })
    -- install man.1 manual file
    add_configfiles("(misc/man/texmacs.1.in)", {
        filename = "texmacs.1",
        pattern = "@([^\n]-)@",
    })

    --- platform check
    if is_plat("mingw", "windows") then
        set_filename("tm2html.exe")
        add_packages("qt5widgets")
    else
        set_filename("tm2html")
    end

    if is_plat("windows") then
        set_optimize("smallest");
    end

    if is_mode("debug", "releasedbg") and is_plat("mingw", "windows") then
        add_rules("qt.console")
    else
        add_rules("qt.widgetapp")
    end

    if is_plat("macosx") then
        add_frameworks("QtMacExtras")
    end

    if is_plat("linux") then
        add_rpathdirs("@executable_path/../lib")
    end
    if not is_plat("windows") then
        add_syslinks("pthread")
    end

    if is_plat("mingw") and is_mode("release") then
        add_deps("windows_icon")
    end

    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/tm_configure.hpp"))
    end)
    -- add_files("src/Mogan/Research/research.cpp")
    add_files("src/Mogan/Command/tm2html.cpp")
end
