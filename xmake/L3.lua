-------------------------------------------------------------------------------
--
-- MODULE      : L3.lua
-- DESCRIPTION : Xmake config file for the L3 Kernel
-- COPYRIGHT   : (C) 2022-2024  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

includes("vars.lua")

local l3_files = {
    "$(projectdir)/src/Kernel/**.cpp",
    "$(projectdir)/src/Data/History/**.cpp",
    "$(projectdir)/src/Data/Observers/**.cpp",
    "$(projectdir)/src/Data/String/**.cpp",
    "$(projectdir)/src/Data/Convert/Generic/**.cpp",
    "$(projectdir)/src/Data/Document/new_document.cpp",
    "$(projectdir)/src/Data/Tree/tree_cursor.cpp",
    "$(projectdir)/src/Data/Tree/tree_observer.cpp",
    "$(projectdir)/src/Graphics/Colors/**.cpp",
    "$(projectdir)/src/Graphics/Types/**.cpp",
    "$(projectdir)/src/Graphics/Fonts/**.cpp",
    "$(projectdir)/src/Graphics/Bitmap_fonts/**.cpp",
    "$(projectdir)/src/Graphics/Renderer/**.cpp",
    "$(projectdir)/src/Graphics/Pictures/**.cpp",
    "$(projectdir)/src/Scheme/L2/**.cpp",
    "$(projectdir)/src/Scheme/L3/**.cpp",
    "$(projectdir)/src/System/Config/**.cpp",
    "$(projectdir)/src/System/Classes/**.cpp",
    "$(projectdir)/src/System/Files/**files.cpp",
    "$(projectdir)/src/System/Files/tm_file.cpp",
    "$(projectdir)/src/System/Link/tm_link.cpp",
    "$(projectdir)/src/System/Link/dyn_link.cpp",
    "$(projectdir)/src/System/Misc/data_cache.cpp",
    "$(projectdir)/src/System/Misc/persistent.cpp",
    "$(projectdir)/src/System/Misc/tm_sys_utils.cpp",
    "$(projectdir)/src/Texmacs/Server/tm_debug.cpp",
    "$(projectdir)/src/Plugins/Metafont/**.cpp",
    "$(projectdir)/src/Plugins/Freetype/**.cpp",
    "$(projectdir)/src/Plugins/Xml/**.cpp"
}
local l3_includedirs = {
    "src/Kernel/Types",
    "src/Kernel/Abstractions",
    "src/Data/Document",
    "src/Data/History",
    "src/Data/Observers",
    "src/Data/String",
    "src/Data/Tree",
    "src/Data/Convert",
    "src/Graphics/Bitmap_fonts",
    "src/Graphics/Colors",
    "src/Graphics/Fonts",
    "src/Graphics/Mathematics",
    "src/Graphics/Pictures",
    "src/Graphics/Renderer",
    "src/Graphics/Spacial",
    "src/Graphics/Types",
    "src/Scheme",
    "src/Scheme/Scheme",
    "src/Scheme/S7",
    "src/Scheme/L2",
    "src/Scheme/L3",
    "src/System/Config",
    "src/System/Language",
    "src/System/Link",
    "src/System/Files",
    "src/System/Classes",
    "src/System/Misc",
    "src/Plugins",
    "src/Texmacs",
    "TeXmacs/include",
}

function add_target_L3()
    ---------------------------------------------------------------------------
    -- generate config files. see also:
    --    * https://github.com/xmake-io/xmake/issues/320
    --    * https://github.com/xmake-io/xmake/issues/342
    ---------------------------------------------------------------------------
    add_configfiles("$(projectdir)/src/System/config_l3.h.xmake", {
        filename = "L3/config.h",
        variables = {
            QTTEXMACS = false,
            USE_FREETYPE = true,
            USE_FONTCONFIG = is_plat("linux") and (not using_legacy_apt()),
            APP_MOGAN_RESEARCH = true,
        }
    })
    add_configfiles("$(projectdir)/src/System/tm_configure_l3.hpp.xmake", {
        filename = "L3/tm_configure.hpp",
        variables = {
            CONFIG_USER = CONFIG_USER,
            CONFIG_OS = CONFIG_OS,
            VERSION = TEXMACS_VERSION,
            LOLLY_VERSION = LOLLY_VERSION,
            XMACS_VERSION = XMACS_VERSION,
            TEXMACS_VERSION = TEXMACS_VERSION,
            GOLDFISH_VERSION = GOLDFISH_VERSION,
        }
    })

    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)
    set_encodings("utf-8")

    set_kind("static")
    set_group("kernel_l3")
    set_basename("kernel_l3")
    set_version(XMACS_VERSION, {build = "%Y-%m-%d"})

    add_packages("s7")
    add_packages("moebius")
    add_packages("freetype")
    if is_plat("linux") then
        add_packages("fontconfig")
    end

    add_includedirs("$(buildir)/L3")
    add_includedirs("$(buildir)/glue")
    add_includedirs("$(buildir)")
    add_includedirs(l3_includedirs, {public = true})
    add_files(l3_files)

    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/L3/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/L3/tm_configure.hpp"))
    end)
end
