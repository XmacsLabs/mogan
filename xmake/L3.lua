-------------------------------------------------------------------------------
--
-- MODULE      : L3.lua
-- DESCRIPTION : Xmake config file for the L3 Kernel
-- COPYRIGHT   : (C) 2022-2023  Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

local l3_files = {
    "$(projectdir)/src/Kernel/**.cpp",
    "$(projectdir)/src/Data/History/**.cpp",
    "$(projectdir)/src/Data/Observers/**.cpp",
    "$(projectdir)/src/Data/Scheme/**.cpp",
    "$(projectdir)/src/Data/String/**.cpp",
    "$(projectdir)/src/Data/Convert/Generic/**.cpp",
    "$(projectdir)/src/Data/Document/new_document.cpp",
    "$(projectdir)/src/Data/Drd/**.cpp",
    "$(projectdir)/src/Data/Tree/tree_helper.cpp",
    "$(projectdir)/src/Data/Tree/tree_label.cpp",
    "$(projectdir)/src/Data/Tree/tree_cursor.cpp",
    "$(projectdir)/src/Data/Tree/tree_observer.cpp",
    "$(projectdir)/src/Graphics/Colors/**.cpp",
    "$(projectdir)/src/Graphics/Types/**.cpp",
    "$(projectdir)/src/Graphics/Fonts/**.cpp",
    "$(projectdir)/src/Graphics/Bitmap_fonts/**.cpp",
    "$(projectdir)/src/Graphics/Renderer/**.cpp",
    "$(projectdir)/src/Graphics/Pictures/**.cpp",
    "$(projectdir)/src/Scheme/L1/**.cpp",
    "$(projectdir)/src/Scheme/L2/**.cpp",
    "$(projectdir)/src/Scheme/L3/**.cpp",
    "$(projectdir)/src/Scheme/S7/**.cpp",
    "$(projectdir)/src/Scheme/Scheme/object.cpp",
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
    "src/Data/Drd",
    "src/Data/Document",
    "src/Data/History",
    "src/Data/Observers",
    "src/Data/Scheme",
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
    "src/Scheme/L1",
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
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)

    set_kind("static")
    set_group("kernel_l3")
    set_basename("kernel_l3")
    set_version(XMACS_VERSION, {build = "%Y-%m-%d"})

    add_packages("s7")
    add_packages("lolly")
    add_packages("freetype")

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
