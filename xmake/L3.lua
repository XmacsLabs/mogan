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
    "src/Kernel/**.cpp",
    "src/Data/History/**.cpp",
    "src/Data/Observers/**.cpp",
    "src/Data/Scheme/**.cpp",
    "src/Data/String/**.cpp",
    "src/Data/Document/new_document.cpp",
    "src/Data/Drd/**.cpp",
    "src/Data/Tree/tree_helper.cpp",
    "src/Data/Tree/tree_label.cpp",
    "src/Data/Tree/tree_cursor.cpp",
    "src/Data/Tree/tree_observer.cpp",
    "src/Graphics/Colors/**.cpp",
    "src/Graphics/Types/**.cpp",
    "src/Scheme/L1/**.cpp",
    "src/Scheme/L2/**.cpp",
    "src/Scheme/L3/**.cpp",
    "src/Scheme/S7/**.cpp",
    "src/Scheme/Scheme/object.cpp",
    "src/System/Config/**.cpp",
    "src/System/Classes/**.cpp",
    "src/System/Files/**files.cpp",
    "src/System/Misc/data_cache.cpp",
    "src/System/Misc/persistent.cpp",
    "src/System/Misc/stack_trace.cpp",
    "src/Texmacs/Server/tm_debug.cpp",
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
    "src/Graphics/Colors",
    "src/Graphics/Types",
    "src/Graphics/Mathematics",
    "src/Scheme",
    "src/Scheme/Scheme",
    "src/Scheme/S7",
    "src/Scheme/L1",
    "src/Scheme/L2",
    "src/Scheme/L3",
    "src/System/Config",
    "src/System/Language",
    "src/System/Files",
    "src/System/Classes",
    "src/System/Misc",
    "src/Plugins",
    "src/Texmacs",
}
target("libkernel_l3") do
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)

    set_kind("static")
    set_group("kernel_l3")
    set_basename("kernel_l3")
    set_version(XMACS_VERSION, {build = "%Y-%m-%d"})

    add_packages("s7")
    add_packages("lolly")

    ---------------------------------------------------------------------------
    -- generate config files. see also:
    --    * https://github.com/xmake-io/xmake/issues/320
    --    * https://github.com/xmake-io/xmake/issues/342
    ---------------------------------------------------------------------------
    add_configfiles("src/System/config_l3.h.xmake", {
        filename = "L3/config.h",
        variables = {
            QTTEXMACS = false,
        }
    })
    add_configfiles("src/System/tm_configure_l3.hpp.xmake", {
        filename = "L3/tm_configure.hpp",
        pattern = "@(.-)@",
        variables = {
            CONFIG_USER = CONFIG_USER,
            CONFIG_OS = CONFIG_OS,
            VERSION = TEXMACS_VERSION,
            LOLLY_VERSION = LOLLY_VERSION,
        }
    })

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

