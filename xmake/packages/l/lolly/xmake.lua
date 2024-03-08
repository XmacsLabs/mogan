--! package lolly
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
-- Copyright (C) 2023-present, TBOOX Open Source Group.
--
-- @author      Charonxin, Darcy Shen
-- @file        lolly.lua
--

package("lolly")
    set_homepage("https://github.com/XmacsLabs/lolly")
    set_description("Lolly is a C++ library")

    add_urls("https://github.com/XmacsLabs/lolly.git")
    add_urls("https://gitee.com/XmacsLabs/lolly.git")
    add_versions("1.3.17", "v1.3.17")
    add_versions("1.3.18", "v1.3.18")
    add_versions("1.3.19", "v1.3.19")
    add_versions("1.3.20", "v1.3.20")
    add_versions("1.3.21", "v1.3.21")
    add_versions("1.3.22", "v1.3.22")
    add_versions("1.3.23", "v1.3.23")
    add_versions("1.3.24", "v1.3.24")
    add_versions("1.3.25", "v1.3.25")
    add_versions("1.4.0", "v1.4.0")

    add_deps("tbox")
    if not is_plat("wasm") then
        add_deps("cpr")
        add_deps("mimalloc")
    end


    on_install("linux", "macosx", "mingw", "wasm", "windows", function (package)
        local configs = {}
        if not is_plat("wasm") then
            configs.malloc = "mimalloc"
        end
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()
