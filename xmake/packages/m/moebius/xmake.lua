--! package moebius
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

package("moebius")
    set_homepage("https://github.com/XmacsLabs/moebius")
    set_description("Lolly is a C++ library")

    add_urls("https://github.com/XmacsLabs/moebius.git")
    add_urls("https://gitee.com/XmacsLabs/moebius.git")
    add_versions("0.1.21", "v0.1.21")

    add_deps("lolly")

    on_install("linux", "macosx", "mingw", "wasm", "windows", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()

