--! package tree-sitter-cpp
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
-- Copyright (C) 2020-present, TBOOX Open Source Group.
--
-- @author      UnbSky
-- @file        tree-sitter-cpp.lua
--

package("tree-sitter-cpp")
    set_homepage("https://tree-sitter.github.io/")
    set_description("C++ grammar for tree-sitter.")

    add_urls("https://github.com/tree-sitter/tree-sitter-cpp/archive/refs/tags/v$(version).zip")

    add_versions("0.22.2", "082dc8bd6d9cf721caf9be55cca478a07c1de5564f3a96a7c0f6b35c1e9faa9e")

    on_install(function(package)
        os.cp(path.join(package:scriptdir(), "port", "xmake.lua"), "xmake.lua")

        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)