--! package tree-sitter-scheme
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
-- Copyright (C) 2024-present, TBOOX Open Source Group.
--
-- @author      UnbSky
-- @file        tree-sitter-scheme.lua
--

package("tree-sitter-scheme")
    set_homepage("https://tree-sitter.github.io/")
    set_description("Scheme grammar for tree-sitter.")

    add_urls("https://github.com/UnbSky/tree-sitter-scheme/archive/refs/tags/v$(version).zip")

    add_versions("0.6.1", "9724cfb9a12289cea6d33a918202f6f27563acbe35942ecb677a2a9e2999b2c6")

    on_install(function(package)
        os.cp(path.join(package:scriptdir(), "port", "xmake.lua"), "xmake.lua")

        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)

    on_test(function (package)
        assert(package:has_cfuncs("tree_sitter_scheme", {includes = "tree-sitter-scheme.h"}))
    end) 
