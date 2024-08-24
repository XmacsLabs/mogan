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

    add_urls("https://github.com/6cdh/tree-sitter-scheme/archive/refs/tags/v$(version).zip")

    add_versions("0.6.0", "3a177fa9189c78ebc37f3764e79bfae230db4c4a23ebe591f8df751d18df9864")

    on_install(function(package)
        os.cp(path.join(package:scriptdir(), "port", "xmake.lua"), "xmake.lua")

        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end

            local header_file = path.join("bindings", "tree-sitter-scheme.h")
        if not os.isfile(header_file) then
            io.writefile(header_file, [[
                #ifndef TREE_SITTER_SCHEME_H_
                #define TREE_SITTER_SCHEME_H_

                typedef struct TSLanguage TSLanguage;

                #ifdef __cplusplus
                extern "C" {
                #endif

                const TSLanguage *tree_sitter_scheme(void);

                #ifdef __cplusplus
                }
                #endif

                #endif // TREE_SITTER_SCHEME_H_
            ]])
        end
        import("package.tools.xmake").install(package, configs)
    end)

    on_test(function (package)
        assert(package:has_cfuncs("tree_sitter_scheme", {includes = "tree-sitter-scheme.h"}))
    end) 
