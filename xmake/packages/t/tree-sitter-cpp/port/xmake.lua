--! xmake.lua for tree-sitter-cpp
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
-- @file        tree-sitter-cpp_xmake.lua
--

add_rules("mode.debug", "mode.release")
target("tree-sitter-cpp")
    set_kind("$(kind)")
    add_files("src/parser.c")
    add_files("src/scanner.c")
    add_headerfiles("bindings/c/tree-sitter-cpp.h")