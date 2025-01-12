--! xmake.lua for s7
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
-- @author      jinkaimori, Darcy Shen
-- @file        s7_xmake.lua
--

add_rules("mode.release", "mode.debug")

option("gmp", {default = false, defines = "WITH_GMP"})

if has_config("gmp") then
    add_requires("gmp")
end

target("libs7") do
    set_kind("$(kind)")
    add_defines("WITH_SYSTEM_EXTRAS=0")
    add_defines("HAVE_OVERFLOW_CHECKS=0")
    add_defines("WITH_WARNINGS")
    set_basename("s7")
    add_files("s7.c")
    add_headerfiles("s7.h")
    add_includedirs(".", {public = true})
    add_options("gmp")
    if is_plat("windows") then
        set_optimize("faster")
        set_languages("c11")
        add_cxxflags("/fp:precise")
    end
    add_packages("gmp")
    if is_mode("debug") then
        add_defines("S7_DEBUGGING")
    end
end
