--! package s7
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
-- @author      jinkaimori, Darcy Shen
-- @file        s7.lua
--

package("s7")

    set_homepage("https://ccrma.stanford.edu/software/snd/snd/s7.html")
    set_description("s7 is a Scheme interpreter intended as an extension language for other applications.")

    add_urls("https://github.com/XmacsLabs/s7.git")
    add_urls("https://gitee.com/XmacsLabs/s7.git")

    add_versions("20230516", "20240516")

    add_configs("gmp", {description = "enable gmp support", default = false, type = "boolean"})

    on_load(function (package)
        package:addenv("PATH", "bin")
        if package:config("gmp") then
            package:add("deps", "gmp")
        end
    end)

    if is_plat("linux") then
        add_syslinks("pthread", "dl", "m")
    end

    on_install("bsd", "cross", "cygwin", "linux", "macosx", "mingw", "msys", "wasm", "windows", function (package)
        os.cp(path.join(package:scriptdir(), "port", "xmake.lua"), "xmake.lua")
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)

    on_test(function(package)
        if not package:is_cross() then
            local file = os.tmpfile() .. ".scm"
            io.writefile(file, [[
                (display "Hello World!")
            ]])
            os.vrunv("s7", {file})
        end
        assert(package:check_csnippets([[
            static s7_pointer old_add;           /* the original "+" function for non-string cases */
            static s7_pointer old_string_append; /* same, for "string-append" */

            static s7_pointer our_add(s7_scheme *sc, s7_pointer args)
            {
                /* this will replace the built-in "+" operator, extending it to include strings:
                *   (+ "hi" "ho") -> "hiho" and  (+ 3 4) -> 7
                */
                if ((s7_is_pair(args)) &&
                    (s7_is_string(s7_car(args))))
                    return(s7_apply_function(sc, old_string_append, args));
                return(s7_apply_function(sc, old_add, args));
            }
        ]], {includes = "s7.h"}))
    end)