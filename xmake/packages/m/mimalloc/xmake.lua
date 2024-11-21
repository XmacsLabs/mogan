--! package mimalloc
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
-- @author      maximegmd, yecate, waruqi, Yamashi, Force67, heheda123123, Darcy Shen
-- @file        mimalloc.lua
--

package("mimalloc")

    set_homepage("https://github.com/microsoft/mimalloc")
    set_description("mimalloc (pronounced 'me-malloc') is a general purpose allocator with excellent performance characteristics.")
    set_license("MIT")

    add_urls("https://github.com/microsoft/mimalloc/archive/v$(version).zip", {alias="archive"})
    add_urls("https://github.com/microsoft/mimalloc.git", {alias="git"})
    add_urls("https://gitee.com/mirrors/mimalloc.git", {alias="git"})

    add_versions("archive:2.1.2", "86281c918921c1007945a8a31e5ad6ae9af77e510abfec20d000dd05d15123c7")
    add_versions("git:2.1.2", "v2.1.2")

    if is_plat("linux") then
        add_extsources("pacman::mimalloc", "apt::libmimalloc-dev")
        if linuxos.name() == "fedora" then
            add_extsources("pkgconfig::libmimalloc")
        end
    end

    add_configs("secure", {description = "Use a secured version of mimalloc", default = false, type = "boolean"})
    add_configs("rltgenrandom", {description = "Use a RtlGenRandom instead of BCrypt", default = false, type = "boolean"})

    add_deps("cmake")

    if is_plat("windows") then
        add_syslinks("advapi32", "bcrypt")
    elseif is_plat("linux") then
        add_syslinks("pthread")
    elseif is_plat("android") then
        add_syslinks("atomic")
    end

    on_install("macosx", "windows", "linux", "android", "mingw", function (package)
        local configs = {"-DMI_OVERRIDE=OFF"}
        table.insert(configs, "-DMI_BUILD_STATIC=" .. (package:config("shared") and "OFF" or "ON"))
        table.insert(configs, "-DMI_BUILD_SHARED=" .. (package:config("shared") and "ON" or "OFF"))
        table.insert(configs, "-DMI_SECURE=" .. (package:config("secure") and "ON" or "OFF"))
        table.insert(configs, "-DMI_BUILD_TESTS=OFF")
        table.insert(configs, "-DMI_BUILD_OBJECT=OFF")
        --x64:mimalloc-redirect.lib/dll x86:mimalloc-redirect32.lib/dll
        if package:version():le("2.0.1") and package:config("shared") and package:is_plat("windows") and package:is_arch("x86") then
            io.replace("CMakeLists.txt", "-redirect.", "-redirect32.", {plain = true})
        end
        local cxflags
        if package:config("rltgenrandom") then
            if xmake:version():ge("2.5.1") then
                cxflags = "-DMI_USE_RTLGENRANDOM"
            else
                -- it will be deprecated after xmake/v2.5.1
                package:configs().cxflags = "-DMI_USE_RTLGENRANDOM"
            end
        end
        import("package.tools.cmake").build(package, configs, {buildir = "build", cxflags = cxflags})

        if package:is_plat("windows") then
            os.trycp("build/**.dll", package:installdir("bin"))
            os.trycp("build/**.lib", package:installdir("lib"))
        elseif package:is_plat("mingw") then
            os.trycp("build/**.dll", package:installdir("bin"))
            os.trycp("build/**.a", package:installdir("lib"))
        else
            os.trycp("build/*.so", package:installdir("bin"))
            os.trycp("build/*.so", package:installdir("lib"))
            os.trycp("build/*.a", package:installdir("lib"))
        end
        os.cp("include", package:installdir())
    end)

    on_test(function (package)
        assert(package:has_cfuncs("mi_malloc", {includes = "mimalloc.h"}))
    end)
