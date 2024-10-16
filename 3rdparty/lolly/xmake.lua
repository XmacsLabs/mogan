set_xmakever("2.8.5")

-- add releasedbg, debug and release modes.
set_allowedmodes("releasedbg", "release", "debug")
add_rules("mode.debug")

set_project("lolly")
LOLLY_VERSION= "1.3.1"

set_languages("c++17")
includes("@builtin/check")

set_allowedplats("linux", "macosx", "mingw", "wasm", "windows")

if is_plat("mingw") and is_host("windows") then
    add_requires("mingw-w64 8.1.0")
    set_toolchains("mingw@mingw-w64")
end

if is_plat("wasm") then
    add_requires("emscripten 3.1.25")
    set_toolchains("emcc@emscripten")
end

--- require packages
tbox_configs = {hash=true, ["force-utf8"]=true, charset=true}
add_requires("tbox 1.7.5", {system=false, configs=tbox_configs})
add_requires("doctest 2.4.11", {system=false})
option("malloc")
    set_default("standard")
    set_showmenu(true)
    set_description("Enable mimalloc or jemalloc library")
    set_values("standard", "mimalloc", "jemalloc")
option_end()
if is_config("malloc", "mimalloc") then 
    add_requires("mimalloc 2.1.2")
elseif is_config("malloc", "jemalloc") then 
    add_requires("jemalloc 5.3.0", {system=false, configs={envs={LD_PRELOAD="`jemalloc-config --libdir`/libjemalloc.so.`jemalloc-config --revision`" }}})
end

if not is_plat("wasm") then
    add_requires("cpr 1.10.5")
end


option("posix_thread")
    set_showmenu(false)
    add_cxxtypes("std::mutex")
    add_cxxincludes("mutex")
option_end()

function my_configvar_check()
    on_config(function (target)
        if target:has_cxxtypes("intptr_t", {includes = "memory"}) then
            target:set("configvar", "HAVE_INTPTR_T", 1)
        end
        if target:has_cxxincludes("stdlib.h") then
            target:set("configvar", "HAVE_STDLIB_H", 1)
        end
        if target:has_cxxincludes("stdint.h") then
            target:set("configvar", "HAVE_STDINT_H", 1)
        end
        if target:has_cxxincludes("inttypes.h") then
            target:set("configvar", "HAVE_INTTYPES_H", 1)
        end
    end)
end

local lolly_files = {
    "Kernel/**/*.cpp",
    "System/**/*.cpp",
    "Data/String/**.cpp",
    "Data/Scheme/**.cpp",
    "lolly/**/**.cpp",
}
local lolly_includedirs = {
    "Kernel/Abstractions",
    "Kernel/Algorithms",
    "Kernel/Containers",
    "Kernel/Types",
    "Data/String",
    "Data/Scheme",
    "System/Classes",
    "System/Files",
    "System/IO",
    "System/Memory",
    "System/Misc",
    "Plugins/Unix",
    "Plugins",
    "$(projectdir)"
}

target("liblolly") do
    set_kind("static")
    set_languages("c++17")
    set_policy("check.auto_ignore_flags", false)
    set_encodings("utf-8")
    my_configvar_check()

    set_basename("lolly")

    --- dependent packages
    add_packages("tbox")
    if is_config("malloc", "mimalloc") then 
        add_defines("MIMALLOC")
        add_packages("mimalloc")
    elseif is_config("malloc", "jemalloc") then 
        add_defines("JEMALLOC")
        add_packages("jemalloc")
    end 
    if not is_plat("wasm") then
        add_packages("cpr")
    end

    if is_plat("mingw", "windows") then 
        add_includedirs("Plugins/Windows")
        add_files("Plugins/Windows/win_*.cpp")
    end 

    if is_plat("mingw") then
        add_files("Plugins/Windows/spawn.cpp")
    end

    if is_plat("linux") or is_plat("macosx") then
        add_includedirs("Plugins/Unix")
        add_files("Plugins/Unix/**.cpp")
    end

    add_configfiles(
        "System/config_l1.h.xmake", {
            filename = "L1/config.h",
            variables = {
                OS_MINGW = is_plat("mingw"),
                OS_WIN = is_plat("windows"),
                OS_MACOS = is_plat("macosx"),
                OS_WASM = is_plat("wasm"),
                OS_LINUX = is_plat("linux"),
            }
        }
    )

    if is_plat("mingw") and (not has_config("posix_thread")) then
        add_defines("DOCTEST_CONFIG_NO_MULTITHREADING")
    end

    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/L1/config.h"))
    end)

    add_headerfiles("Kernel/Abstractions/(*.hpp)")
    add_headerfiles("Kernel/Algorithms/(*.hpp)")
    add_headerfiles("Kernel/Containers/(*.hpp)")
    add_headerfiles("Kernel/Containers/(*.ipp)")
    add_headerfiles("Kernel/Types/(*.hpp)")
    add_headerfiles("System/Classes/(*.hpp)")
    add_headerfiles("System/Files/(*.hpp)")
    add_headerfiles("System/IO/(*.hpp)")
    add_headerfiles("System/Memory/(*.hpp)")
    add_headerfiles("System/Misc/(*.hpp)")
    add_headerfiles("System/Language/(*.hpp)")
    add_headerfiles("Data/String/(*.hpp)")
    add_headerfiles("Data/Scheme/(*.hpp)")
    add_headerfiles("Plugins/Windows/(*.hpp)", {prefixdir = "Windows"})
    add_headerfiles("lolly/(data/*.hpp)", {prefixdir="lolly"})
    add_headerfiles("lolly/(io/*.hpp)", {prefixdir = "lolly"})
    add_headerfiles("lolly/(system/*.hpp)", {prefixdir = "lolly"})
    add_includedirs(lolly_includedirs)
    add_files(lolly_files)
end

local mingw_copied = false 

function add_test_target(filepath)
    local testname = path.basename(filepath)
    target(testname) do 
        set_group("tests")
        add_deps("liblolly")
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)

        if is_plat("mingw") then
            add_packages("mingw-w64")
        end
        add_packages("tbox")
        add_packages("doctest")

        if is_plat("linux") then
            add_syslinks("stdc++", "m")
        end

        if is_plat("windows") then
            set_encodings("utf-8")
            add_ldflags("/LTCG")
        end

        if is_plat("windows") or is_plat("mingw") then
            add_syslinks("secur32")
        end

        add_includedirs("$(buildir)/L1")
        add_includedirs(lolly_includedirs)
        add_includedirs("tests")
        add_forceincludes(path.absolute("$(buildir)/L1/config.h"))
        add_files(filepath) 

        if is_plat("wasm") then
            add_cxxflags("-s DISABLE_EXCEPTION_CATCHING=0")
            add_ldflags("--preload-file xmake.lua")
            add_ldflags("--preload-file tests")
            add_ldflags("-s DISABLE_EXCEPTION_CATCHING=0")
            on_run(function (target)
                node = os.getenv("EMSDK_NODE")
                os.cd("$(buildir)/wasm/wasm32/$(mode)/")
                print("> cd $(buildir)/wasm/wasm32/$(mode)/")
                cmd = node .. " " .. testname .. ".js"
                print("> " .. cmd)
                os.exec(cmd)
            end)
        end

        if is_plat("linux", "macosx") then
            on_run(function (target)
                cmd = "$(buildir)/$(plat)/$(arch)/$(mode)/" .. testname
                print("> " .. cmd)
                os.exec(cmd)
            end)
        end

        if is_plat("windows") or (is_plat ("mingw") and is_host ("windows")) then
            on_run(function (target)
                cmd = "$(buildir)/$(plat)/$(arch)/$(mode)/" .. testname .. ".exe"
                print("> " .. cmd)
                os.exec(cmd)
            end)
        end

        if is_plat("mingw") and is_host("linux") then
            on_run(function (target)
                cmd = "wine $(buildir)/mingw/x86_64/$(mode)/" .. testname .. ".exe"
                print("> " .. cmd)
                if not mingw_copied then
                    mingw_copied = true
                    os.cp("/usr/x86_64-w64-mingw32/lib/libwinpthread-1.dll", "$(buildir)/mingw/x86_64/$(mode)/")
                    os.cp("/usr/lib/gcc/x86_64-w64-mingw32/10-win32/libgcc_s_seh-1.dll", "$(buildir)/mingw/x86_64/$(mode)/")
                    os.cp("/usr/lib/gcc/x86_64-w64-mingw32/10-win32/libstdc++-6.dll", "$(buildir)/mingw/x86_64/$(mode)/")
                end
                os.exec(cmd)
            end)
        end
    end
end


cpp_tests_on_all_plat = os.files("tests/**_test.cpp")
for _, filepath in ipairs(cpp_tests_on_all_plat) do
    add_test_target (filepath)
end


-- xmake plugin
add_configfiles(
    "Doxyfile.in", {
        filename = "../doxyfile",
        pattern = "@(.-)@",
        variables = {
            PACKAGE = "Lolly",
            LOLLY_VERSION = LOLLY_VERSION,
            DOXYGEN_DIR = get_config("buildir"),
            DEVEL_VERSION = DEVEL_VERSION,
            HTML_EXTRA_STYLESHEET = "doxygen-awesome-css/doxygen-awesome.css",
        }
    }
)

---
--- coverage:
--- use `xmake f -m coverage` to enable coverage
--- first `rm -rf build/` to clean build cache
--- then `xmake build` to build
--- then `xmake run --group=test_cov` to run tests for coverage
--- run `lcov --directory . --capture --output-file coverage.info`
--- run `genhtml coverage.info --output-directory coverage`
--- open `coverage/index.html` in browser
--- 

function add_test_cov(filepath)
    local testname = path.basename(filepath)
    target(testname) do
        set_group("test_cov")
        add_deps("liblolly")
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)

        add_packages("tbox")
        add_packages("doctest")
        add_packages("libcurl")
        add_syslinks("stdc++", "m")
        add_includedirs("$(buildir)/L1")
        add_includedirs(lolly_includedirs)
        add_files(filepath) 
        add_cxxflags("-include $(buildir)/L1/config.h")
        add_cxxflags("-O0")
        add_cxxflags("-fprofile-arcs")
        add_cxxflags("-ftest-coverage")
        add_ldflags("-coverage")
        on_run(function (target)
            cmd = "$(buildir)/$(plat)/$(arch)/$(mode)/" .. testname
            print("> " .. cmd)
            os.exec(cmd)
        end)
        
    end
end

for _, filepath in ipairs(os.files("tests/**_test.cpp")) do
    if is_mode("coverage") then
        add_test_cov(filepath)
    end
end

---
--- coverage end
---

--- debug mode
if is_mode("profile") then
    set_symbols("debug")
    add_cxflags("-pg")
    add_ldflags("-pg")
end
