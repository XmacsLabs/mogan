set_project("lolly")
set_languages("c++17")

set_allowedplats("linux", "macosx", "mingw", "wasm", "windows")

if is_plat("mingw") and is_host("windows") then
    add_requires("mingw-w64 8.1.0")
    set_toolchains("mingw@mingw-w64")
end

if is_plat("wasm") then
    add_requires("emscripten")
    set_toolchains("emcc@emscripten")
end

includes("check_cxxtypes.lua")
configvar_check_cxxtypes("HAVE_INTPTR_T", "intptr_t", {includes = {"memory"}})
configvar_check_cxxtypes("HAVE_TIME_T", "time_t", {includes = {"memory"}})

includes("check_cxxincludes.lua")
configvar_check_cxxincludes("HAVE_STDLIB_H", "stdlib.h")
configvar_check_cxxincludes("HAVE_STDINT_H", "stdint.h")
configvar_check_cxxincludes("HAVE_INTTYPES_H", "inttypes.h")

includes("check_cxxfuncs.lua")
configvar_check_cxxfuncs("HAVE_GETTIMEOFDAY", "gettimeofday", {includes={"sys/time.h"}})

includes("check_cxxsnippets.lua")
configvar_check_cxxsnippets(
    "CONFIG_LARGE_POINTER", [[
        #include <stdlib.h>
        static_assert(sizeof(void*) == 8, "");]])

--- require packages
add_requires("tbox dev", {system=false, micro=true})
add_requires("doctest 2.4.11", {system=false})
if is_plat("linux") and (linuxos.name() == "ubuntu" or linuxos.name() == "uos") then
    add_requires("apt::libcurl4-openssl-dev", {alias="libcurl"})
elseif not is_plat("wasm") then
    add_requires("libcurl 7.84.0", {system=false})
end 
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

option("posix_thread")
    set_showmenu(false)
    add_cxxtypes("std::mutex")
    add_cxxincludes("mutex")
option_end()

if is_plat("mingw", "windows") then
    add_requires("nowide_standalone 11.2.0", {system=false})
end

local lolly_files = {
    "Kernel/**/*.cpp",
    "System/**/*.cpp",
    "Data/String/**.cpp",
    "Data/Scheme/**.cpp",
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
    "Plugins/Curl",
    "Plugins/Unix",
    "Plugins",
}

target("liblolly") do
    set_kind("static")
    if is_plat("mingw") then
        set_languages("c++11")
    else
        set_languages("c++98")
    end
    set_policy("check.auto_ignore_flags", false)

    set_basename("lolly")

    --- dependent packages
    add_packages("tbox")
    if not is_plat("wasm") then
        add_packages("libcurl")
        add_includedirs("Plugins/Curl")
        add_files("Plugins/Curl/**.cpp")
    end
    if is_config("malloc", "mimalloc") then 
        add_defines("MIMALLOC")
        add_packages("mimalloc")
    elseif is_config("malloc", "jemalloc") then 
        add_defines("JEMALLOC")
        add_packages("jemalloc")
    end 

    if is_plat("mingw", "windows") then
        add_packages("nowide_standalone")
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
            }
        }
    )

    if is_plat("mingw") and (not has_config("posix_thread")) then
        add_defines("DOCTEST_CONFIG_NO_MULTITHREADING")
    end

    if is_plat("windows") then
        add_cxxflags("-FI " .. path.absolute("$(buildir)\\L1\\config.h"))
    else
        add_cxxflags("-include $(buildir)/L1/config.h")
    end
    add_headerfiles("Kernel/Abstractions/(*.hpp)")
    add_headerfiles("Kernel/Algorithms/(*.hpp)")
    add_headerfiles("Kernel/Containers/(*.hpp)")
    add_headerfiles("Kernel/Containers/(*.ipp)")
    add_headerfiles("Kernel/Types/(*.hpp)")
    add_headerfiles("System/Classes/(*.hpp)")
    add_headerfiles("System/IO/(*.hpp)")
    add_headerfiles("System/Memory/(*.hpp)")
    add_headerfiles("System/Misc/(*.hpp)")
    add_headerfiles("Data/String/(*.hpp)")
    add_headerfiles("Data/Scheme/(*.hpp)")
    add_headerfiles("Plugins/Curl/(*.hpp)", {prefixdir = "Curl"})
    add_headerfiles("Plugins/Windows/(*.hpp)", {prefixdir = "Windows"})
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

        add_packages("tbox")
        add_packages("doctest")
        if not is_plat("wasm") then
            add_packages("libcurl")
        end

        if is_plat("linux") then
            add_syslinks("stdc++", "m")
        end

        if is_plat("windows") then
            add_ldflags("/LTCG")
        end

        if is_plat("windows") or is_plat("mingw") then
            add_syslinks("secur32")
        end

        add_includedirs("$(buildir)/L1")
        add_includedirs(lolly_includedirs)
        add_files(filepath) 

        if is_plat("windows") then
            add_cxxflags("-FI " .. path.absolute("$(buildir)\\L1\\config.h"))
        else
            add_cxxflags("-include $(buildir)/L1/config.h")
        end

        if is_plat("wasm") then
            add_cxxflags("-s DISABLE_EXCEPTION_CATCHING=0")
            add_ldflags("-s DISABLE_EXCEPTION_CATCHING=0")
            on_run(function (target)
                node = os.getenv("EMSDK_NODE")
                cmd = node .. " $(buildir)/wasm/wasm32/$(mode)/" .. testname .. ".js"
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

for _, filepath in ipairs(os.files("tests/Data/**_test.cpp")) do
    add_test_target(filepath)
end

for _, filepath in ipairs(os.files("tests/Kernel/**_test.cpp")) do
    add_test_target(filepath)
end

for _, filepath in ipairs(os.files("tests/System/**_test.cpp")) do
    add_test_target(filepath)
end

for _, filepath in ipairs(os.files("tests/Plugins/Curl/**_test.cpp")) do
    if not is_plat("wasm") then
        add_test_target(filepath)
    end
end

-- xmake plugin
add_configfiles(
    "Doxyfile.in", {
        filename = "doxyfile",
        pattern = "@(.-)@",
        variables = {
            PACKAGE = "Lolly",
            DOXYGEN_DIR = get_config("buildir"),
            DEVEL_VERSION = DEVEL_VERSION,
        }
    }
)

--- debug mode
if is_mode("profile") then
    set_symbols("debug")
    add_cxflags("-pg")
    add_ldflags("-pg")
end
