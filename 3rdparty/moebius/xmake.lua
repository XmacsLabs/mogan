set_xmakever("2.8.7")

set_allowedmodes("releasedbg", "release", "debug")
add_rules("mode.debug")

set_project("moebius")
MOEBIUS_VERSION= "0.1"

local moe_files = {
    "Data/History/**.cpp",
    "Data/String/**.cpp",
    "Data/Tree/**.cpp",
    "Kernel/Types/**.cpp",
    "Kernel/Abstractions/**.cpp",
    "Scheme/**.cpp",
    "moebius/**.cpp",
}
local moe_includedirs = {
    "Data/History",
    "Data/String",
    "Data/Tree",
    "Kernel/Types",
    "Kernel/Abstractions",
    "Scheme",
    "Scheme/L1",
    "Scheme/L2",
    "Scheme/L3",
    "Scheme/S7",
    "Scheme/Scheme",
    "$(projectdir)",
}

add_repositories("moe-repo xmake")

local DOCTEST_VERSION = "2.4.11"

add_requires("lolly")
local tbox_configs = {hash=true, ["force-utf8"]=true, charset=true}
add_requireconfs("lolly.tbox", {configs=tbox_configs, system = false, override=true})
add_requires("doctest " .. DOCTEST_VERSION, {system=false})
add_requires("nanobench", {system=false})
add_requires("s7")


target("libmoebius") do
    set_kind ("static")
    set_languages("c++17")
    set_encodings("utf-8")
    set_basename("moebius")

    add_includedirs(moe_includedirs)
    add_files(moe_files)

    add_packages("lolly")
    add_packages("s7")

    add_headerfiles("Data/History/(*.hpp)")
    add_headerfiles("Data/String/(*.hpp)")
    add_headerfiles("Data/Tree/(*.hpp)")
    add_headerfiles("Kernel/Types/(*.hpp)")
    add_headerfiles("Kernel/Abstractions/(*.hpp)")
    add_headerfiles("Scheme/(*.hpp)")
    add_headerfiles("Scheme/L1/(*.hpp)")
    add_headerfiles("Scheme/L2/(*.hpp)")
    add_headerfiles("Scheme/L3/(*.hpp)")
    add_headerfiles("Scheme/S7/(*.hpp)")
    add_headerfiles("Scheme/Scheme/(*.hpp)")
    add_headerfiles("moebius/(data/*.hpp)", {prefixdir="moebius"})
    add_headerfiles("moebius/(drd/*.hpp)", {prefixdir="moebius"})
    add_headerfiles("moebius/(*.hpp)", {prefixdir="moebius"})
end

target("tests") do
    set_kind ("binary")
    set_languages("c++17")
    set_default (false)

    add_deps("libmoebius")

    add_includedirs(moe_includedirs)
    add_includedirs("tests")

    add_packages("lolly")

    cpp_tests_on_all_plat = os.files("tests/**_test.cpp")
    for _, testfile in ipairs(cpp_tests_on_all_plat) do
        add_tests(path.basename(testfile), {
            kind = "binary",
            files = testfile,
            packages = "doctest",
            defines = "DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN"})
    end
end

target("bench_base")do
    set_kind("object")
    set_languages("c++17")
    set_default (false)
    set_policy("check.auto_ignore_flags", false)
    add_packages("nanobench")

    if is_plat("windows") then
        set_encodings("utf-8")
    end
    add_files("bench/nanobench.cpp")
end

function add_bench_target(filepath)
    local benchname = path.basename(filepath)
    target(benchname) do 
        set_group("bench")
        set_languages("c++17")
        set_default(false)
        set_policy("check.auto_ignore_flags", false)
        set_rundir("$(projectdir)")
        add_deps({"libmoebius", "bench_base"})
        add_packages({"nanobench", "lolly"})

        if is_plat("linux") then
            add_syslinks("stdc++", "m")
        end

        if is_plat("windows") then
            set_encodings("utf-8")
            add_syslinks("secur32", "shell32")
        end

        add_includedirs(moe_includedirs)
        add_files(filepath) 
    end
end

cpp_bench_on_all_plat = os.files("bench/**_bench.cpp")
for _, filepath in ipairs(cpp_bench_on_all_plat) do
    add_bench_target (filepath)
end
