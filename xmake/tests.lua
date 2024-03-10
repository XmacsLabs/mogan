function add_target_cpp_test(filepath, dep)
    local testname = path.basename(filepath)
    target(testname) do
        set_enabled(not is_plat("wasm"))
        add_runenvs("TEXMACS_PATH", path.join(os.projectdir(), "TeXmacs"))
        if dep == "libkernel_l3" then
            set_group("kernel_l3_tests")
        else
            set_group("tests")
        end
        add_deps(dep)
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)
        set_encodings("utf-8") -- eliminate warning C4819 on msvc
        if is_plat("windows") then
            add_ldflags("/LTCG")
            set_runtimes("MT")
        end
        if is_plat("windows", "mingw") then
            add_syslinks("secur32")
        end
        add_rules("qt.console")
        add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg", "QtTest")
        if not is_plat("windows") then
            add_syslinks("pthread")
        end
        if is_plat ("mingw") then
            add_packages("mingw-w64")
        end
        add_packages("s7")
        add_packages("moebius")
        add_packages("pdfhummus")

        add_includedirs({"$(buildir)", "tests/Base"})
        add_includedirs(libmogan_headers)
        build_glue_on_config()
        add_files("tests/Base/base.cpp")
        add_files(filepath)
        add_files(filepath, {rules = "qt.moc"})
        before_build(function (target)
            target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        end)

        if is_plat("wasm") then
            on_run(function (target)
                node = os.getenv("EMSDK_NODE")
                cmd = node .. " $(buildir)/wasm/wasm32/$(mode)/" .. testname .. ".js"
                print("> " .. cmd)
                os.exec(cmd)
            end)
        end
    end
end

function add_target_cpp_bench(filepath, dep)
    local testname = path.basename(filepath)
    target(testname) do
        set_enabled(not is_plat("wasm"))
        add_runenvs("TEXMACS_PATH", path.join(os.projectdir(), "TeXmacs"))
        set_group("bench")
        add_deps(dep)
        set_languages("c++17")
        set_policy("check.auto_ignore_flags", false)
        set_encodings("utf-8") -- eliminate warning C4819 on msvc
        if is_plat("windows") then
            add_ldflags("/LTCG")
            set_runtimes("MT")
        end
        if is_plat("windows", "mingw") then
            add_syslinks("secur32")
        end
        add_rules("qt.console")
        add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg", "QtTest")
        if not is_plat("windows") then
            add_syslinks("pthread")
        end
        if is_plat ("mingw") then
            add_packages("mingw-w64")
        end
        add_packages("s7")
        add_packages("lolly")
        add_packages("moebius")
        add_packages("pdfhummus")

        add_includedirs(libmogan_headers)
        add_includedirs("tests/Base")
        add_files(filepath)
        add_files(filepath, {rules = "qt.moc"})
        add_files("tests/Base/base.cpp")
        before_build(function (target)
            target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        end)

        if is_plat("wasm") then
            on_run(function (target)
                node = os.getenv("EMSDK_NODE")
                cmd = node .. " $(buildir)/wasm/wasm32/$(mode)/" .. testname .. ".js"
                print("> " .. cmd)
                os.exec(cmd)
            end)
        end
    end
end

function add_target_scheme_test(filepath, INSTALL_DIR, RUN_ENVS)
    local testname = path.basename(filepath)
    target(testname) do
        set_enabled(not is_plat("wasm"))
        set_kind("phony")
        set_group("scheme_tests")
        add_deps("research")
        on_run(function (target)
            name = target:name()
            regtest_name = "(regtest-"..string.sub(name, 1, -6)..")"
            print("------------------------------------------------------")
            print("Executing: " .. regtest_name)
            params = {
                "-headless",
                "-b", filepath,
                "-x", regtest_name,
                "-q"
            }
            if is_plat("macosx", "linux") then
                binary = target:deps()["research"]:targetfile()
            elseif is_plat("mingw", "windows") then
                binary = path.join(INSTALL_DIR,"bin","MoganResearch.exe")
            else
                print("Unsupported plat $(plat)")
            end
            cmd = binary
            if is_plat("macosx", "linux") then
                os.execv(cmd, params, {envs=RUN_ENVS})
            else
                os.execv(cmd, params)
            end
        end)
    end
end

function add_target_integration_test(filepath, INSTALL_DIR, RUN_ENVS)
    local testname = path.basename(filepath)
    target(testname) do
        set_enabled(not is_plat("wasm"))
        set_kind("phony")
        set_group("integration_tests")
        add_deps("research")
        on_run(function (target)
            name = target:name()
            test_name = "(test_"..name..")"
            print("------------------------------------------------------")
            print("Executing: " .. test_name)
            params = {
                "-headless",
                "-b", path.join("TeXmacs","tests",name..".scm"),
                "-x", test_name,
                "-q"
            }
            if is_plat("macosx", "linux") then
                binary = target:deps()["research"]:targetfile()
            elseif is_plat("mingw", "windows") then
                binary = path.join(INSTALL_DIR,"bin","MoganResearch.exe")
            else
                print("Unsupported plat $(plat)")
            end
            cmd = binary
            if is_plat("macosx", "linux") then
                os.execv(cmd, params, {envs=RUN_ENVS})
            else
                os.execv(cmd, params)
            end
        end)
    end
end
