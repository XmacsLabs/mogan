package("lolly")
    set_homepage("https://github.com/XmacsLabs/lolly")
    set_description("Lolly is a C++ library")

    add_urls("https://github.com/XmacsLabs/lolly.git")
    add_urls("https://gitee.com/XmacsLabs/lolly.git")

    add_deps("tbox")
    if not is_plat("wasm") then
        add_deps("cpr")
        add_deps("mimalloc")
    end

    add_versions("v1.3.17", "58fac07b47f4df4a9ad8796c18f510a04afd8a97")

    on_install("linux", "macosx", "mingw", "wasm", "windows", function (package)
        local configs = {}
        if not is_plat("wasm") then
            configs.malloc = "mimalloc"
        end
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()
