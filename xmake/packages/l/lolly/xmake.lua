package("lolly")
    set_homepage("https://github.com/XmacsLabs/lolly")
    set_description("Lolly is a C++ library")

    set_sourcedir(path.join(os.scriptdir(), "../../../../3rdparty/lolly"))
    if not is_plat("wasm") then
        add_deps("libcurl", "tbox", "cpr")
    end

    on_install("linux", "macosx", "windows", "wasm", function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"
        end
        import("package.tools.xmake").install(package, configs)
    end)
package_end()
