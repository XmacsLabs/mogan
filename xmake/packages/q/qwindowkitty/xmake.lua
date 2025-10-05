package("qwindowkitty")
    set_homepage("https://github.com/stdware/qwindowkit")
    set_description("Cross-platform frameless window framework for Qt applications")
    set_license("Apache-2.0")

    set_sourcedir(path.join(os.scriptdir(), "../../../../3rdparty/qwindowkitty"))

    -- Add configuration options
    add_configs("style_agent", {description = "Enable Style Agent", default = false, type = "boolean"})
    add_configs("windows_system_borders", {description = "Enable Windows System Borders", default = false, type = "boolean"})

    on_load(function (package)
        -- Set library names based on configuration
        package:add("links", "QWKCore")
        package:add("links", "QWKWidgets")
    end)

    on_install("linux", "macosx", "windows", function (package)
        local configs = {}
        
        -- Set build type
        if package:config("shared") then
            configs.build_static = false
        else
            configs.build_static = true
        end
        
        -- Configure optional features
        configs.style_agent = package:config("style_agent")
        configs.windows_system_borders = package:config("windows_system_borders")

        -- Import and install
        import("package.tools.xmake").install(package, configs)
    end)
package_end()
