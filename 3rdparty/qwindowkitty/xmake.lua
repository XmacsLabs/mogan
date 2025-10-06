set_project("QWindowKit")
set_version("1.0.0")

-- Set languages and policies
set_languages("c++17")
add_rules("mode.debug", "mode.release")

-- Add options for different features
option("style_agent")
    set_default(false)
    set_description("Enable Style Agent")
option_end()

option("windows_system_borders")
    set_default(false)
    set_description("Enable Windows System Borders")
option_end()

option("build_static")
    set_default(true)
    set_description("Build static libraries")
option_end()

if not is_plat("linux") then
    add_requires("qt6core", "qt6gui", "qt6widgets")
end

target("QWKCore")
    -- Set target type
    if has_config("build_static") then
        set_kind("static")
    else
        set_kind("shared")
    end

    add_cxxflags("-fPIC", "-fvisibility=hidden", "-fvisibility-inlines-hidden")
    add_packages("qt6core", "qt6gui", "qt6widgets")
    if is_plat("macosx") then
        add_mxflags("-fno-objc-arc")
        add_frameworks("Foundation", "Cocoa", "AppKit")
        add_frameworks("QtCore", "QtGui", "QtWidgets")
        add_frameworks("QtCorePrivate", "QtGuiPrivate")
    end

    -- Enable MOC generation for Qt
    add_rules("qt.moc")
    
    -- Generate config header before build
    before_build(function (target)
        -- Create build directories
        os.mkdir("$(buildir)/include/QWKCore")
        os.mkdir("$(buildir)/include/QWKCore/private")
        
        -- Generate qwkconfig.h
        local config_content = [[
#ifndef QWKCONFIG_H
#define QWKCONFIG_H

#define QWINDOWKIT_ENABLE_QT_WINDOW_CONTEXT ]] .. 
        "-1" .. [[

#define QWINDOWKIT_ENABLE_STYLE_AGENT ]] ..
        (has_config("style_agent") and "1" or "-1") .. [[

#define QWINDOWKIT_ENABLE_WINDOWS_SYSTEM_BORDERS ]] ..
        (has_config("windows_system_borders") and "1" or "-1") .. [[


#endif // QWKCONFIG_H
]]
        io.writefile("$(buildir)/include/QWKCore/qwkconfig.h", config_content)
        
        -- Copy header files
        os.cp("src/core/*.h", "$(buildir)/include/QWKCore/")
        os.trycp("src/core/*_p.h", "$(buildir)/include/QWKCore/private/")
        os.trycp("src/core/contexts/*_p.h", "$(buildir)/include/QWKCore/private/")
        os.trycp("src/core/contexts/*.h", "$(buildir)/include/QWKCore/private/")
        os.trycp("src/core/kernel/*_p.h", "$(buildir)/include/QWKCore/private/")
        os.trycp("src/core/shared/*_p.h", "$(buildir)/include/QWKCore/private/")
        
        if has_config("style_agent") then
            os.trycp("src/core/style/*_p.h", "$(buildir)/include/QWKCore/private/")
            os.cp("src/core/style/styleagent.h", "$(buildir)/include/QWKCore/styleagent.h")
        end

        -- Get Qt private include paths
        local modules = {"QtCore", "QtGui"}
        local headers_path= ""
        local qt_version= ""
        if is_plat("macosx") then
            headers_path = os.iorun("qmake -query QT_INSTALL_HEADERS"):gsub("%s+", "")
            qt_version = os.iorun("qmake -query QT_VERSION"):gsub("%s+", "")
        else
            headers_path = os.iorun("qmake6 -query QT_INSTALL_HEADERS"):gsub("%s+", "")
            qt_version = os.iorun("qmake6 -query QT_VERSION"):gsub("%s+", "")
        end

        local private_paths = {}
        if is_plat("linux") then
            table.insert(private_paths, string.format("%s", headers_path))
        end

        if is_plat("macosx") then
            for _, module in ipairs(modules) do
                table.insert(private_paths, string.format("%s/%s/%s/%s/private",
                    headers_path, module, qt_version, module))
                table.insert(private_paths, string.format("%s/%s/%s/%s",
                    headers_path, module, qt_version, module))
                table.insert(private_paths, string.format("%s/%s/%s",
                    headers_path, module, qt_version))
            end
        end

        target:add("includedirs", private_paths)
    end)

    -- Include directories
    add_includedirs("$(buildir)/include", {public = true})
    add_includedirs("src/core", "src/core/kernel", "src/core/shared", "src/core/contexts", "src")

    -- Defines
    add_defines("QWINDOWKIT_ENABLE_QT_WINDOW_CONTEXT=-1")
    
    if has_config("style_agent") then
        add_defines("QWINDOWKIT_ENABLE_STYLE_AGENT=1")
    else
        add_defines("QWINDOWKIT_ENABLE_STYLE_AGENT=-1")
    end
    
    if has_config("windows_system_borders") then
        add_defines("QWINDOWKIT_ENABLE_WINDOWS_SYSTEM_BORDERS=1")
    else
        add_defines("QWINDOWKIT_ENABLE_WINDOWS_SYSTEM_BORDERS=-1")
    end

    -- Core source files
    add_files("src/core/qwkglobal.cpp")
    add_files("src/core/windowagentbase.cpp")
    add_files("src/core/windowitemdelegate.cpp")
    add_files("src/core/kernel/nativeeventfilter.cpp")
    add_files("src/core/kernel/sharedeventfilter.cpp")
    add_files("src/core/kernel/winidchangeeventfilter.cpp")
    add_files("src/core/contexts/abstractwindowcontext.cpp")
    add_files("src/core/contexts/cocoawindowcontext.mm")
    if has_config("style_agent") then
        add_files("src/core/style/styleagent.cpp")
        add_files("src/core/style/styleagent_mac.mm")
    end

    -- Add header files that need MOC processing (use add_files for Q_OBJECT headers)
    add_files("src/core/windowagentbase.h")
    add_files("src/core/contexts/abstractwindowcontext_p.h")
    add_files("src/core/contexts/cocoawindowcontext_p.h")
    if has_config("style_agent") then
        add_files("src/core/style/styleagent.h")
    end

    -- Set install headers
    add_headerfiles("$(buildir)/include/QWKCore/**.h", {prefixdir = "QWKCore"})
    add_headerfiles("$(buildir)/include/QWKCore/private/**.h", {prefixdir = "QWKCore/private"})
target_end()

target("QWKWidgets")
    -- Set target type
    if has_config("build_static") then
        set_kind("static")
    else
        set_kind("shared")
    end

    add_cxxflags("-fPIC", "-fvisibility=hidden", "-fvisibility-inlines-hidden")
    add_deps("QWKCore")
    add_packages("qt6core", "qt6gui", "qt6widgets")
    if is_plat("macosx") then
        add_mxflags("-fno-objc-arc")
        add_frameworks("Foundation", "Cocoa", "AppKit")
        add_frameworks("QtCore", "QtGui", "QtWidgets")
        add_frameworks("QtCorePrivate", "QtGuiPrivate")
    end

    -- Enable MOC generation for Qt
    add_rules("qt.moc")

    -- Copy widgets headers before build
    before_build(function (target)
        os.mkdir("$(buildir)/include/QWKWidgets")
        os.cp("src/widgets/*.h", "$(buildir)/include/QWKWidgets/")
    end)

    -- Include directories
    add_includedirs("$(buildir)/include", {public = true})
    add_includedirs("src/widgets", "src")

    -- Source files
    add_files("src/widgets/widgetwindowagent.cpp")
    add_files("src/widgets/widgetitemdelegate.cpp")
    if is_plat("macosx") then
        add_files("src/widgets/widgetwindowagent_mac.cpp")
    end

    -- Add header files that need MOC processing (use add_files for Q_OBJECT headers)
    add_files("src/widgets/widgetwindowagent.h")

    -- Set install headers
    add_headerfiles("$(buildir)/include/QWKWidgets/**.h", {prefixdir = "QWKWidgets"})
target_end()

-- Configuration summary
after_load(function (target)
    print("QWindowKit Configuration:")
    print("  - Build Type: " .. (is_mode("debug") and "Debug" or "Release"))
    print("  - Library Type: " .. (has_config("build_static") and "Static" or "Shared"))
    print("  - Style Agent: " .. (has_config("style_agent") and "Enabled" or "Disabled"))
    print("  - Windows System Borders: " .. (has_config("windows_system_borders") and "Enabled" or "Disabled"))
    print("  - Platform: " .. os.host())
end)
