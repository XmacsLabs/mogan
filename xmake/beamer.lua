function add_target_beamer()
    set_basename("MoganBeamer")

    if is_plat("windows") then
        set_optimize("smallest")
        set_runtimes("MT")
    end

    if is_mode("debug", "releasedbg") and is_plat("windows") then
        add_rules("qt.console")
    else
        add_rules("qt.widgetapp")
    end

    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")

    add_packages("lolly")
    if is_plat("windows") then
        add_packages("qt6widgets")
    end

    add_deps("libmogan")
    add_includedirs({
        "$(buildir)",
    })
    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/tm_configure.hpp"))
    end)
    add_files("src/Mogan/Beamer/beamer.cpp")

    if not is_plat("windows") then
        add_syslinks("pthread")
    end

    -- if is_plat("mingw", "windows") and is_mode("release") then
    --     add_deps("research_windows_icon")
    -- end

    set_configvar("PACKAGE", "Mogan Beamer")

    -- install man.1 manual file
    add_configfiles("(misc/man/texmacs.1.in)", {
        filename = "texmacs.1",
        pattern = "@([^\n]-)@",
    })

    -- package metadata
    if is_plat("macosx") then
        add_installfiles({
            "packages/macos/new-mogan.icns",
            "packages/macos/TeXmacs-document.icns",
            "src/Plugins/Cocoa/(English.lproj/**)",
            "src/Plugins/Cocoa/(zh_CN.lproj/**)"
        })
    end
  
    if is_plat("windows") then
        add_installfiles(TeXmacs_files)
    else
        add_installfiles(TeXmacs_files, {prefixdir="share/Xmacs"})
    end

    -- install tm files for testing purpose
    if is_mode("releasedbg") then
        if is_plat("windows") then
            add_installfiles({
                "TeXmacs(/tests/tm/*.tm)",
                "TeXmacs(/tests/tex/*.tex)",
                "TeXmacs(/tests/bib/*.bib)",
            })
        else
            add_installfiles({
                "TeXmacs(/tests/*.tm)",
                "TeXmacs(/tests/*.bib)",
            }, {prefixdir="share/Xmacs"})
        end
    end

    -- deploy necessary dll
    if is_plat("windows") then
        set_values("qt.deploy.flags", {"-printsupport", "--no-opengl-sw", "--no-translations"})
    end

    after_install(function (target)
        print("after_install of target beamer")
        import("xmake.global")

        if is_plat("macosx") and is_arch("arm64") then
            local app_dir = target:installdir() .. "/../../"
            os.rm(app_dir .. "Contents/Resources/include")
            os.rm(app_dir .. "Contents/Frameworks/QtQmlModels.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQml.framework")
            os.rm(app_dir .. "Contents/Frameworks/QtQuick.framework")
            os.execv("codesign", {"--force", "--deep", "--sign", "-", app_dir})
        end
    end)
end
