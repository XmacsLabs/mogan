function add_target_beamer()
    set_basename("MoganBeamer")

    if is_plat("windows") then
        set_optimize("smallest")
        set_runtimes("MT")
    end

    build_glue_on_config()
    add_configfiles("src/System/config.h.xmake", {
        filename = "beamer/config.h",
        variables = {
            MACOSX_EXTENSIONS = is_plat("macosx"),
            USE_PLUGIN_PDF = true,
            USE_PLUGIN_BIBTEX = false,
            USE_PLUGIN_LATEX_PREVIEW = true,
            USE_PLUGIN_TEX = true,
            USE_PLUGIN_ISPELL = true,
            USE_PLUGIN_HTML = true,
            QTPIPES = not is_plat("wasm"),
            USE_QT_PRINTER = not is_plat("wasm"),
            NOMINMAX = is_plat("windows"),
            SIZEOF_VOID_P = 8,
            USE_FONTCONFIG = is_plat("linux"),
            USE_STACK_TRACE = (not is_plat("mingw")) and (not is_plat("wasm")) and (not is_plat("windows")),
            USE_PLUGIN_GS = true,
            USE_FREETYPE = true,
            USE_ICONV = is_plat("wasm"),
            QTTEXMACS = true,
            APP_MOGAN_BEAMER = true,
        }
    })

    if is_mode("debug", "releasedbg") and is_plat("windows") then
        add_rules("qt.console")
    else
        add_rules("qt.widgetapp")
    end

    add_frameworks("QtGui", "QtWidgets", "QtCore", "QtPrintSupport", "QtSvg")

    add_packages("lolly")
    add_packages("freetype")
    add_packages("s7")
    add_packages("pdfhummus")
    add_packages("libgit2")
    if not is_plat("macosx") then
        add_packages("libiconv")
    end
    if is_plat("windows") then
        add_packages("qt6widgets")
    end

    add_includedirs(libmogan_headers, {public = true})
    add_includedirs("$(buildir)/beamer")
    add_files(libmogan_srcs)

    if is_plat("macosx") then
        add_includedirs("src/Plugins/MacOS", {public = true})
        add_files(plugin_macos_srcs)
    end
    add_files(plugin_database_srcs)
    add_files(plugin_qt_srcs)
    add_files(plugin_freetype_srcs)
    add_files(plugin_ghostscript_srcs)
    add_files(plugin_ispell_srcs)
    add_files(plugin_metafont_srcs)
    add_files(plugin_tex_srcs)
    add_files(plugin_latex_preview_srcs)
    add_files(plugin_openssl_srcs)
    add_files(plugin_xml_srcs)
    add_files(plugin_html_srcs)
    add_files(plugin_pdf_srcs)
    add_files(plugin_git_srcs)

    add_mxflags("-fno-objc-arc")

    before_build(function (target)
        target:add("forceincludes", path.absolute("$(buildir)/beamer/config.h"))
        target:add("forceincludes", path.absolute("$(buildir)/beamer/tm_configure.hpp"))
    end)
    add_files("src/Mogan/Beamer/beamer.cpp")

    if not is_plat("windows") then
        add_syslinks("pthread")
    end

    -- if is_plat("mingw", "windows") and is_mode("release") then
    --     add_deps("research_windows_icon")
    -- end

    set_configvar("PACKAGE", "Mogan Beamer")

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
