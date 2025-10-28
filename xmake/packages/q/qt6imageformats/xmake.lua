package("qt6imageformats")
    set_homepage("https://doc.qt.io/qt-6/qtimageformats-index.html")
    set_description("Additional image format plugins for Qt (WebP, MNG, TGA, TIFF)")
    set_kind("library")

    add_deps("aqt")

    on_install("linux", "macosx", "windows", function (package)
        -- 使用包指定的版本（6.8.3）
        local version = package:version()
        local versionstr = "6.8.3"

        -- 根据平台配置aqt参数
        local pseudo_host, target, arch
        if is_plat("windows") then
            pseudo_host = "windows"
            target = "desktop"
            arch = "win64_msvc2022_64"
        elseif is_plat("linux") then
            pseudo_host = "linux"
            target = "desktop"
            arch = "linux_gcc_64"
        elseif is_plat("macosx") then
            pseudo_host = "mac"
            target = "desktop"
            arch = "clang_64"
        end

        -- 创建临时目录来安装qtimageformats
        local temp_dir = path.join(package:cachedir(), "temp_install")
        os.mkdir(temp_dir)

        -- 通过aqt安装qtimageformats模块到临时目录
        local aqt_args = {"install-qt", "-O", temp_dir, pseudo_host, target, versionstr, arch, "-m", "qtimageformats"}

        local ok, output_or_errors = os.iorunv("aqt", aqt_args)
        if not ok then
            os.raise("Failed to install qtimageformats module: " .. (output_or_errors or "No output"))
        end

        -- 将插件文件复制到qt6imageformats的安装目录
        local package_plugins_dir = path.join(package:installdir(), "plugins", "imageformats")

        -- 尝试不同的目录结构（不同平台aqt行为不一致）
        local possible_paths = {}

        if is_plat("windows") then
            -- Windows可能的目录结构
            table.insert(possible_paths, path.join(temp_dir, versionstr, "win64_msvc2022_64", "plugins", "imageformats"))
            table.insert(possible_paths, path.join(temp_dir, versionstr, "msvc2022_64", "plugins", "imageformats"))
        elseif is_plat("linux") then
            -- Linux可能的目录结构
            table.insert(possible_paths, path.join(temp_dir, versionstr, "linux_gcc_64", "plugins", "imageformats"))
            table.insert(possible_paths, path.join(temp_dir, versionstr, "gcc_64", "plugins", "imageformats"))
        elseif is_plat("macosx") then
            -- macOS可能的目录结构
            table.insert(possible_paths, path.join(temp_dir, versionstr, "clang_64", "plugins", "imageformats"))
        end

        local found_plugins_dir = nil
        for _, test_path in ipairs(possible_paths) do
            if os.isdir(test_path) then
                found_plugins_dir = test_path
                break
            end
        end

        if found_plugins_dir then
            os.mkdir(package_plugins_dir)
            os.cp(path.join(found_plugins_dir, "*"), package_plugins_dir)
        else
            os.raise("Failed to find qtimageformats plugins")
        end

        -- 清理临时目录
        os.rmdir(temp_dir)
    end)


    on_test(function (package)
        -- 验证插件文件是否存在
        local package_plugins_dir = path.join(package:installdir(), "plugins", "imageformats")

        -- 检查WebP插件文件是否存在
        local webp_plugin_path
        if is_plat("windows") then
            webp_plugin_path = path.join(package_plugins_dir, "qwebp.dll")
        elseif is_plat("linux") then
            webp_plugin_path = path.join(package_plugins_dir, "libqwebp.so")
        elseif is_plat("macosx") then
            webp_plugin_path = path.join(package_plugins_dir, "libqwebp.dylib")
        end

        if not (webp_plugin_path and os.isfile(webp_plugin_path)) then
            os.raise("WebP plugin not found: " .. (webp_plugin_path or "unknown"))
        end
    end)