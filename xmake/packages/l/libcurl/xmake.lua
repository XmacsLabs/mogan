package("libcurl")
    set_homepage("https://curl.haxx.se/")
    set_description("The multiprotocol file transfer library.")
    set_license("MIT")

    add_urls("https://gitee.com/mirrors/curl.git")
    add_versions("v8.11.1", "curl-8_11_1")

    add_configs("cares",    {description = "Enable c-ares support.", default = false, type = "boolean"})
    add_configs("openssl",  {description = "Enable OpenSSL for SSL/TLS.", default = nil, type = "boolean"})
    add_configs("openssl3", {description = "Enable OpenSSL-3 for SSL/TLS.", default = nil, type = "boolean"})
    add_configs("mbedtls",  {description = "Enable mbedTLS for SSL/TLS.", default = nil, type = "boolean"})
    add_configs("nghttp2",  {description = "Use Nghttp2 library.", default = false, type = "boolean"})
    add_configs("openldap", {description = "Use OpenLDAP library.", default = false, type = "boolean"})
    add_configs("libidn2",  {description = "Use Libidn2 for IDN support.", default = false, type = "boolean"})
    add_configs("zlib",     {description = "Enable zlib support.", default = false, type = "boolean"})
    add_configs("zstd",     {description = "Enable zstd support.", default = false, type = "boolean"})
    add_configs("brotli",   {description = "Enable brotli support.", default = false, type = "boolean"})
    add_configs("libssh2",  {description = "Use libSSH2 library.", default = false, type = "boolean"})
    add_configs("libpsl",   {description = "Use libpsl library.", default = false, type = "boolean"})

    if is_plat("android") and is_host("windows") then
        add_deps("ninja")
        set_policy("package.cmake_generator.ninja", true)
    end

    -- we init all configurations in on_load, because package("curl") need it.
    on_load(function (package)
        if package:is_plat("linux", "android", "cross") then
            -- if no TLS backend has been enabled nor disabled, enable openssl by default
            if package:config("openssl") == nil and package:config("openssl3") == nil and package:config("mbedtls") == nil then
                package:config_set("openssl", true)
            end
        end

        assert(not (package:config("openssl") and package:config("openssl3")), "OpenSSL and OpenSSL-3 cannot be enabled at the same time.")

        if package:is_plat("macosx", "iphoneos") then
            package:add("frameworks", "Security", "CoreFoundation", "SystemConfiguration")
        elseif package:is_plat("linux") then
            package:add("syslinks", "pthread")
        elseif package:is_plat("windows", "mingw") then
            package:add("syslinks", "advapi32", "crypt32", "wldap32", "winmm", "ws2_32", "user32")
        end

        if package:is_plat("mingw") and is_subhost("msys") then
            package:add("extsources", "pacman::curl")
        elseif package:is_plat("linux") then
            package:add("extsources", "pacman::curl", "apt::libcurl4-gnutls-dev", "apt::libcurl4-nss-dev", "apt::libcurl4-openssl-dev")
        elseif package:is_plat("macosx") then
            package:add("extsources", "brew::curl")
        end

        if package:is_plat("windows", "mingw") then
            if not package:config("shared") then
                package:add("defines", "CURL_STATICLIB")
            end
        end

        package:add("deps", "cmake")
        local configdeps = {cares    = "c-ares",
                            openssl  = "openssl",
                            openssl3 = "openssl3",
                            mbedtls  = "mbedtls",
                            nghttp2  = "nghttp2",
                            openldap = "openldap",
                            libidn2  = "libidn2",
                            libpsl   = "libpsl",
                            zlib     = "zlib",
                            zstd     = "zstd",
                            brotli   = "brotli",
                            libssh2  = "libssh2"}
        local has_deps = false
        for name, dep in pairs(configdeps) do
            if package:config(name) then
                package:add("deps", dep, {host = package:is_binary()})
                has_deps = true
            end
        end
        if has_deps and package:is_plat("linux", "macosx") then
            package:add("deps", "pkg-config")
        end
    end)

    on_install("windows", "mingw", "linux", "macosx", "iphoneos", "cross", "android", function (package)
        local version = package:version()

        local configs = {"-DBUILD_TESTING=OFF", "-DENABLE_MANUAL=OFF", "-DENABLE_CURL_MANUAL=OFF"}
        table.insert(configs, "-DCMAKE_BUILD_TYPE=" .. (package:debug() and "Debug" or "Release"))
        table.insert(configs, "-DBUILD_SHARED_LIBS=" .. (package:config("shared") and "ON" or "OFF"))

        if (package:is_plat("mingw") and version:ge("7.85")) then
            package:add("syslinks", "bcrypt")
        end

        local configopts = {cares    = "ENABLE_ARES",
                            mbedtls  = (version:ge("7.81") and "CURL_USE_MBEDTLS" or "CMAKE_USE_MBEDTLS"),
                            nghttp2  = "USE_NGHTTP2",
                            libidn2  = "USE_LIBIDN2",
                            zlib     = "CURL_ZLIB",
                            zstd     = "CURL_ZSTD",
                            brotli   = "CURL_BROTLI",
                            libssh2  = (version:ge("7.81") and "CURL_USE_LIBSSH2" or "CMAKE_USE_LIBSSH2"),
                            libpsl   = "CURL_USE_LIBPSL"}
        for name, opt in pairs(configopts) do
            table.insert(configs, "-D" .. opt .. "=" .. (package:config(name) and "ON" or "OFF"))
        end
        table.insert(configs, "-D" .. (version:ge("7.81") and "CURL_USE_OPENSSL" or "CMAKE_USE_OPENSSL") .. "=" .. ((package:config("openssl") or package:config("openssl3")) and "ON" or "OFF"))

        if not package:config("openldap") then
            table.insert(configs, "-DCURL_DISABLE_LDAP=ON")
        end
        if package:is_plat("windows", "mingw") then
            table.insert(configs, (version:ge("7.80") and "-DCURL_USE_SCHANNEL=ON" or "-DCMAKE_USE_SCHANNEL=ON"))
        end
        if package:is_plat("macosx", "iphoneos") then
            table.insert(configs, (version:ge("7.65") and "-DCURL_USE_SECTRANSP=ON" or "-DCMAKE_USE_DARWINSSL=ON"))
        end
        if package:is_plat("windows") then
            table.insert(configs, "-DCURL_STATIC_CRT=" .. (package:config("vs_runtime"):startswith("MT") and "ON" or "OFF"))
        end
        if package:is_plat("mingw") and version:le("7.85.0") then
            io.replace("src/CMakeLists.txt", 'COMMAND ${CMAKE_COMMAND} -E echo "/* built-in manual is disabled, blank function */" > tool_hugehelp.c', "", {plain = true})
        end
        if package:is_plat("linux", "cross") then
            io.replace("CMakeLists.txt", "list(APPEND CURL_LIBS OpenSSL::SSL OpenSSL::Crypto)", "list(APPEND CURL_LIBS OpenSSL::SSL OpenSSL::Crypto dl)", {plain = true})
            io.replace("CMakeLists.txt", "list(APPEND CURL_LIBS ${OPENSSL_LIBRARIES})", "list(APPEND CURL_LIBS ${OPENSSL_LIBRARIES} dl)", {plain = true})
        end
        local function handledependency(conf, depname, includeconfig, libconfig)
            if package:config(conf) then
                local dep = package:dep(depname)
                if dep and not dep:is_system() then
                    local fetchinfo = dep:fetch({external = false})
                    if fetchinfo then
                        local includedirs = fetchinfo.includedirs or fetchinfo.sysincludedirs
                        if includedirs and #includedirs > 0 then
                            table.insert(configs, "-D" .. includeconfig .. "=" .. table.concat(includedirs, ";"):gsub("\\", "/"))
                        end
                        if type(libconfig) == "table" then
                            if fetchinfo.libfiles then
                                for _, libfile in ipairs(fetchinfo.libfiles) do
                                    local libname = path.basename(libfile)
                                    if libname:startswith("lib") then
                                        libname = libname:sub(4)
                                    end
                                    for opt, suffix in pairs(libconfig) do
                                        if libname:endswith(suffix) then
                                            table.insert(configs, "-D" .. opt .. "=" .. libfile:gsub("\\", "/"))
                                        end
                                    end
                                end
                            end
                        else
                            if fetchinfo.libfiles then
                                table.insert(configs, "-D" .. libconfig .. "=" .. table.concat(fetchinfo.libfiles, ";"):gsub("\\", "/"))
                            end
                        end
                    end
                end
            end
        end
        handledependency("brotli", "brotli", "BROTLI_INCLUDE_DIR", {BROTLICOMMON_LIBRARY = "brotlicommon", BROTLIDEC_LIBRARY = "brotlidec"})
        handledependency("openssl", "openssl", "OPENSSL_INCLUDE_DIR", {OPENSSL_CRYPTO_LIBRARY = "crypto", OPENSSL_SSL_LIBRARY = "ssl"})
        handledependency("openssl3", "openssl3", "OPENSSL_INCLUDE_DIR", {OPENSSL_CRYPTO_LIBRARY = "crypto", OPENSSL_SSL_LIBRARY = "ssl"})
        handledependency("mbedtls", "mbedtls", "MBEDTLS_INCLUDE_DIRS", {MBEDTLS_LIBRARY = "mbedtls", MBEDX509_LIBRARY = "mbedx509", MBEDCRYPTO_LIBRARY = "mbedcrypto"})
        handledependency("zlib", "zlib", "ZLIB_INCLUDE_DIR", "ZLIB_LIBRARY")
        handledependency("zstd", "zstd", "Zstd_INCLUDE_DIR", "Zstd_LIBRARY")
        import("package.tools.cmake").install(package, configs, {buildir = "build"})
    end)

    on_test(function (package)
        assert(package:has_cfuncs("curl_version", {includes = "curl/curl.h"}))
    end)