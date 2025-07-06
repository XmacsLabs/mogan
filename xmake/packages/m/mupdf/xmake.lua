-- 
-- Copyright (C) 2025 The Mogan Stem Authors
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
-- http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
-- 

package("mupdf")
    set_homepage("https://mupdf.com")
    set_description("MuPDF is an open source software framework for viewing, converting, and manipulating PDF, XPS, and E-book documents.")
    set_urls("https://mupdf.com/downloads/archive/mupdf-$(version)-source.tar.gz")
    set_license("AGPL-3.0")

    add_versions("1.24.10", "939285b5f97caf770fd46cbe7e6cc3a695ab19bb5bfaf5712904549cef390b7b")

    add_deps("pkg-config", "make", "libjpeg", "freetype", "libcurl", "zlib")

    on_install("linux", "macosx", function (package)
        if is_plat("macosx") then
            -- Use pkg-config to detect system library
            io.replace("Makerules", "else ifeq ($(LINUX_OR_OPENBSD),yes)", "", {plain = true})
        end
        -- Use system library from xmake to compat with other program
        import("package.tools.make").build(package, {
            "install-libs",
            "USE_SYSTEM_LIBJPEG=yes",
            "USE_SYSTEM_FREETYPE=yes",
            "USE_SYSTEM_ZLIB=yes",
            "USE_SYSTEM_CURL=yes",
            "tofu=yes",
            "tofu_cjk=yes",
            "prefix=" .. package:installdir()
        })
    end)
