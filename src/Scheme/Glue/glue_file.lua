-------------------------------------------------------------------------------
--
-- MODULE      : glue_file.lua
-- DESCRIPTION : Generating glue on src/System/Files/file.hpp
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        group_name = "glue_file",
        binding_object = "",
        initializer_name = "initialize_glue_file",
        glues = {
            {
                scm_name = "string-save",
                cpp_name = "string_save",
                ret_type = "void",
                arg_list = {
                    "string",
                    "url"
                }
            },
            {
                scm_name = "string-load",
                cpp_name = "string_load",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-test?",
                cpp_name = "is_of_type",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-regular?",
                cpp_name = "is_regular",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-directory?",
                cpp_name = "is_directory",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-link?",
                cpp_name = "is_symbolic_link",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-newer?",
                cpp_name = "is_newer",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-size",
                cpp_name = "file_size",
                ret_type = "int",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-last-modified",
                cpp_name = "last_modified",
                ret_type = "int",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-temp",
                cpp_name = "url_temp",
                ret_type = "url"
            },
            {
                scm_name = "url-scratch",
                cpp_name = "url_scratch",
                ret_type = "url",
                arg_list = {
                    "string",
                    "string",
                    "int"
                }
            },
            {
                scm_name = "url-scratch?",
                cpp_name = "is_scratch",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-format",
                cpp_name = "file_format",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-url->string",
                cpp_name = "sys_concretize",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-move",
                cpp_name = "move",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "system-copy",
                cpp_name = "copy",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "system-remove",
                cpp_name = "remove",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-mkdir",
                cpp_name = "mkdir",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-rmdir",
                cpp_name = "rmdir",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-search-score",
                cpp_name = "search_score",
                ret_type = "int",
                arg_list = {
                    "url",
                    "array_string"
                }
            },
            {
                scm_name = "url-grep",
                cpp_name = "grep",
                ret_type = "url",
                arg_list = {
                    "string",
                    "url"
                }
            },
        }
    }
end
