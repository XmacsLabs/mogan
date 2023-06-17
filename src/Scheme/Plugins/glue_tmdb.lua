-------------------------------------------------------------------------------
--
-- MODULE      : glue_tmdb.lua
-- DESCRIPTION : Generating glue on src/Plugins/Database
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        binding_object = "",
        initializer_name = "initialize_glue_tmdb",
        glues = {
            {
                scm_name = "tmdb-keep-history",
                cpp_name = "keep_history",
                ret_type = "void",
                arg_list = {
                    "url",
                    "bool"
                }
            },
            {
                scm_name = "tmdb-set-field",
                cpp_name = "set_field",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "string",
                    "array_string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-get-field",
                cpp_name = "get_field",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-remove-field",
                cpp_name = "remove_field",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-get-attributes",
                cpp_name = "get_attributes",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-set-entry",
                cpp_name = "set_entry",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "scheme_tree",
                    "double"
                }
            },
            {
                scm_name = "tmdb-get-entry",
                cpp_name = "get_entry",
                ret_type = "scheme_tree",
                arg_list = {
                    "url",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-remove-entry",
                cpp_name = "remove_entry",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-query",
                cpp_name = "query",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "scheme_tree",
                    "double",
                    "int"
                }
            },
            {
                scm_name = "tmdb-inspect-history",
                cpp_name = "inspect_history",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "tmdb-get-completions",
                cpp_name = "get_completions",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "tmdb-get-name-completions",
                cpp_name = "get_name_completions",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string"
                }
            },
        }
    }
end
