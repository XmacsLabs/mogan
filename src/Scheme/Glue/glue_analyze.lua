-------------------------------------------------------------------------------
--
-- MODULE      : glue_analyze.lua
-- DESCRIPTION : Generating glue on src/Kernel/Types/analyze.hpp
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        group_name = "glue_analyze",
        binding_object = "",
        initializer_name = "initialize_glue_analyze",
        glues = {
            {
                scm_name = "string-alpha?",
                cpp_name = "is_alpha",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-locase-alpha?",
                cpp_name = "is_locase_alpha",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "integer->hexadecimal",
                cpp_name = "as_hexadecimal",
                ret_type = "string",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "integer->padded-hexadecimal",
                cpp_name = "as_hexadecimal",
                ret_type = "string",
                arg_list = {
                    "int",
                    "int"
                }
            },
            {
                scm_name = "hexadecimal->integer",
                cpp_name = "from_hexadecimal",
                ret_type = "int",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-occurs?",
                cpp_name = "occurs",
                ret_type = "bool",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-count-occurrences",
                cpp_name = "count_occurrences",
                ret_type = "int",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-search-forwards",
                cpp_name = "search_forwards",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int",
                    "string"
                }
            },
            {
                scm_name = "string-search-backwards",
                cpp_name = "search_backwards",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int",
                    "string"
                }
            },
            {
                scm_name = "string-quote",
                cpp_name = "scm_quote",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-unquote",
                cpp_name = "scm_unquote",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-overlapping",
                cpp_name = "overlapping",
                ret_type = "int",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-replace",
                cpp_name = "replace",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-find-non-alpha",
                cpp_name = "find_non_alpha",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int",
                    "bool"
                }
            },
            {
                scm_name = "upcase-first",
                cpp_name = "upcase_first",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "locase-first",
                cpp_name = "locase_first",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "upcase-all",
                cpp_name = "upcase_all",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "locase-all",
                cpp_name = "locase_all",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-union",
                cpp_name = "string_union",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-minus",
                cpp_name = "string_minus",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "escape-generic",
                cpp_name = "escape_generic",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "escape-verbatim",
                cpp_name = "escape_verbatim",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "escape-shell",
                cpp_name = "escape_sh",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "unescape-guile",
                cpp_name = "unescape_guile",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-differences",
                cpp_name = "differences",
                ret_type = "array_int",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-distance",
                cpp_name = "distance",
                ret_type = "int",
                arg_list = {
                    "string",
                    "string"
                }
            },
        }
    }
end
