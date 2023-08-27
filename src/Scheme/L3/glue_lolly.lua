-------------------------------------------------------------------------------
--
-- MODULE      : glue_lolly.lua
-- DESCRIPTION : Generating glue to get information about Lolly
-- COPYRIGHT   : (C) 2023       charonxin
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        binding_object = "",
        initializer_name = "initialize_glue_lolly",
        glues = {
            {
                scm_name = "lolly-version",
                cpp_name = "lolly_version",
                ret_type = "string",
            },
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
                scm_name = "utf8->t2a",
                cpp_name = "utf8_to_t2a",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "t2a->utf8",
                cpp_name = "t2a_to_utf8",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "utf8->cork",
                cpp_name = "utf8_to_cork",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "cork->utf8",
                cpp_name = "cork_to_utf8",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
              -- routines for strings in the TeXmacs encoding
            {
                scm_name = "string->tmstring",
                cpp_name = "tm_encode",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring->string",
                cpp_name = "tm_decode",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-length",
                cpp_name = "tm_string_length",
                ret_type = "int",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-ref",
                cpp_name = "tm_forward_access",
                ret_type = "string",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "tmstring-reverse-ref",
                cpp_name = "tm_backward_access",
                ret_type = "string",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "tmstring->list",
                cpp_name = "tm_tokenize",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-next",
                cpp_name = "tm_char_next",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "string-previous",
                cpp_name = "tm_char_previous",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "tmstring-split",
                cpp_name = "tm_string_split",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "list->tmstring",
                cpp_name = "tm_recompose",
                ret_type = "string",
                arg_list = {
                    "array_string"
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
                scm_name = "cpp-string-tokenize",
                cpp_name = "tokenize",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "cpp-string-recompose",
                cpp_name = "recompose",
                ret_type = "string",
                arg_list = {
                    "array_string",
                    "string"
                }
            },
            {
                scm_name = "string-trim-spaces-left",
                cpp_name = "trim_spaces_left",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-trim-spaces-right",
                cpp_name = "trim_spaces_right",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-trim-spaces",
                cpp_name = "trim_spaces",
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
            -- Kernel/Types/string.hpp
            {
                scm_name = "cpp-string-number?",
                cpp_name = "is_double",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            -- Data/String/base64.hpp
            {
                scm_name = "encode-base64",
                cpp_name = "encode_base64",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "decode-base64",
                cpp_name = "decode_base64",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            -- System/Misc/sys_utils.hpp
            {
                scm_name = "get-user-login",
                cpp_name = "get_user_login",
                ret_type = "string"
            },
            {
                scm_name = "get-user-name",
                cpp_name = "get_user_name",
                ret_type = "string"
            },
            {
                scm_name = "os-win32?",
                cpp_name = "os_win",
                ret_type = "bool"
            },
            {
                scm_name = "os-mingw?",
                cpp_name = "os_mingw",
                ret_type = "bool"
            },
            {
                scm_name = "os-macos?",
                cpp_name = "os_macos",
                ret_type = "bool"
            },
            {
                scm_name = "evaluate-system",
                cpp_name = "evaluate_system",
                ret_type = "array_string",
                arg_list = {
                    "array_string",
                    "array_int",
                    "array_string",
                    "array_int"
                }
            },
            {
                scm_name = "system-setenv",
                cpp_name = "set_env",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
        }
    }
end
