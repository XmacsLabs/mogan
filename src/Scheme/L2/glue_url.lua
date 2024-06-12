-------------------------------------------------------------------------------
--
-- MODULE      : glue_url.lua
-- DESCRIPTION : Generating glue on src/System/Classes/url.hpp
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        group_name = "glue_url",
        binding_object = "",
        initializer_name = "initialize_glue_url",
        glues = {
            {
                scm_name = "url->url",
                cpp_name = "url",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "root->url",
                cpp_name = "url_root",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string->url",
                cpp_name = "url",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url->string",
                cpp_name = "as_string",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url->stree",
                cpp_name = "as_tree",
                ret_type = "scheme_tree",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system->url",
                cpp_name = "url_system",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url->system",
                cpp_name = "as_system_string",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "unix->url",
                cpp_name = "url_unix",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url->unix",
                cpp_name = "as_unix_string",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-unix",
                cpp_name = "url",
                ret_type = "url",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "url-none",
                cpp_name = "url_none",
                ret_type = "url"
            },
            {
                scm_name = "url-any",
                cpp_name = "url_wildcard",
                ret_type = "url"
            },
            {
                scm_name = "url-wildcard",
                cpp_name = "url_wildcard",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url-pwd",
                cpp_name = "url_pwd",
                ret_type = "url"
            },
            {
                scm_name = "url-parent",
                cpp_name = "url_parent",
                ret_type = "url"
            },
            {
                scm_name = "url-ancestor",
                cpp_name = "url_ancestor",
                ret_type = "url"
            },
            {
                scm_name = "url-append",
                cpp_name = "url_concat",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-or",
                cpp_name = "url_or",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-none?",
                cpp_name = "is_none",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted?",
                cpp_name = "is_rooted",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted-protocol?",
                cpp_name = "is_rooted",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-rooted-web?",
                cpp_name = "is_rooted_web",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted-tmfs?",
                cpp_name = "is_rooted_tmfs",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted-tmfs-protocol?",
                cpp_name = "is_rooted_tmfs",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-root",
                cpp_name = "get_root",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-unroot",
                cpp_name = "unroot",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-atomic?",
                cpp_name = "is_atomic",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-concat?",
                cpp_name = "is_concat",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-or?",
                cpp_name = "is_or",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-ref",
                cpp_name = "url_ref",
                ret_type = "url",
                arg_list = {
                    "url",
                    "int"
                }
            },
            {
                scm_name = "url-head",
                cpp_name = "head",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-tail",
                cpp_name = "tail",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-suffix",
                cpp_name = "suffix",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-basename",
                cpp_name = "basename",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-glue",
                cpp_name = "glue",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-unglue",
                cpp_name = "unglue",
                ret_type = "url",
                arg_list = {
                    "url",
                    "int"
                }
            },
            {
                scm_name = "url-relative",
                cpp_name = "relative",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-expand",
                cpp_name = "expand",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-factor",
                cpp_name = "factor",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-delta",
                cpp_name = "delta",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-secure?",
                cpp_name = "is_secure",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-descends?",
                cpp_name = "descends",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "url"
                }
            },
            
            {
                scm_name = "url-complete",
                cpp_name = "complete",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-resolve",
                cpp_name = "resolve",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-resolve-in-path",
                cpp_name = "resolve_in_path",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-exists?",
                cpp_name = "exists",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-exists-in-path?",
                cpp_name = "exists_in_path",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-concretize*",
                cpp_name = "concretize_url",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-concretize",
                cpp_name = "concretize",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-sys-concretize",
                cpp_name = "sys_concretize",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-materialize",
                cpp_name = "materialize",
                ret_type = "string",
                arg_list = {
                    "url",
                    "string"
                }
            },
        }
    }
end
