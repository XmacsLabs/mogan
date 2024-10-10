-------------------------------------------------------------------------------
--
-- MODULE      : glue_misc.lua
-- DESCRIPTION : Generating glue on src/System/Misc/
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        group_name = "glue_misc",
        binding_object = "",
        initializer_name = "initialize_glue_misc",
        glues = {
            {
                scm_name = "persistent-set",
                cpp_name = "persistent_set",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "persistent-remove",
                cpp_name = "persistent_reset",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "persistent-has?",
                cpp_name = "persistent_contains",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "persistent-get",
                cpp_name = "persistent_get",
                ret_type = "string",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "persistent-file-name",
                cpp_name = "persistent_file_name",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "system-1",
                cpp_name = "system",
                ret_type = "void",
                arg_list = {
                    "string",
                    "url"
                }
            },
            {
                scm_name = "system-2",
                cpp_name = "system",
                ret_type = "void",
                arg_list = {
                    "string",
                    "url",
                    "url"
                }
            },
            {
                scm_name = "eval-system",
                cpp_name = "eval_system",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "var-eval-system",
                cpp_name = "var_eval_system",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "get-texmacs-path",
                cpp_name = "get_texmacs_path",
                ret_type = "url"
            },
            {
                scm_name = "get-texmacs-home-path",
                cpp_name = "get_texmacs_home_path",
                ret_type = "url"
            },
            {
                scm_name = "get-tm-cache-path",
                cpp_name = "get_tm_cache_path",
                ret_type = "url"
            },
            {
                scm_name = "default-look-and-feel",
                cpp_name = "default_look_and_feel",
                ret_type = "string"
            },
        }
    }
end
