-------------------------------------------------------------------------------
--
-- MODULE      : glue_path.lua
-- DESCRIPTION : Generating glue on src/Kernel/Types/path.hpp
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        group_name = "glue_path",
        binding_object = "",
        initializer_name = "initialize_glue_path",
        glues = {
            {
                scm_name = "path-strip",
                cpp_name = "strip",
                ret_type = "path",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-inf?",
                cpp_name = "path_inf",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-inf-eq?",
                cpp_name = "path_inf_eq",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-less?",
                cpp_name = "path_less",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-less-eq?",
                cpp_name = "path_less_eq",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-start",
                cpp_name = "start",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-end",
                cpp_name = "end",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next",
                cpp_name = "next_valid",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous",
                cpp_name = "previous_valid",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next-word",
                cpp_name = "next_word",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-word",
                cpp_name = "previous_word",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next-node",
                cpp_name = "next_node",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-node",
                cpp_name = "previous_node",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next-tag",
                cpp_name = "next_tag",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-previous-tag",
                cpp_name = "previous_tag",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-next-tag-same-argument",
                cpp_name = "next_tag_same_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-previous-tag-same-argument",
                cpp_name = "previous_tag_same_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-next-argument",
                cpp_name = "next_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-argument",
                cpp_name = "previous_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-section",
                cpp_name = "previous_section",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
        }
    }
end
