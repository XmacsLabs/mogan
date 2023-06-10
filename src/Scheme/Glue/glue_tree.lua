-------------------------------------------------------------------------------
--
-- MODULE      : glue_tree.lua
-- DESCRIPTION : Generating glue for src/Kernel/Types/tree.hpp
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        group_name = "glue_tree",
        binding_object = "",
        initializer_name = "initialize_gen_tree",
        glues = {
            {
                scm_name = "tree-atomic?",
                cpp_name = "is_atomic",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-compound?",
                cpp_name = "is_compound",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-simplify",
                cpp_name = "simplify_correct",
                ret_type = "tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-as-string",
                cpp_name = "tree_as_string",
                ret_type = "string",
                arg_list = {
                    "content"
                }
            },
        }
    }
end

