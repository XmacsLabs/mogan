-------------------------------------------------------------------------------
--
-- MODULE      : glue_scheme.lua
-- DESCRIPTION : Generating glue on scheme
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
        initializer_name = "initialize_glue_scheme",
        glues = {
            {
                scm_name = "tree->stree",
                cpp_name = "tree_to_scheme_tree",
                ret_type = "scheme_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "stree->tree",
                cpp_name = "scheme_tree_to_tree",
                ret_type = "tree",
                arg_list = {
                    "scheme_tree"
                }
            },
        }
    }
end
