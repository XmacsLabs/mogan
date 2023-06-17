-------------------------------------------------------------------------------
--
-- MODULE      : glue_string.lua
-- DESCRIPTION : Generating glue on src/Kernel/Types/tree.hpp and src/Data/Tree
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
        initializer_name = "initialize_glue_tree",
        glues = {
            {
                scm_name = "tree->string",
                cpp_name = "coerce_tree_string",
                ret_type = "string",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "string->tree",
                cpp_name = "coerce_string_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tm->tree",
                cpp_name = "tree",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
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
                scm_name = "tree-label",
                cpp_name = "L",
                ret_type = "tree_label",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-children",
                cpp_name = "A",
                ret_type = "array_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-arity",
                cpp_name = "N",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-child-ref",
                cpp_name = "tree_ref",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-child-set!",
                cpp_name = "tree_set",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-child-insert",
                cpp_name = "tree_child_insert",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-ip",
                cpp_name = "obtain_ip",
                ret_type = "path",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-active?",
                cpp_name = "tree_active",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-eq?",
                cpp_name = "strong_equal",
                ret_type = "bool",
                arg_list = {
                    "tree",
                    "tree"
                }
            },
            {
                scm_name = "subtree",
                cpp_name = "subtree",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "path"
                }
            },
            {
                scm_name = "tree-range",
                cpp_name = "tree_range",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "tree-copy",
                cpp_name = "copy",
                ret_type = "tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-append",
                cpp_name = "tree_append",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "tree"
                }
            },
            {
                scm_name = "tree-right-index",
                cpp_name = "right_index",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-label-extension?",
                cpp_name = "is_extension",
                ret_type = "bool",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-label-macro?",
                cpp_name = "is_macro",
                ret_type = "bool",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-label-parameter?",
                cpp_name = "is_parameter",
                ret_type = "bool",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-label-type",
                cpp_name = "get_tag_type",
                ret_type = "string",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-multi-paragraph?",
                cpp_name = "is_multi_paragraph",
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
                scm_name = "tree-minimal-arity",
                cpp_name = "minimal_arity",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-maximal-arity",
                cpp_name = "maximal_arity",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-possible-arity?",
                cpp_name = "correct_arity",
                ret_type = "bool",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-insert_point",
                cpp_name = "insert_point",
                ret_type = "int",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-is-dynamic?",
                cpp_name = "is_dynamic",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-accessible-child?",
                cpp_name = "is_accessible_child",
                ret_type = "bool",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-accessible-children",
                cpp_name = "accessible_children",
                ret_type = "array_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-all-accessible?",
                cpp_name = "all_accessible",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-none-accessible?",
                cpp_name = "none_accessible",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-name",
                cpp_name = "get_name",
                ret_type = "string",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-long-name",
                cpp_name = "get_long_name",
                ret_type = "string",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-child-name",
                cpp_name = "get_child_name",
                ret_type = "string",
                arg_list = {
                    "content",
                    "int"
                }
            },
            {
                scm_name = "tree-child-long-name",
                cpp_name = "get_child_long_name",
                ret_type = "string",
                arg_list = {
                    "content",
                    "int"
                }
            },
            {
                scm_name = "tree-child-type",
                cpp_name = "get_child_type",
                ret_type = "string",
                arg_list = {
                    "content",
                    "int"
                }
            },
            {
                scm_name = "tree-child-env*",
                cpp_name = "get_env_child",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-child-env",
                cpp_name = "get_env_child",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "int",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "tree-descendant-env*",
                cpp_name = "get_env_descendant",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "path",
                    "content"
                }
            },
            {
                scm_name = "tree-descendant-env",
                cpp_name = "get_env_descendant",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "path",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "tree-load-inclusion",
                cpp_name = "load_inclusion",
                ret_type = "tree",
                arg_list = {
                    "url"
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
            {
                scm_name = "tree-extents",
                cpp_name = "tree_extents",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-empty?",
                cpp_name = "is_empty",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-multi-line?",
                cpp_name = "is_multi_line",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-is-buffer?",
                cpp_name = "admits_edit_observer",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-search-sections",
                cpp_name = "search_sections",
                ret_type = "array_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-search-tree",
                cpp_name = "search",
                ret_type = "array_path",
                arg_list = {
                    "content",
                    "content",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-search-tree-at",
                cpp_name = "search",
                ret_type = "array_path",
                arg_list = {
                    "content",
                    "content",
                    "path",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-spell",
                cpp_name = "spell",
                ret_type = "array_path",
                arg_list = {
                    "string",
                    "content",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-spell-at",
                cpp_name = "spell",
                ret_type = "array_path",
                arg_list = {
                    "string",
                    "content",
                    "path",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-spell-selection",
                cpp_name = "spell",
                ret_type = "array_path",
                arg_list = {
                    "string",
                    "content",
                    "path",
                    "path",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "previous-search-hit",
                cpp_name = "previous_search_hit",
                ret_type = "array_path",
                arg_list = {
                    "array_path",
                    "path",
                    "bool"
                }
            },
            {
                scm_name = "next-search-hit",
                cpp_name = "next_search_hit",
                ret_type = "array_path",
                arg_list = {
                    "array_path",
                    "path",
                    "bool"
                }
            },
            {
                scm_name = "navigate-search-hit",
                cpp_name = "navigate_search_hit",
                ret_type = "array_path",
                arg_list = {
                    "path",
                    "bool",
                    "bool",
                    "bool"
                }
            },
            {
                scm_name = "tag-minimal-arity",
                cpp_name = "minimal_arity",
                ret_type = "int",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tag-maximal-arity",
                cpp_name = "maximal_arity",
                ret_type = "int",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tag-possible-arity?",
                cpp_name = "correct_arity",
                ret_type = "bool",
                arg_list = {
                    "tree_label",
                    "int"
                }
            },
            {
                scm_name = "set-access-mode",
                cpp_name = "set_access_mode",
                ret_type = "int",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "get-access-mode",
                cpp_name = "get_access_mode",
                ret_type = "int"
            },
            
            {
                scm_name = "tree-assign",
                cpp_name = "tree_assign",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "content"
                }
            },
            {
                scm_name = "tree-var-insert",
                cpp_name = "tree_insert",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-remove",
                cpp_name = "tree_remove",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "tree-split",
                cpp_name = "tree_split",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "tree-join",
                cpp_name = "tree_join",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-assign-node",
                cpp_name = "tree_assign_node",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "tree_label"
                }
            },
            {
                scm_name = "tree-insert-node",
                cpp_name = "tree_insert_node",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-remove-node",
                cpp_name = "tree_remove_node",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            
            {
                scm_name = "cpp-tree-correct-node",
                cpp_name = "correct_node",
                ret_type = "void",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "cpp-tree-correct-downwards",
                cpp_name = "correct_downwards",
                ret_type = "void",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "cpp-tree-correct-upwards",
                cpp_name = "correct_upwards",
                ret_type = "void",
                arg_list = {
                    "tree"
                }
            },
        }
    }
end
