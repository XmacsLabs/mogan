-------------------------------------------------------------------------------
--
-- MODULE      : glue_string.lua
-- DESCRIPTION : Generating glue on src/Kernel/Types/string.hpp
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--                   2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        group_name = "glue_string",
        binding_object = "",
        initializer_name = "initialize_glue_string",
        glues = {
            {
                scm_name = "cpp-string-number?",
                cpp_name = "is_double",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
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
        }
    }
end