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
        }
    }
end
