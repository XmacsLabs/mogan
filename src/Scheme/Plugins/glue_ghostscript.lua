-------------------------------------------------------------------------------
--
-- MODULE      : glue_ghostscript.lua
-- DESCRIPTION : Generating glue on src/Plugins/Ghostscript
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
        initializer_name = "initialize_glue_ghostscript",
        glues = {
            {
                scm_name = "supports-ghostscript?",
                cpp_name = "supports_ghostscript",
                ret_type = "bool"
            },
        }
    }
end

