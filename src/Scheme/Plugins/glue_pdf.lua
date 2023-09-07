-------------------------------------------------------------------------------
--
-- MODULE      : glue_pdf.lua
-- DESCRIPTION : Generating glue on src/Plugins/Pdf
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
        initializer_name = "initialize_glue_pdf",
        glues = {
            {
                scm_name = "supports-native-pdf?",
                cpp_name = "supports_native_pdf",
                ret_type = "bool"
            },
            {
                scm_name = "pdfhummus-version",
                cpp_name = "pdfhummus_version",
                ret_type = "string"
            },
            {
                scm_name = "get-attachments",
                cpp_name = "scm_get_attachments",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
        }
    }
end
