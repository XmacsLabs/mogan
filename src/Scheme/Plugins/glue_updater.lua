-------------------------------------------------------------------------------
--
-- MODULE      : glue_updater.lua
-- DESCRIPTION : Generating glue on src/Plugins/Updater
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
        initializer_name = "initialize_glue_updater",
        glues = {
            {
                scm_name = "updater-supported?",
                cpp_name = "updater_supported",
                ret_type = "bool"
            },
            {
                scm_name = "updater-running?",
                cpp_name = "updater_is_running",
                ret_type = "bool"
            },
            {
                scm_name = "updater-check-background",
                cpp_name = "updater_check_background",
                ret_type = "bool"
            },
            {
                scm_name = "updater-check-foreground",
                cpp_name = "updater_check_foreground",
                ret_type = "bool"
            },
            {
                scm_name = "updater-last-check",
                cpp_name = "updater_last_check",
                ret_type = "long"
            },
            {
                scm_name = "updater-set-interval",
                cpp_name = "updater_set_interval",
                ret_type = "bool",
                arg_list = {
                    "int"
                }
            },
        }
    }
end
