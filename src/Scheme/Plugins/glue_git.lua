
-------------------------------------------------------------------------------
--
-- MODULE      : glue_git.lua
-- DESCRIPTION : Generating glue on src/Plugins/Git
-- COPYRIGHT   : (C) 2024       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        binding_object = "",
        initializer_name = "initialize_glue_git",
        glues = {
            {
                scm_name = "libgit2-version",
                cpp_name = "libgit2_version",
                ret_type = "string"
            },
            {
                scm_name = "git-load-blob",
                cpp_name = "git_load_blob",
                ret_type = "string",
                arg_list = {
                    "string",
                    "url",
                    "url"
                }
            },
            {
                scm_name = "git-status-file",
                cpp_name = "git_status_file",
                ret_type = "string",
                arg_list = {
                    "url",
                    "url"
                }
            },
        }
    }
end


