-------------------------------------------------------------------------------
--
-- MODULE      : tm_s7.lua
-- DESCRIPTION : Xmake config file for tm_s7
-- COPYRIGHT   : (C) 2024   Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

target ("tm_s7") do
    set_targetdir("$(projectdir)/TeXmacs/plugins/s7/bin/")
    add_files ("$(projectdir)/TeXmacs/plugins/s7/src/tm_s7.cpp")
    add_packages("s7")
end
