-------------------------------------------------------------------------------
--
-- MODULE      : zlib.lua
-- DESCRIPTION : customize zlib
-- COPYRIGHT   : (C) 2023       Darcy Shen
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

package("zlib")
    set_base("zlib")
    set_urls("https://github.com/madler/zlib/archive/refs/tags/v$(version).tar.gz", {alias="github"})
    add_versions("github:1.2.11",  "629380c90a77b964d896ed37163f5c3a34f6e6d897311f1df2a7016355c45eff")

    if is_plat("linux") then
        add_extsources("pacman::zlib", "apt::zlib1g-dev")
    end
package_end()
