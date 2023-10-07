-------------------------------------------------------------------------------
--
-- MODULE      : global.lua
-- DESCRIPTION : Global functions & variables for Mogan STEM Suite
-- COPYRIGHT   : (C) 2023 Oyyko
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.



-- The file for Global functions and variables that will be used in the SCRIPT DOMAIN of xmake.lua
-- see https://github.com/xmake-io/xmake/discussions/4070

-- usage:
-- import("xmake_modules.global")
-- globals.XXX

globals={}

function copy_icons(target) 
  os.cp ("TeXmacs/misc/images/texmacs.svg", 
        path.join(target:installdir(), "share/icons/hicolor/scalable/apps", "Mogan.svg"))
  for _,size in ipairs({32, 48, 64, 128, 256, 512}) do
    os.cp (
      "TeXmacs/misc/images/texmacs-"..size..".png",
      path.join(target:installdir(), "share/icons/hicolor/", size .."x"..size, "/apps/Xmacs.png"))
  end
  print("COPY ICONS DONE!")
end

return globals