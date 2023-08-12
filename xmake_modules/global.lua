-- The file for Global functions and variables that will be used in the SCRIPT field of xmake.lua

-- usage:


--
-- copy icons
--
global_functions={}

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

return global_functions