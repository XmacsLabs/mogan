(tm-define (rsvg-convert x opts)
  (let* ((dest (assoc-ref opts 'dest))
         (fm (url-format (url-concretize dest)))
         (res (get-raster-resolution opts))
	 (cmd (get-shell-command "rsvg-convert")))
    (system-2 (string-append cmd " -f " fm " -d " res " -o ") dest x)
    (if (url-exists? dest) dest #f)))

(converter svg-file postscript-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-f" from "-P" to))

(converter svg-file pdf-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-f" from "-A" to))

(converter svg-file png-file
  (:require (url-exists-in-path? "inkscape"))
  (:shell "inkscape" "-z" "-d" "600" from "--export-png" to))

(converter svg-file png-file
  (:require (and (url-exists-in-path? "rsvg-convert")
                 (not (url-exists-in-path? "inkscape"))))
    (:function-with-options rsvg-convert))

(converter svg-file postscript-document
  (:require (qt5-or-later-gui?))
  (:function image->psdoc))
 
