
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tabpage-menu.scm
;; DESCRIPTION : The tab bar below the main icon bar
;; COPYRIGHT   : (C) 2024 Zhenjun Guo
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus tabpage-menu)
  (:use (kernel gui menu-widget)
        (texmacs menus file-menu)))

(tm-menu (texmacs-tab-pages)
  (for (buf (buffer-menu-unsorted-list 99)) ; buf is the url
    (let* ((title  (buffer-get-title buf))
        (title*    (if (== title "") (url->system (url-tail buf)) title))
        (mod?      (buffer-modified? buf))
        (tab-title (string-append title* (if mod? " *" "")))
        (doc-path  (url->system buf))
        (active?   (== (current-buffer) buf)))
      (tab-page
        (eval buf)
        ((balloon (eval `(verbatim ,tab-title)) (eval `(verbatim ,doc-path))) (switch-to-buffer* buf))
        ((balloon "✕" "Close") (safely-kill-buffer-by-url buf))
        (eval active?)
      ))))