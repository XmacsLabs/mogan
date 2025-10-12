
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-tools.scm
;; DESCRIPTION : various tools
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-tools))

(import (scheme base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: if string in scheme represent unicode codepoint with single character 
;;   rather than utf-8 encoding, replace u8-string-length with string-length
(tm-define (count-characters doc)
  (with s (convert doc "texmacs-tree" "verbatim-snippet")
    (u8-string-length s)))

(define (compress-spaces s)
  (let* ((s1 (string-replace s "\n" " "))
         (s2 (string-replace s1 "\t" " "))
         (s3 (string-replace s2 "  " " "))
         (s4 (if (string-starts? s3 " ") (string-drop s3 1) s3))
         (s5 (if (string-ends? s4 " ") (string-drop-right s4 1) s4)))
    (if (== s5 s) s (compress-spaces s5))))

(tm-define (count-words doc)
  (with s (convert doc "texmacs-tree" "verbatim-snippet")
    (length (string-tokenize-by-char (compress-spaces s) #\space))))

(tm-define (count-lines doc)
  (with s (convert doc "texmacs-tree" "verbatim-snippet")
    (length (string-tokenize-by-char s #\newline))))

(define (selection-or-document)
  (if (selection-active-any?)
      (selection-tree)
      (buffer-tree)))

(tm-widget ((show-counts-widget m1 m2 m3) cmd)
  (resize "500guipx" "200guipx"
    (centered
      (bold (text "Statistics"))
      (glue #t #f 120 6)
      (text m1)
      (glue #t #f 120 3)
      (text m2)
      (glue #t #f 120 3)
      (text m3)
      (glue #t #f 120 6)
    (bottom-buttons
      ("Close" (cmd "cancel"))))))

(tm-define (show-counts)
  (:interactive #t)
  (let* ((doc (selection-or-document))
         (chars (string-append "Character count: " (number->string (count-characters doc))))
         (words (string-append "Word count: " (number->string (count-words      doc))))
         (lines (string-append "Line count: " (number->string (count-lines      doc)))))
    (dialogue-window (show-counts-widget chars words lines) noop "Document statistics")))

(define (save-aux-enabled?) (== (get-env "save-aux") "true"))
(tm-define (toggle-save-aux)
  (:synopsis "Toggle whether we save auxiliary data")
  (:check-mark "v" save-aux-enabled?)
  (let ((new (if (== (get-env "save-aux") "true") "false" "true")))
    (init-env "save-aux" new)))

(tm-define (toggle-show-kbd)
  (:synopsis "Toggle whether we show keyboard presses")
  (:check-mark "v" get-show-kbd)
  (set-show-kbd (not (get-show-kbd))))

(tm-define (clear-font-cache)
  (:synopsis "Clear font cache under TEXMACS_HOME_PATH and local cache path.")
  (system-remove (url-append (get-tm-cache-path) (string->url "font_cache.scm")))
  (map (lambda (x)
        (system-remove (url-append (get-tm-cache-path)
                        (url-append (string->url "fonts") (string->url x)))))
       (list "font-database.scm" "font-features.scm" "font-characteristics.scm")))

(tm-define (scan-disk-for-fonts)
  (:interactive #t)
  (:synopsis "Scan disk for more fonts")
  (system-wait "Full search for more fonts on your system"
               "(can be long)")
  (font-database-build-local))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (picture-gc)
  (picture-cache-reset)
  (update-all-buffers))
