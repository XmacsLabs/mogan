;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(texmacs-module (binary elvish)
  (:use (binary common)))

(define (elvish-binary-candidates)
  (cond ((os-macos?)
         (list "/opt/homebrew/bin/elvish"
               "/usr/local/bin/elvish"))
        ((os-win32?)
         (list
          "$USERPROFILE\\scoop\\apps\\elvish\\current\\elvish.exe"))
        (else
         (list "/usr/bin/elvish"))))

(tm-define (find-binary-elvish)
  (:synopsis "Find the url to the elvish binary, return (url-none) if not found")
  (find-binary (elvish-binary-candidates) "elvish"))

(tm-define (has-binary-elvish?)
  (not (url-none? (find-binary-elvish))))

(tm-define (version-binary-elvish)
  (version-binary (find-binary-elvish)))

