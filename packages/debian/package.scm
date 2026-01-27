(import (liii os) (liii path) (liii sys) (liii string) (liii list))

(define (get-script-dir)
  (let* ((args (argv))
         (script-path (if (and (list? args) (> (length args) 1))
                          (cadr args)
                          "")))
    (if (string-contains script-path "/")
        (let ((last-slash (string-index-right script-path (lambda (c) (char=? c #\/)))))
          (if last-slash
              (substring script-path 0 last-slash)
              "."))
        ".")))

(define (call-or-quit . params)
  (define ec (os-call (string-join params " ")))
  (when (not (= ec 0))
        (exit ec)))

(define (extract-version-from-file file-path)
  (let* ((content (path-read-text file-path))
         (has-version (string-contains content "XMACS_VERSION")))
    (if has-version
        (let* ((version-start (string-index content (lambda (c) (char=? c #\X))))
               (quote-start-pos (string-index content (lambda (c) (char=? c #\")) version-start)))
          (if quote-start-pos
              (let ((quote-end-pos (string-index content (lambda (c) (char=? c #\")) (+ quote-start-pos 1))))
                (if quote-end-pos
                    (substring content (+ quote-start-pos 1) quote-end-pos)
                    #f))
              #f))
        #f)))

(define PACKAGE_HOME (get-script-dir))
(define VERSION (extract-version-from-file (string-append PACKAGE_HOME "/../../xmake/vars.lua")))

(display* "Start packing via package.sh...\n")

(display* "Start xmake config...\n")
(call-or-quit "xmake" "config" "--yes" "-vD" "-m" "release" "--policies=build.ccache")
(display* "Config finished.\n")

(display* "Start xmake build...\n")
(call-or-quit "xmake" "build" "-vD" "stem")
(display* "Build finished.\n")

(call-or-quit "chmod" "+x" (string-append PACKAGE_HOME "/./package.sh"))

(call-or-quit "env" (string-append "VERSION=" VERSION)
              "bash" (string-append PACKAGE_HOME "/./package.sh"))

(define PACK_NAME (string-append "mogan-stem-" VERSION "-debian13-amd64.deb"))
(call-or-quit "mv" (string-append PACKAGE_HOME "/../../../mogan-stem_*.deb") (string-append PACKAGE_HOME "/../../" PACK_NAME))

(display* "Pack has already been placed in mogan root directory.\n")

(display* "Packaging finished.\n")

