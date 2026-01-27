(import (liii os) (liii path) (liii sys) (liii string))

(display* "Start packing via package.sh...\n")

(display* "Start xmake config...\n")
(define config-ec (os-call "xmake config --yes -vD -m release --policies=build.ccache"))
(if (= config-ec 0)
    (display* "Config finished.\n")
    (exit config-ec))

(display* "Start xmake build...\n")
(define build-ec (os-call "xmake build -vD stem"))
(if (= build-ec 0)
    (display* "Build finished.\n")
    (exit build-rc))

(display* "Start determine VERSION...\n")
(define raw-args (cddr (argv)))
(define RAW_VERSION
  (if (and raw-args (not (null? raw-args)))
      (car raw-args)
      (getenv "VERSION" "2026")))

(if (string=? RAW_VERSION "2026")
    (display* "Can't detected VERSION, use VERSION=2026\n")
    (display* (string-append "Detected VERSION: " VERSION "\n")))

(define VERSION "")
(if (string-starts? RAW_VERSION "v")
    (set! VERSION (substring RAW_VERSION 1 (- (string-length RAW_VERSION) 1)))
    (set! VERSION RAW_VERSION))

(define chmod-ec (os-call "chmod +x ./package.sh"))
(when (not (= chmod-ec 0))
      (exit chmod-ec))

(os-call (string-append "export VERSION=" VERSION))

(define package-ec (os-call "bash ./package.sh"))
(when (not (= package-ec 0))
      (exit package-ec))

(define PACK_NAME (string-append "mogan-stem-" VERSION "-debian13-amd64.deb"))
(define mv-ec (os-call (string-append "mv ../../../mogan-stem_*.deb ../../" PACK_NAME)))
(when (not (= mv-ec 0))
      (exit mv-ec))

(display* "Pack has already been placed in mogan root directory.\n")

(display* "Packaging finished.\n")

