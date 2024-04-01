
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-octave.scm
;; DESCRIPTION : Initialize octave plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (binary octave))

(define (octave-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (octave-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/octave")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/octave/octave/tmstart.m")
      (system-url->string "$TEXMACS_PATH/plugins/octave/octave/tmstart.m")))

(define (octave-launcher)
  (with boot (octave-entry)
    (string-append (url->system (find-binary-octave)) " -qi " boot)))

(plugin-configure octave
  (:require (has-binary-octave?))
  (:serializer ,octave-serialize)
  (:launch ,(octave-launcher))
  (:tab-completion #t)
  (:session "Octave"))

(when (supports-octave?)
  (plugin-input-converters octave))
