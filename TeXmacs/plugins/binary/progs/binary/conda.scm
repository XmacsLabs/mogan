
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : conda.scm
;; DESCRIPTION : conda Binary plugin
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary conda)
  (:use (binary common)))

(define (conda-binary-candidates)
  (cond ((os-macos?)
         (list "$HOME/miniconda3/bin/conda"))
        ((os-win32?)
         (list "$USERPROFILE/miniconda3/Scripts/conda.exe"))
        (else
         (list "$HOME/miniconda3/bin/conda"))))

(tm-define (find-binary-conda)
  (:synopsis "Find the url to the conda binary, return (url-none) if not found")
  (find-binary (conda-binary-candidates) "conda"))

(tm-define (has-binary-conda?)
  (not (url-none? (find-binary-conda))))

(tm-define (version-binary-conda)
  (version-binary (find-binary-conda)))

(define (conda-prefix)
  (url-or
    (if (os-win32?)
        (system->url "$USERPROFILE/.conda")
        (system->url "$HOME/.conda"))
    (url-append (url-append (find-binary-conda) (url-parent)) (url-parent))))

(tm-define (conda-env-python-list)
  (with path (if (os-win32?) "envs/*/python.exe" "envs/*/bin/python")
    (url->list (url-expand (url-complete (url-append (conda-prefix) path) "fr")))))
