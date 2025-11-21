
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
         (list "/opt/homebrew/anaconda3/bin/conda"
               "/opt/anaconda3/bin/conda"
               "/opt/homebrew/miniconda3/bin/conda"
               "/opt/miniconda3/bin/conda"))
        ((os-win32?)
         (list "$USERPROFILE/anaconda3/Scripts/conda.exe"
               "$PROGRAMDATA/anaconda3/Scripts/conda.exe"
               "$USERPROFILE/miniconda3/Scripts/conda.exe"))
        (else
         (list "$HOME/anaconda3/bin/conda"
               "$HOME/miniconda3/bin/conda"))))

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
  (let* ((path (if (os-win32?) "envs/*/python.exe" "envs/*/bin/python"))
         (env-python-list
          (url->list
           (url-expand
            (url-complete (url-append (conda-prefix) path) "fr"))))
         (base-python-raw
           (url-append (conda-prefix)
                       (if (os-win32?) "python.exe" "bin/python")))
         (base-python (url-complete base-python-raw "fr"))
         (base-python-list
          (if (url-exists? base-python) (list base-python) (list))))
    (append base-python-list env-python-list)))

(define (cond-env-name-on-win u)
  (if (== (url-tail (url-head (url-head u)))
          (string->url "envs"))
      (url->string (url-tail (url-head u)))
      "base"))

(define (cond-env-name-on-nix u)
  (if (== (url-tail (url-head (url-head (url-head u))))
          (string->url "envs"))
      (url->string (url-tail (url-head u)))
      "base"))

(tm-define (conda-env-name u)
  (:synopsis "Extract the conda env name from the interpreter path")
  (if (os-win32?)
      (cond-env-name-on-win u)
      (cond-env-name-on-nix u)))
