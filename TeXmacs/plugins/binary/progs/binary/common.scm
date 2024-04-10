
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : common.scm
;; DESCRIPTION : routines for Binary plugins
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (binary common))

(define (find-binary-in-path name)
  (let* ((u (url-resolve-in-path name))
         (excluded? (and (os-win32?) (url-descends? u (system->url "C:\\Windows\\System32")))))
    (if excluded? (url-none) u)))

; If candidates name could bin found in path, return it, otherwise, return (url-none)
(define (find-binary-in-candidates-name candidates)
  (with names (list-remove-duplicates (map (lambda (x) (url->string (url-tail x))) candidates))
    (with u (list-find (map (lambda (x) (find-binary-in-path x)) names) url-exists?)
      (or u (url-none)))))

; If candidates exist, return it, otherwise, return #f
(define (find-binary-in-candidates candidates)
  (with u (list-find candidates (lambda (x) (url-exists? (url-resolve x "r"))))
    (and u (url-resolve u "r"))))

(define (find-binary-in-specified path)
  (with u (url-resolve path "r")
    (if (and (url-exists? u) (url-regular? u))
        u
        #f)))

(tm-define (find-binary candidates binary-id)
  (let* ((global-binary-opt (get-preference "plugin:binary"))
         (this-binary-opt (get-preference (string-append "plugin:binary:" binary-id))))
    (cond ((== global-binary-opt "off") (url-none))
          ((== this-binary-opt "off") (url-none))
          ((== this-binary-opt "candidates-only") (find-binary-in-candidates candidates))
          (else
           (or (and (!= this-binary-opt "default") (find-binary-in-specified this-binary-opt))
               (find-binary-in-candidates candidates)
               (find-binary-in-candidates-name candidates))))))

(tm-define (version-binary u)
  (if (url-none? u)
    ""
    (let*
     ((msg (check-stdout (string-append (url->system u) " --version")))
      (msg-l (filter (lambda (x) (not (string-null? x)))
                 (string-split msg #\newline))))
     (if (== (length msg-l) 0)
         ""
         (car msg-l)))))
