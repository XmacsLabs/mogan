
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : help-funcs.scm
;; DESCRIPTION : loading help files
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc help-funcs)
  (:use (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading help buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define help-file-path "$TEXMACS_DOC_PATH")
(define help-url-cache (make-ahash-table))
(define help-titles (make-ahash-table))
(define parse-times (make-ahash-table))

(define (parse-title u)
  (with t (tree-import u "texmacs")
    (with tt (select t '(:* (:or title doc-title tmdoc-title 
                                 tmdoc-title* tmweb-title) :%1))
      (if (null? tt) '() (car tt)))))

(tm-define (help-file-title u)
  (let ((mod-time (url-last-modified u))
        (parse-time (or (ahash-ref parse-times u) 0)))
    (if (> mod-time parse-time) ; handy: false also if url invalid
        (begin
          (ahash-set! parse-times u mod-time)
          (ahash-set! help-titles u (parse-title u))))
    (ahash-ref help-titles u)))

(tm-define (url-exists-in-help? s)
  (with entry (ahash-ref help-url-cache s)
    (if (list? entry)
      (car entry)
      (car (ahash-set! help-url-cache s
                       (list (url-exists? (url-unix help-file-path s))))))))

(define (url-resolve-help s)
  (if (and (in? (url-suffix s) '("tex" "tm" "tmu")) (url-exists? s))
      s
      (let* ((lan (string-take (language-to-locale (get-output-language)) 2)) 
             (suf (string-append "." lan ".tm"))
             (dir help-file-path))
        (cond ((url-exists? (url-unix dir (string-append s suf)))
               (url-resolve (url-unix dir (string-append s suf)) "r"))
              ((and (!= suf ".en.tm")
                    (url-exists? (url-unix dir (string-append s ".en.tm"))))
               (url-resolve (url-unix dir (string-append s ".en.tm")) "r"))
              (else (url-none))))))

(define (load-help-buffer-sub s type)
  (let ((name (url-resolve-help s)))
    (cond ((url-none? name)
           (set-message
            `(concat "Error: help file " (verbatim ,s) " not found")
            "load help file"))
          ((== type "book") (tmdoc-expand-help-manual name))
          (else (tmdoc-expand-help name type)))))

(tm-define (load-help-buffer s) (load-help-buffer-sub s "normal"))
(tm-define (load-help-article s) (load-help-buffer-sub s "article"))
(tm-define (load-help-book s) (load-help-buffer-sub s "book"))

(tm-define (load-help-online s)
  (load-help-buffer (url-append "https://www.texmacs.org/tmbrowse" s)))


(define (get-remote-welcome-url)
  (if (== (get-output-language) "chinese")
      "http://git.tmml.wiki/XmacsLabs/planet/raw/main/doc/welcome.zh.tm"
	  "http://git.tmml.wiki/XmacsLabs/planet/raw/main/doc/welcome.en.tm"))

(define (load-remote-welcome)
  (load-buffer (get-remote-welcome-url)))

(define (get-remote-planet-url)
  (let* ((lan (string-take (language-to-locale (get-output-language)) 2))
         (url "http://git.tmml.wiki/XmacsLabs/planet/raw/main/")
         (doc (string-append url "index." lan ".tmu"))
         (en_doc (string-append url "index.en.tmu")))
    (if (url-exists? doc)
      doc
      en_doc)))

(define (load-remote-planet)
  (load-buffer (get-remote-planet-url)))

(define (mogan-beamer-welcome)
  (let* ((lan (string-take (language-to-locale (get-output-language)) 2))
         (path (string-append "$TEXMACS_PATH/doc/about/mogan/beamer." lan ".tmu"))
         (en_doc (string-append "$TEXMACS_PATH/doc/about/mogan/beamer.en.tmu")))
    (if (url-exists? path)
        (load-buffer (system->url path))
        (load-buffer (system->url en_doc)))
    (delayed (:idle 25) (fit-to-screen-width))))

(define (mogan-research-welcome)
  (let* ((lan (string-take (language-to-locale (get-output-language)) 2))
         (path (string-append "$TEXMACS_PATH/doc/about/mogan/research." lan ".tmu"))
         (en_doc (string-append "$TEXMACS_PATH/doc/about/mogan/research.en.tmu")))
    (if (url-exists? path)
        (load-buffer (system->url path))
        (load-buffer (system->url en_doc)))))

(tm-define (mogan-welcome)
  (cond ((== (mogan-app-id) "beamer")
         (mogan-beamer-welcome))
        (else
         (mogan-research-welcome))))

(tm-define (xmacs-planet)
  (if (url-exists? (get-remote-planet-url))
      (load-remote-planet)
      (load-help-article "about/welcome/new-welcome")))

(tm-define (load-remote-doc path)
  (let* ((lan (string-take (language-to-locale (get-output-language)) 2))
         (lan_doc (string-append "http://git.tmml.wiki/texmacs/doc/raw/master/" path "." lan ".tm"))
         (en_doc (string-append "http://git.tmml.wiki/texmacs/doc/raw/master/" path ".en.tm")))
   (if (== (http-status-code lan_doc) 200)
       (load-buffer lan_doc)
       (load-buffer en_doc))))

(tm-define (load-local-plugin-doc name)
  (let* ((local_plugin_path (system->url "$TEXMACS_HOME_PATH"))
         (plugin_path (system->url "$TEXMACS_PATH"))
         (lan (string-take (language-to-locale (get-output-language)) 2))
         (path (string-append "plugins/" name "/doc/" name))
         (lan_doc
          (url-append plugin_path (string->url (string-append path "." lan ".tm"))))
         (local_lan_doc
          (url-append local_plugin_path (string->url (string-append path "." lan ".tm"))))
         (en_doc
          (url-append plugin_path (string->url (string-append path ".en.tm"))))
         (local_en_doc
          (url-append local_plugin_path (string->url (string-append path ".en.tm")))))
   (cond
    ((url-exists? local_lan_doc) (load-buffer local_lan_doc))
    ((url-exists? lan_doc) (load-buffer lan_doc))
    ((url-exists? local_en_doc) (load-buffer local_en_doc))
    (else (load-buffer en_doc)))))
