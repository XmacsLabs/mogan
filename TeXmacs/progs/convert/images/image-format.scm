
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-images.scm
;; DESCRIPTION : setup image converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert images image-format))

(define-preferences
  ("texmacs->image:raster-resolution" "300" noop))

(tm-define (get-raster-resolution opts)
  (or (assoc-ref opts "texmacs->image:raster-resolution")
      (get-preference "texmacs->image:raster-resolution")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphical document and geometric image formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format postscript
  (:name "Postscript")
  (:suffix "ps" "eps"))

(define-format pdf
  (:name "Pdf")
  (:suffix "pdf"))

(define-format svg
   (:name "Svg")
   (:suffix "svg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bitmap image formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format xpm
  (:name "Xpm")
  (:suffix "xpm"))

(define-format jpeg
  (:name "Jpeg")
  (:suffix "jpg" "jpeg"))

(define-format tif
  (:name "Tif")
  (:suffix "tif" "tiff"))

(define-format ppm
  (:name "Ppm")
  (:suffix "ppm"))

(define-format gif
  (:name "Gif")
  (:suffix "gif"))

(define-format png
  (:name "Png")
  (:suffix "png"))

(define-format webp
  (:name "WebP")
  (:suffix "webp"))
