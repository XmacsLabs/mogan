
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : locale.scm
;; DESCRIPTION : Mapping of locale and language
;; COPYRIGHT   : (C) 2023 Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

(texmacs-module (language locale))

(define (locale-to-language-name locale)
  (cond ((== locale "bg_BG") "Bulgarian")
        ((== locale "cs_CZ") "Czech")
        ((== locale "da_DK") "Danish")
        ((== locale "de_DE") "German")
        ((== locale "en_US") "English (United States)")
        ((== locale "en_GB") "English (United Kingdom)")
        ((== locale "eo_EO") "Esperanto")
        ((== locale "es_ES") "Spanish")
        ((== locale "fi_FI") "Finnish")
        ((== locale "fr_FR") "French")
        ((== locale "gr_GR") "Greek")
        ((== locale "hr_HR") "Croatian")
        ((== locale "hu_HU") "Hungarian")
        ((== locale "it_IT") "Italian")
        ((== locale "ja_JP") "Japanese")
        ((== locale "ko_KR") "Korean")
        ((== locale "nl_NL") "Dutch")
        ((== locale "pl_PL") "Polish")
        ((== locale "pt_PT") "Portuguese")
        ((== locale "ro_RO") "Romanian")
        ((== locale "ru_RU") "Russian")
        ((== locale "sk_SK") "Slovak")
        ((== locale "sl_SI") "Slovenian")
        ((== locale "sv_SV") "Swedish")
        ((== locale "uk_UA") "Ukrainian")
        ((== locale "zh_CN") "Chinese")
        ((== locale "zh_TW") "Chinese (Taiwan)")
        (else "Unknown")))

(tm-define (language-to-language-name lan)
  (locale-to-language-name (language-to-locale lan)))
