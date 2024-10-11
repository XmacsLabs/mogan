(import (liii check))
(import (liii base))

;("#00"	"#60")
;("#01"	"#B4")
;("#02"	"#02C6") ; modifier letter circumflex accent
;("#03"	"#02DC") ; small tilde
;("#04"	"#A8")
;("#05"	"#02DD")
;("#06"	"#02DA")
;("#07"	"#02C7")
;("#08"	"#02D8")
;("#09"	"#AF")
;("#0A"	"#02D9")
;("#0B"	"#B8")
;("#0C"	"#02DB")
;("#0D"	"#201A")
;("#0E"	"#2039")
;("#0F"	"#203A")

(define (test-herk-0x)
  (check (herk->utf8 (string #\x00)) => "`") ; U+0060
  (check (herk->utf8 (string #\x01)) => "´") ; U+00B4
  (check (herk->utf8 (string #\x02)) => "ˆ") ; U+02C6
  (check (herk->utf8 (string #\x03)) => "˜") ; U+02DC
  (check (herk->utf8 (string #\x04)) => "¨") ; U+00A8
  (check (herk->utf8 (string #\x05)) => "˝") ; U+02DD
  (check (herk->utf8 (string #\x06)) => "˚") ; U+02DA
  (check (herk->utf8 (string #\x07)) => "ˇ") ; U+02C7
  (check (herk->utf8 (string #\x08)) => "˘") ; U+02D8
  (check (herk->utf8 (string #\x09)) => "¯") ; U+00AF
  (check (herk->utf8 (string #\x0A)) => "˙") ; U+02D9
  (check (herk->utf8 (string #\x0B)) => "¸") ; U+00B8
  (check (herk->utf8 (string #\x0C)) => "˛") ; U+02DB
  (check (herk->utf8 (string #\x0D)) => "‚") ; U+201A
  (check (herk->utf8 (string #\x0E)) => "‹") ; U+2039
  (check (herk->utf8 (string #\x0F)) => "›") ; U+203A
)

(define (test-herk)
  (test-herk-0x))

(tm-define (test_66_13)
  (test-herk)
  (check-report))
