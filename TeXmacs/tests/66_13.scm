(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

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

(define (test-herk-1x)
  (check (herk->utf8 (string #\x10)) => "“") ; U+201C
  (check (herk->utf8 (string #\x11)) => "”") ; U+201D
  (check (herk->utf8 (string #\x12)) => "„") ; U+201E
  (check (herk->utf8 (string #\x13)) => "«") ; U+00AB
  (check (herk->utf8 (string #\x14)) => "»") ; U+00BB
  (check (herk->utf8 (string #\x15)) => "–") ; U+2013
  (check (herk->utf8 (string #\x16)) => "—") ; U+2014
  (check (herk->utf8 (string #\x17)) => (utf8->string #u8(#xE2 #x81 #xA0))) ; U+2060
  (check (herk->utf8 (string #\x19)) => "ı") ; U+0131
  (check (herk->utf8 (string #\x1B)) => "ﬀ") ; U+FB00
  (check (herk->utf8 (string #\x1C)) => "ﬁ") ; U+FB01
  (check (herk->utf8 (string #\x1D)) => "ﬂ") ; U+FB02
  (check (herk->utf8 (string #\x1E)) => "ﬃ") ; U+FB03
  (check (herk->utf8 (string #\x1F)) => "ﬄ") ; U+FB04
)

(define (test-herk)
  (test-herk-0x)
  (test-herk-1x))

(tm-define (test_66_13)
  (test-herk)
  (check-report))
