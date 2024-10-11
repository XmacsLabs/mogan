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

(define (test-herk-3x)
  (check (herk->utf8 (string #\x30)) => "0")
  (check (herk->utf8 (string #\x31)) => "1")
  (check (herk->utf8 (string #\x32)) => "2")
  (check (herk->utf8 (string #\x33)) => "3")
  (check (herk->utf8 (string #\x34)) => "4")
  (check (herk->utf8 (string #\x35)) => "5")
  (check (herk->utf8 (string #\x36)) => "6")
  (check (herk->utf8 (string #\x37)) => "7")
  (check (herk->utf8 (string #\x38)) => "8")
  (check (herk->utf8 (string #\x39)) => "9")
  (check (herk->utf8 (string #\x3A)) => ":")
  (check (herk->utf8 (string #\x3B)) => ";")
  (check (herk->utf8 (string #\x3D)) => "=")
  (check (herk->utf8 (string #\x3F)) => "?")
)

(define (test-herk-4x)
  (check (herk->utf8 (string #\x40)) => "@")
  (check (herk->utf8 (string #\x41)) => "A")
  (check (herk->utf8 (string #\x42)) => "B")
  (check (herk->utf8 (string #\x43)) => "C")
  (check (herk->utf8 (string #\x44)) => "D")
  (check (herk->utf8 (string #\x45)) => "E")
  (check (herk->utf8 (string #\x46)) => "F")
  (check (herk->utf8 (string #\x47)) => "G")
  (check (herk->utf8 (string #\x48)) => "H")
  (check (herk->utf8 (string #\x49)) => "I")
  (check (herk->utf8 (string #\x4A)) => "J")
  (check (herk->utf8 (string #\x4B)) => "K")
  (check (herk->utf8 (string #\x4C)) => "L")
  (check (herk->utf8 (string #\x4D)) => "M")
  (check (herk->utf8 (string #\x4E)) => "N")
  (check (herk->utf8 (string #\x4F)) => "O")
)

(define (test-herk-5x)
  (check (herk->utf8 (string #\x50)) => "P")
  (check (herk->utf8 (string #\x51)) => "Q")
  (check (herk->utf8 (string #\x52)) => "R")
  (check (herk->utf8 (string #\x53)) => "S")
  (check (herk->utf8 (string #\x54)) => "T")
  (check (herk->utf8 (string #\x55)) => "U")
  (check (herk->utf8 (string #\x56)) => "V")
  (check (herk->utf8 (string #\x57)) => "W")
  (check (herk->utf8 (string #\x58)) => "X")
  (check (herk->utf8 (string #\x59)) => "Y")
  (check (herk->utf8 (string #\x5A)) => "Z")
  (check (herk->utf8 (string #\x5B)) => "[")
  (check (herk->utf8 (string #\x5C)) => "\\")
  (check (herk->utf8 (string #\x5D)) => "]")
  (check (herk->utf8 (string #\x5E)) => "^")
  (check (herk->utf8 (string #\x5F)) => "_")
)

(define (test-herk-6x)
  (check (herk->utf8 (string #\x60)) => "‘")
  (check (herk->utf8 (string #\x61)) => "a")
  (check (herk->utf8 (string #\x62)) => "b")
  (check (herk->utf8 (string #\x63)) => "c")
  (check (herk->utf8 (string #\x64)) => "d")
  (check (herk->utf8 (string #\x65)) => "e")
  (check (herk->utf8 (string #\x66)) => "f")
  (check (herk->utf8 (string #\x67)) => "g")
  (check (herk->utf8 (string #\x68)) => "h")
  (check (herk->utf8 (string #\x69)) => "i")
  (check (herk->utf8 (string #\x6A)) => "j")
  (check (herk->utf8 (string #\x6B)) => "k")
  (check (herk->utf8 (string #\x6C)) => "l")
  (check (herk->utf8 (string #\x6D)) => "m")
  (check (herk->utf8 (string #\x6E)) => "n")
  (check (herk->utf8 (string #\x6F)) => "o")
)

(define (test-herk-7x)
  (check (herk->utf8 (string #\x70)) => "p")
  (check (herk->utf8 (string #\x71)) => "q")
  (check (herk->utf8 (string #\x72)) => "r")
  (check (herk->utf8 (string #\x73)) => "s")
  (check (herk->utf8 (string #\x74)) => "t")
  (check (herk->utf8 (string #\x75)) => "u")
  (check (herk->utf8 (string #\x76)) => "v")
  (check (herk->utf8 (string #\x77)) => "w")
  (check (herk->utf8 (string #\x78)) => "x")
  (check (herk->utf8 (string #\x79)) => "y")
  (check (herk->utf8 (string #\x7A)) => "z")
  (check (herk->utf8 (string #\x7B)) => "{")
  (check (herk->utf8 (string #\x7C)) => "|")
  (check (herk->utf8 (string #\x7D)) => "}")
  (check (herk->utf8 (string #\x7E)) => "~")
  (check (herk->utf8 (string #\x7F)) => "‐") ; U+2010
)

(define (test-herk-8x)
  (check (herk->utf8 (string #\x80)) => "Ă") ; U+0102
  (check (herk->utf8 (string #\x81)) => "Ą")
  (check (herk->utf8 (string #\x82)) => "Ć")
  (check (herk->utf8 (string #\x83)) => "Č")
  (check (herk->utf8 (string #\x84)) => "Ď")
  (check (herk->utf8 (string #\x85)) => "Ě")
  (check (herk->utf8 (string #\x86)) => "Ę")
  (check (herk->utf8 (string #\x87)) => "Ğ")
  (check (herk->utf8 (string #\x88)) => "Ĺ")
  (check (herk->utf8 (string #\x89)) => "Ľ")
  (check (herk->utf8 (string #\x8A)) => "Ł")
  (check (herk->utf8 (string #\x8B)) => "Ń")
  (check (herk->utf8 (string #\x8C)) => "Ň")
  (check (herk->utf8 (string #\x8D)) => "Ŋ")
  (check (herk->utf8 (string #\x8E)) => "Ő")
  (check (herk->utf8 (string #\x8F)) => "Ŕ")
)

(define (test-herk-9x)
  (check (herk->utf8 (string #\x90)) => "Ř")  ; U+0158
  (check (herk->utf8 (string #\x91)) => "Ś")  ; U+015A
  (check (herk->utf8 (string #\x92)) => "Š")  ; U+0162
  (check (herk->utf8 (string #\x93)) => "Ş")  ; U+015E
  (check (herk->utf8 (string #\x94)) => "Ť")  ; U+0164
  (check (herk->utf8 (string #\x95)) => "Ţ")  ; U+0166
  (check (herk->utf8 (string #\x96)) => "Ű")  ; U+0170
  (check (herk->utf8 (string #\x97)) => "Ů")  ; U+016E
  (check (herk->utf8 (string #\x98)) => "Ÿ")  ; U+0178
  (check (herk->utf8 (string #\x99)) => "Ź")  ; U+0178 with diaeresis
  (check (herk->utf8 (string #\x9A)) => "Ž")  ; U+017D
  (check (herk->utf8 (string #\x9B)) => "Ż")  ; U+017B
  (check (herk->utf8 (string #\x9C)) => "Ĳ")  ; U+0132
  (check (herk->utf8 (string #\x9D)) => "İ")  ; U+0130
  (check (herk->utf8 (string #\x9E)) => "đ")  ; U+0111
  (check (herk->utf8 (string #\x9F)) => "§")  ; U+00A7
)

(define (test-herk-Ax)
  (check (herk->utf8 (string #\xA0)) => "ă")
  (check (herk->utf8 (string #\xA1)) => "ą")
  (check (herk->utf8 (string #\xA2)) => "ć")
  (check (herk->utf8 (string #\xA3)) => "č")
  (check (herk->utf8 (string #\xA4)) => "ď")
  (check (herk->utf8 (string #\xA5)) => "ě")
  (check (herk->utf8 (string #\xA6)) => "ę")
  (check (herk->utf8 (string #\xA7)) => "ğ")
  (check (herk->utf8 (string #\xA8)) => "ĺ")
  (check (herk->utf8 (string #\xA9)) => "ľ")
  (check (herk->utf8 (string #\xAA)) => "ł")
  (check (herk->utf8 (string #\xAB)) => "ń")
  (check (herk->utf8 (string #\xAC)) => "ň")
  (check (herk->utf8 (string #\xAD)) => "ŋ")
  (check (herk->utf8 (string #\xAE)) => "ő")
  (check (herk->utf8 (string #\xAF)) => "ŕ")
)

(define (test-herk-Bx)
  (check (herk->utf8 (string #\xB0)) => "ř")
  (check (herk->utf8 (string #\xB1)) => "ś")
  (check (herk->utf8 (string #\xB2)) => "š")
  (check (herk->utf8 (string #\xB3)) => "ş")
  (check (herk->utf8 (string #\xB4)) => "ť")
  (check (herk->utf8 (string #\xB5)) => "ţ")
  (check (herk->utf8 (string #\xB6)) => "ű")
  (check (herk->utf8 (string #\xB7)) => "ů")
  (check (herk->utf8 (string #\xB8)) => "ÿ")
  (check (herk->utf8 (string #\xB9)) => "ź")
  (check (herk->utf8 (string #\xBA)) => "ž")
  (check (herk->utf8 (string #\xBB)) => "ż")
  (check (herk->utf8 (string #\xBC)) => "ĳ")
  (check (herk->utf8 (string #\xBD)) => "¡")
  (check (herk->utf8 (string #\xBE)) => "¿")
  (check (herk->utf8 (string #\xBF)) => "£")
)

(define (test-herk-Cx)
  (check (herk->utf8 (string #\xC0)) => "À")
  (check (herk->utf8 (string #\xC1)) => "Á")
  (check (herk->utf8 (string #\xC2)) => "Â")
  (check (herk->utf8 (string #\xC3)) => "Ã")
  (check (herk->utf8 (string #\xC4)) => "Ä")
  (check (herk->utf8 (string #\xC5)) => "Å")
  (check (herk->utf8 (string #\xC6)) => "Æ")
  (check (herk->utf8 (string #\xC7)) => "Ç")
  (check (herk->utf8 (string #\xC8)) => "È")
  (check (herk->utf8 (string #\xC9)) => "É")
  (check (herk->utf8 (string #\xCA)) => "Ê")
  (check (herk->utf8 (string #\xCB)) => "Ë")
  (check (herk->utf8 (string #\xCC)) => "Ì")
  (check (herk->utf8 (string #\xCD)) => "Í")
  (check (herk->utf8 (string #\xCE)) => "Î")
  (check (herk->utf8 (string #\xCF)) => "Ï")
)

(define (test-herk-Dx)
  (check (herk->utf8 (string #\xD0)) => "Ð")
  (check (herk->utf8 (string #\xD1)) => "Ñ")
  (check (herk->utf8 (string #\xD2)) => "Ò")
  (check (herk->utf8 (string #\xD3)) => "Ó")
  (check (herk->utf8 (string #\xD4)) => "Ô")
  (check (herk->utf8 (string #\xD5)) => "Õ")
  (check (herk->utf8 (string #\xD6)) => "Ö")
  (check (herk->utf8 (string #\xD7)) => "Œ")
  (check (herk->utf8 (string #\xD8)) => "Ø")
  (check (herk->utf8 (string #\xD9)) => "Ù")
  (check (herk->utf8 (string #\xDA)) => "Ú")
  (check (herk->utf8 (string #\xDB)) => "Û")
  (check (herk->utf8 (string #\xDC)) => "Ü")
  (check (herk->utf8 (string #\xDD)) => "Ý")
  (check (herk->utf8 (string #\xDE)) => "Þ")
  (check (herk->utf8 (string #\xDF)) => "ẞ")
)

(define (test-herk-Ex)
  (check (herk->utf8 (string #\xE0)) => "à")
  (check (herk->utf8 (string #\xE1)) => "á")
  (check (herk->utf8 (string #\xE2)) => "â")
  (check (herk->utf8 (string #\xE3)) => "ã")
  (check (herk->utf8 (string #\xE4)) => "ä")
  (check (herk->utf8 (string #\xE5)) => "å")
  (check (herk->utf8 (string #\xE6)) => "æ")
  (check (herk->utf8 (string #\xE7)) => "ç")
  (check (herk->utf8 (string #\xE8)) => "è")
  (check (herk->utf8 (string #\xE9)) => "é")
  (check (herk->utf8 (string #\xEA)) => "ê")
  (check (herk->utf8 (string #\xEB)) => "ë")
  (check (herk->utf8 (string #\xEC)) => "ì")
  (check (herk->utf8 (string #\xED)) => "í")
  (check (herk->utf8 (string #\xEE)) => "î")
  (check (herk->utf8 (string #\xEF)) => "ï")
)

(define (test-herk-Fx)
  (check (herk->utf8 (string #\xF0)) => "ð")
  (check (herk->utf8 (string #\xF1)) => "ñ")
  (check (herk->utf8 (string #\xF2)) => "ò")
  (check (herk->utf8 (string #\xF3)) => "ó")
  (check (herk->utf8 (string #\xF4)) => "ô")
  (check (herk->utf8 (string #\xF5)) => "õ")
  (check (herk->utf8 (string #\xF6)) => "ö")
  (check (herk->utf8 (string #\xF7)) => "œ")
  (check (herk->utf8 (string #\xF8)) => "ø")
  (check (herk->utf8 (string #\xF9)) => "ù")
  (check (herk->utf8 (string #\xFA)) => "ú")
  (check (herk->utf8 (string #\xFB)) => "û")
  (check (herk->utf8 (string #\xFC)) => "ü")
  (check (herk->utf8 (string #\xFD)) => "ý")
  (check (herk->utf8 (string #\xFE)) => "þ")
  (check (herk->utf8 (string #\xFF)) => "ß")
)

(define (test-herk)
  (test-herk-0x)
  (test-herk-1x)
  (test-herk-3x)
  (test-herk-3x)
  (test-herk-4x)
  (test-herk-5x)
  (test-herk-6x)
  (test-herk-7x)
  (test-herk-8x)
  (test-herk-9x)
  (test-herk-Ax)
  (test-herk-Bx)
  (test-herk-Cx)
  (test-herk-Dx)
  (test-herk-Ex)
  (test-herk-Fx))

(tm-define (test_66_13)
  (test-herk)
  (check-report))
