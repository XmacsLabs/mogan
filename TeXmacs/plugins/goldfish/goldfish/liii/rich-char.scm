;
; Copyright (C) 2025 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii rich-char)
  (import (liii oop) (liii bitwise) (liii base) (liii string))
  (export rich-char)
  (begin

    (define-case-class rich-char ((data any?))
  
      (define code-point
        (cond ((char? data)
               (char->integer data))
              ((integer? data)
               (if (and (>= data 0) (<= data #x10FFFF))
                   data
                   (value-error "rich-char: code point out of range" data)))
              (else
               (type-error "rich-char: only accept char and integer"))))

      (define (%equals that)
        (cond ((char? that)
               (= code-point (char->integer that)))
              ((rich-char :is-type-of that)
               (= code-point (that :to-integer)))
              (else #f)))

      (define (%ascii?)
        (and (>= code-point 0) (<= code-point 127)))


      (define (%numeric?)
        (if (and (>= code-point 0) (<= code-point 255))
            (let ((ch (integer->char code-point)))
              (char-numeric? ch))
            #f))

      (define (%upper?)
        (and (>= code-point #x41) (<= code-point #x5A)))

      (define (%lower?)
        (and (>= code-point #x61) (<= code-point #x7A)))

      (define (%digit?)
        (or
         (and (>= code-point 48) (<= code-point 57))
         (and (>= code-point #xFF10) (<= code-point #xFF19))
         (and (>= code-point #x0660) (<= code-point #x0669))
         (and (>= code-point #x06F0) (<= code-point #x06F9))
         (and (>= code-point #x0966) (<= code-point #x096F))
         (and (>= code-point #x09E6) (<= code-point #x09EF))
         (and (>= code-point #x0A66) (<= code-point #x0A6F))
         (and (>= code-point #x0AE6) (<= code-point #x0AEF))
         (and (>= code-point #x0B66) (<= code-point #x0B6F))
         (and (>= code-point #x0BE6) (<= code-point #x0BEF))
         (and (>= code-point #x0C66) (<= code-point #x0C6F))
         (and (>= code-point #x0CE6) (<= code-point #x0CEF))
         (and (>= code-point #x0D66) (<= code-point #x0D6F))
         (and (>= code-point #x0E50) (<= code-point #x0E59))
         (and (>= code-point #x0ED0) (<= code-point #x0ED9))
         (and (>= code-point #x0F20) (<= code-point #x0F29))
         (and (>= code-point #x1040) (<= code-point #x1049))
         (and (>= code-point #x17E0) (<= code-point #x17E9))
         (and (>= code-point #x1810) (<= code-point #x1819))))
  
      (define (%to-upper . args)
        (chain-apply args
          (rich-char
            (if (and (>= code-point #x61) (<= code-point #x7A))
                (bitwise-and code-point #b11011111)
                code-point))))

      (define (%to-lower . args)
        (chain-apply args
          (rich-char
            (if (and (>= code-point #x41) (<= code-point #x5A))
                (bitwise-ior code-point #b00100000)
                code-point))))

      (define (%to-bytevector)
        (cond
          ((<= code-point #x7F)
           (bytevector code-point))

          ((<= code-point #x7FF)
           (let ((byte1 (bitwise-ior #b11000000 (bitwise-and (arithmetic-shift code-point -6) #b00011111)))
                 (byte2 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
             (bytevector byte1 byte2)))

          ((<= code-point #xFFFF)
           (let ((byte1 (bitwise-ior #b11100000 (bitwise-and (arithmetic-shift code-point -12) #b00001111)))
                 (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -6) #b00111111)))
                 (byte3 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
             (bytevector byte1 byte2 byte3)))

          ((<= code-point #x10FFFF)
           (let ((byte1 (bitwise-ior #b11110000 (bitwise-and (arithmetic-shift code-point -18) #b00000111)))
                 (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -12) #b00111111)))
                 (byte3 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift code-point -6) #b00111111)))
                 (byte4 (bitwise-ior #b10000000 (bitwise-and code-point #b00111111))))
             (bytevector byte1 byte2 byte3 byte4)))

          (else
           (value-error "Invalid code point"))))

      (define (@from-bytevector x)
        (define (utf8-byte-sequence->code-point byte-seq)
          (let ((len (bytevector-length byte-seq)))
            (cond
              ((= len 1)
               (bytevector-u8-ref byte-seq 0))
              ((= len 2)
               (let ((b1 (bytevector-u8-ref byte-seq 0))
                     (b2 (bytevector-u8-ref byte-seq 1)))
                 (bitwise-ior
                  (arithmetic-shift (bitwise-and b1 #x1F) 6)
                  (bitwise-and b2 #x3F))))
              ((= len 3)
               (let ((b1 (bytevector-u8-ref byte-seq 0))
                     (b2 (bytevector-u8-ref byte-seq 1))
                     (b3 (bytevector-u8-ref byte-seq 2)))
                 (bitwise-ior
                  (arithmetic-shift (bitwise-and b1 #x0F) 12)
                  (arithmetic-shift (bitwise-and b2 #x3F) 6)
                  (bitwise-and b3 #x3F))))
              ((= len 4)
               (let ((b1 (bytevector-u8-ref byte-seq 0))
                     (b2 (bytevector-u8-ref byte-seq 1))
                     (b3 (bytevector-u8-ref byte-seq 2))
                     (b4 (bytevector-u8-ref byte-seq 3)))
                 (bitwise-ior
                   (arithmetic-shift (bitwise-and b1 #x07) 18)
                   (arithmetic-shift (bitwise-and b2 #x3F) 12)
                   (arithmetic-shift (bitwise-and b3 #x3F) 6)
                   (bitwise-and b4 #x3F))))
              (else
               (value-error "Invalid UTF-8 byte sequence length")))))

        (rich-char (utf8-byte-sequence->code-point x)))

      (define (%to-string)
        (if (%ascii?)
            (object->string (integer->char code-point))
            (string-append "#\\" (utf8->string (%to-bytevector)))))

      (define (@from-string x)
        (when (not (string-starts? x "#\\"))
          (value-error "char@from-string: the input must start with #\\"))
        (if (= 1 ($ x :drop 2 :length))
            (rich-char :from-bytevector (string->utf8 ($ x :drop 2 :get)))
            (value-error "rich-char: must be u8 string which length equals 1")))

      (define (%make-string)
        (utf8->string (%to-bytevector)))

      (define (@from-integer x . args)
        (chain-apply args
          (rich-char x)))

      (define (%to-integer)
        code-point)

      )

    ) ; end of begin
  ) ; end of define-library