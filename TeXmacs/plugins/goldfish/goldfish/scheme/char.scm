;
; Copyright (C) 2024 The Goldfish Scheme Authors
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

(define-library (scheme char)
(export
  char-upcase char-downcase char-upper-case? char-lower-case? digit-value
)
(begin
(define (digit-value ch)
  (if (char-numeric? ch)
      (- (char->integer ch) (char->integer #\0))
      #f))

) ; end of begin
) ; end of define-library

