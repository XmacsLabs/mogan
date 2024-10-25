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

(define-library (liii vector)
(import (srfi srfi-133)
        (liii base))
(export
  ; S7 Scheme built-in
  make-vector vector vector-length vector-ref vector-set! vector->list list->vector
  ; from (scheme base)
  vector-copy vector-fill! vector-copy! vector->string string->vector
  vector-map vector-for-each
  ; from (srfi srfi-133)
  vector-empty?
  vector-count
  vector-any vector-every vector-copy vector-copy!
  vector-index vector-index-right vector-partition
  vector-swap!)
(begin

) ; end of begin
) ; end of define-library

