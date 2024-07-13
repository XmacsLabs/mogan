;
; Copyright (C) 2024 The S7 SRFI Authors
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

(import (srfi srfi-78)
        (srfi srfi-39))

(check-set-mode! 'report-failed)

(define mp (make-parameter "initial value"))

(check (mp) => "initial value")

(check
  (parameterize ((mp "new value"))
    (mp))
  =>
  "new value")

(check (mp) => "initial value")

