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

(import (srfi srfi-78))

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

(display "----------\n")
(display "check mode: report\n")

(check-set-mode! 'report)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-reset!)

(display "\n----------\n")
(display "check mode: off\n")

(check-set-mode! 'off)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-reset!)

(display "\n----------\n")
(display "check mode: report-failed\n")

(check-set-mode! 'report-failed)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-reset!)

(display "\n----------\n")
(display "check mode: summary\n")

(check-set-mode! 'summary)

(check (+ 1 2) => 3)

(check (+ 1 2) => 4)

(check-report)

(check-set-mode! 'report)

(check-report)

