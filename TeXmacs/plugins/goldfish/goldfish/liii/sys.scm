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

(define-library (liii sys)
  (export argv executable)
  (import (scheme process-context))
  (begin
#|
argv
返回一个程序命令行参数列表

语法
----
(argv)

参数
----
无

返回值
-----
list
例如：
> bin/goldfish demo/demo_argv.scm 
("bin/goldfish" "demo/demo_argv.scm")
返回一个列表，第一个表示的是命令行的命令名；第二个表示的是命令行第一个参数

功能
----
argv 用于获取命令行参数返回一个存储命令行参数的列表。


性能特征
-------
空间复杂度：O(1)：存储一个列表


|#

    (define (argv) (command-line))
    
#|
executable
用来返回程序可执行文件的绝对路径

语法
----
(executable)

参数
----
无

返回值
-----
string
返回Goldfish Scheme解释器的绝对路径

边界条件
--------
无

性能特征
-------
-时间复杂度：O(1)，函数只进行一次绝对路径的访问。
-空间复杂度：O(1)，函数只返回一个字符串。


|#

    (define (executable) (g_executable))

    ) ; end of begin
  ) ; end of define-library

