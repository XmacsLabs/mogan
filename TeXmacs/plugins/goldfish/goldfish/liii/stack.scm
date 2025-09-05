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

(define-library (liii stack)
  (import (liii rich-list) (liii oop))
  (export stack)
  (begin

    (define-case-class stack ((data list?))
#|
@empty
生成一个空栈对象。

语法
----
(stack :empty)

参数
----
无参数

返回值
-----
返回一个内部数据为空的链表的空栈对象。

功能
----
创建并返回一个没有任何元素的栈对象，作为栈数据结构的初始状态。返回的栈对象可以立即用于`push`、`pop`等操作，这些操作将在此空栈基础上进行。

边界条件
--------
- 函数始终返回有效的空栈对象，不会产生错误
- 空栈的 `:length` 和 `:size` 方法返回 0
- 空栈的 `:top` 和 `:pop` 方法将抛出 `out-of-range` 异常

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)
|#
      (define (@empty) (stack (list )))

#|
%length
获取栈中元素的数量。

语法
----
(stack :length)

参数
----
无参数

返回值
-----
返回一个非负整数，表示栈中当前存储的元素数量。

功能
----
返回栈中元素的数量，等同于%size方法。对于空栈返回0，对于非空栈返回元素个数。

边界条件
--------
- 空栈返回0
- 栈中有n个元素时返回n

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)
|#
      (define (%length) (length data))

#|
%size
获取栈中元素的数量。

语法
----
(stack :size)

参数
----
无参数

返回值
-----
返回一个非负整数，表示栈中当前存储的元素数量。

功能
----
返回栈中元素的数量，功能与%length方法完全一致，作为获取栈大小的别名方法。

边界条件
--------
- 空栈返回0
- 栈中有n个元素时返回n

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)
|#
      (define (%size) (length data))

#|
%top
获取栈顶元素。

语法
----
(stack :top)

参数
----
无参数

返回值
-----
返回栈顶部的元素，即最后压入栈的元素。

功能
----
返回栈顶元素但不将其移除。当栈中只有一个元素时，返回该元素。栈的操作遵循LIFO（后进先出）原则，因此返回的是最近压入的元素。

边界条件
--------
- 当栈为空时抛出 out-of-range 异常
- 当栈只有一个元素时返回该元素
- 当栈有多个元素时返回最顶端的元素

异常类型
--------
- out-of-range：栈为空时抛出的异常类型

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)
|#
      (define (%top)
        (if (null? data)
            (error 'out-of-range)
            (car data)))

#|
%pop
移除栈顶元素并返回新栈。

语法
----
(stack :pop)

参数
----
无参数

返回值
-----
返回移除了栈顶元素的新栈对象。原栈保持不变。

功能
----
创建并返回一个新的栈对象，该栈移除了当前栈的栈顶元素（最后压入的元素）。原栈对象不受影响，这是函数式风格的pop操作，返回的是新栈而不是修改原栈。

边界条件
--------
- 空栈pop时抛出 out-of-range 异常
- 单元素栈pop后返回空栈
- 多元素栈pop后返回移除栈顶元素的新栈

异常类型
--------
- out-of-range：栈为空时抛出的异常类型

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(n)，n为剩余元素数量
|#
      (chained-define (%pop)
        (if (null? data)
            (error 'out-of-range "Cannot pop from an empty stack")
            (stack (cdr data))))

#|
%pop!
移除栈顶元素并修改原栈。

语法
----
(stack :pop!)

参数
----
无参数

返回值
-----
返回修改后的原栈对象本身，支持链式调用。

功能
----
直接修改当前栈对象，移除其栈顶元素（最后压入的元素）。这是命令式风格的pop操作，会改变原栈的状态。与%pop的区别在于，%pop!修改的是原栈，而%pop返回新栈。

边界条件
--------
- 空栈pop!时抛出 out-of-range 异常
- 单元素栈pop!后变为空栈
- 多元素栈pop!后移除栈顶元素

异常类型
--------
- out-of-range：栈为空时抛出的异常类型

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)，直接在原栈上修改，不创建新对象

链式调用
--------
该方法返回原栈对象本身，因此支持链式调用，如：(stack :pop! :pop!)
|#
      (chained-define (%pop!)
        (if (null? data)
            (error 'out-of-range)
            (stack (set! data (cdr data))))
        (%this))

#|
%push
将元素压入栈顶并返回新栈。

语法
----
(stack :push element)

参数
----
element : any-type
要压入栈顶的任意类型元素。

返回值
-----
返回一个新的栈对象，该栈在栈顶添加了指定元素。原栈保持不变。

功能
----
创建并返回一个新的栈对象，该栈在栈顶压入指定的元素。这是函数式风格的push操作，原栈不受影响，支持链式调用。

边界条件
--------
- 空栈压入元素后变成单元素栈
- 已有元素栈压入后栈顶为新增元素
- 支持任意类型的数据作为栈元素

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(n+1)，n为原栈元素数量

链式调用
--------
支持链式调用，如：(stack :push 1 :push 2 :push 3)
|#
      (chained-define (%push element)
        (stack (cons element data)))

#|
%push!
将元素压入栈顶并修改原栈。

语法
----
(stack :push! element)

参数
----
element : any-type
要压入栈顶的任意类型元素。

返回值
-----
返回修改后的原栈对象本身，支持链式调用。

功能
----
直接修改当前栈对象，将指定元素压入到栈顶位置。这是命令式风格的push操作，会改变原栈的状态。与%push的区别在于，%push!修改的是原栈，而%push返回新栈。

边界条件
--------
- 空栈压入元素后变成单元素栈
- 已有元素栈压入后栈顶为新增元素
- 支持任意类型的数据作为栈元素

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)，直接在原栈上修改

链式调用
--------
该方法返回原栈对象本身，支持链式调用，如：(stack :push! 1 :push! 2 :push! 3)
|#
      (chained-define (%push! element) 
                      (stack (set! data (cons element data))) 
                      (%this))

#|
%to-list
将栈转换为链表表示。

语法
----
(stack :to-list)

参数
----
无参数

返回值
-----
返回一个链表，其中栈顶元素为链表的头部元素，栈底元素为链表尾部。

功能
----
将栈的内部数据结构（链表形式）直接作为Scheme链表返回。转换保持元素的顺序不变，栈顶元素是链表的第一个元素（car），栈底元素是链表的最后一个元素。

边界条件
--------
- 空栈转换后返回空链表 ()
- 单元素栈转换后返回单元素链表 (element)
- 多元素栈转换后返回对应顺序的链表

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)，直接返回内部数据引用
|#
      (define (%to-list) data)

#|
%to-rich-list
将栈转换为rich-list表示。

语法
----
(stack :to-rich-list)

参数
----
无参数

返回值
-----
返回一个rich-list对象，其中栈顶元素为rich-list的第一个元素，栈底元素为rich-list的最后一个元素。

功能
----
将栈的内部数据结构转换为rich-list对象，保持原始的LIFO顺序不变。转换后的rich-list可进一步使用rich-list的所有方法进行操作，如filter、map等。

边界条件
--------
- 空栈转换后返回空的rich-list对象
- 单元素栈转换后返回含该元素的rich-list
- 多元素栈转换后返回对应顺序的rich-list

性能特征
--------
- 时间复杂度：O(n)，其中n为栈内元素数量
- 空间复杂度：O(n)，需要创建新的rich-list对象
|#
      (define (%to-rich-list) (rich-list data))
      ) ; end of define-case-class
    ) ; end of begin
  ) ; end of define-library

