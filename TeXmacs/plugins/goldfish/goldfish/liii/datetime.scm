;
; Copyright (C) 2024-2025 The Goldfish Scheme Authors
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

(define-library (liii datetime)
  (import (liii oop) (liii error) (liii string))
  (export datetime date years days)
  (begin

    (define-object years

#|
datetime@leap?
判断指定年份是否为闰年。

语法
----
(years :leap? year)

参数
----
year:integer
待判断的年份。

返回值
-----
boolean
若指定年份为闰年则返回#t，否则返回#f。

错误
----
type-error
若 year 不是整数，则引发类型错误。

额外信息
----
闰年判定规则
能被 4 整除但不能被 100 整除 → 闰年（如 2024）
能被 400 整除 → 闰年（如 2000）
其他情况 → 非闰年（如 2025, 1000）

|#

      (define (@leap? year)
        (when (not (integer? year))
          (type-error "years@leap? must accept integer"))
        (or (and (zero? (modulo year 4)) 
                 (not (zero? (modulo year 100))))
            (zero? (modulo year 400))))

      )

#|
datetime%weekday
计算当前日期是星期几。

语法
----
(datetime-object :weekday)

参数
----
无参数

返回值
------
整数 (0-6)
0 表示星期一 (Monday)，1 表示星期二 (Tuesday)，...，6 表示星期日 (Sunday)。

使用示例
--------
((datetime :year 2024 :month 1 :day 1) :weekday) => 0  ; 2024年1月1日是星期一

额外信息
------
这是基于 Zeller 公式的变体的计算结果，已调整为周一为起始日。
|#

#|
datetime@is-type-of
判断给定对象是否为 datetime 类型的实例。
通过 define-case-class 实现的类型检查方法，无需额外实现。

语法
----
(datetime :is-type-of obj)

参数
----
obj:any
待检查的对象，可以是任何Goldfish Scheme值。

返回值
-----
boolean
若对象obj为datetime类型实例则返回#t，否则返回#f。

额外信息
----
这是通过 define-case-class 宏自动生成的方法，所有案例类都具备类似功能。
|#

    (define (weekday-for-date year month day)
      (let* ((m (if (> month 2) month (+ month 12)))
             (y (if (> month 2) year (- year 1)))
             (century (quotient y 100))
             (year-of-century (remainder y 100)))
        (modulo
         (- (+ day
               (quotient (+ (* 13 (- m 2)) 13) 5)
               year-of-century
               (quotient year-of-century 4)
               (quotient century 4)
               (* 5 century) 6)
            3)
         7)))

    (define-case-class datetime
      ((year integer?)
       (month integer?)
       (day integer?)
       (hour integer? 0)
       (minute integer? 0)
       (second integer? 0)
       (micro-second integer? 0))

#|
datetime@now
创建一个表示当前系统时间的日期时间对象。
该对象精确到微秒级别，可用于获取当前时间的各个时间分量。

语法
----
(datetime :now)

参数
----
无参数（使用 :now 关键字创建当前时间对象）

返回值
-----
返回一个表示当前日期时间的对象，该对象支持以下字段查询：
'year         : 年份 (>= 2023)
'month        : 月份 (1-12)
'day          : 日期 (1-31)
'hour         : 小时 (0-23)
'minute       : 分钟 (0-59)
'second       : 秒   (0-59)
'micro-second : 微秒 (0-999999)

错误
----
无特定错误（始终返回有效时间对象）

|#

      (chained-define (@now)
        (let ((time-vec (g_datetime-now)))
          (datetime 
            :year (vector-ref time-vec 0)
            :month (vector-ref time-vec 1)
            :day (vector-ref time-vec 2)
            :hour (vector-ref time-vec 3)
            :minute (vector-ref time-vec 4)
            :second (vector-ref time-vec 5)
            :micro-second (vector-ref time-vec 6))))

#|
datetime%to-string
将 datetime 对象格式化为标准字符串表示。
当微秒为0时，返回 "YYYY-MM-DD HH:MM:SS"，
当微秒非0时，返回 "YYYY-MM-DD HH:MM:SS.MMMMMM" （6位微秒）。

语法
----
(datetime-object :to-string)

参数
----
无参数（直接调用对象方法）。

返回值
-----
返回日期时间字符串：
日期部分：年-月-日
时间部分：时:分:秒
微秒部分：.6位微秒数（不足6位补零）

错误
----
如果调用对象不是有效的 datetime 类型，抛出类型错误。

格式规则
------
|  字段  |   格式化规则   |  示例  |
|--------|----------------|--------|
|   年   |    4位数字     |  2025  |
|  月/日 | 2位数字（补零）|  01,09 |
|时/分/秒| 2位数字（补零）|  00,05 |
|  微秒  | 6位数字（补零）| 000001 |

|#

      (define (%to-string)
        (define (pad6 n)  ; 补零到 6 位（微秒）
          (let ((s (number->string n)))
            (string-append (make-string (- 6 (string-length s)) #\0) s)))
        (if (zero? micro-second)
            (%format "yyyy-MM-dd HH:mm:ss")
            (string-append (%format "yyyy-MM-dd HH:mm:ss") "." (pad6 micro-second))))

#|
datetime%plus-days
计算当前日期增加/减少指定天数后的新日期对象。

语法
----
(datetime-object :plus-days days)

参数
----
days:integer
整数，表示要增加的天数（正数）或减少的天数（负数）。

返回值
-----
datetime
新的日期时间对象。

错误
----
type-error
若 days 不是整数，则引发类型错误。

额外信息
----
能自动识别闰年（如 2024）与非闰年（如 2023）
跨月时自动调整月份/年份
跨年时自动递增/递减年份
days=0 时返回原日期副本

|#

      (define (%plus-days days-to-add)
  
        (define (days-in-month m y)
          (cond ((member m '(4 6 9 11)) 30)
                ((= m 2) (if (years :leap? y) 29 28))
                (else 31)))
  
        (let loop ((y year)
                   (m month)
                   (d day)
                   (remaining-days days-to-add))
          (cond
            ;; No more days to add
            ((zero? remaining-days)
             (datetime :year y
                       :month m
                       :day d
                       :hour hour
                       :minute minute
                       :second second
                       :micro-second micro-second))
      
            ;; Adding days (positive)
            ((> remaining-days 0)
             (let ((days-in-current-month (days-in-month m y)))
               (if (<= (+ d remaining-days) days-in-current-month)
                   ;; Simple case: result is within the same month
                   (loop y m (+ d remaining-days) 0)
                   ;; Complex case: need to move to next month
                   (let ((next-month (if (= m 12) 1 (+ m 1)))
                         (next-year (if (= m 12) (+ y 1) y)))
                     (loop next-year 
                           next-month 
                           1 
                           (- (+ remaining-days d) days-in-current-month 1))))))
      
            ;; Subtracting days (negative)
            (else
             (if (> d (abs remaining-days))
                 ;; Simple case: result is within the same month
                 (loop y m (+ d remaining-days) 0)
                 ;; Complex case: need to move to previous month
                 (let* ((prev-month (if (= m 1) 12 (- m 1)))
                        (prev-year (if (= m 1) (- y 1) y))
                        (days-in-prev-month (days-in-month prev-month prev-year)))
                   (loop prev-year 
                         prev-month 
                         days-in-prev-month 
                         (+ remaining-days d))))))))

#|
datetime%plus-months
计算当前日期增加/减少指定月数后的新日期对象，自动处理月末日期调整。

语法
----
(datetime-object :plus-months months)

参数
----
months:integer
整数，表示要增加的月数（正数）或减少的月数（负数）。

返回值
-----
datetime
新的日期时间对象。

错误
----
type-error
若 months 不是整数，则引发类型错误。

额外信息
----
当原始日期是月末时，结果自动调整为目标月份的最后一天
跨年时自动调整年份
二月天数根据目标年份的闰年状态自动确定
months=0 时返回原日期副本

|#

      (define (%plus-months months-to-add)
        (define (days-in-month m y)
          (cond ((member m '(4 6 9 11)) 30)
                ((= m 2) (if (years :leap? y) 29 28))
                (else 31)))
  
        ;; Calculate new year and month
        (let* ((total-months (+ (+ (* year 12) month -1) months-to-add))
               (new-year (quotient total-months 12))
               (new-month (+ (remainder total-months 12) 1))
               ;; Adjust day if necessary
               (days-in-new-month (days-in-month new-month new-year))
               (new-day (min day days-in-new-month)))
    
          (datetime :year new-year
                    :month new-month
                    :day new-day
                    :hour hour
                    :minute minute
                    :second second
                    :micro-second micro-second)))

#|
datetime%plus-years
计算当前日期增加/减少指定年数后的新日期对象。

语法
----
(datetime-object :plus-years years)

参数
----
years:integer
整数，表示要增加的年（正数）或减少的年（负数）。

返回值
-----
datetime
新的日期时间对象。

错误
----
type-error
若 years 不是整数，则引发类型错误。

额外信息
----
能自动识别闰年（如 2024）与非闰年（如 2023）；
当原始日期为闰年2月29日且目标年份非闰年时，日期将调整为2月28日；
years=0 时返回原日期副本。

|#

      (define (%plus-years years-to-add)
        (define (days-in-month m y)
          (cond ((member m '(4 6 9 11)) 30)
                ((= m 2) (if (years :leap? y) 29 28))
                (else 31)))
  
        ;; Calculate new year
        (let* ((new-year (+ year years-to-add))
               ;; Adjust day if necessary (for Feb 29 in leap years)
               (days-in-new-month (days-in-month month new-year))
               (new-day (min day days-in-new-month)))
    
          (datetime :year new-year
                    :month month
                    :day new-day
                    :hour hour
                    :minute minute
                    :second second
                    :micro-second micro-second)))

      (define (%weekday)
        (weekday-for-date year month day))

#|
datetime%to-date
将 datetime 对象转换为 date 对象，保留年月日信息，丢弃时间信息。

语法
----
(datetime-object :to-date)

参数
----
无参数

返回值
-----
date
一个新的 date 对象，包含原 datetime 的年、月、日信息

使用示例
--------
(let ((dt (datetime :year 2024 :month 1 :day 15 :hour 12 :minute 30 :second 45)))
  (dt :to-date)) => (date :year 2024 :month 1 :day 15)

额外信息
----
与Java 8 DateTime API中的 LocalDateTime.toLocalDate() 类似，是datetime的重要转换方法
|#

#|
datetime%format
按照指定的格式字符串格式化日期时间。

语法
----
(datetime-object :format format-string)

参数
----
format-string:string
  格式字符串，支持以下格式符：
  - yyyy: 4位年份
  - MM: 2位月份（01-12）
  - dd: 2位日期（01-31）
  - HH: 2位小时（00-23）
  - mm: 2位分钟（00-59）
  - ss: 2位秒（00-59）
  - SSS: 3位毫秒（000-999）

返回值
-----
string
格式化后的日期时间字符串。

错误
----
value-error
如果格式字符串无效则抛出该异常。

使用示例
--------
((datetime :year 2024 :month 1 :day 15 :hour 14 :minute 30 :second 45 :micro-second 123456) :format "yyyy-MM-dd HH:mm:ss.SSS")
=> "2024-01-15 14:30:45.123"

((datetime :year 2024 :month 1 :day 15) :format "yyyy-MM-dd")
=> "2024-01-15"

只要格式字符串中使用到年月日时分秒毫秒其中一个字段，就算格式正确。
|#

      (define (%format format-str)
        (define (pad2 n)
          (if (< n 10)
              (string-append "0" (number->string n))
              (number->string n)))
        
        (define (pad3 n)
          (let ((s (number->string (quotient n 1000)))) ; Convert micro-second to milli-second
            (let ((len (string-length s)))
              (cond ((>= len 3) (substring s 0 3))
                    ((= len 2) (string-append "0" s))
                    ((= len 1) (string-append "00" s))
                    (else "000")))))
        
        (define (format-is-valid? format-str)
          (or (string-contains format-str "yyyy")
              (string-contains format-str "MM")
              (string-contains format-str "dd")
              (string-contains format-str "HH")
              (string-contains format-str "mm")
              (string-contains format-str "ss")
              (string-contains format-str "SSS")))
        
        (unless (format-is-valid? format-str)
          (value-error "datetime%format: invalid format string"))
        
        (let loop ((result "")
                   (remaining format-str))
          (if (string-null? remaining)
              result
              (cond
                ((string-starts? remaining "yyyy")
                 (loop (string-append result (number->string year))
                       (substring remaining 4 (string-length remaining))))
                ((string-starts? remaining "MM")
                 (loop (string-append result (pad2 month))
                       (substring remaining 2 (string-length remaining))))
                ((string-starts? remaining "dd")
                 (loop (string-append result (pad2 day))
                       (substring remaining 2 (string-length remaining))))
                ((string-starts? remaining "HH")
                 (loop (string-append result (pad2 hour))
                       (substring remaining 2 (string-length remaining))))
                ((string-starts? remaining "mm")
                 (loop (string-append result (pad2 minute))
                       (substring remaining 2 (string-length remaining))))
                ((string-starts? remaining "ss")
                 (loop (string-append result (pad2 second))
                       (substring remaining 2 (string-length remaining))))
                ((string-starts? remaining "SSS")
                 (loop (string-append result (pad3 micro-second))
                       (substring remaining 3 (string-length remaining))))
                (else
                 (loop (string-append result (substring remaining 0 1))
                       (substring remaining 1 (string-length remaining))))))))

      (define (%to-date)
        (date :year year :month month :day day))

      )

    (define-case-class date
      ((year integer?)
       (month integer?)
       (day integer?))

#|
date@now
创建一个表示当前系统日期的日期对象。
可用于获取当前日期的年份、月份、日期等字段。

语法
----
(date :now)

参数
----
无参数（使用 :now 关键字创建当前日期对象）

返回值
-----
返回一个表示当前日期的对象，该对象支持以下字段查询：
'year  : 年份 (>= 2023)
'month : 月份 (1-12)
'day   : 日期 (1-31)

错误
----
无特定错误（始终返回有效日期对象）

|#

      (chained-define (@now)
        (let ((time-vec (g_date-now)))
          (date 
            :year (vector-ref time-vec 0)
            :month (vector-ref time-vec 1)
            :day (vector-ref time-vec 2))))

#|
date%to-string
将日期转换为格式化的日期字符串，格式为"YYYY-MM-DD"。

语法
----
(date-object :to-string)

参数
----
无参数

返回值
-----
string
格式化的日期字符串。

格式说明
----
- 年份：4位数字
- 月份：2位数字，01-12
- 日期：2位数字，01-31

当数值小于10时，前导补零确保固定长度格式。

错误
----
无特定错误（始终返回格式化的日期字符串）
|#

      (define (%to-string)
        (%format "yyyy-MM-dd"))

#|
date%weekday
计算当前日期是星期几。

语法
----
(date-object :weekday)

参数
----
无参数

返回值
------
整数 (0-6)
0 表示星期一 (Monday)，1 表示星期二 (Tuesday)，...，6 表示星期日 (Sunday)。

使用示例
--------
((date :year 2024 :month 1 :day 1) :weekday) => 0  ; 2024年1月1日是星期一
((date :year 2024 :month 2 :day 29) :weekday) => 2 ; 2024年2月29日是星期四

额外信息
------
这是基于 Zeller 公式的变体的计算结果，已调整为周一为起始日。
|#

#|
date%to-datetime
将 date 对象转换为 datetime 对象，时间是 (00:00:00.000000)。

语法
----
(date-object :to-datetime)

参数
----
无参数

返回值
-----
datetime
一个新的 datetime 对象，包含原 date 的年月日信息，时分为 0:0:0。

使用示例
--------
((date :year 2024 :month 1 :day 15) :to-datetime) 
=> (datetime :year 2024 :month 1 :day 15 :hour 0 :minute 0 :second 0 :micro-second 0)

额外信息
------
与Java 8 DateTime API中的 LocalDate.atStartOfDay() 类似，是date的重要转换方法
|#

#|
date%format
按照指定的格式字符串格式化日期。

语法
----
(date-object :format format-string)

参数
----
format-string:string
  格式字符串，支持以下格式符：
  - yyyy: 4位年份
  - MM: 2位月份（01-12）
  - dd: 2位日期（01-31）

返回值
-----
string
格式化后的日期字符串。

错误
----
value-error
如果格式字符串无效则抛出该异常。

使用示例
--------
((date :year 2024 :month 1 :day 15) :format "yyyy-MM-dd") 
=> "2024-01-15"

((date :year 2024 :month 1 :day 15) :format "dd/MM/yyyy") 
=> "15/01/2024"
|#

      (define (%format format-str)
        (let ((pad2 (lambda (n)
                      (if (< n 10)
                          (string-append "0" (number->string n))
                          (number->string n)))))
          
          (unless (or (string-contains format-str "yyyy")
                      (string-contains format-str "MM")
                      (string-contains format-str "dd"))
            (value-error "date%format: invalid format string"))
          
          (let loop ((result "")
                     (remaining format-str))
            (if (string-null? remaining)
                result
                (cond
                  ((string-starts? remaining "yyyy")
                   (loop (string-append result (number->string year))
                         (substring remaining 4 (string-length remaining))))
                  ((string-starts? remaining "MM")
                   (loop (string-append result (pad2 month))
                         (substring remaining 2 (string-length remaining))))
                  ((string-starts? remaining "dd")
                   (loop (string-append result (pad2 day))
                         (substring remaining 2 (string-length remaining))))
                  (else
                   (loop (string-append result (substring remaining 0 1))
                         (substring remaining 1 (string-length remaining)))))))))

      (define (%to-datetime)
        (datetime :year year :month month :day day 
                  :hour 0 :minute 0 :second 0 :micro-second 0))

      (define (%weekday)
        (weekday-for-date year month day))

      )

    
    (define-object days

#|
days@between
计算两个日期之间的天数差异。

语法
----
(days :between start end)

参数
----
start:datetime 或 date
  起始日期，可以是 datetime 对象或 date 对象。
end:datetime 或 date
  结束日期，可以是 datetime 对象或 date 对象。

返回值
-----
long
返回两个日期之间的天数差异，结果为正值或负值。
- 如果 end 在 start 之后，返回正数(表示从开始到结束经过了多少天)
- 如果 end 在 start 之前，返回负数(表示从开始到结束需要倒退多少天)
- 如果两个日期相同，返回 0

错误
----
type-error
如果 start 或 end 不是 datetime 或 date 对象，则抛出类型异常。

使用示例
--------
(days :between (date :year 2024 :month 1 :day 1) (date :year 2024 :month 1 :day 15))
  => 14  相隔14天

(days :between (datetime :year 2024 :month 1 :day 15) (datetime :year 2024 :month 1 :day 1))
  => -14 相隔负14天，即需要往前推14天

(days :between (date :year 2024 :month 1 :month 15) (date :year 2024 :month 1 :day 15))
  => 0  同一天

额外信息
----
与Java 8 Date Time API中的 DAYS.between(start, end) 类似，计算方法基于日期与1970年1月1日之间的相对天数。
|#

      (define (julian-day-number year month day)
        (let* ((a (quotient (- 14 month) 12))
               (y (+ year 4800 (- a)))
               (m (+ month (* 12 a) (- 3)))
               (jdn (+ day
                       (quotient (+ (* 153 m) 2) 5)
                       (* 365 y)
                       (quotient y 4)
                       (- (quotient y 100))
                       (quotient y 400)
                       (- 32045))))
          jdn))

      (define (@between start end)
        (define (extract-date-fields obj)
          (cond 
            ((datetime :is-type-of obj)
             (list (obj 'year) (obj 'month) (obj 'day)))
            ((date :is-type-of obj)
             (list (obj 'year) (obj 'month) (obj 'day)))
            (else
             (type-error "days@between expects datetime or date objects"))))
        
        (let* ((start-fields (extract-date-fields start))
               (end-fields (extract-date-fields end))
               (start-jdn (apply julian-day-number start-fields))
               (end-jdn (apply julian-day-number end-fields))
               (days-difference (- end-jdn start-jdn)))
          days-difference))

    )

  ) ; end of begin
  ) ; end of define-library

