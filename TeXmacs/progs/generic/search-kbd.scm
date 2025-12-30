
(texmacs-module (generic search-kbd)
  (:use (generic search-widgets)))

(kbd-map
  (:require (inside-search-or-replace-buffer?))
  ("std ?" (make 'select-region))    ;创建选择区域
  ("std 1" (insert '(wildcard "x"))) ;插入通配符 x
  ("std 2" (insert '(wildcard "y"))) ;插入通配符 y
  ("std 3" (insert '(wildcard "z"))) ;插入通配符 z
  ;; 导航快捷键
  ("up" (search-next-match #f))      ; 上一个匹配项
  ("down" (search-next-match #t))    ; 下一个匹配项
  ("pageup" (search-next-match #f))  ; 上一个匹配项（备用）
  ("pagedown" (search-next-match #t)); 下一个匹配项（备用）
  ("home" (search-extreme-match #f)) ; 第一个匹配项
  ("end" (search-extreme-match #t))  ; 最后一个匹配项
  ;; 功能键支持
  ("F3" (search-next-match #t))      ; 下一个匹配项（标准搜索键）
  ("S-F3" (search-next-match #f))    ; 上一个匹配项（Shift+F3）
  ;; 特殊组合键
  ("std return" (search-rotate-match #f)); Ctrl+Enter：向前循环匹配
  ("return"(search-rotate-match #t))) ;向后循环匹配

;; 替换缓冲区专有快捷键
(kbd-map
  (:require (inside-replace-buffer?))
  ("return" (replace-one (window->buffer (car (window-list))) (replace-buffer))); Enter：替换当前匹配项
  ("std return" (replace-all (window->buffer (car (window-list))) (replace-buffer)))); Ctrl+Enter：替换所有后续匹配项

