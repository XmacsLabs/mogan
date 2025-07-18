# 测试
拥有两种测试方式，其一为单元测试，其二为集成测试
## 单元测试
### C++单元测试
|方法一                      |方法二                            |作用                 |
|---------------------------|---------------------------------|---------------|
|bin/test_all               |bash bin/test_all                |执行所有C++测试.      |
|bin/test_only [target-name]|bash bin/test_only [target-name] |只执行某一个单元测试.   |
### Scheme 测试
执行Scheme测试：
+ 可以使用 `xmake run --yes -vD --group=scheme_tests` 或 `bin/test_all_scheme` 执行所有Scheme测试.

## 集成测试
集成测试的代码和测试文档都放在`TeXmacs/tests`这个目录下。执行集成测试前，必须构建并安装墨干到临时目录：
+ 可以使用`bin/test_all_integrated`或`bash bin/test_all_integrated`执行所有集成测试
+ 可以使用`xmake run [target-name]`执行单个集成测试，比如`xmake run 9_1`
+ 可以使用`xmake run --yes -vD --group=integration_tests`执行所有集成测试。

## 列出所有target
下面的命令行可以列出所有target，其中以`_test`结尾的是C++的单元测试，形如`[0_9]*_[0_9]*`的是集成测试。
``` shell
$ xmake show -l targets
11_36               converter_test         convert_test
12_1                tm_url_test            24_19
15_3_7              70_7                   data-test
43_15               15_3_5                 libmogan
15_3_3              generic-test           qt_utilities_test
smart_font_test     tm-define-test         66_7
environment-test    view_history_test      parse_variant_test
11_28               pdf_test               old-gui-test
minimal-test        math_test              xml_test
66_13               24_14                  image_files_test
cork-test           scheme-tools-test      11_4
9_1                 goldfish               keyword_parser_test
64_1                stem                   24_15
parsexml_test       tmlength-test          203_5
liii_packager       graphics-group-test    queryxml_test
tm-convert-test     71_41                  15_3_2
tm_file_test        201_6                  svg_parse_test
43_7                libmoebius             otmath_parse_test
regexp-test         env_length_test        15_3_1
url-test            43_14                  19_8
11_38               203_1                  menu-test
201_5
```