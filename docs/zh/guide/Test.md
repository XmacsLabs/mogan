# 如何测试
对于开发者来说，一共有两种测试方式，其一为单元测试，其二为集成测试。

## 单元测试
### C++单元测试
| 命令                        | 作用                   |
| --------------------------- | ---------------------- |
| bin/test_all                | 执行所有C++测试        |
| bin/test_kernel_l1          | 执行Kernel L1的C++测试 |
| bin/test_kernel_l2          | 执行Kernel L2的C++测试 |
| bin/test_only [target-name] | 只执行某一个单元测试   |

## Scheme 测试
执行Scheme测试：
+ 可以使用 `xmake run --yes -vD --group=scheme_tests` 执行所有Scheme测试.

## 集成测试
集成测试的代码和测试文档都放在`TeXmacs/tests`这个目录下。执行集成测试前，必须构建并安装墨干到临时目录：
+ 可以使用`bin/test_all_doc`执行所有集成测试
+ 可以使用`xmake run [target-name]`执行单个集成测试，比如`xmake run 9_1`
+ 可以使用`xmake run --yes -vD --group=integration_tests`执行所有集成测试。

## 列出所有target
下面的命令行可以列出所有target，其中以`_test`结尾的是C++的单元测试，形如`[0_9]*_[0_9]*`的是集成测试。
``` shell
$ xmake show -l targets
color_test        parsexml_test          qt_utilities_test
string_test       converter_test         17_1
list_test         keyword_parser_test    image_files_test
xml_test          9_1                    array_test
12_1              mogan_install          queryxml_test
point_test        tree_test              hashmap_test
rectangle_test    mogan                  env_length_test
url_test          libkernel_l1           analyze_test
libkernel_l2      hashset_test           libmogan
```