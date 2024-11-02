# How to test
For Developers, there are two way of testing: Unit tests and Integration tests

## Unit Tests
### C++ Unit Tests
| Command                     | Results                              |
| --------------------------- | ------------------------------------ |
| bin/test_all                | Run all C++ tests                    |
| bin/test_kernel_l1          | Run C++ tests in Kernel L1           |
| bin/test_kernel_l2          | Run C++ tests in Kernel L2           |
| bin/test_only [target-name] | Only run the specified C++ unit test |



## Integration Tests
Source code and docs for integration tests are in `TeXmacs/tests`. To run integration tests, one must build and install mogan:
+ use `bin/test_all_doc` to run all integration tests
+ use `xmake run [target-name]` to run the specific integration test, eg. `xmake run 9_1`
+ use `xmake run --yes -vD --group=integration_tests` to run all integration tests.

## List all targets
Use the following commandline to list all targets. The targets (ending with `_test`) are C++ unit tests, the targets like `[0_9]*_[0_9]*` are integration tests.

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
