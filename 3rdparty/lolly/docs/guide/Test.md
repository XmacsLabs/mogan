# How to Perform Testing
For developers, there are two testing methods available: unit testing and integration testing.

## Listing all Targets
The following command line can list all targets. Targets ending with `_test` are C++ unit tests.
``` shell
$ xmake show -l targets
path_test            ntuple_test          fast_search_test
base64_test          blackbox_test        hashset_test
tm_timer_test        string_test          fast_alloc_test
promise_test         liblolly             array_test
url_test             sys_utils_test       hashmap_test
curl_test            iterator_test        hashfunc_test
hashtree_test        tm_ostream_test      list_test
modification_test    parse_string_test    generic_tree_test
analyze_test         rel_hashmap_test     tree_test
```

## Unit Testing
As shown above, unit tests are in the format of xxx_test. You can run the following command to perform the tests (ensure you have built the code beforehand, see [xmake](https://xmake.io) for details):
```
xmake run xxx_test
```

## Integration Testing
Run the following command for integration testing (make sure you have built the code beforehand, see [xmake](https://xmake.io) for details):
```
xmake run --group=tests
```