# 参与贡献
## 如果你是普通用户
+ 可以在 [Github](https://github.com/XmacsLabs/mogan/issues) 用英文报bug 
或者 在[Gitee](https://gitee.com/XmacsLabs/mogan/issues)用中文报bug

## 如果你是开发者
对于内核部分，可以用如下命令进行构建和运行单元测试：
```
xmake build --group=kernel_tests
xmake run --group=kernel_tests
```

以下是在各个平台的开发指南：
+ 在Windows上开发 ([English](/guide/Develop_on_Windows.html)/[中文](/zh/guide/Develop_on_Windows.html))
+ 在macOS上开发 ([English](/guide/Develop_on_macOS.html)/[中文](/zh/guide/Develop_on_macOS.html))
+ 在Linux上开发 ([English](/guide/Develop_on_Linux.html)/[中文](/zh/guide/Develop_on_Linux.html))

### 使用CMake
CMake的构建定义从墨干编辑器V1.1.3开始，已经移除，相关代码在 https://github.com/XmacsLabs/mogan/pull/589 中继续维护。
