# 计划：为 Mogan 创建 IntelliJ IDEA 风格登录弹窗

## 需求概述
1. 参考 `/home/jiadong/git/qt-hello` 项目的弹窗实现
2. 创建模仿 IntelliJ IDEA 的启动弹窗，包含登录按钮和跳过登录按钮
3. 点击跳过登录按钮直接进入主界面
4. 登录按钮需要与现有 `src/Plugins/Qt/qt_tm_widget.cpp` 中的 `LoginButton` 集成
5. 弹窗代码放在 `/home/jiadong/git/mogan/src/Plugins/Qt` 目录下
6. 新建 C++ 文件，格式参考现有文件

## 探索结果

### 1. Mogan 启动流程
- **主函数入口**：`/home/jiadong/git/mogan/src/Mogan/Research/research.cpp` 第166行
- **核心初始化**：`/home/jiadong/git/mogan/src/System/Boot/init_texmacs.cpp` 中的 `TeXmacs_main()` 函数
- **插入弹窗的最佳位置**：在 `gui_open()` 之后、`open_window()` 之前（`init_texmacs.cpp` 第816-860行之间）
- 现有流程：`gui_open()` → 创建 `server` 实例 → 加载欢迎文档 → `open_window()` → `gui_start_loop()`

### 2. 现有登录按钮和对话框
- **`QWK::LoginButton`**：位于 `/home/jiadong/git/mogan/src/Plugins/QWindowKit/loginbutton.hpp`，继承自 `QPushButton`
- **现有登录对话框**：`QWK::LoginDialog` 位于 `/home/jiadong/git/mogan/src/Plugins/QWindowKit/logindialog.hpp`
- **在 `qt_tm_widget.cpp` 中的使用**：第318行创建，连接到 `checkLocalTokenAndLogin()` 函数
- **OAuth 认证**：`QTMOAuth` 类处理 OAuth2 流程

### 3. Qt 插件文件格式规范
- **版权声明**：标准 GPLv3+ 许可证模板
- **包含保护**：`#ifndef FILENAME_HPP` / `#define FILENAME_HPP`
- **命名空间**：自定义组件使用 `QWK` 命名空间
- **D-Pointer 模式**：使用 `Q_DECLARE_PRIVATE` / `Q_DECLARE_PUBLIC` 宏
- **Qt 元对象系统**：使用 `Q_OBJECT` 宏

### 4. 现有对话框实现模式
- **`QTMFileDialog`**：继承自 `QDialog`，支持文件选择和预览
- **`QTMPrintDialog`**：使用 Qt Designer 生成的 UI
- **`QWK::LoginDialog`**：弹出式对话框，支持自定义内容

## 设计决策（基于用户反馈）

### 弹窗类设计
- **类名**：`QWK::StartupLoginDialog`（遵循现有 `QWK` 命名空间）
- **基类**：继承自 `QDialog`
- **位置**：放在 `/home/jiadong/git/mogan/src/Plugins/Qt/` 目录下
- **文件命名**：`startup_login_dialog.hpp` / `startup_login_dialog.cpp`（遵循现有命名模式）
- **私有实现**：使用 D-Pointer 模式（`startup_login_dialog_p.hpp`）

### 弹窗功能
1. **布局**：参考 qt-hello 的 `LoginDialog`，包含标题、副标题、功能列表、按钮
2. **登录按钮**：直接触发现有的 OAuth 登录流程（调用 `qt_tm_widget_rep::checkLocalTokenAndLogin()`）
3. **跳过登录按钮**：关闭对话框，继续启动流程
4. **模态**：应用程序模态，阻止主窗口显示直到用户操作
5. **样式**：模仿 qt-hello 项目的暗色风格

### 显示逻辑（参考 39_7.md）
弹窗显示条件与欢迎文档保持一致：
1. **新用户初次安装**（`install_status == 1`）
2. **老用户安装新版本**（`install_status == 2`）
3. **开发者手动移除 settings.scm**（可通过 `install_status` 判断）

**不显示的情况**：
- 正常启动（`install_status == 0`）且用户已登录（检查本地 token）

### 集成方案
1. **插入位置**：在 `init_texmacs.cpp` 的 `TeXmacs_main()` 函数中，`gui_open()` 之后、`open_window()` 之前
2. **显示函数**：创建 `show_startup_login_dialog()` 函数处理弹窗逻辑
3. **登录触发**：通过信号槽机制连接登录按钮到现有登录功能
4. **结果处理**：
   - 登录点击 → 触发 OAuth 流程，关闭弹窗，继续启动
   - 跳过点击 → 关闭弹窗，继续启动
   - 对话框拒绝 → 退出程序

### 关键文件
**需要创建的新文件**：
1. `/home/jiadong/git/mogan/src/Plugins/Qt/startup_login_dialog.hpp`
2. `/home/jiadong/git/mogan/src/Plugins/Qt/startup_login_dialog.cpp`
3. `/home/jiadong/git/mogan/src/Plugins/Qt/startup_login_dialog_p.hpp`

**需要修改的现有文件**：
1. `/home/jiadong/git/mogan/src/System/Boot/init_texmacs.cpp` - 插入弹窗显示逻辑
2. `/home/jiadong/git/mogan/src/Plugins/Qt/qt_tm_widget.cpp` - 可能需要辅助函数访问登录功能

### 实现步骤
1. 创建 `StartupLoginDialog` 类（头文件 + 源文件 + 私有头文件）
2. 实现弹窗 UI 布局和样式（暗色风格）
3. 在 `init_texmacs.cpp` 中添加弹窗显示逻辑
4. 实现登录功能桥接（连接到现有 `checkLocalTokenAndLogin()`）
5. 测试各种场景（首次安装、版本升级、已登录用户等）

## 用户确认的设计选择
1. **登录按钮行为**：直接触发 OAuth 登录流程（调用 `checkLocalTokenAndLogin()`）
2. **弹窗显示逻辑**：参考 `39_7.md`，与欢迎文档保持一致（基于 `install_status` 变量）
3. **弹窗样式**：模仿 qt-hello 项目的暗色风格

## 待办事项清单
- [x] 1. 创建 `StartupLoginDialog` 头文件（`startup_login_dialog.hpp`）
- [x] 2. 创建 `StartupLoginDialog` 私有头文件（`startup_login_dialog_p.hpp`）
- [x] 3. 创建 `StartupLoginDialog` 源文件（`startup_login_dialog.cpp`）
- [x] 4. 实现弹窗 UI 布局（参考 qt-hello 暗色风格）
- [x] 5. 实现登录按钮信号槽连接
- [x] 6. 实现跳过按钮信号槽连接
- [x] 7. 在 `init_texmacs.cpp` 中添加弹窗显示函数
- [x] 8. 在 `init_texmacs.cpp` 中集成弹窗显示逻辑
- [x] 9. 实现登录功能桥接（连接到 `checkLocalTokenAndLogin()`）
- [ ] 10. 测试弹窗显示（首次安装场景）
- [ ] 11. 测试弹窗显示（版本升级场景）
- [ ] 12. 测试登录按钮功能
- [ ] 13. 测试跳过按钮功能
- [ ] 14. 验证样式和布局