# 在 macOS 上编译 Mogan

本文档记录了如何在 macOS 系统上成功编译 Mogan STEM 的详细步骤。

## 系统要求

- macOS 15.6+ (Apple Silicon/ARM64)
- Xcode 16.0+
- Homebrew

## 依赖安装

### 1. 安装 Xmake

```bash
curl -fsSL https://xmake.io/shget.text | bash
```

安装完成后，重启终端或运行：
```bash
source ~/.bashrc
```

### 2. 安装 Qt 6

使用 Homebrew 安装 Qt 6：

```bash
brew install qt
```

验证安装：
```bash
brew list qt
/opt/homebrew/bin/qmake --version
```

### 3. 安装其他依赖

```bash
brew install pkg-config libcurl
```

## 编译步骤

### 1. 克隆仓库

```bash
git clone https://github.com/XmacsLabs/mogan.git
cd mogan
```

### 2. 配置构建环境

```bash
cd mogan
xmake config --qt=/opt/homebrew/Cellar/qt/6.9.2 -vD
```

**注意**: 根据您安装的 Qt 版本调整路径。可以通过 `ls /opt/homebrew/Cellar/qt/` 查看具体版本。

### 3. 编译 Mogan STEM

```bash
xmake build stem
```

编译过程可能需要几分钟时间。编译成功后，会生成以下文件：

- `build/macosx/arm64/release/MoganSTEM` - 可执行文件
- `build/macosx/arm64/release/MoganSTEM.app` - macOS 应用程序包

### 4. 运行应用程序

```bash
open build/macosx/arm64/release/MoganSTEM.app
```

或直接运行可执行文件：
```bash
./build/macosx/arm64/release/MoganSTEM
```

## 常见问题

### Qt 版本问题

如果遇到 Qt 相关错误，请确保：

1. 使用 Homebrew 安装的 Qt 6（不是 conda 版本）
2. 正确配置 Qt 路径：
   ```bash
   # 查找 Qt 安装路径
   find /opt/homebrew -name "qmake" 2>/dev/null
   
   # 使用正确路径重新配置
   xmake config --qt=/opt/homebrew/Cellar/qt/[版本号] -vD
   ```

### 编译警告

编译过程中可能出现关于 `override` 声明的警告，这些是非致命警告，不影响程序正常运行。

### 依赖缺失

如果遇到依赖缺失错误，请确保安装了所有必要的依赖：

```bash
brew install pkg-config libcurl freetype libpng jpeg
```

## 构建结果

成功编译后，您将获得：

- 静态库文件：`libmoebius.a` 和 `libmogan.a`
- 可执行文件：`MoganSTEM`
- 完整的 macOS 应用程序包：`MoganSTEM.app`

应用程序包包含所有必要的框架和资源，可以直接分发给其他用户使用。

## 技术细节

- 构建系统：Xmake
- 编译器：Clang (Apple)
- Qt 版本：6.9.2
- 目标架构：ARM64 (Apple Silicon)
- 构建模式：Release

## 贡献

如果您在编译过程中遇到问题或有改进建议，欢迎提交 Issue 或 Pull Request。
