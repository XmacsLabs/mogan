#!/bin/bash

# ================= 配置部分 =================
APP_NAME="mogan-stem"
BINARY_NAME="moganstem"
ARCH="amd64"
INSTALL_PREFIX="/opt/$APP_NAME"

# 图标源路径 (相对于项目根目录)
ICON_SOURCE_REL="3rdparty/qwindowkitty/src/styles/app/stem.png"

# 尝试获取 VERSION
if [ -z "$VERSION" ]; then
    VERSION="2025.2.3"
else
    echo "✅ 检测到版本号: $VERSION"
fi

# 定位路径
if [ -L ${BASH_SOURCE-$0} ]; then
  FWDIR=$(dirname $(readlink "${BASH_SOURCE-$0}"))
else
  FWDIR=$(dirname "${BASH_SOURCE-$0}")
fi
APP_HOME="$(cd "${FWDIR}/../.."; pwd)"

APP_DIR="$APP_HOME/AppDir"
DEB_BUILD_DIR="$APP_HOME/deb_package"
DEPLOY_TOOL="linuxdeploy-x86_64.AppImage"
QT_PLUGIN="linuxdeploy-plugin-qt-x86_64.AppImage"

set -e

# ================= 1. 收集文件 =================
echo "📂 [1/6] 运行 xmake install 收集文件..."
cd "$APP_HOME"
rm -rf "$APP_DIR" "$DEB_BUILD_DIR"

# 安装二进制和资源
xmake install -o "$APP_DIR/usr" -y stem

if [ ! -f "$APP_DIR/usr/bin/$BINARY_NAME" ]; then
    echo "❌ 错误: 未找到二进制文件 $BINARY_NAME"
    exit 1
fi

# ================= 2. 处理图标 (直接复制 PNG) =================
echo "🎨 [2/6] 处理图标文件..."
ICON_SRC="$APP_HOME/$ICON_SOURCE_REL"
# 这里假设图标足够清晰，放入 256x256 目录（如果图标尺寸不同，Linux 也能显示，只是目录归类不太规范）
ICON_DEST_DIR="$APP_DIR/usr/share/icons/hicolor/256x256/apps"
mkdir -p "$ICON_DEST_DIR"

if [ -f "$ICON_SRC" ]; then
    echo "   -> 复制图标: $ICON_SRC"
    # 将图标重命名为 包名.png (mogan-stem.png)
    cp "$ICON_SRC" "$ICON_DEST_DIR/$APP_NAME.png"
    ICON_FINAL_NAME="$APP_NAME" # .desktop 文件里通常不需要写 .png 后缀
else
    echo "⚠️ 警告: 未找到图标源文件: $ICON_SRC"
    touch "$ICON_DEST_DIR/$APP_NAME.png"
    ICON_FINAL_NAME="$APP_NAME"
fi

# # ================= 3. 确保 .desktop 文件存在并正确 =================
# DESKTOP_PATH="$APP_DIR/usr/share/applications/$APP_NAME.desktop"
# if [ ! -f "$DESKTOP_PATH" ]; then
#     echo "📄 [3/6] 生成 .desktop 文件..."
#     mkdir -p "$(dirname "$DESKTOP_PATH")"
#     cat > "$DESKTOP_PATH" <<EOF
# [Desktop Entry]
# Type=Application
# Name=Mogan Stem
# Comment=Scientific Editor
# Exec=$BINARY_NAME
# Icon=$ICON_FINAL_NAME
# Categories=Education;Science;Qt;
# Terminal=false
# EOF
# else
#     # 强制修正 Icon 字段，确保它使用我们刚才复制进去的图标名
#     echo "   -> 更新现有 .desktop 文件的图标设置..."
#     sed -i "s|^Icon=.*|Icon=$ICON_FINAL_NAME|" "$DESKTOP_PATH"
# fi


# ================= 4. 准备工具 =================
echo "🛠️ [4/6] 准备 LinuxDeploy..."
if [ ! -f "$DEPLOY_TOOL" ]; then
    wget -q "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/$DEPLOY_TOOL"
    chmod +x "$DEPLOY_TOOL"
fi
if [ ! -f "$QT_PLUGIN" ]; then
    wget -q "https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases/download/continuous/$QT_PLUGIN"
    chmod +x "$QT_PLUGIN"
fi

# ================= 5. 打包依赖 (Bundle) =================
echo "🔍 [5/6] 注入 Qt 依赖..."
XMAKE_QMAKE=$(find ~/.xmake/packages -type f -name qmake 2>/dev/null | grep "qt" | head -n 1)
if [ -n "$XMAKE_QMAKE" ]; then
    export QMAKE="$XMAKE_QMAKE"
    export PATH="$(dirname "$XMAKE_QMAKE"):$PATH"
fi

# -------------------------------------------------------------
# 手动导入输入法插件 (Fix for Chinese Input Method)
# -------------------------------------------------------------
echo "🔧 [Manual Import] 正在导入 Fcitx5/中文输入法支持..."

# 1. 定义我们要在 AppDir (安装包) 里存放插件的位置
#    Qt 程序默认去 plugins/platforminputcontexts 找输入法
DEST_PLUGIN_DIR="$APP_DIR/usr/plugins/platforminputcontexts"
mkdir -p "$DEST_PLUGIN_DIR"

# 2. 定义系统源路径 
SRC_PLUGIN_DIR="/usr/lib/x86_64-linux-gnu/qt6/plugins/platforminputcontexts"

# 3. 执行复制
if [ -d "$SRC_PLUGIN_DIR" ]; then
    echo "   -> 发现系统插件目录: $SRC_PLUGIN_DIR"
    # 复制该目录下所有 .so 文件到包内的插件目录
    cp -v "$SRC_PLUGIN_DIR/"*.so "$DEST_PLUGIN_DIR/" 2>/dev/null || true
    echo "   -> 复制完成。"
else
    echo "⚠️ 警告: 未在系统中找到 $SRC_PLUGIN_DIR"
    echo "   请确保构建环境安装了 'fcitx5-frontend-qt6' 或 'libqt6gui6'。"
fi
# -------------------------------------------------------------

# 运行 linuxdeploy
# 它会扫描我们刚才复制进去的 .so 文件，并把它们依赖的 fcitx 库也打包进去
./"$DEPLOY_TOOL" --appdir "$APP_DIR" --plugin qt --executable "$APP_DIR/usr/bin/$BINARY_NAME" --icon-file "$ICON_SRC"

# ================= 6. 构建 /opt 包结构 =================
echo "📦 [6/6] 组装并生成 Deb..."
mkdir -p "$DEB_BUILD_DIR/DEBIAN"
mkdir -p "$DEB_BUILD_DIR$INSTALL_PREFIX"

# 移动内容到 /opt/mogan-stem
cp -r "$APP_DIR/usr/"* "$DEB_BUILD_DIR$INSTALL_PREFIX/"

# 修正 Exec 路径为绝对路径
TARGET_DESKTOP=$(find "$DEB_BUILD_DIR$INSTALL_PREFIX/share/applications" -name "*.desktop" | head -n 1)
if [ -f "$TARGET_DESKTOP" ]; then
    sed -i "s|^Exec=.*|Exec=$INSTALL_PREFIX/bin/$BINARY_NAME|" "$TARGET_DESKTOP"
fi

# 生成 Control
INSTALLED_SIZE=$(du -s "$DEB_BUILD_DIR" | cut -f1)
cat > "$DEB_BUILD_DIR/DEBIAN/control" <<EOF
Package: $APP_NAME
Version: $VERSION
Architecture: $ARCH
Maintainer: Mogan Team <dev@mogan.app>
Installed-Size: $INSTALLED_SIZE
Section: science
Priority: optional
Description: Mogan Stem
 Scientific editor powered by Mogan.
 Installed in $INSTALL_PREFIX.
EOF

OUTPUT_DEB="${APP_HOME}/../${APP_NAME}_${VERSION}_${ARCH}.deb"
dpkg-deb --build "$DEB_BUILD_DIR" "$OUTPUT_DEB"

echo "✅ 打包完成: $OUTPUT_DEB"