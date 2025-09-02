#!/bin/bash

# 定义新的软件源和 backport 源信息
NEW_SOURCES=$(cat <<EOF
Types: deb
URIs: http://mirrors.ustc.edu.cn/debian
Suites: trixie trixie-updates trixie-backports
Components: main contrib non-free non-free-firmware
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg

Types: deb
URIs: http://mirrors.ustc.edu.cn/debian-security
Suites: trixie-security
Components: main contrib non-free non-free-firmware
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg
EOF
)

# 检查 /etc/apt/sources.list.d/debian.sources 文件是否存在
if [ -f /etc/apt/sources.list.d/debian.sources ]; then
    # 如果存在，先备份原始文件
     cp /etc/apt/sources.list.d/debian.sources /etc/apt/sources.list.d/debian.sources.backup
fi

# 清除当前的软件源设置，并写入新的 USTC 镜像源
echo -e "$NEW_SOURCES" |  tee /etc/apt/sources.list.d/debian.sources > /dev/null

echo "USTC mirror and backport sources have been added."
echo "Original sources.list.d/debian.sources is backed up as sources.list.d/debian.sources.backup if it existed."
