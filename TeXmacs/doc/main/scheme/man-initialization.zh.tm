<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|撰写初始化配置文件>

  启动程序时，<with|font|roman|<TeXmacs>>将读取并执行这个文件：

  <with|font|roman|<verbatim| \ \ \ $TEXMACS_PATH/progs/init-texmacs.scm>>

  以及用户自己的配置文件（如果存在的话）：<em|>

  <with|font|roman|<verbatim| \ \ \ $TEXMACS_HOME_PATH/progs/my-init-texmacs.scm>>

  <verbatim|$TEXMACS_HOME_PATH>的默认值在<name|Windows>上是<verbatim|%appdata%\\TeXmacs>，在<name|GNU>/<name|Linux>和<name|macOS>上则是<verbatim|$HOME/.TeXmacs>。类似地，每次你新建一个buffer，程序将执行:

  <with|font|roman|<verbatim| \ \ \ $TEXMACS_PATH/progs/init-buffer.scm>>

  以及（如果存在的话）

  <with|font|roman|<verbatim| \ \ \ $TEXMACS_HOME_PATH/progs/my-init-buffer.scm>>

  <tmdoc-copyright|2013\U2019|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>