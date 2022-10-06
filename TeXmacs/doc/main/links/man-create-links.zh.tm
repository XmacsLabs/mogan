<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|创建标签，链接和参考>

  通过<key|Meta+!>或者<menu|插入|链接|标签>创建新的未激活标签，通过<key|Meta+?>或者<menu|插入|链接|参考>创建对应的参考。在输入标签或者参考的名字后，莫忘按下回车键<hlink|激活|../text/keyboard/man-hybrid.zh.tm>。在编辑参考的名字时，按下<key|Tab>可以自动补全。对小节加标签时，建议加在小节标题后。给单行公式加标签时，建议加在公式前。给多行公式加标签时，建议加在公式编号后。回顾一下，按下<key|Ctrl+#>可以退出或进入编号环境。

  通过<key|inactive \<gtr\>>或者<menu|Insert|Link|Hyperlink>插入超链接。第一个域填写的是相关的文本，激活后显示为蓝色。第二个域包含了超链接。超链接可以是本地文档的路径，也可以是网络上文档的地址。使用<verbatim|#<with|font-shape|italic|label>>作为超链接则会指向当前文档中的标签，使用<verbatim|<with|font-shape|italic|url>#<with|font-shape|italic|label>>则指向url所定位到的文档中标签的位置。<label|test>

  类似地，通过<key|inactive *>或者<menu|Insert|Link|Action>可以把一个动作关联到一段文本或者图片上。第二个域填写的是一个Guile/Scheme脚本命令。回车激活后，双击这个动作链接，命令就被执行了。为了系统安全，有些脚本会被拒绝执行。<TeXmacs>默认会弹出对话框询问是否执行，但可以在<menu|Preferences|Security>中更改这一默认行为。注意，这条Guile/Scheme命令

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  会将<verbatim|shell-command>作为shell命令执行。

  通过<key|inactive i>或者<menu|Insert|Link|Include>可以直接将其它文档的内容包含到当前文档中。这意味着，如果你在那些被包含的文档中做了更改，那么这些更改会自动地更新到当前的文档中。

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>