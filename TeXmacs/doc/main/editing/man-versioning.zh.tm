<TeXmacs|2.1.3>

<style|<tuple|tmdoc|chinese|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|版本控制工具>

  与他人合作撰写文档时，会产生这样的需求，即审阅他人对文档的更改，然后或接受或舍弃或更正。在<menu|Tools|Versioning
  tool>启用版本控制菜单后，菜单栏中会新增<menu|版本>这一项。下面我们来谈谈这个菜单是如何使版本控制的过程更加方便。

  当前流行的版本控制工具有<hlink|<name|Subversion>|http://subversion.tigris.org/>，
  <hlink|<name|Git>|http://git-scm.com/>，
  <hlink|<name|Darcs>|http://darcs.net/>， <hlink|<name|GNU
  Arch>|http://www.gnu.org/software/gnu-arch/>等。<TeXmacs>目前只对<name|Subversion>和<name|Git>提供基础支持，不过要集成其他的版本控制工具也并不困难。

  <paragraph*|比较两个版本>

  假定我们有同一个文档的两个版本<shell|old.tm>和<shell|new.tm>。通过以下步骤可以显示旧版本和新版本的差异：首先打开<shell|new.tm>这个文件，然后点击<menu|Version|Compare|With
  older version>选中<shell|old.tm>打开。此时，缓冲区的命名仍为<shell|new.tm>，更改通过特殊的标记显示在缓冲区中。假定存在差异，那么光标初始定位在第一个差异处。类似地，你还可以先打开旧版本，在点击<menu|Version|Compare|With
  newer version>和新版本比较。

  通过子菜单<menu|Version|Move>，你可以在文档中的差异间上下移动。相应的快捷键是
  <shortcut|(version-previous-difference)>和<shortcut|(version-next-difference)>。如果你已经了解了<hlink|结构化光标移动|man-structured-move.zh.tm>，不妨使用更加通用的快捷键，即
  <shortcut|(traverse-first)> ， <shortcut|(traverse-last)> ，
  <shortcut|(traverse-previous)> 和 <shortcut|(traverse-next)>。

  <paragraph*|差异的可视化>

  版本间的差异有三种方式显示：只显示旧版本，只显示新版本，或者两者兼顾。旧版本字体呈深红色的，新版本则为墨绿。

  每一个差异的可视化的显示方式都可以通过
  <menu|Version|Show> 或者快捷键<shortcut|(version-show 'version-old)>
  (旧)， <shortcut|(version-show 'version-new)> (新) and
  <shortcut|(version-show 'version-both)>
  (兼)更改。也可以用<shortcut|(variant-circulate (focus-tree)
  #t)>在这些方式间轮换，此即所谓<hlink|结构化变元|man-structured-variants.zh.tm>。通过选中文本，可以扩大以上快捷键的作用域，特别地，选中全文，则所有差异的显示方式同步更改。

  <paragraph*|保留特定版本>

  我们经常需要在两个版本之间斟酌推敲，逐步对每一个差异的选择保留其中之一。假定当前光标是在某一个差异之中，那么此时可用子菜单<menu|Version|Retain>或者快捷键
  <shortcut|(version-retain 0)>，<shortcut|(version-retain 1)> 和
  <shortcut|(version-retain 'current)> 分别保留旧版本，新版本和当前版本。如果当前是新旧版本兼显示，那么按下<shortcut|(version-retain
  'current)>会保留新版本。选定版本后，光标会自动跳到下一个差异处，这样我们可以继续进行操作。

  <paragraph*|粒度控制和差异重现>

  子菜单<menu|Version|Grain>用于控制版本间差异的计算。默认地，我们使用最细的粒度<menu|Detailed>。使用<menu|Block>，则版本间的差异将以段落为基础计算。也是就说，在比较中，只要两个版本在段落上有任何差异，就会高亮显示。最粗糙的粒度是<menu|Rough>，只要两个版本间存在差异，全文就会高亮显示。

  粒度选项在使用<menu|Version|File|Compare>时产生作用，但也可以为选中文本更改比较的粒度，只要在选中文本后，再更改<menu|Version|Grain>子菜单即可。特别地，整个缓冲区的比较粒度也可以这样更改。类似地，如果你在一个差异中更改了粒度值，那么差异将以新的粒度重新计算并显示。

  注意您还可以更改文本后，再改变比较粒度。效果将是重新显示选中部分的差异或者是光标处差异。这在特定情境下非常实用。有如下情境，旧版本中有一个引理，而您在新版本中将其改成了定理并在其中做了少量更改。此时，若显示差异，整个定理都将高亮显示。这其实是由<TeXmacs>文档内部的结构决定的。如果要显示其中内容的差异，您只需将旧版本中的引理改成定理即可。

  <paragraph*|使用外部版本控制工具>

  如果你正在编辑的文件所在的目录在某个版本控制系统下，那么<menu|版本>下拉菜单中分隔符以上的部分会被激活。目前<TeXmacs>支持<name|Subversion>和<name|Git>。

  首先，如果当前的缓冲区在版本控制下，那么你可以通过<menu|Version|History>浏览该文件的历史版本。该子菜单的下拉菜单是一系列旧版本的超链接及相关信息，比如何时何人更改。历史版本是只读的，但你可以使用<menu|Version|Compare|With
  current user version>将它们和当前的版本比较。

  对版本控制下的文本更改后，编辑器中的版本不再和版本库中的版本相对应。此时，点击<menu|Version|Commit>以提交更新。提交之后，系统会显示您已经提交的更改。通过<menu|Version|Register>，您可以把不在版本控制下的文件添加到版本控制系统中。只是注册文件并不会将文件提交到版本库中，您仍然需要点击<menu|Version|Commit>以提交您的更改。

  如果在你编辑某文件时，其对应的版本库中的文件正好被更改，此时，你需要点击<menu|Version|Update>，将两者合并。现在还没有对完成针对冲突的解决方案，不过，不久您就能轻松解决冲突了。

  <tmdoc-copyright|2013\U2019|Joris van der Hoeven|沈达>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>