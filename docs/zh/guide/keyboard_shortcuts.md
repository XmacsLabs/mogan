# 墨干的键盘快捷键

欢迎查看墨干的键盘快捷键指南。这里列出了可用快捷键组合，帮助您更高效地使用软件。

文档来自 [Liii STEM 官网](https://liiistem.cn/docs/guide-keyboard-shortcuts.html)。该官网的快捷键列表会持续更新，墨干通常会及时同步更新内容，但可能存在一定延迟，请以官方文档为准。您也可在官网获取 PDF 版本的最新快捷键手册。

## 环境快捷键

::: tip 提示
这些快捷键用于控制文档的整体环境和格式设置。包括空格、缩进、对齐方式、章节标题、列表等基本排版功能。正确使用这些快捷键可以帮助您更高效地进行文档编辑。
:::

| Windows | GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|---------|-----------|-----|------------|------|
| <kbd>space</kbd> + <kbd>tab</kbd> | <kbd>space</kbd> + <kbd>tab</kbd> | <kbd>space</kbd> + <kbd>tab</kbd> | Non-breaking space ($\nbsp$ or $\sim$) | 插入不间断空格，保持文字之间的固定间距 |
| <kbd>ctrl</kbd> + <kbd>t</kbd> |/ | / | $\indent$ | 段落首行缩进 |
| <kbd>ctrl</kbd> + <kbd>l</kbd> | / | /| $\raggedleft$ | 将文本右对齐 |
| <kbd>ctrl</kbd> + <kbd>e</kbd> | / | / | $\centering$ | 将文本居中对齐 |
| <kbd>ctrl</kbd> + <kbd>r</kbd> | / | / | $\raggedright$ | 将文本左对齐 |
| <kbd>alt</kbd> + <kbd>1</kbd> | <kbd>alt</kbd> + <kbd>1</kbd> | <kbd>option</kbd> + <kbd>1</kbd> | $\section$ | 创建一级标题 |
| <kbd>alt</kbd> + <kbd>2</kbd> | <kbd>alt</kbd> + <kbd>2</kbd> | <kbd>option</kbd> + <kbd>2</kbd> | $\subsection$ | 创建二级标题 |
| <kbd>alt</kbd> + <kbd>3</kbd> | <kbd>alt</kbd> + <kbd>3</kbd> | <kbd>option</kbd> + <kbd>3</kbd> | $\subsubsection$ | 创建三级标题 |
| <kbd>alt</kbd> + <kbd>4</kbd> | <kbd>alt</kbd> + <kbd>4</kbd> | <kbd>option</kbd> + <kbd>4</kbd> | $\paragraph$ | 创建段落标题 |
| <kbd>+</kbd> + <kbd>tab</kbd> | <kbd>+</kbd> + <kbd>tab</kbd> | <kbd>+</kbd> + <kbd>tab</kbd> | $\itemize$ | 创建无序列表项 |
| <kbd>1</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> | <kbd>1</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> | <kbd>1</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> | $\enumerate$ | 创建有序列表项 |
| <kbd>$</kbd> | <kbd>$</kbd> | <kbd>$</kbd> | inline math mode | 插入行内数学公式 |
| <kbd>alt</kbd> + <kbd>$</kbd> | <kbd>alt</kbd> + <kbd>$</kbd> | <kbd>option</kbd> + <kbd>$</kbd> | single-line math mode | 插入单行独立数学公式 |
| <kbd>alt</kbd> + <kbd>7</kbd> | <kbd>alt</kbd> + <kbd>7</kbd> | <kbd>option</kbd> + <kbd>7</kbd> | multi-line math mode | 插入多行数学公式环境 |
| <kbd>ctrl</kbd> + <kbd>#</kbd> | <kbd>ctrl</kbd> + <kbd>#</kbd> | <kbd>ctrl</kbd> + <kbd>#</kbd> | add equation number | 为数学公式添加编号 |
| <kbd>alt</kbd> + <kbd>arrow</kbd> | <kbd>alt</kbd> + <kbd>arrow</kbd> | <kbd>option</kbd> + <kbd>arrow</kbd> | add new row/column in matrix/table | 在矩阵或表格中添加新的行或列 |
| <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>f</kbd> | <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>f</kbd> | <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>f</kbd> | add footnote | 添加脚注 |
| <kbd>ctrl</kbd> + <kbd>n</kbd> | <kbd>ctrl</kbd> + <kbd>n</kbd> | <kbd>ctrl</kbd> + <kbd>n</kbd> | add new script | 创建新文档 |
| <kbd>ctrl</kbd> + <kbd>p</kbd> | <kbd>ctrl</kbd> + <kbd>p</kbd> | <kbd>ctrl</kbd> + <kbd>p</kbd> | export to PDF | 将文档导出为PDF格式 |

::: warning 注意事项
1. Mac系统下使用 option 键代替 Windows/Linux 下的 alt 键
2. 某些快捷键可能与系统快捷键冲突，需要在系统设置中更改
3. 数学公式相关的快捷键需要在相应的数学环境中使用
4. 表格和矩阵的操作快捷键仅在对应环境中生效
5. 建议熟记常用快捷键以提高编辑效率
6. 段落首行缩进、右对齐、居中对齐、左对齐快捷键暂只支持Windows使用
:::

## 常用构造

:::tip 提示
本文档列出了在编辑器中输入数学公式的常用快捷键。所有快捷键按顺序依次按下即可。在开始使用这些快捷键之前，请先按下 <kbd>$</kbd> 进入数学输入模式。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd>x</kbd> + <kbd>^</kbd> + <kbd>2</kbd> | <kbd>x</kbd> + <kbd>^</kbd> + <kbd>2</kbd> | $x^2$ (x^2) | 上标 |
| <kbd>x</kbd> + <kbd>_</kbd> + <kbd>{i,j}</kbd> | <kbd>x</kbd> + <kbd>_</kbd> + <kbd>{i,j}</kbd> | $x_{i,j}$ (x_{i.j}) | 下标 |
| <kbd>alt</kbd> + <kbd>s</kbd> + <kbd>2</kbd> | <kbd>option</kbd> + <kbd>s</kbd> + <kbd>2</kbd> | $\sqrt{2}$ (\sqrt{2}) | 平方根 |
| <kbd>alt</kbd> + <kbd>s</kbd> + <kbd>Tab</kbd> + <kbd>3</kbd> + <kbd>←</kbd> + <kbd>←</kbd> + <kbd>n</kbd> | <kbd>option</kbd> + <kbd>s</kbd> + <kbd>Tab</kbd> + <kbd>3</kbd> + <kbd>←</kbd> + <kbd>←</kbd> + <kbd>n</kbd> | $\sqrt[n]{3}$ (\sqrt[n]{3}) | n次方根 |
| <kbd>alt</kbd> + <kbd>f</kbd> | <kbd>option</kbd> + <kbd>f</kbd> | $\frac{2}{3}$ (\frac{2}{3}) | 分数 |
| <kbd>alt</kbd> + <kbd>t</kbd> | / | $\tabular$ | 居中无框表格 |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd> | / | $\matrix$ | 矩阵 |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | / |  | 行列式 |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | / |  | 选择 |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | / |  | 堆叠 |

:::warning 注意事项
1. 在使用这些快捷键时，需要确保编辑器处于数学输入模式
2. 如果公式没有正确显示，请检查是否正确进入了数学输入模式
3. 表格、矩阵等构造的快捷键在Mac中过于复杂，因此没有列出
:::

## 字体样式

:::tip 提示
在输入数学表达式时，可以使用这些快捷键快速改变字体样式。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd>ctrl</kbd> + <kbd>u</kbd> + <kbd>A</kbd> | <kbd>ctrl</kbd> + <kbd>u</kbd> + <kbd>A</kbd> | $\underline{A}$  (\underline{A}) | 下划线字体 |
| <kbd>ctrl</kbd> + <kbd>i</kbd> + <kbd>A</kbd> | <kbd>ctrl</kbd> + <kbd>i</kbd> + <kbd>A</kbd> | $\mathit{A}$ (\mathit{A}) | 斜体字 |
| <kbd>ctrl</kbd> + <kbd>b</kbd> + <kbd>A</kbd> | <kbd>ctrl</kbd> + <kbd>b</kbd> + <kbd>A</kbd> | $\mathbf{A}$ (\mathbf{A}) | 粗体字 |
| <kbd>F7</kbd> + <kbd>A</kbd> | <kbd>F7</kbd> + <kbd>A</kbd> | $\mathcal{A}$ (\mathcal{A}) | 花体字 |
| <kbd>F8</kbd> + <kbd>A</kbd> | <kbd>F8</kbd> + <kbd>A</kbd> | $\mathfrak{A}$ (\mathfrak{A}) | 哥特体 |
| <kbd>A</kbd> + <kbd>A</kbd> | <kbd>A</kbd> + <kbd>A</kbd> | $\mathbb{A}$ (\mathbb{A}) | 黑板粗体 |

:::warning 注意事项
某些字体样式（如花体、哥特体、黑板体等）仅适用于拉丁字母。
:::

## 希腊字母

:::tip 提示
- 所有希腊字母快捷键都是字母+Tab组合
- 大写希腊字母使用大写字母+Tab
- 在数学编辑器中输入字母后按Tab即可
:::

| Windows/GNU/Linux/Mac | $\LaTeX$等价 | 说明 |
|------------------|------------|------|
| <kbd>a</kbd> + <kbd>tab</kbd> | $\alpha$ (\alpha) | Alpha |
| <kbd>b</kbd> + <kbd>tab</kbd> | $\beta$ (\beta) | Beta |
| <kbd>g</kbd> + <kbd>tab</kbd>, <kbd>G</kbd> + <kbd>tab</kbd> | $\gamma$ (\gamma), $\Gamma$ (\Gamma) | Gamma |
| <kbd>d</kbd> + <kbd>tab</kbd>, <kbd>D</kbd> + <kbd>tab</kbd> | $\delta$ (\delta), $\Delta$ (\Delta) | Delta |
| <kbd>e</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\epsilon$ (\epsilon) | Epsilon |
| <kbd>e</kbd> + <kbd>tab</kbd> | $\varepsilon$ (\varepsilon) | Epsilon |
| <kbd>z</kbd> + <kbd>tab</kbd> | $\zeta$ (\zeta) | Zeta |
| <kbd>h</kbd> + <kbd>tab</kbd> | $\eta$ (\eta) | Eta |
| <kbd>j</kbd> + <kbd>tab</kbd>, <kbd>J</kbd> + <kbd>tab</kbd> | $\theta$ (\theta) | Theta |
| <kbd>j</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\vartheta$ (\vartheta) | Theta |
| <kbd>i</kbd> + <kbd>tab</kbd> | $\iota$ (\iota) | Iota |
| <kbd>k</kbd> + <kbd>tab</kbd>, <kbd>K</kbd> + <kbd>tab</kbd> | $\kappa$ (\kappa) | Kappa |
| <kbd>l</kbd> + <kbd>tab</kbd>, <kbd>L</kbd> + <kbd>tab</kbd> | $\lambda$ (\lambda), $\Lambda$ (\Lambda) | Lambda |
| <kbd>m</kbd> + <kbd>tab</kbd> | $\mu$ (\mu) | Mu |
| <kbd>n</kbd> + <kbd>tab</kbd> | $\nu$ (\nu) | Nu |
| <kbd>x</kbd> + <kbd>tab</kbd>, <kbd>X</kbd> + <kbd>tab</kbd> | $\xi$ (\xi), $\Xi$ (\Xi) | Xi |
| <kbd>p</kbd> + <kbd>tab</kbd>, <kbd>P</kbd> + <kbd>tab</kbd> | $\pi$ (\pi), $\Pi$ (\Pi) | Pi |
| <kbd>p</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\varpi$ (\varpi) | Pi |
| <kbd>o</kbd> + <kbd>tab</kbd>, <kbd>O</kbd> + <kbd>tab</kbd> | $\omicron$ (\omicron) | Omicron |
| <kbd>r</kbd> + <kbd>tab</kbd> | $\rho$ (\rho) | Rho |
| <kbd>r</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\varrho$ (\varrho) | Rho |
| <kbd>s</kbd> + <kbd>tab</kbd>, <kbd>S</kbd> + <kbd>tab</kbd> / <kbd>C</kbd> + <kbd>tab</kbd> | $\sigma$ (\sigma), $\Sigma$ (\Sigma) | Sigma |
| <kbd>s</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> / <kbd>c</kbd> + <kbd>tab</kbd> | $\varsigma$ (\varsigma) | Sigma |
| <kbd>t</kbd> + <kbd>tab</kbd> | $\tau$ (\tau) | Tau |
| <kbd>u</kbd> + <kbd>tab</kbd>, <kbd>U</kbd> + <kbd>tab</kbd> | $\upsilon$ (\upsilon), $\Upsilon$ (\Upsilon) | Upsilon |
| <kbd>f</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>, <kbd>F</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\phi$ (\phi), $\Phi$ (\Phi) | Phi |
| <kbd>f</kbd> + <kbd>tab</kbd> | $\varphi$ (\varphi) | Phi |
| <kbd>q</kbd> + <kbd>tab</kbd> | $\chi$ (\chi) | Chi |
| <kbd>y</kbd> + <kbd>tab</kbd>, <kbd>Y</kbd> + <kbd>tab</kbd> | $\psi$ (\psi), $\Psi$ (\Psi) | Psi |
| <kbd>w</kbd> + <kbd>tab</kbd>, <kbd>W</kbd> + <kbd>tab</kbd> | $\omega$ (\omega), $\Omega$ (\Omega) | Omega |

:::warning 注意事项
- 某些希腊字母有多个变体形式（如 ε 和 ϵ），请注意区分
- 大小写敏感，请确保输入正确的大小写形式
:::

## 集合与逻辑

:::tip 提示
这些快捷键可以帮助你快速输入数学符号，提高编写数学公式的效率。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd>%</kbd> + <kbd>tab</kbd> | <kbd>%</kbd> + <kbd>tab</kbd> | $\cup$ (\cup) | 并集符号 |
| <kbd>&</kbd> + <kbd>tab</kbd> | <kbd>&</kbd> + <kbd>tab</kbd> | $\cap$ (\cap) | 交集符号 |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\subset$ (\subset) | 真子集 |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> | $\subseteq$ (\subseteq) | 子集 |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\supset$ (\supset) | 真子集 |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> | $\supseteq$ (\supseteq) | 子集 |
| <kbd><</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>tab</kbd> | $\in$ (\in) | 属于 |
| <kbd>></kbd> + <kbd>tab</kbd> | <kbd>></kbd> + <kbd>tab</kbd> | $\ni$ (\ni) | 属于 |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>/</kbd> | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>/</kbd> | $\notin$ (\notin) | 不属于 |
| <kbd>R</kbd> + <kbd>R</kbd> | <kbd>R</kbd> + <kbd>R</kbd> | $\mathbb{R}$ (\mathbb{R}) | 实数集合 |
| <kbd>Z</kbd> + <kbd>Z</kbd> | <kbd>Z</kbd> + <kbd>Z</kbd> | $\mathbb{Z}$ (\mathbb{Z}) | 整数集合 |
| <kbd>Q</kbd> + <kbd>Q</kbd> | <kbd>Q</kbd> + <kbd>Q</kbd> | $\mathbb{Q}$ (\mathbb{Q}) | 有理数集合 |
| <kbd>N</kbd> + <kbd>N</kbd> | <kbd>N</kbd> + <kbd>N</kbd> | $\mathbb{N}$ (\mathbb{N}) | 自然数集合 |
| <kbd>C</kbd> + <kbd>C</kbd> | <kbd>C</kbd> + <kbd>C</kbd> | $\mathbb{C}$ (\mathbb{C}) | 复数集合 |
| <kbd>@</kbd> + <kbd>/</kbd> | <kbd>@</kbd> + <kbd>/</kbd> | $\varnothing$ (\varnothing) | 空集 |
| <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\aleph$ (\aleph) | 阿列夫数 |
| <kbd>=</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>=</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\equiv$ (\equiv) | 定义为 / 恒等于 |
| <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\forall$ (\forall) | 全称量词 |
| <kbd>E</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>E</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\exists$ (\exists) | 存在量词 |
| <kbd>!</kbd> + <kbd>tab</kbd> | <kbd>!</kbd> + <kbd>tab</kbd> | $\neg$ (\neg) | 逻辑非 |
| <kbd>%</kbd> | <kbd>%</kbd> | $\vee$ (\vee) | 析取 |
| <kbd>&</kbd> | <kbd>&</kbd> | $\wedge$ (\wedge) | 合取 |
| <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>-</kbd> | <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>-</kbd> | $\vdash$ (\vdash) | 推导符号 |
| <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> | <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> | $\models$ (\models) | 模型符号 |
| <kbd>=</kbd> + <kbd>></kbd> | <kbd>=</kbd> + <kbd>></kbd> | $\Rightarrow$ (\Rightarrow) | 蕴含符号 |
| <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd> | <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd> | $\nRightarrow$ (\nRightarrow) | 非蕴含符号 |

:::warning 注意事项
- 确保在输入快捷键时不要有其他字符干扰
- Tab键需要完整按下后再进行下一步操作
:::

## 装饰符号

:::tip 提示
在Liii STEM中，您可以使用键盘快捷键快速输入常用的数学装饰符号。这些符号通常用于表示导数、向量等数学概念。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd>alt</kbd> + <kbd>˙</kbd> | <kbd>option</kbd> + <kbd>˙</kbd> | $\dot{}$ (\dot{}) | 一阶导数符号 |
| <kbd>alt</kbd> + <kbd>˙</kbd> + <kbd>˙</kbd> | <kbd>option</kbd> + <kbd>˙</kbd> + <kbd>˙</kbd> | $\ddot{}$ (\ddot{}) | 二阶导数符号 |
| <kbd>alt</kbd> + <kbd>ˆ</kbd> | <kbd>option</kbd> + <kbd>ˆ</kbd> | $\hat{}$ (\hat{}) | 单位向量符号 |
| <kbd>alt</kbd> + <kbd>˜</kbd> | <kbd>option</kbd> + <kbd>˜</kbd> | $\tilde{}$ (\tilde{}) | 等价符号 |
| <kbd>alt</kbd> + <kbd>-</kbd> | <kbd>option</kbd> + <kbd>-</kbd> | $\bar{}$ (\bar{}) | 平均值符号 |

:::warning 注意事项
1. 装饰符号需要在字符输入后才能添加
2. 某些装饰符号可能与系统快捷键冲突，请确保您的系统设置允许这些快捷键
3. Mac用户请确保已启用option键的特殊字符输入功能
:::

## 点号

:::tip 提示
输入加号和点号后，可以通过按 <kbd>Tab</kbd> 键在不同类型的点号之间切换。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd>.</kbd> + <kbd>.</kbd> | <kbd>.</kbd> + <kbd>.</kbd> | $\ldots$ (\ldots) | 基线省略号，用于文本 |
| <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> | <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> | $\cdots$ (\cdots) | 居中省略号，用于数学运算 |
| <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\vdots$ (\vdots) | 垂直省略号 |
| <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\ddots$ (\ddots) | 对角省略号 |

:::warning 注意事项
请注意区分水平点号 ($\cdots$) 和基线点号 ($\ldots$) 的使用场景。
:::

## 可变大小运算符

::: tip 提示
这些快捷键可以快速输入常见的数学运算符号。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd> | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd> | $\int$ (\int) | 积分符号 |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd> + <kbd>I</kbd> | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd> + <kbd>I</kbd> | $\iint$ (\iint) | 二重积分符号 |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>O</kbd> | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>O</kbd> | $\oint$ (\oint) | 曲线积分符号 |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>U</kbd> | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>U</kbd> | $\bigcup$ \bigcup | 并集符号 |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>N</kbd> | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>N</kbd> | $\bigcap$ (\bigcap) | 交集符号 |

::: warning 注意事项
请确保您的键盘上的功能键(F5)可以正常使用。某些笔记本电脑可能需要额外按下Fn键。
:::

## 箭头

::: tip 提示
在Liii STEM中，箭头符号可以通过简单的键盘组合快速输入。记住这些快捷键可以提高数学公式的输入效率。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd>-</kbd> + <kbd>></kbd> | <kbd>-</kbd> + <kbd>></kbd> | $\rightarrow$ (\rightarrow) | 右箭头 |
| <kbd>-</kbd> + <kbd>></kbd> + <kbd>/</kbd> | <kbd>-</kbd> + <kbd>></kbd> + <kbd>/</kbd> | $\nrightarrow$ (\nrightarrow) | 否定右箭头 |
| <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd> | <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd> | $\longrightarrow$ (\longrightarrow) | 长右箭头 |
| <kbd>=</kbd> + <kbd>></kbd> | <kbd>=</kbd> + <kbd>></kbd> | $\Rightarrow$ (\Rightarrow) | 双线右箭头 |
| <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd> | <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd> | $\nRightarrow$ (\nRightarrow) | 双线否定右箭头 |
| <kbd>=</kbd> + <kbd>=</kbd> + <kbd>></kbd> | <kbd>=</kbd> + <kbd>=</kbd> + <kbd>></kbd> | $\Longrightarrow$ (\Longrightarrow) | 双线长右箭头 |
| <kbd>~</kbd> + <kbd>></kbd> | <kbd>~</kbd> + <kbd>></kbd> | $\leadsto$ (\leadsto) | 波浪箭头 |
| <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>></kbd> | <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>></kbd> | $\mapsto$ (\mapsto) | 映射箭头 |
| <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd> | <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd> | $\longmapsto$ (\longmapsto) | 长映射箭头 |
| <kbd><</kbd> + <kbd>-</kbd> | <kbd><</kbd> + <kbd>-</kbd> | $\leftarrow$ (\leftarrow) | 左箭头 |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd> | <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd> | $\leftrightarrow$ (\leftrightarrow) | 双向箭头 |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> | $\uparrow$ (\uparrow) | 上箭头 |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\downarrow$ (\downarrow) | 下箭头 |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd> + <kbd>tab</kbd> | $\updownarrow$ (\updownarrow) | 上下双向箭头 |

::: warning 注意事项
输入箭头时需要按照顺序依次按下组合键
:::

## 括号与定界符

::: tip 提示
Liii STEM提供了多种括号和定界符的快捷输入方式。通过组合键可以快速输入常用的数学符号，提高公式编辑效率。
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\langle \rangle$ (\langle \rangle) | 尖括号对，常用于向量表示 |
| <kbd>\|</kbd> + <kbd>.</kbd> | <kbd>\|</kbd> + <kbd>.</kbd> | $\lfloor \rfloor$ (\lfloor \rfloor) | 向下取整符号 |
| <kbd>\|</kbd> + <kbd>'</kbd> | <kbd>\|</kbd> + <kbd>'</kbd> | $\lceil \rceil$ (\lceil \rceil) | 向上取整符号 |
| <kbd>\|</kbd> + <kbd>\|</kbd> | <kbd>\|</kbd> + <kbd>\|</kbd> | $\parallel \parallel$ (\parallel \parallel) | 范数符号 |

::: warning 注意事项
1. 输入括号时注意配对，确保左右括号数量相等
2. 某些定界符在不同上下文中可能有不同含义
3. 建议先熟悉常用的快捷键组合，逐步掌握更多组合
:::

## 其他符号

::: tip 提示
- 所有快捷键都需要在数学环境中使用
- 键入快捷键后会立即触发符号输入
- Tab键的数量影响最终输出的符号类型
:::

| Windows/GNU/Linux | Mac | $\LaTeX$等价 | 说明 |
|------------------|-----|------------|------|
| <kbd><</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | $\leq$ (\leq) | 小于等于 |
| <kbd>></kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | <kbd>></kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | $\geq$ (\geq) | 大于等于 |
| <kbd>=</kbd> + <kbd>\\</kbd> | <kbd>=</kbd> + <kbd>\\</kbd> | $\neq$ (\neq) | 不等于 |
| <kbd><</kbd> + <kbd><</kbd> | <kbd><</kbd> + <kbd><</kbd> | $\ll$ (\ll) | 远小于 |
| <kbd>></kbd> + <kbd>></kbd> | <kbd>></kbd> + <kbd>></kbd> | $\gg$ (\gg) | 远大于 |
| <kbd>˜</kbd> + <kbd>˜</kbd> | <kbd>˜</kbd> + <kbd>˜</kbd> | $\approx$ (\approx) | 约等于 |
| <kbd>=</kbd> + <kbd>tab</kbd> | <kbd>=</kbd> + <kbd>tab</kbd> | $\asymp$ (\asymp) | 渐近等于 |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\prec$ (\prec) | 先于 |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | $\preceq$ (\preceq) | 先于等于 |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\succ$ (\succ) | 后于 |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | $\succeq$ (\succeq) | 后于等于 |
| <kbd>@</kbd> + <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>@</kbd> + <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\propto$ (\propto) | 正比于 |
| <kbd>.</kbd> + <kbd>=</kbd> | <kbd>.</kbd> + <kbd>=</kbd> | $\doteq$ (\doteq) | 点等于 |
| <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\angle$ (\angle) | 角度 |
| <kbd>l</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>l</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\ell$ (\ell) | 手写体小写L |
| <kbd>⇧</kbd> + <kbd>f5</kbd> + <kbd>b</kbd> | <kbd>⇧</kbd> + <kbd>f5</kbd> + <kbd>b</kbd> | $\parallel$ (\parallel) | 平行于 |
| <kbd>~</kbd> + <kbd>=</kbd> | <kbd>~</kbd> + <kbd>=</kbd> | $\cong$ (\cong) | 全等于 |
| <kbd>~</kbd> + <kbd>=</kbd> + <kbd>\/</kbd> | <kbd>~</kbd> + <kbd>=</kbd> + <kbd>\/</kbd> | $\ncong$ (\ncong) | 不全等于 |
| <kbd>~</kbd> | <kbd>~</kbd> | $\sim$ (\sim) | 相似于 |
| <kbd>~</kbd> + <kbd>-</kbd> | <kbd>~</kbd> + <kbd>-</kbd> | $\simeq$ (\simeq) | 相似且等于 |
| <kbd>~</kbd> + <kbd>\/</kbd> | <kbd>~</kbd> + <kbd>\/</kbd> | $\nsim$ (\nsim) | 不相似 |
| <kbd>@</kbd> + <kbd>+</kbd> | <kbd>@</kbd> + <kbd>+</kbd> | $\oplus$ (\oplus) | 圆圈加 |
| <kbd>@</kbd> + <kbd>-</kbd> | <kbd>@</kbd> + <kbd>-</kbd> | $\ominus$ (\ominus) | 圆圈减 |
| <kbd>@</kbd> + <kbd>.</kbd> | <kbd>@</kbd> + <kbd>.</kbd> | $\odot$ (\odot) | 圆圈点 |
| <kbd>@</kbd> + <kbd>*</kbd> | <kbd>@</kbd> + <kbd>*</kbd> | $\otimes$ (\otimes) | 圆圈乘 |
| <kbd>@</kbd> + <kbd>\/</kbd> | <kbd>@</kbd> + <kbd>\/</kbd> | $\oslash$ (\oslash) | 圆圈斜杠 |
| <kbd>\/</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> |  <kbd>\/</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\upharpoonright$ (\upharpoonright) | 限制 |
| <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\cdot$ (\cdot) | 居中点 |
| <kbd>+</kbd> + <kbd>-</kbd> | <kbd>+</kbd> + <kbd>-</kbd> | $\pm$ (\pm) | 正负号 |
| <kbd>-</kbd> + <kbd>+</kbd> | <kbd>-</kbd> + <kbd>+</kbd> | $\mp$ (\mp) | 负正号 |
| <kbd>*</kbd> + <kbd>tab</kbd> | <kbd>*</kbd> + <kbd>tab</kbd> | $\times$ (\times) | 乘号 |
| <kbd>\/</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>\/</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\div$ (\div) | 除号 |
| <kbd>*</kbd> + <kbd>tab</kbd> | <kbd>*</kbd> + <kbd>tab</kbd> | $\ast$ (\ast) | 星号 |
| <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\partial$ (\partial) | 偏导符号 |
| <kbd>v</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>v</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\nabla$ (\nabla) | 梯度符号 |
| <kbd>@</kbd> | <kbd>@</kbd> | $\circ$ (\circ) | 圆圈符号 |
| <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\star$ (\star) | 星星 |
| <kbd>i</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>i</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\imath$ (\imath) | 无点小写i |
| <kbd>j</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>j</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\jmath$ (\jmath) | 无点小写j |
| <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\beth$ (\beth) | 希伯来字母beth |
| <kbd>g</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>g</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\gimel$ (\gimel) | 希伯来字母gimel |
| <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\daleth$ (\daleth) | 希伯来字母daleth |
| <kbd>r</kbd> + <kbd>e</kbd> | <kbd>r</kbd> + <kbd>e</kbd> | $\Re$ (\Re) | 实数部分 |
| <kbd>w</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>w</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\mho$ (\mho) | 倒立的欧姆符号 |
| <kbd>p</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>p</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\wp$ (\wp) | 手写体大写P |
| <kbd>@</kbd> + <kbd>@</kbd> | <kbd>@</kbd> + <kbd>@</kbd> | $\infty$ (\infty) | 无穷大 |
| <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\top$ (\top) | 上界符号 |
| <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\bot$ (\bot) | 下界符号 |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\clubsuit$ (\clubsuit) | 梅花 |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> | $\diamondsuit$ (\diamondsuit) | 方块 |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\heartsuit$ (\heartsuit) | 红心 |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\spadesuit$ (\spadesuit) | 黑桃 |
| <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\flat$ (\flat) | 降号 |
| <kbd>#</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>#</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\natural$ (\natural) | 还原号 |
| <kbd>#</kbd> + <kbd>tab</kbd> | <kbd>#</kbd> + <kbd>tab</kbd> | $\sharp$ (\sharp) | 升号 |
| <kbd>@</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | <kbd>@</kbd> + <kbd>=</kbd> + <kbd>tab</kbd> | $\triangleq$ (\triangleq) | 定义等号 |
| <kbd>+</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>+</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\dagger$ (\dagger) | 剑号/单剑号 |

::: warning 注意
在不同的输入法下需要切换到英文输入模式
:::