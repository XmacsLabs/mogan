# LaTeX速成班：从零基础到超越手写速度

## 授课师资
讲师为伊利诺伊大学芝加哥电子与计算机工程全奖博士，助教为南科大数学系博士在读以及中科院严济慈英才班学生

## 课程目标
通过6节实操课程（共4小时），掌握：
1. 线上环境与基础语法
2. 基础排版与数学公式
3. 图片与表格
4. 参考文献管理
5. 进阶管理和排版技巧
6. NeurIPS模板实战

---

## 课程安排

### 第1课：线上环境与基础语法
```latex
% 示例代码：最小工作文档
\documentclass{article}
\begin{document}
Hello NeurIPS!
\end{document}
```
**学习内容**：
- LaTeX简介（原理、特点和优势）
- loongtex在线平台使用
- 常用包（如amsmath、graphicx）与模板预定义设置
- 文档类（article/report/...）与基础结构（preamble/document/title/author/date/...）
- 命令与环境基础语法（注释、空白符号、特殊字符转义...）
- 编译流程与错误排查

**课后任务**：
- 创建论文基础框架（标题/作者/摘要/...）并完成编译

---

### 第2课：基础排版与数学公式
```latex
% 示例代码：数学环境
\begin{equation}
E = mc^2 \quad \text{和} \quad \nabla\cdot\mathbf{D} = \rho
\end{equation}
```
**学习内容**：
- 章节层次结构设置（section/subsection）
- 文本格式（加粗/斜体/下划线）、字体样式（粗体/斜体/下划线）、字体大小（large/small/tiny）、文本对齐、段落和换行、列表（itemize/enumerate/description）
- 基本数学符号：如希腊字母（\alpha）、运算符（\times）。
- 高级结构：下标（_）、上标（^）、分数（\frac）、根号（\sqrt）、矩阵（array或matrix环境）、数学函数（sin, cos, log）。
- 公式：行内公式、独立公式、多行公式对齐（align环境）、公式的标记以及引用
- 特殊符号与运算符
- 教学过程中使用Mogan辅助生成对应的Latex代码

**课后任务**：
- 完成方法章节数学公式排版

---

### 第3课：图片，表格与定理定义
```latex
% 示例代码：三线表与插图
\begin{table}[ht]
\caption{实验参数设置}
\begin{tabular}{ccc}
\toprule
参数 & 值 & 单位 \\
\midrule
学习率 & 0.001 & - \\
批量大小 & 32 & - \\
\bottomrule
\end{tabular}
\end{table}
```
**学习内容**：
- 图片插入：使用\includegraphics命令。
- 表格创建：使用tabular环境，设置列宽、对齐方式。
- 定理（Theorem）、引理（Lemma）、命题（Proposition）、推论（Corollary）、定义（Definition）、示例（Example）、备注（Remark）等结构式环境的使用和自定义
- 排版定位：学习figure和table环境，确保图表在合适位置，并调整尺寸。
- 交叉引用机制：\label 和 \ref
- 定制计数器：公式/图表编号规则
- 教学过程中使用Mogan辅助生成对应的Latex代码

**课后任务**：
- 制作实验结果对比表格
- 插入算法流程图

---

### 第4课：参考文献管理
```latex
% 示例代码：BibTeX引用
深度学习方法\cite{resnet}在... 
\bibliographystyle{unsrt}
\bibliography{references}
```
**学习内容**：
- BibTeX工作流程
- Zotero+betterbibtex的使用和高级技巧
- natbib宏包高级引用（NeurIPS模板内置）
- 参考文献样式定制
- 避免常见引用错误

**课后任务**：
- 建立论文参考文献库
- 实现文中引用标注

---

### 第5课：进阶管理和排版技巧
- 多栏布局与分页控制
- 代码块与算法排版，介绍algorithm和algorithmic包的基本使用
- 颜色与背景
- 超链接与书签设置
- 自定义命令和环境：使用 \newcommand 定义新命令，使用 \newenvironment 创建自定义环境。
- 条件编译与版本控制：使用 \ifthenelse 实现条件编译；管理论文草稿和最终版本。


**课后任务**：
- 创建并插入一个简单机器学习算法的伪代码，如线性回归的训练过程。

---

### 第6课：NeurIPS模板实战
```latex
% 示例代码：完整论文框架
\title{基于深度强化学习的...}
\author{张三\quad 李四\\ 北京大学}
\begin{abstract}
本文提出...
\end{abstract}
```
- 从空白模板构建论文
- 分模块内容整合
- 最终格式微调
- 生成投稿版本（PDF）
- 常见问题解决方案

**最终成果**：
- 符合NeurIPS格式要求的完整论文
- 包含：摘要/方法/实验/参考文献
- 支持公式/图表/算法的专业排版

---

## 课程价格
原价150元，早期优惠价100元。

## 推荐资源
1. loongtex官方文档：[loongtex.com/docs/app/help](https://www.loongtex.com/docs/app/help)
2. NeurIPS2025投稿指南：[nips.cc](https://nips.cc/)
3. LaTeX数学符号手册：[latexmath.com](http://latexmath.com)
4. BibTeX样式库：[CTAN](https://ctan.org/pkg/bibtex)

## 联系方式
<img src="../../images/wechatQR_jack.jpg" alt="微信二维码" class="qr-code" />
<p class="contact-note">如果您想购买或咨询，请添加以上微信.</p>

<style>
.qr-code {
  width: 300px;
}
.contact-note {
  color: #808080;
}
</style>