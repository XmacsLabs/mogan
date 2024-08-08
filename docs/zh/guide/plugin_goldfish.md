# 金鱼Scheme
金鱼Scheme是三鲤Scheme的社区版，墨干理工套件是三鲤理工套件的社区版。

金鱼Scheme作为插件内置在墨干理工套件中，Goldfish Scheme本身不提供REPL的实现，墨干理工套件中提供了Goldfish Scheme的结构化REPL。

Scheme语言的初学者，可以在墨干中利用Goldfish Scheme学习符合R7RS标准的Scheme语言。

## 结构化REPL
结构化REPL和常见的REPL的显著区别是输入和输出都可以是结构化的。对于编程语言的REPL，一般来说，输入都是纯文本格式的代码；在Racket中，支持在代码中将图片作为代码的一部分。在金鱼Scheme会话中，输入仍旧是纯文本的Scheme代码，输出则是结构化文档。本文档以如下示例展示金鱼Scheme会话的结构化输出效果：

### 特殊的渲染规则
墨干的文档本质上是Scheme代码，金鱼Scheme会话的求值结果，如果以`document`、`math`、`equation*`、`align`、`with`、`graphics`开头，那么会被渲染为墨干文档片段：
![](../../images/goldfish_rendering.png)

### 区分副作用和求值结果
求值结果以浅绿色的背景色标注。副作用的背景色是文档的背景色，通常是白色。
![](../../images/goldfish_side_effect.png)


## 安装
无须安装，Goldfish Scheme本身和Goldfish Scheme插件都是在墨干中内置的。

## 内置文档见`帮助->插件->Goldfish Scheme`
