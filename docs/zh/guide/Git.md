# Git Workflow
Git是一个开源的**分布式**版本控制系统。

## 托管服务
墨干托管于以下多个平台：

| 平台 | 链接 | 机构或者公司 | 地址 |
|-----|-----|------------|-----|
| Gitlab | https://git.lug.ustc.edu.cn/XmacsLabs/mogan | 中国科学技术大学Linux用户协会 | 中国 |
| Codeberg | https://codeberg.org/XmacsLabs/mogan | Codeberg e.V. | 德国 |
| Gitee | https://gitee.com/XmacsLabs/mogan           | 深圳市奥思网络科技有限公司    | 中国 |
| Github | https://github.com/XmacsLabs/mogan          | Github, Inc.             | 美国 |

我们在上述多个平台都会接受用户创建Issues和Pull Requests。

如果你希望在更多平台可以下载到墨干的源代码，请通过以上平台联系我们。
+ 对于商业公司，我们希望在一个国家只选择其中一个最为流行的平台。
+ 对于学术机构自建的Git托管服务，我们希望越多越好。这样学术机构的雇员和学生不需要泄露任何个人隐私给商业公司，在机构自建的Git托管服务直接参与开发即可。

不同托管服务的Git工作流略有不同，罗列如下：

### Gitee的工作流
初次: 首先[生成/添加SSH公钥](https://gitee.com/help/articles/4181)（注意：这个操作是针对Gitee用户，而不是Gitee代码仓库的）。

一次：下载并添加Gitee这个远程仓库。
```
# 从Gitee克隆这个代码仓库，并进入刚刚下载的代码仓库目录
git clone git@gitee.com:XmacsLabs/mogan.git
cd mogan

# 我们这里使用gitee而不是origin作为远程仓库的名字，这和常见的帮助文档略有不同
git remote add gitee git@gitee.com:XmacsLabs/mogan.git
```

多次：创建代码合并请求，可参考[Gitee官方文档](https://gitee.com/help/articles/4346):
```
git checkout -b [branch-name]
# coding and `git commit`
git push gitee [branch-name]:main
```
按照如上流程，将你的分支推送到Gitee的main分支，Gitee会自动创建代码合并请求。

一个例子：
```
git checkout -b da/gitee_workflow
# 开始写文档并提交代码
git push gitee da/gitee_workflow:main
```
这就是自动生成的代码合并请求: https://gitee.com/XmacsLabs/mogan/pulls/45

### Github、Gitlab和Gitee的工作流
> 以Github为例。

首先需要添加github这个remote仓库：
```
git remote add github git@github.com:XmacsLabs/mogan.git
```

创建代码合并请求，可参考[Github的官方文档](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/working-with-your-remote-repository-on-github-or-github-enterprise/creating-an-issue-or-pull-request#creating-a-pull-request): 
```
git checkout -b [branch-name]
# coding and `git commit`
git push github [branch-name]
# manually create the github pull request
```

### 为什么使用多个Git代码托管平台？
**因为我们不想**只用某一个Git代码托管平台。而且我们希望获得来自用户和开发者的更多反馈和贡献。

然后，墨干是一个**自由软件**。我们愿意付出额外的努力，让**地球上的所有人**(或许50年之后需要加上火星？)轻松获得墨干的源代码。

### 如何在多个Git代码托管平台之间同步代码?
目前，Mogan的管理员会手动同步。
