# Git Workflow
Git是一个开源的**分布式**版本控制系统。

## 托管服务
墨干托管于以下多个平台：

| 平台 | 链接 | 机构或者公司 | 地址 |
|-----|-----|------------|-----|
| Gitlab | https://git.lug.ustc.edu.cn/XmacsLabs/mogan | 中国科学技术大学Linux用户协会 | 中国 |
| Gitee | https://gitee.com/XmacsLabs/mogan           | 深圳市奥思网络科技有限公司    | 中国 |
| Github | https://github.com/XmacsLabs/mogan          | Github, Inc.             | 美国 |

如果你希望在更多平台可以下载到墨干的源代码，请通过以上平台联系我们。
+ 对于商业公司，我们希望在一个国家只选择其中一个最为流行的平台。
+ 对于学术机构自建的Git托管服务，我们希望越多越好。这样学术机构的雇员和学生不需要泄露任何个人隐私给商业公司，在机构自建的Git托管服务直接参与开发即可。

不同托管服务的Git工作流略有不同，罗列如下：

### Gitee的工作流
首先需要添加Gitee这个远程仓库：
```
git remote add gitee git@gitee.com:XmacsLabs/mogan.git
```

创建Pull Request，可参考[Gitee官方文档](https://gitee.com/help/articles/4346):
```
git checkout -b [branch-name]
# coding and `git commit`
git push gitee [branch-name]:main
```
按照如上流程，将你的分支push到Gitee的main分支，Gitee会自动创建Pull Request。

一个例子：
```
git checkout -b da/gitee_workflow
# 开始写文档并提交代码
git push gitee da/gitee_workflow:main
```
这就是自动生成的Pull Request: https://gitee.com/XmacsLabs/mogan/pulls/45

### Github和Gitlab的工作流
> 以Github为例。

首先需要添加github这个remote仓库：
```
git remote add github git@github.com:XmacsLabs/mogan.git
```

创建Pull Request，可参考[Github的官方文档](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/working-with-your-remote-repository-on-github-or-github-enterprise/creating-an-issue-or-pull-request#creating-a-pull-request): 
```
git checkout -b [branch-name]
# coding and `git commit`
git push github [branch-name]
# manually create the github pull request
```
