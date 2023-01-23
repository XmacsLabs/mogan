# Git Workflow
Git is a **distributed** version control system.

## Hosting Services
Mogan is available on:
+ https://git.lug.ustc.edu.cn/XmacsLabs/mogan by USTC LUG
+ https://gitee.com/XmacsLabs/mogan by 深圳市奥思网络科技有限公司
+ https://github.com/XmacsLabs/mogan by Github, Inc.
+ (please tell us via issues from one of the hosting services)

We receives issues and pull requests from various git hosting services.

The Git workflow for different hosting services is different. There are described below.

### Workflow on Gitee
Clone and add the Gitee remote:
```
git clone git@gitee.com:XmacsLabs/mogan.git
cd mogan

git remote add gitee git@gitee.com:XmacsLabs/mogan.git
```

Create a pull request, see [Gitee Official Doc](https://gitee.com/help/articles/4346):
```
git checkout -b [branch-name]
# coding and `git commit`
git push gitee [branch-name]:main
```
Just push to the main branch, and the pull request will be created automatically.

### Workflow on Github
Add the Github remote:
```
git remote add github git@github.com:XmacsLabs/mogan.git
```

Create a pull request, see [Github Official Doc](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/working-with-your-remote-repository-on-github-or-github-enterprise/creating-an-issue-or-pull-request#creating-a-pull-request) for a detailed instruction:
```
git checkout -b [branch-name]
# coding and `git commit`
git push github [branch-name]
# manually create the github pull request
```
