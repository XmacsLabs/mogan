# Git Workflow
Git is a **distributed** version control system.

## Hosting Services
Mogan is available on:

| Platform | Link | Organization or Company | Location |
|-----|-----|------------|-----|
| Gitlab | https://git.lug.ustc.edu.cn/XmacsLabs/mogan | USTC LUG | China |
| Codeberg | https://codeberg.org/XmacsLabs/mogan | Codeberg e.V. | Germany |
| Gitee | https://gitee.com/XmacsLabs/mogan           | 深圳市奥思网络科技有限公司    | China |
| Github | https://github.com/XmacsLabs/mogan          | Github, Inc.            | US    |

We receives issues and pull requests from the git hosting services listed above.

If you hope to add your favorite Git hosting service, please contact us:
+ For hosting services owned by commercial company, we hope to choose the most popular one for each country
+ For hosting services owned by organization like university, the more the better. We hope the employees or students of those oganizations can comtribute to Mogan without leaking their own privacy to commercial companies.


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

### Workflow on Github/GitLab/Gitea
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

### Why are you using multiple Git hosting services?
Because **we do not want to** stick to one Git hosting service. And we hope to receive more feedbacks and contributions from users and developers.

And Mogan is a **libre software**. We are willing to pay extra efforts to deliver the source code of Mogan to **everyone on Earth** (also on Mars in 50 years?).

### How to sync between multiple Git hosting services?
Currently, the admins of the Mogan project will sync manually.