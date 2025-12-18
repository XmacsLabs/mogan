import os
import requests

access_token = os.getenv("Comment_TOKEN")
pull_id = os.getenv("GITEE_PULL_ID")
repo = "LiiiLabs/mogan"  
headers = {"Content-Type": "application/json;charset=UTF-8"}
api_base = f"https://gitee.com/api/v5/repos/{repo}/pulls/{pull_id}"

if not access_token or not pull_id:
    print("❌ 缺少必要环境变量：Comment_TOKEN 或 GITEE_PULL_ID")
    exit(1)

# 获取 PR 文件列表
files_url = f"{api_base}/files?access_token={access_token}"
files_response = requests.get(files_url, headers=headers)
if files_response.status_code != 200:
    print("❌ 获取文件失败:", files_response.status_code, files_response.text)
    exit(1)

files = [f["filename"] for f in files_response.json()]

# 判断是否包含文档修改，并收集文档文件
doc_files = [
    f for f in files if f.endswith((".md")) and f.startswith("devel/")
]

# 获取该 PR 的 HEAD commit SHA
pr_info_url = f"{api_base}?access_token={access_token}"
pr_info_response = requests.get(pr_info_url, headers=headers)
if pr_info_response.status_code != 200:
    print("❌ 获取 PR 信息失败:", pr_info_response.status_code, pr_info_response.text)
    exit(1)

# 通过分支名获取任务编号
pr_info = pr_info_response.json() 
commit_sha = pr_info.get("head", {}).get("sha", "")
source_branch = pr_info.get("head",{}).get("ref","")
parts = source_branch.split("/")
if len(parts)>= 2:
    pr_number = parts[1]
else :
    pr_number = "Nan"

if not commit_sha:
    print("❌ 无法获取 commit SHA")
    exit(1)


# 构建评论内容
file_name = f"{pr_number}.md"
file_url = f"https://gitee.com/XmacsLabs/mogan/blob/{source_branch}/devel/{file_name}"
if doc_files:
    message_lines = [
        "[CI 自动评论]",
        f"📂 该 PR 修改了 {len(files)} 个文件",
        "该 PR 包含文档修改 ✅，相关文件如下："
    ]
    flag = False
    for f in doc_files:
        link = f"https://gitee.com/{repo}/blob/{commit_sha}/{f}"
        if f.endswith(file_name):
            flag = True
        message_lines.append(f"- [{f}]({link})")
    if pr_number == "Nan":
        message_lines.append("⚠️  分支命名不规范")
    elif flag != True :
        message_lines.append("⚠️ 分支名和被修改的文档不匹配")
    if len(doc_files) >1:
        message_lines.append("⚠️ 注意此处修改了多个文档")
    message = "\n".join(message_lines)
else:
    message_lines = [
        "[CI 自动评论]",
        "PR 提交成功 ✅（未发现文档修改）"
    ]
    check_response = requests.get(file_url,headers=headers)
    print(pr_number,file_url)
    if pr_number == "Nan":
        message_lines.append(f"❌分支命名不规范,以至于无法找到对应文件")
    elif check_response.status_code == 200:
        message_lines.append(f"对应文档:[{file_name}]({file_url})")
    else :
        message_lines.append("⚠️ 找不到对应的文档")

    message = "\n".join(message_lines)

print(message)

# 获取所有评论
existing_comments_url = f"{api_base}/comments?access_token={access_token}"
comments_response = requests.get(existing_comments_url, headers=headers)
comments = comments_response.json() if comments_response.status_code == 200 else []

# 删除已有的 CI 自动评论
ci_comments = [c for c in comments if "[CI 自动评论]" in c["body"]]
for c in ci_comments:
    comment_id = c["id"]
    delete_url = f"https://gitee.com/api/v5/repos/{repo}/pulls/comments/{comment_id}?access_token={access_token}"
    del_response = requests.delete(delete_url, headers=headers)
    if del_response.status_code == 204:
        print(f"🗑️ 已删除旧评论 ID: {comment_id}")
    else:
        print(f"⚠️ 删除评论失败 ID: {comment_id}, 状态码: {del_response.status_code}")


# 发送评论
comment_url = f"{api_base}/comments?access_token={access_token}"
payload = {"body": message}
comment_response = requests.post(comment_url, json=payload, headers=headers)

if comment_response.status_code == 201:
    print("✅ 成功评论到 PR 页面")
else:
    print("❌ 评论失败:", comment_response.status_code, comment_response.text)
