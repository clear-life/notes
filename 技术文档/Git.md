# Git入门



## 运行前配置

#### 配置文件

系统级配置：`/etc/gitconfig` 文件

用户级配置：`~/.gitconfig` 或 `~/.config/git/config` 文件

仓库级配置：`.git/config`

查看所有的配置文件

```
git config --list --show-origin
```



#### 用户信息

```
git config --global user.name "John Doe"
git config --global user.email johndoe@example.com
```



#### 检查配置信息

```
git config --list
```



### 获取帮助

全面帮助

* `git help <verb>`
* `git <verb> --help`
* `man git-<verb>`

简单描述

* `git <verb> -h`



## 2.Git基础

### 2.1获取git仓库

#### 将已有目录转化为Git仓库

进入目录后,执行

```
git init
```

会创建一个`.git` 子目录,包含所有的必须文件



追踪文件

```
git add 
```



提交仓库

```
git commit -m "..."
```



#### 克隆已有的仓库

克隆远程仓库,仓库名为`newname`	

```
git clone <url> newname
直接克隆,不要在已有的库中克隆
```



### 2.5远程仓库的使用

查看远程仓库

```
git remote -v 简写与URL
git remote show <remote>	查看远程仓库的详细信息
```

添加远程仓库

```
git remote add <shortname> <url>
添加url中的远程仓库,并设置本地简写为shortname,以后可以用简写来访问远程仓库
```

从远程仓库中抓取与拉取

```
git fetch <remote>	从远程仓库remote中抓取数据
git pull	拉取数据(抓取数据后合并)
```

推送到远程仓库

```
git push <remote> <branch>	把branch推送到remote远程仓库
```

远程仓库的重命名与移除

```
git remote rename <oldname> <newname>重命名
git remote remove/rm <remote>移除
```



## 4.服务器上的Git

### 4.3生成SSH公钥

公钥在`~/.ssh`目录下

**创建密钥(公钥/私钥对)**

```
ssh-keygen -t rsa -C "email"
```



