# Anaconda

#### 下载

去官网下载对应`python`版本的`64bit`安装包

#### 安装

`python 3.x`

```
bash ~/Downloads/Anaconda3-5.2.0-Linux-x86_64.sh
```

#### 添加环境变量

在`~/.bashrc`文件后面加上

```
export PATH="/<path to anaconda>/bin:$PATH" 
注意一定要用pwd命令来查找文件真正的路径,文件系统中的home包含了/home/用户名/两层路径
```

#### 目录

安装目录

```
/home/用户名/anaconda3
```

#### 配置环境基础

```
sudo apt-get update && sudo apt-get install cmake libopenmpi-dev python3-dev zlib1g-dev
```