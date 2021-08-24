# Linux 基础课

## Linux 常用命令

| 命令       | 作用           |
| ---------- | -------------- |
| ctrl c     | 取消并换行     |
| cp XXX YYY | XXX 复制为 YYY |
| mkdir      | 创建目录       |
| touch      | 创建文件       |
| rm         | 删除文件       |
| mv XXX YYY | XXX 移动到 YYY |
| cat        | 展示文件内容   |

## tmux

### 作用 

1. 分屏
2. 后台运行进程

### 结构

一个 `tmux` 包含多个 `session`, 一个 `session` 包含多个 `window` , 一个 `window` 包含多个 `pane`

### 操作

**新建**

`tmux`

> 新建一个 session, 包含一个 window, window 包含一个 pane, pane 打开了一个 shell 对话框

前置 `ctrl + a`, 然后 `c`

> 在当前 session 创建一个新的 window

**分屏**

前置 `ctrl + a`, 然后 `%`

> 当前 pane 左右分为两个 pane 

前置 `ctrl + a`, 然后 `"`

> 当前 pane 上下分为两个 pane 

**关闭**

`ctrl + d`

> 关闭当前 pane, 若均已关闭, 则逐级关闭

**选中**

* 鼠标点击选中 `pane`
* 前置 `ctrl + a`, 然后按方向键
* 前置 `ctrl + a`, 然后 `s`, 选择其他 `session`
  * 左: 展开 
  * 右: 闭合
* 前置 `ctrl + a`, 然后 `w`, 选择其他 `window`

**设置**

* 鼠标拖动分割线调整分割线位置
* 前置 `ctrl + a`, 同时按方向键, 可调整分割线位置
* 前置 `ctrl + a`, 然后 `z`, 当前 `pane` 全屏 / 取消全屏

**挂起**

前置 `ctrl + a`, 然后 `d`

> 挂起当前 session

`tmux a`

> 打开之前挂起的 session

**文本**

* 前置 `ctrl + a`, 然后 `PageUp`, 翻页
* 在 `tmux` 中选中文本时, 要按住 `shift`

