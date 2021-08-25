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

* `shift + insert`

  > 粘贴/复制

## vim

### 功能

命令行下的文本编辑器

**vim filename**

* 如果已有文件, 就打开文件
* 如果没有文件, 就新建文件

### 模式

**普通命令模式**

默认模式

**编辑模式**

输入文本

**命令行模式**

普通命令模式下 `:/?` 进入命令行模式

### 操作

**模式转换**

`i`

> 进入编辑模式

`ESC`

> 进入普通命令模式

**光标移动**

`n<space>`

> 移动 n 个字符

`0` 或 `[Home]`

> 移动到行头

`$` 或 `[end]`

> 移动到行尾

`n<Enter>`

> 下移 n 行

`nG` 或 `:n`

> 移动到第 n 行

`gg`

> 移动到首行, 等价于 1G

`G:`

> 移动到尾行

**查找**

`/word`

> 光标后顺序查找第一个 word

`?word`

> 光标前逆序查找第一个 word

`n`

> 下一个查找操作

`N`

> 上一个查找操作

**替换**

`:n1,n2s/word1/word2/g`

> 第 n1 行到第 n2 行查找 word1, 替换为 word2

`:n1,$s/word1/word2/g`

> 全文查找 word1, 替换为 word2

`:n1,$s/word1/word2/gc`

> 全文查找 word1, 替换为 word2, 替换前需确认

**文本**

`v`

> 选中文本

`d`

> 删除选中的文本

`dd`

> 删除当前行

`y`

> 复制选中的文本

`yy`

> 赋值当前行

`p`

> 在下一个/下一行粘贴赋值的文本

`u`

> 撤销

`ctrl + r`

> 取消撤销

`>` 和 `<`

> 将选中的文本整体向右/向左缩进一次

**设置**

`:w`

> 保存

`:w!`

> 强制保存

`:q`

> 退出

`q!`

> 强制退出

`wq`

> 保存并退出

`:set paste`

> 设置粘贴模式, 取消代码自动缩进

`:set nopaste`

> 取消粘贴模式, 开启代码自动缩进

`set nu`

> 显示行号

`set nonu`

> 隐藏行号

**异常处理**

