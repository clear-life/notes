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

`G`

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

`ggdG`

> 删除所有行

`mGdnG`

> 删除 m 行到 n 行

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

`gg=G`

> 自动缩进所有行

`==`

> 自动缩进当前行

`mG=nG`

> m 行到 n 行自动缩进

**设置**

`:w`

> 保存

`:w!`

> 强制保存

`:q`

> 退出

`:q!`

> 强制退出

`:wq`

> 保存并退出

`:set paste`

> 设置粘贴模式, 取消代码自动缩进

`:set nopaste`

> 取消粘贴模式, 开启代码自动缩进

`:set nu`

> 显示行号

`:set nonu`

> 隐藏行号

**异常处理**

用 `vim` 编辑文件时, 自动创建一个 `.filename.swp` 临时文件

如果打开文件时临时文件已存在, 则会报错, 解决办法

1. 找到另一个打开的程序并退出
2. 直接删掉 `.filename.swp` 临时文件

## shell

### 概述

shell 是通过命令行**与操作系统交互**的语言

linux 默认使用 bash, `/bin/bash`

**运行方式**

```shell
# 可执行文件
chmod +x test.sh
./tesh.sh 或 /home/user/test.sh

# bash 解释器执行
bash test.sh
```

**查看命令类型**

```shell
type commend
```

**注释**

```shell
# 单行注释

:<<EOF(或其他任意字符串)
多行注释
EOF(对映上面)
```



### 变量

**定义变量**

```shell
# 定义变量时不可以在 = 前后加空格
name1='yjq'  # 不转义, 原样输出
name2="yjq"  # 转义, 不原样输出
name3=yjq    # 相当于双引号, 转义
```

**变量类型**

1. 自定义变量

   局部变量, 子进程不能访问

   ```shell
   name=yjq			# 自定义变量
   export name 		# 设为 exportname 变量(环境变量)
   或
   declare -x name		# 设为 exportname 变量(环境变量) 
   ```

2. 环境变量

   全局变量, 子进程可以访问

   ```shell
   export name=yjq		# 环境变量
   declare +x name		# 设为自定义变量, 与 -x 反着来
   ```

**使用变量**

```shell
name=yjq
echo $name  
echo ${name} 
```

> 只读变量
>
> `readonly name`
>
> 或
>
> `declare -r name`

**删除变量**

```shell
name=yjq 	
unset name
echo $name
```

**字符串**

```shell
name=yjq
echo '$name'	# 输出 $name
echo "$name"	# 输出 yjq

# 字符串长度
name"yjq"
echo ${#name}	# 输出 3

# 提取字串
name="Hello, world!"
echo ${name:2:3}  # 提取从0开始的3个字符, 下标从 1 开始, 输出 llo
```

**自带变量**

执行 shell 脚本时, 可以向脚本传递参数

* `$0` 是文件名(带路径)
* `$1` 是第一个参数，`$2` 是第二个参数, ...

```shell
#! /bin/bash
echo $0		# 带路径的文件名
echo $1		# 第一个参数
echo $2		# 第二个参数
...
```

| 参数        | 说明                                  |
| ----------- | ------------------------------------- |
| $#          | 传入的参数个数                        |
| $*          | 所有参数的字符串 "\$1 \$2 $3"         |
| $@          | 每个参数的字符串 "$1" "\$2" "\$3"     |
| $$          | 当前运行脚本的进程 ID                 |
| $?          | 上调命令的 exit code, 0 表示正常      |
| $(command)  | 返回 command 命令的 stdout (可嵌套)   |
| \`command\` | 返回 command 命令的 stdout (不可嵌套) |

### 数组

* 可以存放**不同类型**的值

* 但只支持**一维数组**

* 下标从 0 开始

> 数组可以跳下标使用

```shell
array=(1 'yjq' "yjq")

array[0]=1
array[1]='yjq' 
array[2]="yjq"


```

**读取数组元素**

```shell
echo ${array[2]}

# 读取整个数组
${array[*]}  
或
${array[@]}

# 数组长度
${#array[*]}  
或
${#array[@]}
```

### expr 命令

求表达式的值

```shell
expr expression
```

* 空格隔开每一项
* 特殊字符要转义
* 最好用 "" 括住一个整体
* 输出
  * stdout: 为真输出 1, 否则输出 0
  * exot code: 为真输出 0, 否则输出 0

**字符串表达式**

* `length` str	

  返回字符串 str 长度

* `index` str set

  返回 set 中任意一个字符在 str 首次出现的位置, 下标从 1 开始, 不存在则返回 0

* `substr` str pos len

  返回 str 字符串中从 pos 开始的 len 个字符的字符串, 否则返回空字符串

```shell
str="Hello World!"

expr length "$str" 		# 返回 12
expr index "$str" abcd 	# 返回 10
expr substr "$str" 2 3	# 返回 llo
```



