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
| $*       |一个字符串包含所有参数|
| $@  |每个参数一个字符串|
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
echo ${array[2]}	# 需要用 {} 圈定哪些是变量

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

expr length "$str" 		# stdout 返回 12
expr index "$str" abcd 	# stdout 返回 10
expr substr "$str" 3 3	# stdout 返回 llo, 下标从 1 开始
```

**整数表达式**

优先级: 逻辑表达式 < 算术表达式 < 字符串表达式

```shell
expr \( $a + 1 \) \* \( $b + 1 \)	
# 表达式 (a + 1) * (b + 1)
```

**逻辑关系表达式**

* `|`

  如果第一个为真, 则返回第一个参数的值, 短路第二个参数

  如果第一个为假, 则看第二个参数

  如果两个都为假, 则返回 0

* `&`

  如果两个参数都为真, 则返回第一个参数的值, 否则返回 0

  如果第一个参数为假, 则短路第二个参数

* `< <= = == != >= >`

  如果表达式为真, 返回 1, 否则返回 0

> `()` 可以表示优先级, 但需要转义 

### read 命令

从标准输入读取单行数据,  `exit code` 为 0

读到文件结束符时, `exit code` 为 1

参数: `-p`: 提示信息, `-t`: 等待时间, 单位为秒

```shell
read name	# 从标准输入读一行数据到 name 变量中
```

### echo 命令

输出字符串

```shell
echo str

echo yjq
echo 'yjq'
echo "yjq"
```

**转义**

```shell
# 显示转义后的字符
name=yjq
echo "${name}\""	# 输出 yjq"

echo "\n"			# 输出 \n
# 开启转义
echo -e "\n"		# 不输出 \n , 而是换行

echo -e "y\c"		# 不换行输出 y
echo -e "j\c"		# 不换行输出 j
echo -e "q"			# 输出 q 后换行
					# 最终输出 yjq 后换行
					
# 原样输出字符串, 不转义也不取变量
echo '${name}\"'	# 输出 ${name}\", 如果是双引号则输出 yjq"
```

**重定向**

```shell
echo "yjq" > output.txt	# stdout 重定向至 output.txt
```

**显示命令的 stdout**

```shell
echo `date`			# 显示 date 命令的 stdout
```

### printf 命令

格式化输出, 默认不添加换行符

```shell
printf format-string [arguments...]

printf "%-8.2f.\n" 123.123  # 占10位，保留2位小数，左对齐
printf "%d * %d = %d\n"  2 3 `expr 2 \* 3`
```

### test 命令与判断符号[]

**逻辑运算符 && 和 ||**

* 短路原则
* exit code 作为判断条件, 0 表示真, 非 0 表示假

**test 命令**

* 判断文件类型, 对变量作比较
* exit code 作为判断条件, 0 表示真, 非 0 表示假

```shell
test 2 -lt 3 	# 为真, 返回 exit code 0
echo $?			# 输出 0

test -e test.sh && echo "exist" || echo "not exist"
# -e 表示测试是否存在, 如果存在就判断 echo "exist", 一般为真, 然后输出 echo "exist" 的 stdout, 即 exist
# 如果 test.sh 文件不存在, 就判断 echo "not exist", 一般为真, 然后输出 echo "exist" 的 stdout, 即 not exist
```

**文件类型判断**

| 参数 | 含义         |
| ---- | ------------ |
| -e   | 文件是否存在 |
| -f   | 是否为文件   |
| -d   | 是否为目录   |

**文件权限判断**

| 参数 | 含义       |
| ---- | ---------- |
| -r   | 是否可读   |
| -w   | 是否可写   |
| -x   | 是否可执行 |
| -s   | 是否非空   |

**整数的比较**

```shell
test $a -eq $b
```

| 参数 | 含义         |
| ---- | ------------ |
| -eq  | 是否相等     |
| -ne  | 是否不等     |
| -gt  | 是否大于     |
| -lt  | 是否小于     |
| -ge  | 是否大于等于 |
| -le  | 是否小于等于 |

**字符串的比较**

| 参数              | 含义                 |
| ----------------- | -------------------- |
| test -z str       | str 是否为空         |
| test -n str       | str 是否非空         |
| test str1 == str2 | str1 是否等于 str2   |
| test str1 != str2 | str1 是否不等于 str2 |

**多重条件**

| 参数 | 含义             |
| ---- | ---------------- |
| -a   | 是否都成立       |
| -o   | 是否至少一个成立 |
| !    | 取反             |

**判断符号 []**

[] 与 test 用法几乎一样, 常用于 `if` 语句中, [[]] 是 [] 的加强版

```shell
[ 2 -lt 3 ]	# 为真 exit code 返回 0
echo $?  	# 输出 0

# 判断文件是否存在
[ -e test.sh ] && echo "exist" || echo "Not exist"
```

### 判断语句

#### if 条件判断

```shell
# 单层 if
if condition
then
    语句1
    语句2
    ...
fi

# 单层 if-else
if condition
then
    语句1
    语句2
    ...
else
    语句1
    语句2
    ...
fi

# 多层 if-else
if condition
then
    语句1
    语句2
    ...
elif condition
then
    语句1
    语句2
    ...
elif condition
then
    语句1
    语句2
else
    语句1
    语句2
    ...
fi
```

示例

```shell
a=3
b=4

if ! [ "$a" -lt "$b" ]	# 如果 a < b
then					# 为真
    echo ${a}不小于${b}
else					# 为假
    echo ${a}小于${b}
fi
```

#### case 条件判断

```shell
case $变量名称 in
    值1)
        语句1
        语句2
        ...
        ;;  # 类似于C/C++中的break
    值2)
        语句1
        语句2
        ...
        ;;
    *)  	# 类似于C/C++中的default
        语句1
        语句2
        ...
        ;;
esac
```

示例

```shell
a=3

case $a in
    1)
        echo ${a}等于1
        ;;  
    2)
        echo ${a}等于2
        ;;  
    3)                                                
        echo ${a}等于3
        ;;  
    *)
        echo 其他
        ;;  
esac

# 输出 3
```

### 循环语句

#### for 循环

**for…in…do…done**

```shell
for var in val1 val2 val3
do
    语句1
    语句2
    ...
done
```

**for ((…;…;…)) do…done**

```shell
for ((expression; condition; expression))
do
    语句1
    语句2
done

for ((i=1; i<=10; i++))
do
    echo $i
done
```

#### while 循环

**while…do…done**

```shell
while condition
do
    语句1
    语句2
    ...
done
```

**until…do…done**

```shell
until condition
do
    语句1
    语句2
    ...
done
```

#### break

跳出当前循环, 但不能跳出 `case` 语句

#### continue

跳出当前循环, 执行下一层循环

#### 死循环处理

1. Ctrl + c 杀死进程
2. 关闭进程
   1. 使用 `top` 找到进程 PID
   2. `kill -9 PID` 杀死进程

### 函数

返回 exit code , 范围为 0-255, 0 表示正常 

函数的返回值(exit code) 通过 `$?` 获取

函数的内容(stdout) 通过 `echo $(function)` 获取

```shell
[function] func_name() {	# function关键字可以省略
    语句1
    语句2
    ...
}
```

**获取 exit code 和 stdout 值**

```shell
func()
{
    name=yjq
    echo "Hello $name"

    return 17
}
echo $?			# 输出 17
echo $(func)	# 输出 Hello yjq
 
```

**函数的参数**

`$1` 表示第一个参数, `$2` 表示第二个参数, 以此类推 

> `$0` 仍是 shell 文件的文件名, 不是函数名

```shell
fun()
{
	echo $1
}

fun 3	# 函数调用
```



**函数的局部变量**

函数内定义的局部变量作用范围仅限于当前函数

```shell
#! /bin/bash

func() {
    local name=yjq	# 局部变量 name	
    echo $name		# 输出 name 的内容(stdout)
}
func		# 调用 func 函数, 输出 yjq

echo $name	# 不能输出局部变量的内容
```

**exit 命令**

退出当前 shell 进程, 返回一个退出状态 exit code, 使用 `$?` 接收退出状态

```shell
#! /bin/bash

if [...]  
then
	...
    exit 1		# exit code 为 1
else
	...
    exit 0		# exit code 为 0
fi
```

### 文件重定向

每个进程默认打开 3 个文件描述符

* `stdin` 标准输入, 文件描述符为 0
* `stdout` 标准输出, 文件描述符为 1
* `stderr` 标准错误输出, 文件描述符为 2

| 命令             | 说明                           |
| ---------------- | ------------------------------ |
| command > file   | stdout 重定向到 file           |
| command >> file  | stdout 追加重定向到 file       |
| command < file   | stdin 重定向到 file            |
| command n> file  | 文件描述符 n 重定向到 file     |
| command n>> file | 文件描述符 n 追加重定向到 file |

### 引入外部文件

```shell
. filename 

或

source filename
```

## ssh

### 登录

**普通登录**

```shell
ssh user@hostname
# user 用户名
# hostname 主机名: IP 地址或域名
```

第一次回会提示无法确定真实性, 确认是否连接

输入 `yes` 后远程服务器的信息会记录在 `~/.ssh/known_hosts` 中

默认登陆端口号为 22

```shell
ssh user@hostname -p 22
```

**配置文件**

创建文件 `~/.ssh/config`

```shell
Host remoteserver1			# remoteserver1 就是远程服务器的别名了, 以后可以直接用别名登录
	HostName IP地址或域名
	User 用户名
	
Host remoteserver2
	HostName IP地址或域名
	User 用户名

...
```

用别名登录

```shell
ssh remoteserver1
```

**密钥登陆**

创建密钥

```shell
ssh-keygen
```

密钥

* `~/.ssh/id_rsa` :  私钥
* `~/.ssh/id_rsa.pub` :  公钥

公钥传递

* 将公钥中的内容复制到远程服务器的 `~/.ssh/authorized_keys` 文件里
* `ssh-copy-id remoteserver1` 命令

**远程执行命令**

```shell
ssh user@hostname command

如:
ssh user@hostname ls -a

# 引号的作用和小括号一样, 表明是一个整体, 而不是 shell 语法中的字符串

# 单引号中的$i可以求值
ssh myserver 'for ((i = 0; i < 10; i ++ )) do echo $i; done'

# 双引号中的$i不可以求值
ssh myserver "for ((i = 0; i < 10; i ++ )) do echo $i; done"
```

### scp

```shell
# 将 source 文件复制到 destination 中
scp source1 source2 destination


# 参数放在 source 前
# 复制文件夹
scp -r source destination

# 指定端口号
scp -P 22 source destination

# 将本地的配置文件复制到远程服务器的家目录下(~/)
scp ~/.vimrc ~/.tmux.conf myserver:
```

