# shell

### 基础

```shell
#! /bin/bash	# 指定解释器
```

**执行**

1. 可执行文件
2. 解释器运行

### 变量

```shell
# 定义
x = 'yjq'	// 默认是字符串
    
# 使用
$x
${x}

# 删除
unset x
    
# 字符串
单引号: 原样输出, 不执行, 不取变量
双引号: 可以执行, 取变量
```

### 默认变量

```shell
$0 文件名及路径
$1 第一个参数
$2 第二个参数
$# 参数个数
$$ 进程 id
```

### 函数

**两种返回值**

1. `exit code`

   0-255

   用 `$?` 获取

2. `stdout`

   `echo`

   用 `$(fun)` 获取

**参数**

`$1` 第一个参数, `$2` 第二个参数

### 常见命令

**expr**

表达式求值

**read**

输入中读单行数据

**echo**

输出字符串

**printf**

格式化输出

**test**

判断文件类型, 变量做比较

### 控制语句

```C++
if 
then
    
elif
then
    
elif
then
    
else
    
    
for  in 
do
    
done
```

### 引入外部脚本

```shell
. filename
source filename
```

