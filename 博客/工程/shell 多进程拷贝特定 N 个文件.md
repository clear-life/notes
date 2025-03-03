# shell 多进程拷贝特定 N 个文件

## 需求

### 背景

项目中, `bin/`存放编译产生的二进制文件, `run/` 存放运行所需的二进制文件，即**开发环境**与**生产环境**分离

> 开发流程： 开发 -> 编译 -> 运行

运行时需要把 `bin/` 里所有二进制文件 copy 到 `run/` 中

### 问题

优化 copy 速度

## 方案

### 简单方案 rm-cp

```bash
rm -rf run/
cp -rf bin/ run/
```

### 优化方案 时间戳+多进程

#### 优化思路

**1. 时间戳**

记录 `bin/` 中每个文件的**时间戳**和**大小**， 如果和上次相等，就不 copy

**2. 多进程**

由于各个二进制文件的 rm-cp 是相互独立的，所以可以**并发编程**解决，不容易触发**条件竞争**

所以可以通过 shell 多进程方式来并行 rm-cp

#### 完整实现

```bash
#!/bin/bash

WORK=$(pwd)

[[ -d "$WORK" && "$WORK" != *' '* ]] || { echo "$WORK 不是一个有效目录或路径中包含空格"; exit 1; }

file_list="file1 file2 file3"
timestamps="$WORK/run/last_modified_time_stamps.txt"

remove_copy()
{
    if [ -f "$WORK/run/$1" ]; then
        rm "$WORK/run/$1"
    fi
    cp "$WORK/bin/$1" "$WORK/run/$1"

    (
        flock -w 10 -x 200 || exit 1
        sed -i "/^$1/d" $timestamps
        stat -c "$1 %y %s" "$WORK/bin/$1" >> $timestamps
    )200>lockfile
}

copy_after_check_time_stamp()
{
    bin_timestamp=$(stat -c "%y" "$WORK/bin/$1")    
    bin_size=$(stat -c "%s" "$WORK/bin/$1")    

    temp_=$(
        (
            flock -w 10 -s 200 || exit 1
            awk -v var=$1 '$1==var {print $2,$3,$4,$5}' $timestamps
            # 理解 shell awk 的解释机制
            # awk "\$1==\"$1\" {print \$2,\$3,\$4,\$5}" $timestamps
        )200>lockfile
    )

    echo "$1 $temp_"
    run_timestamp=$(echo $temp_ | awk '{print $1,$2,$3}')
    run_size=$(echo $temp_ | awk '{print $4}')

    if [[ "$bin_timestamp" != "$run_timestamp" || "$bin_size" != "$run_size" ]]; then
        echo "正在 copy $1"
        remove_copy $1
        echo "$1 copy 完成"
    else
        echo "$1 文件一致, 无需 copy"
    fi
}

if [ ! -f $timestamps ]; then
    touch $timestamps
    echo "文件名 时间戳 大小" > $timestamps
fi

for file in $file_list; do
    if [ -n "$file" ]; then
        if [ -f "$WORK/bin/$file" ]; then
            copy_after_check_time_stamp $file &
        else
            echo "文件 bin/$file 不存在"
            (
                flock -w 10 -x 200 || exit 1
                sed -i "/^$file/d" $timestamps
            )200>lockfile
            # echo "源文件 bin/$file 不存在, 结束脚本, 请重新 make"
            # ecit 1
        fi
    fi
done

wait
```

#### 收获点

**1. 多进程**

```bash
copy_after_check_time_stamp $file &
```

**2. 异常情况下 flag 文件维护**

```bash
echo "文件 bin/$file 不存在"
(
	flock -w 10 -x 200 || exit 1
	sed -i "/^$file/d" $timestamps
)200>lockfile
```

**3. 多进程读写同一文件-读**

子进程通过 stdout 向父进程返回消息

```bash
temp_=$(
    (
        flock -w 10 -s 200 || exit 1
        awk -v var=$1 '$1==var {print $2,$3,$4,$5}' $timestamps
    )200>lockfile
)
```

**4. 多进程读写同一文件-写**

```bash
(
    flock -w 10 -x 200 || exit 1
    sed -i "/^$1/d" $timestamps
    stat -c "$1 %y %s" $WORK/bin/$1 >> $timestamps
)200>lockfile
```

**5. 研究 shell awk 的解释机制**

```bash
awk "\$1==\"$1\" {print \$2,\$3,\$4,\$5}" $timestamps
等效于
awk -v var=$1 '$1==var {print $2,$3,$4,$5}' $timestamps
```

**6. 管道传递字符串**

```bash
run_timestamp=$(echo $temp_ | awk '{print $1,$2,$3}')
```

**7. shell 中一切皆为字符串**

```bash
if [ -n $file ] 为错误做法
if [ -n "$file" ] 为正确做法
```

**8. 灵活使用 flock**

```bash
exec 200>lockfile
if flock -w 10 -x 200; then
	# critical code
	flock -u 200
	exec 200>&-
else
	exec 200>&-
	exit 1
fi
```

**9. () 会创建一个子 shell**

一共有三个 shell

```bash
tmp=$(
	(
	)
)
```

验证代码

```bash
#!/bin/bash

echo "pid1:$BASHPID"

tmp=$(
    echo "pid2:$BASHPID"
    (   
        echo "pid3:$BASHPID"
        cp bin/file1 run/
    )   
)

echo $tmp
```

> 一共有 4 个子进程，最后一个为 cp 进程

**10. () 内 exit 退出 ()**

```bash
#!/bin/bash

(
    exit 1	# 不会退出整个 shell
)

echo "test"	# 正常运行
```

**11. ps -x 的参数**

```bash
PID   	TTY   	STAT   	TIME   		CMD
进程id  终端类型  进程状态 占用CPU时间 执行的命令

Teletype: pts/n 伪终端  ttyn 真终端  ? 没有关联终端(系统进程或后台进程)
STAT: R 运行/运行队列  S 中断休眠  D 不可中断休眠(I/O操作)  Z 僵尸  + 前台进程
附加标志: s 进程领导者  l 多线程  < 高优先级
```

