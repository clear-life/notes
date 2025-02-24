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
    )200>.lockfile
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
        )200>.lockfile
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
            )200>.lockfile
            # echo "源文件 bin/$file 不存在, 结束脚本, 请重新 make"
            # ecit 1
        fi
    fi
done

wait

[ -f .lockfile ] && rm .lockfile
