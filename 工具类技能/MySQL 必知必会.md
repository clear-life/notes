# MySQL 必知必会

MySQL 不区分大小写, 建议**关键字用大写**, **列名和表名用小写**

`;` 表示语句的结束

空格会被忽略, 一条语句可分成多行

下标从 0 开始

## 通用

```mysql
use 数据库名;	选择数据库
show databases;
show tables;
show columns from 表名;	返回表的字段信息
help show;	显示允许的 show 语句

通配符 * 表示 "全部"

完全名称:
表名.列名
数据库名.表名
```



### 关键字

```mysql
distinct	不同
limit b, x	从 b 开始的不多于 x 行
limit x offset b 从偏移 b 开始的不多于 x 行
```



## 增删改查

### 查

```mysql
select 列名1, 列名2, 列名3, ...
from 表名
limit b, x;		# 从 b 开始的不多于 x 行, 下标从 0 开始

select *
from 表名;

select distinct 列名
from 表名;		# 返回不重复的值
```

