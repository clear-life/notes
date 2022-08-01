# MySQL 必知必会

MySQL 不区分大小写, 建议**关键字用大写**, **列名和表名用小写**

`;` 表示语句的结束

空格会被忽略, 一条语句可分成多行

下标从 0 开始

**子句:** SQL 语句由**子句**构成, 子句由**关键字**和**参数**组成

正则表达式用来匹配文本或者叫字符串

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

[寻找用户推荐人](https://leetcode-cn.com/problems/find-customer-referee/)

mysql 使用三值逻辑: true, false, unknown

任何值与 null 比较的结果都是 unknown, 包括 null

只能用 is null 和 is not null 来判断是否为 null

**条件操作符**

| =                   | 等于           |
| ------------------- | -------------- |
| <>  或  !=          | 不等于         |
| <  和 <=            | 小于, 小于等于 |
| > 和 >=             | 大于, 大于等于 |
| between ... and ... | 在 ... 之间    |
| is null             | 是空值         |
| and                 | 并且           |
| or                  | 或者           |
| ()                  | 指定运算次序   |
| in                  | in (a, b, c)   |
| not                 | 否定           |

**通配符**

| like | 表示搜索模式     |
| ---- | ---------------- |
| %    | 任意字符任意次数 |
| _    | 单个任意字符     |
|      |                  |

> 尾空格会干扰通配符匹配
>
> % 不会匹配 NULL

### 关键字

```mysql
distinct	不同
limit b, x	从 b 开始的不多于 x 行
limit x offset b 从偏移 b 开始的不多于 x 行
order by 列名1, 列名2, 列名3		排序(默认升序), 列名可以是任意的列
desc 		降序
where 		过滤

union 		组合查询, 默认去除重复行
union all	不去除重复行
```



## 增删改查

### select

```mysql
select distinct 列名
from 表名;		# 返回不重复的值

select 列名1, 列名2, 列名3, ...
from 表名
where 条件
order by 列名 desc
limit b, x;		# 从 b 开始的不多于 x 行, 下标从 0 开始

select 列, from 表, where 条件
选完之后排个序 order
排序后输出哪几列
```

