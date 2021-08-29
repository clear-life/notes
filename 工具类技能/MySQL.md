# MySQL

\`\`

> 反引号的作用是防止字段名与 SQL 关键字冲突

## SELECT

### SELECT DISTINCT

查询不重复列数据

```mysql
SELECT DISTINCT `column` 	# 查询不重复的列数据
```

### SELECT WHERE 

筛选过滤

```MySQL
select `column1`, `column2`
from `table`
where 判断表达式
```

## INSERT

### INSERT INTO

插入数据

```mysql
# 不指定列, 插入多个一整行数据
insert into `table`
values ("a",1),("b",2)	# 插入数据要与所有列对应上, 可插入多行数据

# 指定列
insert into `table` (`column1`,`column2`)
values ("a",1),("b",2)	# 插入数据要与指定的列对应上, 可插入多行数据
```

## UPDATE

更新数据

```mysql
update `table`
set `column1` = value1, `column2` = value2,...
where `some_column` = some_value;
```

## DELETE

删除数据

```mysql
delete from `table`		# 删除所有行数据

delete from `table`		# 删除符合条件的行数据
where `column` relation_operator value
```

## 运算符

### 比较运算符

| 比较运算符 | 说明   |
| ---------- | ------ |
| =          |        |
| !=         | 不等于 |
| <>         | 不等于 |
| <          |        |
| <=         |        |
| >          |        |
| >=         |        |

### 逻辑运算符

| 逻辑运算符 | 说明 |
| ---------- | ---- |
| AND        | 与   |
| OR         | 或   |
| NOT        | 非   |



## 特殊条件

### IN

多条件 and

```mysql
SELECT *
FROM `table`
WHERE column IN (value1, value2);
```

### NOT IN

排除

```mysql
SELECT *
FROM `table`
WHERE `column` NOT IN (value1, value2);
```

### BETWEEN AND

两个值之间

```mysql
SELECT *
FROM `table`
WHERE `column` BETWEEN value1 AND value2;
# mysql 中包含两端点, 即闭区间
```



### IS NULL 和 IS NOT NULL

判断是否为空

```mysql
SELECT *
FROM `table`
WHERE `column` IS NULL;

SELECT *
FROM `table`
WHERE `column` IS NOT NULL;
```

### LIKE 

模糊查询

```mysql
SELECT *
FROM `table`
WHERE `column` LIKE  value;

"D%" 表示 D 开头的所有单词
D 表示字母 "D"
% 是通配符, 表示 0~n 个字符
```

## 输出格式

### ORDER BY

排序

```mysql
SELECT `column1`, `column2`
FROM `table`
ORDER BY `column1`, `column2` ASC|DESC;	# ASC 升序, DESC 降序
# 排序依据: 先按 column1, 再按 column2
```

### LIMIT

限制

```mysql
SELECT `column1`, `column1`
FROM `table`
LIMIT `offset` , `count`;
```

> 如有 ORDER BY , 则 LIMIT 需放在 ORDER BY 之后

## 算数函数

### AVG()

均值

```mysql
SELECT AVG(`column`) AS `别名`
FROM `table`;
```

> AS 取别名

