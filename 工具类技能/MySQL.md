# MySQL

\`\`

> 反引号的作用是防止字段名与 SQL 关键字冲突

## 语句

### SELECT

**SELECT DISTINCT**

查询不重复列数据

```mysql
SELECT DISTINCT `column` 	# 查询不重复的列数据
```

**SELECT WHERE** 

筛选过滤

```MySQL
select `column1`, `column2`
from `table`
where 判断表达式
```

### INSERT

**INSERT INTO**

插入数据

```mysql
# 不指定列, 插入多个一整行数据
insert into `table`
values ("a",1),("b",2)	# 插入数据要与所有列对应上, 可插入多行数据

# 指定列
insert into `table` (`column1`,`column2`)
values ("a",1),("b",2)	# 插入数据要与指定的列对应上, 可插入多行数据
```

### UPDATE

更新数据

```mysql
update `table`
set `column1` = value1, `column2` = value2,...
where `some_column` = some_value;
```

### DELETE

删除数据

```mysql
delete from `table`		# 删除所有行数据

delete from `table`		# 删除符合条件的行数据
where `column` relation_operator value
```

## 控制

### 运算符

#### 比较运算符

| 比较运算符 | 说明   |
| ---------- | ------ |
| =          |        |
| !=         | 不等于 |
| <>         | 不等于 |
| <          |        |
| <=         |        |
| >          |        |
| >=         |        |

#### 逻辑运算符

| 逻辑运算符 | 说明 |
| ---------- | ---- |
| AND        | 与   |
| OR         | 或   |
| NOT        | 非   |



### 特殊条件

**IN**

多条件 and

```mysql
SELECT *
FROM `table`
WHERE column IN (value1, value2);
```

**NOT IN**

排除

```mysql
SELECT *
FROM `table`
WHERE `column` NOT IN (value1, value2);
```

**BETWEEN AND**

两个值之间

```mysql
SELECT *
FROM `table`
WHERE `column` BETWEEN value1 AND value2;
# mysql 中包含两端点, 即闭区间
```



**IS NULL 和 IS NOT NULL**

判断是否为 NULL

> NULL 不是空值的意思, 而是未知的意思, 本身就是一个与其他所有值区分开的值

```mysql
SELECT *
FROM `table`
WHERE `column` IS NULL;

SELECT *
FROM `table`
WHERE `column` IS NOT NULL;
```

**LIKE** 

模糊查询

```mysql
SELECT *
FROM `table`
WHERE `column` LIKE  value;

"D%" 表示 D 开头的所有单词
D 表示字母 "D"
% 是通配符, 表示 0~n 个字符
```

### 输出格式

**ORDER BY**

排序

```mysql
SELECT `column1`, `column2`
FROM `table`
ORDER BY `column1`, `column2` ASC|DESC;	# ASC 升序, DESC 降序
# 排序依据: 先按 column1, 再按 column2
```

**LIMIT**

限制

```mysql
SELECT `column1`, `column1`
FROM `table`
LIMIT `offset` , `count`;
```

> 如有 ORDER BY , 则 LIMIT 需放在 ORDER BY 之后

## 函数

### 算数函数

**AVG()**

均值

```mysql
SELECT AVG(`column`) AS `别名`
FROM `table`;
```

> AS 取别名

**MAX()**

最大值

```mysql
SELECT MAX(`column`) 
FROM `table`;
```

**MIN()**

最小值

```mysql
SELECT MIN(`column`) 
FROM `table`;
```

**SUM()**

求和

```mysql
SELECT SUM(`column`) 
FROM `table`;
```

**ROUND()**

四舍五入

```mysql
SELECT ROUND(`column`, `decimals`) 
FROM `table`;
# decimals: 返回的小数位数, 默认为 0 
```

**NULL()**

判断空值

**ISNULL()**

判断是否为空

```mysql
SELECT ISNULL(`column`)
FROM `table`;
```

**IFNULL()**

判断是否为空, 并有备用值

```mysql
SELECT IFNULL(`column`, `value`)
FROM `table`;
# 如果是 NULL 则返回 value
# 如果不是, 则正常返回
```

> COALESCE(column, value) 用法和 IFNULL(column, value) 一摸一样

**COUNT()**

计数

```mysql
SELECT COUNT(`column`) 
FROM `table`;

COUNT(column)
# 对指定列进行计数
# 不包含值为 NULL 的行
# 包含空字符串 "" 所在的行

COUNT(*)
# 统计表中所有行数, 包括值为 NULL 的行和重复项所在的行
# 不包含值全为 NULL 的行
# 包含空字符串 "" 所在的行

COUNT(DISTINCT column)
# 统计不重复的行
```

### 时间函数

#### 当前时间

NOW(),CURDATE(),CURTIME()

**NOW()**

```mysql
NOW(number)
number 为精确的毫秒位数, 默认为 0
返回当前时间
格式: Y-M-D h:m:s
```

**CURDATE()**

```myql
返回当前时间
格式: Y-M-D
```

**CURTIME()**

```mysql
CURTIME(number)
number 为精确的毫秒位数, 默认为 0
返回当前时间
格式: h:m:s
```

#### 提取日期和时间

**DATE()**

提取时间中的年月日信息

```mysql
DATE(Y-M-D h:m:s)
返回  Y-M-D
```

**TIME()**

提取时间中的时分秒信息

```mysql
TIME(Y-M-D h:m:s)
返回  h:m:s
```

**EXTRACT()**

提取想要的一种时间信息

```mysql
SELECT EXTRACT(unit FROM date)
FROM `table`
# 从 date 中提取 unit 类型的时间信息
# unit: YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
# date: 合法的时间表达式 Y-M-D h:m:s
```

**DATE_FORMAT()**

格式化提取时间信息

```mysql
DATE_FORMAT(date,format)
# 从 data 中按照 format 格式提取时间
# date: 数据
# format: 时间的格式
# 返回时间字符串
```

#### 修改时间

**DATE_ADD()**

增加时间

```mysql
SELECT DATE_ADD(`date`, INTERVAL expr type)
FROM `table`
# date: 起始时间
# INTERVAL: 关键字, 间隔
# expression: 表达式, 时间间隔的数值
# type: 类型, 时间间隔的单位(MICROSECOND,SECOND,MINUTE,HOUR,DAY,WEEK,MONTH,QUARTER,YEAR)
```

**DATE_SUB()**

减少时间

```mysql
SELECT DATE_SUB(`date`, INTERVAL expr type)
FROM `table`
# date: 起始时间
# INTERVAL: 关键字, 间隔
# expression: 表达式, 时间间隔的数值
# type: 类型, 时间间隔的单位(MICROSECOND,SECOND,MINUTE,HOUR,DAY,WEEK,MONTH,QUARTER,YEAR)
```

#### 计算日期差

**DATEDIFF()**

天数差

```mysql
DATEDIFF(time1, time2)
# 计算 time1 - time2 的天数差 
# time 格式为 "Y-M-D"
```

**TIMESTAMPDIFF()**

时间戳差

```mysql
TIMESTAMPDIFF(type, time1, time2)
# 计算 time2 - time1 的时间差, type 为时间单位
# time 格式为 "Y-M-D"
# type: 时间单位
```

## 约束

规定表的数据规则

### 非空约束

**NOT NULL**

列不能为空

```mysql
CREATE TABLE `table` (
    `id` int NOT NULL,
    `name` varchar(255) NOT NULL,
    `age` int
);
# 创建表, 设置列名及属性
```

**ALTER**

修改表

```mysql
ALTER TABLE `Persons`		# 修改表
MODIFY `age` int NOT NULL;	# 修改 `age` 列为 int NOT NULL
MODIFY `age` int NOT NULL;	# 修改 `age` 列为 int NULL, 即允许为空

# 空串不是空值
# 空串是 "", 是一个有效值
# 空值是 NULL, 是没有值的意思
```

### 唯一约束

**UNIQUE**

列的值不能重复

```mysql
CREATE TABLE `table`
(
`id` int NOT NULL,
`name` varchar(255) NOT NULL,
`age` int
UNIQUE (`id`)	# id 列的值不能重复
CONSTRAINT id_name UNIQUE (`id`,`name`)	# 唯一约束两列
# CONSTRAINT id_name 给约束起名字, 方面操作
)

# 在已有的表上增加唯一约束
ALTER TABLE `table`
ADD CONSTRAINT id_name UNIQUE (`id`,`name`)

# 在已有的表上删除约束
ALTER TABLE `table`
DROP INDEX id_name	# 删除约束名为 id_name 的约束
```

### 主键约束

**PRIMARY KEY**

* 约束一列时, `PRIMARY KEY = UNIQUE + NOT NULL`
* 约束多列时, `PRIMARY KEY` 仅保证多列之和唯一, 某一列可能重复
* 主键必须设为 `NOT NULL`

**创建表时设置主键约束**

```mysql
# 创建时添加主键约束
CREATE TABLE `table` (
    `id` int NOT NULL,
    `name` varchar(255) NOT NULL,
    `age` int
    PRIMARY KEY (`id`)
);

# 多列为主键
CREATE TABLE `table` (
    `id` int NOT NULL,
    `name` varchar(255) NOT NULL,
    `age` int
    CONSTRAINT id_name PRIMARY KEY (`id`,`name`)
);
```

**新增主键约束**

```mysql
# 一列为主键
ALTER TABLE `table`
ADD PRIMARY KEY (`id`)

# 多列为主键
ALTER TABLE `table`
ADD CONSTRAINT id_name PRIMARY KEY (`id`,`name`)
```

**删除主键约束**

```mysql
# 删除一列型主键
ALTER TABLE `table`
DROP PRIMARY KEY

# 删除多列型主键
ALTER TABLE `Persons`
DROP CONSTRAINT id_name
```

### 外键约束

**创建表时设置外键约束**

```mysql
CREATE TABLE `table`
(
`id` int NOT NULL,
`foreign_id` int,
PRIMARY KEY (`id`),
FOREIGN KEY (`foreign_id`) REFERENCES table2(`foreign_id`)
)
# 外键 foreign_id 引用自 table2 的主键 foreign_id

# 命名外键约束
CONSTRAINT f_if FOREIGN KEY (`foreign_id`) REFERENCES Persons(`foreign_id`)
```

> REFERENCES 表示 引用一个表

**添加外键约束**

```mysql
ALTER TABLE `table`
ADD FOREIGN KEY (`id`) REFERENCES table2(`foreign_id`)
```

**删除外键约束**

```mysql
ALTER TABLE `table`
DROP FOREIGN KEY (`foreign_id`)
```

### 检查约束

限制列的值的范围

**创建时添加**

```mysql
# 一列添加约束
CREATE TABLE `table`
(
`id` int,
`count` int,
CHECK (`count` > 0)
)

# 多列添加约束
CREATE TABLE `table`
(
`id` int,
`count` int,
CHECK (`count` > 0 AND `id` > 0)
)

# 为检查约束命名
CONSTRAINT chk_count CHECK (`count` > 0);
```

**添加约束**

```mysql
ALTER TABLE `table`  
ADD CONSTRAINT chk_courses CHECK ( `count` > 0 AND `id` > 0);
```

**删除约束**

```mysql
ALTER TABLE `table` 
DROP CHECK chk_courses
```

### 默认约束

设置列的默认值

```mysql
# 创建时添加
CREATE TABLE `table`
(
    `id` int NOT NULL,
    `city` varchar(255) DEFAULT 'Beijing'
)

CREATE TABLE `table`
(
    `id` int NOT NULL,
    `date` date DEFAULT GETDATE()
)

# 添加默认约束
ALTER TABLE `table`
ALTER `city` SET DEFAULT 'Beijing'

# 删除默认约束
ALTER TABLE `table`
ALTER `city` DROP DEFAULT
```

## 多表连结

**联结**

关联多个表, 返回一组输出

### 创建联结

```mysql
`table1`.`column1` = `table2`.`column2`
# 把 table1 中的 column1 与 table2 中的 column2 联结起来

# teachers 表中的 id 是主键, courses 表中的 teacher_id 是外键
`teachers`.`id` = `courses`.`teacher_id` 
```

### JOIN 连接子句

将两个及以上表的数据组合起来, 主要有四类

* **INNER JOIN(内连接)**
* **OUTER JOIN(外连接)**
  * **LEFT JOIN(左外连接)**
  * **RIGHT JOIN(右外连接)**
  * **FULL JOIN(全连接)**
* **CROSS JOIN(交叉连接)**

#### INNER JOIN(内连接)

A 和 B 的交集

```mysql
SELECT `table1`.`column1`, `table2`.`column2`...
FROM `table1`
INNER JOIN `table2`		# INNER 可省略不写
ON `table1`.`common_field` = `table2`.`common_field`;
# common_field 在 table1 表中是外键
# common_field 在 table2 表中是主键

SELECT `c`.`id`, `c`.`name` AS `cname`, `t`.`name` AS `tname`
FROM `courses` `c`
    INNER JOIN `teachers` `t` ON `c`.`teacher_id` = `t`.`id`;
# courses.teacher_id 是 courses 中的外键
# teachers.id 是 teacher 中的主键
```

> ```mysql
> FROM `courses` `c`			# courses 的别名是 c
> INNER JOIN `teachers` `t`	# teachers 的别名是 t
> ```

#### OUTER JOIN(外连接)

A 和 B 的差集

```mysql
SELECT column1,column2 ... columnn
    FROM table1
        LEFT | RIGHT | FULL  (OUTER) JOIN table2
        ON CONDITION;
# table1 是 A 集合
# table2 是 B 集合
# 左外联结, A-B + AB, A 的所有行 + B 的匹配行
# 右外连接, B-A + AB, B 的所有行 + A 的匹配行
# 全外连接, A + B 
```

**左外连接**

以左边(table1)为主

```mysql
SELECT column1,column2 ... columnn
    FROM table1
        LEFT JOIN table2
        ON CONDITION ;
```

**右外连接**

以右边(table2)为主

```mysql
SELECT column1,column2 ... columnn
    FROM table1
        RIGHT JOIN table2 ON CONDITION ;
```

**全外连接**

table1 + table2

> MySQL 不支持全连接, 但可以用 UNION ALL 将左连接和右连接的结果组合起来实现全连接

#### UNION

把两次及以上的查询结果合并起来

> * 列数和(数据的)顺序必须一致, 列名可以不同
>
> * UNION 结果中的列名等于第一个 SELECT 语句中的列名
> * UNION 会将数据相同的行合并为一行
>   * UNION ALL 不会去掉重复行
> * 如果子句有 order by, limit, 则要用 () 包起来放在所有语句的最后

```mysql
SELECT column1,column2 ... columnn
    FROM table1
        LEFT JOIN table2 ON CONDITION 
UNION
SELECT column1,column2 ... columnn
    FROM table1
        RIGHT JOIN table2 ON CONDITION ;
```

**CROSS JOIN(交叉连接)**

笛卡尔乘积

```mysql
# 隐式交叉连接
SELECT `table1`.`column1`, `table2`.`column2`...
FROM `table1`,`table2`;

# 显式交叉连接
SELECT `table1`.`column1`, `table2`.`column2`...
FROM `table1`
CROSS JOIN `table2`;
```

## 分组查询

### GROUP BY

分组

```mysql
SELECT `column`, aggregate_function(`column`)
FROM `table`
WHERE `column` operator value
GROUP BY `column`;
# aggregate_function: 分组统计函数, 分组统计用
# 以不同的 column 为分组

# 单表分组
SELECT `country`, COUNT(`country`) AS `teacher_count`
FROM `teachers`
GROUP BY `country`
ORDER BY `teacher_count`, `country`;
# 以 country 的不同为分组, 统计每个分组的大小作为 teacher_count
# 最后以 teacher_count 和 country 作为排序依据

# 多表分组
SELECT T.name AS `teacher_name`, IFNULL(SUM(C.student_count), 0) AS `student_count`
FROM `courses` C
	RIGHT JOIN `teachers` T ON C.teacher_id = T.id
GROUP BY T.id;
# 以 teachers 为主, 以 teachers.id 的不同为分组依据
# 统计教师名字和教授课程的学生的人数
```

### HAVING

对分组统计函数过滤

> WHERE 在分组前过滤, HAVIND 在分组后过滤

```mysql
SELECT   `column`, aggregate_function(`column`) 
FROM     `table` 
WHERE    `column` operator value 	# 分组前过滤
GROUP BY `column` 	# 分组依据
HAVING   aggregate_function(`column`) operator value;	# 分组后过滤
```

```mysql
# 错误代码
select *
from `teachers`
group by `country`
having avg(`age`) > (select avg(`age`) from `teachers`)

# 正确代码
select *
from `teachers`
where `country` in (
                    select `country`
                    from `teachers`
                    group by `country`
                    having avg(`age`) > (select avg(`age`) from `teachers`)
                    )
```

## 子查询

一个查询是另一个查询的条件

### SELECT

```mysql
SELECT `column`		# table1 表的 column 列
FROM `table1`
WHERE `column` OPERATOR (
    SELECT `column`	# table2 表的 column列
    FROM `table2`
);
```

### INSERT

子查询的结果作为要插入的数据

```mysql
INSERT INTO `table`
	SELECT `colnum`
	FROM `table`
	[ WHERE VALUE OPERATOR ]
```

### UPDATE

用子查询的结果更新数据

```mysql
UPDATE `table` 
SET `column` = `value`
WHERE `column` OPERATOR 
   (SELECT `column`
   FROM `table`
   [WHERE] )
```

### DELETE

用子查询的结果筛选条件

 ```mysql
 DELETE FROM `table1`
 WHERE `column` OPERATOR 
    (SELECT `column`
    FROM `table2`  
    [WHERE] )
 ```

> 删除数据的表不能是查询的表

### 内联视图子查询

在子查询中查询

```mysql
SELECT *
FROM (			# 子查询结果为国家是美国的所有教师
	SELECT *
	FROM `teachers`
	WHERE `country` = 'USA'
) `T`
WHERE `age` = (
	SELECT MAX(`age`)
	FROM `teachers`
);

select *
from (
    select `c`.`name` as `course_name`, `c`.`student_count`,`t`.`name` as `teacher_name`
    from `courses` `c`
    inner join `teachers` `t` on `c`.`teacher_id` = `t`.`id`
) `T`
where `T`.`student_count` = (select max(`student_count`) from `courses`)
```

### IN 子查询

查询条件是 IN 子查询

```mysql
SELECT `column`
FROM `table`
WHERE `column` IN(
    SELECT `column`
    FROM `table`
    WHERE `column` = VALUE
);

select `name`
from `courses`
where `teacher_id` in (
    select `id`
    from `teachers`
    where `age` > 20 
)
```

### ANY 子查询

子查询满足任何一个比较运算就返回真

```mysql
SELECT `column`
FROM `table`
WHERE `column` OPERATOR
   ANY(	SELECT column
   		FROM table)
   		
select `name`
from `courses`
where `created_at` > any(
    select `c`.`created_at`
    from `courses` `c`
    	right join `teachers` `t` on `c`.`teacher_id` = `t`.`id`
    where `t`.`name` = "Southern Emperor"
)
and `teacher_id` != (select `id` from `teachers` where `name` = "Southern Emperor")
```

### ALL 子查询

所有子查询的结果满足就返回真

```mysql
SELECT `column`
FROM `table`
WHERE `column` OPERATOR
   ALL(SELECT column
   FROM table)
   
   
select *
from `courses`
where `student_count` > all(
    select `student_count`
    from `courses`
    where `teacher_id` in (
        select `id`
        from `teachers`
        where `age` in (select max(`age`) from `teachers`)
    )
)
```

### 多列子查询

* 单行多列子查询
  * 用单行比较符号: ＝，>，<，>=，<=， <> 
* 多行多列子查询
  * 用多行比较符号: IN，ANY，ALL

```mysql
SELECT `name`, `age`, `country` 
FROM `teachers` 
WHERE (`country`, `age`) IN ( 
    SELECT `country`, MAX(`age`) 
    FROM `teachers` 
    GROUP BY `country` 
);

select `name`,`student_count`
from `courses`
where `id` in (
    select `id`
    from `courses`
    where (`teacher_id`,`student_count`) in (
        select `teacher_id`,max(`student_count`)
        from `courses`
        group by `teacher_id`
    ) 
)
```

### HAVING 子查询

分组后过滤子查询

```mysql
SELECT `name`
FROM `teachers`
WHERE `id` IN (
	SELECT `teacher_id`
	FROM `courses`
	GROUP BY `teacher_id`
	HAVING AVG(`student_count`) > (
		SELECT AVG(`student_count`)
		FROM `courses`
	)
);

select `t`.`country` as `country`,ifnull(sum(`c`.`student_count`),0) as `student_count`
from `courses` `c`
inner join `teachers` `t` on `c`.`teacher_id` = `t`.`id` 
where `t`.`country` like "U%"
group by `t`.`country`
having `student_count` between 2000 and 5000
order by `student` desc, `t`.`country` asc
```

## 事物

**加锁**, 要么成功执行, 要么失败回滚到最初

### MySQL 事物语句

* `START TRANSACTION`
  * 启动事物, 别名: `BEGIN` 或 `BEGIN WORK`

* `COMMIT`

  * **提交**当前事务并永久化

* `ROLLBACK`

  * **回滚**当前事务并取消变化

* `SET autocommit`

  * **禁用或启用**当前事务的**自动提交**

    ```mysql
    # 禁用自动提交
    SET autocommit = 0
    SET autocommit = OFF
    
    # 启用自动提交
    SET autocommit = 1
    SET autocommit = ON
    
    insert into `teachers`(`name`,`age`,`country`)
    values ("Xie Xu", 49,"CN")
    ```

    

### 实例

```mysql
BEGIN;

insert into `teachers`(`name`,`age`,`country`)
values ("Xie Xun", 49,"CN");

COMMIT;


```

## 锁

访问共享资源的一种保护机制

* 重要性: 保证数据并发访问的一致性, 有效性
* 缺点: 锁的操作需要额外消耗资源

**查看数据库的事务隔离级别**

```mysql
show variables like "%isolation%"
```

### InnoDB 锁的类型

InnoDB 是 MySQL 上第一个提供外键约束的数据存储引擎

**自增锁**

```mysql
# 查询自增锁模式
show variables like '%innodb_autoinc_lock_mode%';
```

* **0**：`traditonal`(传统)

  每次 `insert` 语句执行都会产生**表级别的锁**

* **1**：`consecutive simple insert`(连续简单插入, 默认级别)

  批量加锁, 保证插入的连续性, 但在**插入前解锁**

* **2**：`interleaved`(交错)

  不加锁, 实施插入, 并发最高, 但不安全

**意向锁**

特殊的表级锁

**提前声明**未来某一时刻**可能要加共享锁或排他锁**

* 意向共享锁(Intention Shared Lock) IS

  事务有意向对**某些行加共享锁(S锁)**

  ```mysql
  # 事务要获取某些行的 S 锁，必须先获得表的 IS 锁。
  SELECT column FROM table ... LOCK IN SHARE MODE;
  ```

* 意向排他锁(Intention Exclusive Lock) IX

  事务有意向对**某些行加排他锁(X锁)**

  ```mysql
  # 事务要获取某些行的 X 锁，必须先获得表的 IX 锁。
  SELECT column FROM table ... FOR UPDATE;
  ```

* 例:

  ```mysql
  # 设置 IS 锁 
  select * from A where id = 1 lock in share mode;
  # 设置 IX 锁
  select * from A where id > 0 for update;
  ```

**意向锁协议**

* 事务要获取表 A 某些行的 S 锁必须要获取表 A 的 IS 锁

* 事务要获取表 A 某些行的 X 锁必须要获取表 A 的 IX 锁

**记录锁**

锁定索引记录

**间隙锁**

防止不可重复读

**临键锁**

记录锁加 gap 锁

**插入意向锁(Insert Intention Locks)**

针对 insert 操作的一种间隙锁

### 上锁

* MySQL 常用引擎有 MyISAM 和 InnoDB
* InnoDB 是 MySQL 默认的引擎
* MyISAM 不支持行锁, InnoDB 支持**行锁和表锁**

#### 表锁

**隐式上锁**

默认, 自动加锁, 自动释放

```mysql
# 上读锁
SELECT 

# 上写锁
INSERT, UPDATE, DELETE
```

**显式上锁**

```mysql
# 上共享锁(读锁)
select  column from table where lock in share mode;

# 上排他锁(写锁)
select column from table where conditions for update;
```

**解锁(手动)**

```mysql
# 解锁所有锁表
UNLOCK TABLES;
```

**实例**

```mysql
# 对 teachers 表上读锁
LOCK TABLES teachers READ;

# 解锁
UNLOCK TABLES;

# 更新邮箱
UPDATE teachers 
SET email = '1336509100@qq.com'
WHERE name = 'Yjq';
```

#### 行锁

行锁优劣

* 优点: 粒度小, 锁冲突几率小, 并发能力强
* 缺点: 开销大, 加锁慢, 会出现死锁

行锁分为共享锁(S 锁, 读锁) 和排他锁(X 锁, 写锁)

**隐式上锁**

默认, 自动加锁, 自动释放

* UPDATE, DELETE, INSERT 操作, InnoDB 会自动加排他锁
* SELECT 操作, InnoDB 不会加锁 

```mysql
# 不会上锁
SELECT

# 上写锁
INSERT, UPDATE, DELETE
```

**显式上锁**

```mysql
# 读锁 
SELECT *
FROM table
LOCK IN SHARE MODE;

# 写锁
SELECT *
FROM table
FOR UPDATE;
```

**解锁**

1. 提交事务( commit )
2. 回滚事务( rollback )
3. 阻塞进程( kill )

