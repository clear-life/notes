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

