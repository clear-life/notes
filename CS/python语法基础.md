## 定义

`__iter__()` 方法

* 返回 `iterator`

`__next__()` 方法

* 返回下一 `item`

**iterable**

* 可迭代对象
* 提供 `__iter__()` 方法

**iterator**

* 迭代器
* 提供 `__iter__()` 方法
* 提供 `__next__()` 方法

## 输入

```python
x, y = map(lambda x : x + 1, input().split())
```

**`input(prompt)`** 

* 输入
* `str`

**`str.split(sep=None)`** 

* 字符串分割
* `[]`

**`lambda [parameters]: expression`**

* `lambda` 表达式

**`map(func, iterable)`** 

* 映射
* `iterator`

**`eval(expression)`**

* 执行 `str` 表达式
* 表达式结果

**`v1, v2, ..., vn = iterable`**

* 元组解包, 调用 `__next__()` 方法

## 输出

```python
print(f"{:.f}")
print(,end=' ')
```

## 数据结构

**列表**

* 创建
  * `[]` `list()` `[v1, v2]`
  * `[expr for i in iterable]`
* 操作
  * `append()` `pop()` `reverse()` `sort()`
  * `[n : m : k]`
  * `l1 = l2` 引用

**字典**

* 创建
  * `{}`  `dict()` `{key:value}`
* 操作
  * `k in d` `k not in d`
  * `d.keys()` `d.values()` `d.items()`
  * `list(d)`

## 功能

**变量作用域**: 

**LEGB**

1. L Local 局部(函数内)
2. E Enclosing 外围(外层函数)
3. G Global 全局(文件)
4. B Built-in 内置(所有)

**三目运算符**

* `a if a > b else b`

## 条件

`if a > b > c`
