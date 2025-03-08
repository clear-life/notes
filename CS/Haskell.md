# Haskell

## 从零开始

### 函数

**函数定义**

$f(x,y...)=...$

`f x y ... = ...`

**函数调用** 

$f(x,y...)$

`f x y ...`

* 函数调用优先级最高

* 函数间无顺序

* 函数名首字母不能大写

* `'`意为稍加修改的函数

* 无参函数称为**定义**

* 占位符 `_`

  $\pi = 3.14$​

`if` 语句

`if ... then ... else ...`

```haskell
if ...
then ...
else ...
```


$$
\begin{cases}
\dots & \text{if ...}\\
\dots & \text{else}  
\end{cases}
$$

* 是表达式(值), 必须有两种情况的值
* 负数最好加括号

`odd` 奇数

`even` 偶数

$~$

### List

数列

#### List 定义

`list = [a1, a2 ... ]`

${a_1, a_2\ ...}$

* 字串只是字符的 List

* `"String"` 只是 `[Char]` 的语法糖

#### List 操作

**List 自操作**

**1. 插入**

`:` 运算子

插入操作在前面更快

`[a..]` 是 `a.. : []` 的语法糖

**2. 索引**

`!!` 运算子

`[a] !! i`

$a_i$

**3. 嵌套**

多级数列所有值类型必须相同

**List 间操作**

**1. 合并**

 `++` 运算子

`++` 运算子会遍历左边整个 List

`listA ++ listB`

${A,B}$

**2. 比较**

 `>` `>=` 

#### List 常用函数

**List 项**

数列取数

`head` 头部(首元素)

`tail` 尾部(非头部)

`last` 尾元素

`init` 非尾元素

`take` 前n个元素

`maximum` 最大元素

`minimum` 最小元素



**List 信息**

`length` 长度

`null` 是否为空

`elem` 是否含有元素

**item \`elem\` list**

$a\in A$

`sum` 元素之和

`product` 元素之积

**List 操作**

`reverse` 反转

`drop` 删除前 n 个元素

> n 大于 List 长度当作 List 长度处理

$~$

### Range

#### Range 定义

构造 List

集合表示方法: **列举法**

**`[a1, a2 .. n]` **

* **不写 a2 步长为 1**

* **不写 n 无限 List**

  惰性求值, 用多少求多少

* Range 避免使用浮点数

  ```haskell
  [0.1, 0.3 .. 1]
  [0.1, 0.3, 0.5, 0.7, 0.8999999999999999, 1.0999999999999999]
  ```

  

#### 无限 List 生成函数

`cycle`  A

$A,A...$

`repeat` a

$a,a...$

`replicate` n a

$\underbrace {a,a...}_n$

### List Comprehension

#### 定义

集合函数: $集合 A.. \longmapsto 集合 B$

`[ 输出函数 | 限制条件 ]`

* 限制条件包含输入集合, 多个输入集合以笛卡尔积形式作为输出函数的定义域
* `a <- A` 
* $a \in A$

集合表示方法: **描述法**

$\{x\in X|f(a,b...), a\in A,b\in B.. \}$

#### 嵌套

剥葱

`[ [ [ f(x) | limit3] | limit2] | limit1 ]`

```haskell
xxs = [ [1,3,5,2,3], [1,2,3,4,9], [1,2,3,6]]
[ [ x | x <- xs, even x ] | xs <- xxs]
[[2],[2,4],[2,6]]
```

### Tuple

#### Tuple 定义

`(typeA, typeB..)`

$(a,b..)$

* 类型取决于项的数目和类型

* 无单元素 Tuple

序对: 长度 2 Tuple

`fst` 首项

`snd` 尾项

#### 生成序对List

`zip` 两个 List 生成序对 List, 较长的 List 会被截断

`zip A B`
$$
A = a1,a2\dots, B=b1,b2\dots 	\\
\{(a_i,b_i)|a_i\in A,b_i\in B\}
$$

* 惰性求值, zip 可处理有限和无限的 List

## Types 和 Typeclasses

`:t` 查看类型

类型首字母必大写 

### 常见类型

函数类型

输入类型 -> 输出类型

$x_1 \rightarrow x_2 ..\rightarrow y$

`Int` 有界整数

`Integer` 无界整数

`Float` 

`Double`

`Bool`

`Char`  `[Char]` `String` 

Tuple 类型取决于项类型和数目

空 Tuple 只有一种值 `()`

### Type variables

类型变量

`[a] -> a`

多态函数: 使用类型变量

### Typeclasses 

Typeclasses 所属类型都实现 Typeclasses 描述的行为

```haskell
(==) :: Eq a => a -> a -> Bool
```

`类型约束 => 类型` 

`Eq a` 

$a \in Eq$

#### 基本 Typeclasses

`Eq` 可判断相等性 `==` `/=`

`Ord` 可比较大小 `<` `>` `<=` `>=`

```haskell
compare :: Ord a => a -> a -> Ordering 
Ordering [GT, LT, EQ]
```

`Ord` $\subset$ `Eq`

`Show` 可用字串表示

 `show` 函数将 Show 的成员类型转换为 `String`

```haskell
show :: Show a => a -> String
```

`Read`  

 `read` 函数将 `String` 转换为 `Read` 的成员类型

```haskell
read :: Read a => String -> a
```

`read "5"` 出错

5 既能是 `Int` 也能是 `Float`

类型注释

`:: type`

```haskell
read "5" :: Int
```

`Enum` 可枚举

$a \in Enum$ 

a 每个值都有后继子(successer)和前置子(predecesor), `succ` 和 `pred` 函数

`Enum`: `()` `Bool` `Char` `Ordering` `Int` `Integer` `Float`  `Double`

`Bounded` 有上界和下界

若 Tuple 的所有项都属于 `Bounded`, 则该 Tuple 也属于 `Bounded`

```haskell
minBound :: Bounded a => a
maxBound :: Bounded a => a
```

`Num` 数字

数字都是多态常量, 可作为 `Num` 中的所有类型

```haskell
(*) :: Num a => a -> a -> a
```

`Num` $\subseteq$ `Show`  且  `Num` $\subseteq$ `Eq`

`Integral`: `Int` `Integer`

`Floating`: `Float` `Double`

`fromIntegral` 整数换为通用数

```haskell
fromIntegral :: (Integral a, Num b) => a -> b
```

## 函数语法

### 模式匹配

pattern  `<-` $A$

* 自上而下进行匹配
* 匹配失败就跳过
* 不能用 `++`

#### 函数模式匹配

分段函数

```
f a =
f x = 
```

$$
f(a) = \\
f(x) = \\
$$

#### List 模式匹配

`x : y : z : xs`  `<-`  length≥3 的 List

`[]`  `<-`  空 List

* `(x:[])`  `(x:y:[])`  
* `[x]`  `[x,y]`

```C++
head' [] = error "error"
head' (x:_) = x
```

`error` 生成运行时错误

#### as 模式

`@` 对整体的使用

`xs@pattern`

### Guards

#### 定义

条件判断, 检查布尔表达式

* 语法

```
f x...
	| ... = 
	| ... = 
	| ... = 
	| otherwise = 
```

`otherwise = True`

```
a `fun` b
	| a > b		= GT
	| a == b	= EQ
	| otherwise	= LT
```

### 关键字 Where

限定条件

* 语法结构, 函数底部定义名字
* 标识符垂直排开
* 只对整个函数可见

```
a `fun` b
	| a > b		= GT
	| a == b	= EQ
	| otherwise	= LT
	where 	c = ...
			d = ...
			...
```

具体函数也是个值, 能在 where 中定义

```
fun xs = [ bmi w h | (w,h) <- xs]
	where bmi w h = w / h ^ 2
```

### 关键字 Let

`let [bindings] in [expressions]`

* 表达式, 任何地方都能用
* 标识符垂直排开或 `;` 分隔在一列 
* let 在 List comprehension 中的输出函数和限制条件中都可见

```
let a =
	b = 
in 	c = 
```

```
[bmi | (w,h) <- xs, let bmi = w / h ^ 2]
```

### Case expressions

模式匹配是 case 语句的语法糖

* case 语句是表达式, 可用在任何地方

```
case expression of 	pattern -> result
					pattern -> result
					...
```



```
fun xs = case xs of [] -> ...
					(x:_) -> x
```

## 递归

### Maximum

```
maximum' [] = error
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maxinum' xs
```

### 递归函数

`replicate`

```
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x : replicate' n-1 x
```

`take`

```
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0	= []
take' _ []		= []
take' n (x:xs)	= x : take' (n-1) xs
```

`reverse`

```
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
```

`repeat`

```
repeat' :: a -> [a]
repeat' x = x : repeat' x
```

`zip`

```
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
```

`elem`

```
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
	| a == x	= True
	| otherwise	= a `elem'` xs
```

### 快速排序

```
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs)
	let left = quicksort [a | a <- xs, a <= x]
		righy = quicksort [a | a <- xs, a > x]
	in left ++ [x] ++ right
```

