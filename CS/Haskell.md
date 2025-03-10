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
* `x:xs` 要加 `()` 因为 `:` 优先级小于函数调用

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

## 高阶函数

函数可作为参数和返回值

### Curried functions

**数学理解**

每次带入一个变量的值

$f(x,y,z) = g(x)*h(y)*j(z)$

带入 x = a 得到函数 $f(a,y,z) = g(a)*h(y)*j(z)$

带入 y = b 得到函数 $f(a,b,z) = g(a)*h(b)*j(z)$

带入 z = c 得到值 $f(a,b,c) = g(a)*h(b)*j(c)$

**haskell语法**

函数 `f x y z = x * y * z` (`a -> a -> a -> a`)执行 `f 1 2 3` 会:

带入 x = 1 得到函数 `f2 y z = y * z`(`a -> a -> a`)

带入 y = 2 得到函数 `f3 z = 2 * z`(`a -> a`

带入 z = 3 得到值 `6`

```haskell
f :: a -> a -> a -> a
f x y z = x * y * z

f2 :: a -> a -> a
f2 y z = y * z

f3 :: a -> a
f3 z = 2 * z
```

* 可以不按顺序带入

  ```
  f2 x z = f x 1 z
  ```

* 中缀函数不全调用 `()`

  ```
  f = (/10)
  f x = x / 10
  ```

* `(-x)` 中 `-` 不代表减法运算, `subtract` 减法函数

* 函数类型不属于 `Show`

* 函数类似 `f x = g y x` 可改为 `f = g y`

### 高阶函数

#### 函数作为参数

```
g :: (a -> a) -> a -> a
g f x = f (f x)
```

* `->` 右结合
* `()` 为一个参数 

**`zipWith`**

```
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
```

`flip`

```
flip' :: (a->b->c) -> (b->a->c)
filp' f = g
	where g y x = f x y
```

等价于

```
flip' :: (a->b->c) -> b -> a -> c
flip' f y x = f x y
```

### map 与 filter

`map`

映射

```
map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

`filter`

过滤

```
filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
	| f x = x : filter f xs 
	| otherwise = filter f xs
```

`takeWhile`

```
takeWhile :: (a->Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
	| f x = x : takeWhile f xs
	| otherwise = []
```

### lambda

匿名函数

`\ x y.. -> ...`

* 表达式, 返回函数
* 只能匹配一种模式, 匹配失败引发运行时错误

```
f :: (Num a) => a -> a -> a -> a
f x y z = x + y + z
```

等价于

```
f :: (Num a) => a -> a -> a -> a
f = \x -> \y -> \z -> x + y + z
```

### 关键字 fold

遍历 List 元素并操作后返回值

`flodl f acc0 x:xs`

$acc_1\ = f\ acc_0\ x_1$

...

$acc_n\ =\ f\ acc_{n-1}\ x_{n}$

```
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0
```

`flodr f acc0 xs:x`

$acc_1\ = f\ x_1\ acc0$

...

$acc_n\ =\ f\ x_{n}\ acc_{n-1}$

```C++
map' :: (a->b) -> [a] -> [b]
map' f xs = flodr (\x acc -> f x : acc) [] xs
```

* 生成新 List 一般使用 `flodr`

* `foldl1` 与 `flodr1` 设定首尾元素为初值, 从旁边开始 flod
* `foldl1` 与 `flodr1` 处理空 List 会出错
* `foldl1` 不能处理无限 List
* `foldr1` 能处理无限 List

```
maximum' :: (Ord a) [a] -> a
maximum' [] = []

= foldl ()  
```

常见库函数 fold 实现

```
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  

reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  

product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  

filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  

head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  

last' :: [a] -> a  
last' = foldl1 (\_ x -> x)
```

`scanl` `scanl1`  `scanr` `scanr1`记录累加值的所有状态到一个 List 中

### $ 函数调用

`$` 函数调用符

```
($) :: (a -> b) -> a -> b  
f $ x = f x
```

* 优先级最低
* 右结合

```
sum (map sqrt [1..130])

sum $ map sqrt [1..130]
```

`$` 可将数据作为函数使用

```
map ($ 3) [(4+),(10*),(^2),sqrt]  
```

### Function composition

数学, 复合函数

$(f\circ g) = f(g(x))$

```
(.) :: (b->c) -> (a->b) -> a -> c
f . g = \x -> f (g x)
```

* 右结合

* 定义 point free style 函数

   ```
   fn x = ceiling (negate (tan (cos (max 50 x))))
   fn = ceiling . negate . tan . cos . max 50
   ```

   

## 模块

### 状态模块

`import Module`

`:m Module`

`import Module (Functions)`

`import Module hiding (Functions)`

`import qualified Module` 全名使用函数

`import qualified Module as M`

> 翻阅标准库中的模块和函数是提升个人 Haskell 水平的重要途径

### Data.List

**intersperse** 元素穿插 List

**intercalate** List 穿插 List

**transpose** 翻转二维 List

**fold' 和 foldl1'** 非惰性实现, 防止堆栈溢出

**concat** 移除一级 List

**concatMap** map List 后再 `concat`

**and** 整个 List

**or** 整个 List

**any** **all** 对 List 每个元素进行 f 函数判断

**iterate** 一直调用函数, 用返回值做下次的输入

**splitAt** List 在特定位置断开, 返回二元组List

**takeWhile** 从 List 取元素, 直到 False

**dropWhile** 扔掉符合条件的元素, 直到 False, 返回剩余部分

**span** 与 `takeWhile` 相似, 但返回两个 List, 一个 True 部分, 一个 False 部分

**break** 与 `dropWhile` 相似, 在 False 处断开, 返回两个 List

**sort**

**group** 对 List 连续且相等元素分段

**inits** 和 **tails** 与 `init` `tail` 但会递归地调用自身直到为空

**isInfixOf** 是否包含子集合

**isPrefixOf** 与 **isSuffixOf** 是否以某子集合开头或结尾

**elem** 与 **notElem** 是否包含某元素

**partition** 限制函数 limit 对 List 划分, 一个 List True 元素, 一个 List False 元素

**find** 返回首个符合条件的`Maybe`元素

> Maybe: Just something 或 Nothing

**elemIndex** 返回元素`Maybe`索引

**elemIndices** 返回元素索引 List

**findIndex** 返回所有符合条件元素 List

**zip3 ... zip7** 和 **zipWith3 ... zipWith7**

**lines** 返回 String 的所有行, List

**unlines** `lines` 的反函数

**words** 和 **unwords** 把 String 分为一组单词和反操作

**nub** 去除 List 重复元素

**delete** 删除首次出现的某元素

**\\** 差集操作 $A - B$

**union** 并集 $A\bigcup B$ 

**intersect** 交集 $A \bigcap B$

**insert** 将元素插入首个大于等于的元素前

**genericLength  genericTake  genericDrop  genericSplitAt  genericIndex  genericReplicate** 返回值为 Num

**nubBy  deleteBy  unionBy  intersectBy  groupBy** 用函数判定相等性而不是 `==`

**on**

```
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)
```

```
ghci> groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

**sortBy  insertBy  maximumBy**   **minimumBy**

```
ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  
ghci> sortBy (compare `on` length) xs  
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
```

### Data.Char

**isControl** 控制字符

**isSpace** 空白符

**isLower** 小写

**isUper** 大写

**isAlpha** 字母

**isAlphaNum** 字母或数字

**isPrint** 可打印

**isDigit** 数字

**isOctDigit** 八进制数字

**isHexDigit** 十六进制数字

**isLetter** 字母

**isMark** 注音字符

**isNumber** 数字

**isPunctuation** 标点符号

**isSymbol** 货币符号

**isSeperater** 空格或分隔符

**isAscii** unicode 前128

**isLatin1** unicode 前 256

**isAsciiUpper** 大写 ascii

**isAsciiLower** 小写 ascii

**GeneralCategory** 类型(枚举)

**toUpper** 大写

**toLower** 小写

**toTitle** title-case

**digitToInt** char -> int

**intToDigit** int -> char

**ord char** char <-> num

### Data.Map

字典, key-value, 无顺序 List

`import qualified Data.Map as Map`

**lookup** key -> Maybe value

**fromList** List -> Map

**empty**  
