# Haskell

## 从零开始

函数调用

`函数名 参数表`

函数调用优先级最高

函数声明

`函数名 参数列表 = 函数体`

函数无顺序, 调用前不需要声明

`if` 语句是表达式

```haskell
if x > 0
then x
else -x

if x > 0 then x else -x
```

负数最好加括号

用 `'`区分稍加修改的函数

函数名首字母不大写

定义: 无参数的函数

```haskell
a'b'c = "abc"
```



List

> ghci 可用 `let` 定义常量

```haskell
ghci> let list1 = [1, 2, 3, 4, 5]
```

字串只是字符的 List, "Hello" 只是 `['H', 'e', 'l', 'l', 'o']` 的语法糖

List 合并: `++` 运算子

`++` 运算子遍历左边整个 List

List 插入: `:` 运算子

List 插入操作在前端更快

`[1, 2, 3]` 是 `1 : 2 : 3 : []` 的语法糖

List 索引: `!!` 运算子

```haskell
"Hello world" !! 4
'o'
```

List 嵌套 List: 类型必须相同

List 比较 `>` `>=` 

从第一个元素开始比较, 相等则比较下一个 

List 常用函数:

`head` 返回 List 头部(首元素)

`tail` 返回 List 尾部(非头部)

`last` 返回 List 尾元素

`init` 返回 List 非尾元素

`length` 返回 List 长度

`null` 检查 List 是否为空

`reverse` 反转 List

`take` 返回 List 前n个元素

`drop` 删除 List 前n个元素

> n 大于 List 长度当作 List 长度处理

`maximum` 返回 List 最大元素

`minimum` 返回 List 最小元素

`sum` 返回 List 元素之和

`product` 返回 List 元素之积

`elem` 判断元素是否在 List 中

```haskell
item `elem` List
1 `elem` [1, 2, 3]
True
```

Range

构造 List

```haskell
[0..9]
['a'..'z']
```

跨步

```haskell
[3, 6 .. 20]
[9, 8 .. 0]
```

避免 Range 使用浮点数

```haskell
[0.1, 0.3 .. 1]
[0.1, 0.3, 0.5, 0.7, 0.8999999999999999, 1.0999999999999999]
```

无限 List: 不标注 Range 上限

惰性求值, 取多少求多少

生成无限 List 的函数

`cycle`

```haskell
take 12 (cycle "LOL ")
"LOL LOL LOL "
```

`repeat`

```haskell
take 5 (repeat 2)
[2,2,2,2,2]
```

`replicate` 

```haskell
replicate 3 10
[10,10,10]
```

List Comprehension

`[ 输出函数 | 输入集合, 限制条件 ]`

```haskell 
[x*2 | x <- [1..10], x*2 >= 12]
fun xs = [ if x < 10 then "BOOM" else "BANG" | x <- xs, oddx]
```

多个输入集合

`[ 输出函数 | 输入集合1, 输入集合2, 限制条件]`

comprehension 将 `输入集合1, 输入集合2` 的笛卡尔积交给输出函数

多个限制条件

```haskell
[x | x <- [10..20], x /= 13, x/= 15, x /= 19]
```

嵌套的 List comprehension 

```haskell
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
[ [ x | x <- xs, even x ] | xs <- xxs]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```



函数

`odd` 奇数

`even` 偶数



Tuple

类型取决于项的数目和类型

不能有单元素 Tuple

`zip` 两个 List 生成序对 List, 较长的 List 会被截断

惰性求值, zip 可处理有限和无限的 List

```haskell
zip [1, 2, 3] [5, 6, 7]
[(1, 5), (2, 6), (3, 7)]
```

三边长度皆为整数且小于等于 10，周长为 24 的直角三角形

```haskell
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
[(6,8,10)]
```

## Types 和 Typeclasses

每个表达式都有类型, 标明该表达式所属范畴

`:t` 查看类型

```
"Hello!" :: String
"HELLO!" :: [Char] 

(True, 'a') :: (Bool, Char)  
```

函数类型

```
fun :: 输入类型 -> 输出类型
fun 参数 = 定义

fun :: Int -> Int -> Int -> Int
fun x y z = x + y + z
```

常见类型:

`Int` 有界整数

`Integer` 无界整数

`Float` 单精度浮点数

`Double` 双精度浮点数

`Bool`

`Char`  `[Char]` `String`

Tuple 类型取决于项类型和数目, 空 Tuple 也是一种类型, 只有一种值 `()`



Type variables 类型变量

类型首字母必大写

```
head :: [a] -> a
```

a 为类型变量

多态函数: 使用类型变量

```
fst :: (a, b) -> a 
```



Typeclasses 

Typeclasses 下的类型都实现 Typeclasses 所描述的行为

`==` 函数的类型声明

```
(==) :: Eq a => a -> a -> Bool
```

`类型约束 => 类型d` 

`Eq a` 类型 a 在 Eq 类 中

基本 Typeclass:

`Eq` 可判断相等性 `==` `/=` 除函数类型外的所有类型都属于 `Eq`

`Ord` 可比较大小 `<` `>` `<=` `>=`

```
compare :: Ord a => a -> a -> Ordering 
Ordering [GT, LT, EQ]
```

`Ord` $\subset$ `Eq`

`Show` 可用字串表示  `show` 函数将 Show 的成员类型转换为 `String`

```
show :: Show a => a -> String
```

`Read`   `read` 函数将 `String` 转换为 `Read` 的成员类型

```
read :: Read a => String -> a
```

`read "5"` 会出错, 因为 5 既可以是 `Int` 也可以是 `Float`

```
read "5" ::
```



`Enum` 可枚举(连续)

Range: 每个值都有后继子(successer)和前置子(predecesor), 通过 `succ` 和 `pred` 函数

`Enum`: `()` `Bool` `Char` `Ordering` `Int` `Integer` `Float`  `Double`

`Bounded` 有界, 上界和下界

若 Tuple 的所有项都属于 `Bounded`, 则该 Tuple 也属于 `Bounded`

```
minBound :: Bounded a => a
maxBound :: Bounded a => a
```

`Num` 数字

所有数字都是多态常量, 可以作为 `Num` 中的所有类型

```
(*) :: Num a => a -> a -> a
```

`Num` $\subseteq$ `Show`  且  `Num` $\subseteq$ `Eq`

`Integral`: `Int` `Integer`

`Floating`: `Float` `Double`

fromIntegral 整数换为通用数

```
fromIntegral :: (Integral a, Num b) => a -> b
```

