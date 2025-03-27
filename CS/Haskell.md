# Haskell

## 从零开始

### 函数

> 前缀函数

**函数绑定** `f x y ... = ...`

**函数调用** `f x y ...`

* 优先级最高
* 中缀函数 ```` ``
* 函数无顺序

#### 数学

$f(x,y...)=...$

$f(x,y...)$

函数: 将表达式绑定到函数名, 模式为参数, 函数调用为带参数 

$~$

### List

#### List 绑定

`l = [a1, a2 ... ]`

#### 数学

数列 ${a_1, a_2\ ...}$

### List 函数

#### 取元素

|   **!!**    | `[a] !! i` |
| :---------: | :--------: |
|  **take**   | 前n个元素  |
|  **head**   |    头部    |
|  **tail**   |    尾部    |
|  **last**   |   尾元素   |
|  **init**   |    非尾    |
| **maximum** |  最大元素  |
| **minimum** |  最小元素  |

#### 查信息

| length  |     长度      |
| :-----: | :-----------: |
|  null   |     判空      |
|  elem   | 判存 $a\in A$ |
|   sum   |    元素和     |
| product |    元素积     |

#### 变换

|    **:**    |       前插        |
| :---------: | :---------------: |
|   **++**    | 合并, 遍历左 List |
| **reverse** |       反转        |
|  **drop**   |  删除前 n 个元素  |

### List 生成

#### Range

集合**列举法**

**`[a1, a2 .. n]` **

* **默认步长为 1**

* 避免使用浮点数

  ```haskell
  [0.1, 0.3 .. 1]
  [0.1, 0.3, 0.5, 0.7, 0.8999999999999999, 1.0999999999999999]
  ```


#### List 生成函数

**cycle**  A

**repeat** a

**replicate** n a

$\underbrace {a,a...}_n$

#### List Comprehension

集合**描述法**

$\{x\in X|f(a,b...), a\in A,b\in B.. \}$

$集合 A.. \longmapsto 集合 B$

`[ 输出函数 | 限制条件 ]`

* 输入集合笛卡尔积
* `a <- A`   $a \in A$

**嵌套**

`[ [ [ f(x) | limit3] | limit2] | limit1 ]`

`[ [ x | x <- xs, even x ] | xs <- xxs]`

$~$

### Tuple

#### Tuple 绑定

`t = (typeA, typeB..)`

$(a,b..)$

* 无单元素 Tuple

**Pair**

* **fst** 首项

* **snd** 尾项

#### Pair List 生成

`zip :: [a] -> [b] -> [(a, b)]`
$$
A = a1,a2\dots, B=b1,b2\dots 	\\
\{(a_i,b_i)|a_i\in A,b_i\in B\}
$$



$~$

## Type 和 Typeclass

### Type

**函数类型**

$x_1 \rightarrow x_2 ..\rightarrow y$

**Int** 有界整数

**Integer** 无界整数

**空 Tuple** 只有一种值 `()`

### Typeclass

Typeclass 定义抽像函数, type 实现抽像函数

> 抽像函数: 只有函数类型, 无具体实现

| Typeclass   |    作用    |      抽像函数      |  示例  |
| :---------- | :--------: | :----------------: | :----: |
| **Eq**      |   相等性   |     `==` `/=`      |  Bool  |
| **Ord**     |    比较    | `<=` `>` `compare` |  Char  |
| **Show**    |  toString  |       `show`       | [Char] |
| **Read**    | fromString |       `read`       |  Bool  |
| **Num**     |  数值运算  |   `+` `*` `abs`    | Double |
| **Functor** |  容器映射  |       `fmap`       | Maybe  |

* **类型约束 =>** **类型注释 ::**
* **read "4"** 出错, 4 既能是 `Int` 也能是 `Float` 

* **Enum** 可枚举, 后继子 前置子 `succ` `pred` 

* **Bounded** 有上下界

   * ```haskell
      minBound :: Bounded a => a
      maxBound :: Bounded a => a
      ```

* **fromIntegral** 整数换为通用数

   ```haskell
   fromIntegral :: (Integral a, Num b) => a -> b
   ```

## 函数

### 模式匹配

检查值是否匹配并取值

* 通过构造子进行模式匹配
* 模式绑定多个名称加 `()`

#### 函数模式匹配

```haskell
f :: 
f p1 = 
f p2 =
```

#### List 模式匹配

```
[]
(x:[])	[x]
(x:xs)
(x:y)	[x, y]
(x:y:_)
```

#### as 模式

`xs@pattern` 对整体的引用

### Guards

检查值是否符合条件

```haskell
f x
	| ... = 
	| ... = 
	| ... = 
	| otherwise = 
```

### 关键字 Where

* 在函数定义末尾**定义名称**
* 作用域: **函数或模式匹配分支**

```haskell
f x
	| 
	|
	where 	c = 
			g x = x + 1
```

### 关键字 Let

* 表达式
* 一行 `;` 隔开
* **let bindings in expression** bindings 名字 expression 使用名字的表达式
* 作用域: in, guard, List Comprehension(in 省略)

```haskell
let [bindings] in [expressions]
```

### Case expressions

* 表达式

```haskell
case expression of 	pattern -> result
					pattern -> result
					...
```

## 递归

* 边界条件
* 函数调用自身

**Maximum'**

```haskell
maximum' [] = error
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
```

### 递归函数

**replicate**

```haskell
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x : replicate' n-1 x
```

**take**

```haskell
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0	= []
take' _ []		= []
take' n (x:xs)	= x : take' (n-1) xs
```

**reverse**

```haskell
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
```

**repeat**

```haskell
repeat' :: a -> [a]
repeat' x = x : repeat' x
```

**zip**

```haskell
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
```

**elem**

```haskell
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
	| a == x	= True
	| otherwise	= a `elem'` xs
```

**快速排序**

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs)
	let left = quicksort [a | a <- xs, a <= x]
		righy = quicksort [a | a <- xs, a > x]
	in left ++ [x] ++ right
```

## 高阶函数

函数作为参数和返回值

### Curried functions

#### 数学

$f(x,y,z) = g(x)*h(y)*j(z)$

每次带入一个变量的值

$f(a,y,z) = g(a)*h(y)*j(z)$

$f(a,b,z) = g(a)*h(b)*j(z)$

$f(a,b,c) = g(a)*h(b)*j(c)$

#### Curried functions

* 函数都只有一个参数
* `->` 右结合, 优先级最低, 传入函数要用 `()`
* **不全参数**调用函数, 得到**不全调用函数**

**Currying**

`f1 :: a1 -> a2 -> .. -> an`

第一次调用, 返回 `f2 :: a2 -> .. -> an`

第二次调用, 返回 `f3 :: a3 -> .. -> an`

....

第 n 次调用, 返回 `fn :: an`

#### 高阶函数

**zipWith**

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
```

**flip**

```haskell
flip' :: (a->b->c) -> b -> a -> c
flip' f y x = f x y
```

### map 与 filter

#### map

映射

```haskell
map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

#### filter

过滤

```haskell
filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
	| f x = x : filter f xs 
	| otherwise = filter f xs
```

#### takeWhile

取元素直到不合条件

```haskell
takeWhile :: (a->Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
	| f x = x : takeWhile f xs
	| otherwise = []
```

### lambda

匿名函数

`\ x y .. -> ...`

* 表达式
* 只匹配一种模式

```haskell
f :: (Num a) => a -> a -> a -> a
f = \x -> \y -> \z -> x + y + z
```

### 关键字 fold

**fold 二元函数 f 初值 c List**

`fold f c L`

$c1 = f(x_0, c_0)$

$c2 = f(x_1, c_1)$

$...$

$c_{n+1} = f(x_{n}, c_n)$

#### flodl

`foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b`

`foldl f c L`

`f ... (f (f c x1) x2) ... xn `

* 左折叠
* 从左到右
* 初值在左
* 不能处理无限 List

```haskell
foldl (-) 0 List
```

#### foldr

`foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b`

`foldr f c L`

`f xn ... (f x2 (f x1 c)) ...`

* 右折叠
* 从右到左
* 初值在右
* 能处理无限 List

```haskell
map' :: (a->b) -> [a] -> [b]
map' f xs = flodr (\x acc -> f x : acc) [] xs
```

#### foldl1 与 foldr1

* 首尾元素为初值

```haskell
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

####  scan

**scanl** **scanl1**  **scanr** **scanr1** 记录累加值的所有状态到 List

### $ 函数调用符

```haskell
($) :: (a -> b) -> a -> b  
f $ x = f x
```

* 优先级最低
* 右结合
* **等价于在右边加括号**

```haskell
sum (map sqrt [1..130])

sum $ map sqrt [1..130]
```

* **$** 将值看作函数使用
   *  `$ 值` 就是一个函数

```haskell
map ($ 3) [(4+),(10*),(^2),sqrt]  
```

### Function composition

#### 复合函数

$(f\circ g) = f(g(x))$

#### 函数组合

**.** 函数

```haskell
(.) :: (b->c) -> (a->b) -> a -> c
f . g = \x -> f (g x)
```

* 右结合

* 映射方向

   ```haskell
   f.g :: c <- b < a
   f . g = 
   ```

   


#### point free style

* 无参函数
* 函数组合定义函数

```haskell
f x = ceiling (negate (tan (cos (max 50 x))))
f = ceiling . negate . tan . cos . max 50
```



## 模块

模块: 相关的函数, 类型, 类型类的组合

### 装载模块

**import Data.List**  `Data.List` 所有函数进入全局命名空间

**import Data.List (f, g)** 仅包含 f g

**import Data.List hiding (f)** 除 f 之外都包含

**import qualified Data.List** 模块名.函数名

**import qualified Data.List as L** 模块别名

### Data.List

**intersperse** 元素穿插 List

**intercalate** List 穿插 List

**transpose** 翻转二维 List (矩阵)

**fold' 和 foldl1'** 非惰性实现, 防止堆栈溢出

**concat** 移除 List 一层嵌套

**concatMap** map List 后再 `concat`

**and** 整个 List 与

**or** 整个 List 或

**any** **all** 检查 List 每个/某个元素是否符合 f 函数

**iterate** 一直调用函数, 产生无限 List

**splitAt** List 在特定位置断开, 返回二元组List

**takeWhile** 从 List 取元素, 直到 False

**dropWhile** 扔掉符合条件的元素, 直到 False, 返回剩余部分

**span** 首次 False 断开, 返回两个 List  True 部分和 False 部分

**break** 首次 True 断开, 返回两个 List

**sort**

**group** 对 List 连续且相等元素分段

**inits** 和 **tails** 与 `init` `tail` 相似但会递归地调用自身直到为空

**isInfixOf** 是否包含子集合

**isPrefixOf** 与 **isSuffixOf** 是否以某子集合开头或结尾

**elem** 与 **notElem** 是否包含某元素

**partition** 限制函数 limit 对 List 划分, 一个 List True 元素, 一个 List False 元素

**find** 返回首个符合条件的`Maybe`元素

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

**setNub** 不保留原有顺序, 但速度比 `num` 快

**genericLength  genericTake  genericDrop  genericSplitAt  genericIndex  genericReplicate** 返回值为 Num

**nubBy  deleteBy  unionBy  intersectBy  groupBy** 用函数判定相等性而不是 `==`

**on**

```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)
```

```haskell
ghci> groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

**sortBy  insertBy  maximumBy**   **minimumBy**

```haskell
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

**import qualified Data.Map as Map**

**lookup** key -> Maybe value

**fromList** List -> Map

**empty**  空 map

**insert** 插入入新 k-v

**null** 是否空

**size** 大小

**singleton** 一个 k-v 的 map

**lookup**

**member** 是否有 k

**map filter**  f 对 v 作用

**toList** `fromList` 的反函数

**keys elems** keys values

**fromListWith** f 对重复 key 的 values 处理, 默认放在 List 中

**insertWith** 处理已存在 k 的 value

### Data.Set

**import qualified Data.Set as Set**

**fromList** List -> Set

**difference** $A - A \bigcap B$

**union** $A \bigcap B$

**null  size  member  empty  singleton  insert  delete**

**isSubsetOf isProperSubsetOf** 子集 真子集

**map filter**

**toList** Set->List

### 自定义模块

`Module.hs`

 ```haskell
 module Module
 ( fun1
 , fun2
 , fun3
 , ...
 ) where
 ```

#### Geometry 文件夹

Geometry 模块

**Sphere.hs**

Sphere 子模块

```haskell
module Geometry.Sphere  
(
) where
```

**Cuboid.hs**

Cuboid  子模块

```haskell
module Geometry.Cuboid  
( 
) where 
```

**Cube.hs**

Cube 子模块

```haskell
module Geometry.Cube  
( 
) where
```

## 自定义 Type 和 Typeclass

### Algebraic Data Type

#### 自定义 ADT

**data TypeName = Value Constructor | ... [deriving (TypeClass)]**

* **data** 表示定义数据类型
* **Type** 类型名称
* **Value Constructor** **值构造子**
* **|** 或
* **deriving (TypeClass)** 将该类型归类于 **TypeClass** 类型类

```haskell
data Bool = False | True

data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

#### Value Constructor

**data TypeName = Constructor Type1 Type2**

`Constructor :: Type1 -> Type2 -> Type`

* **值构造子(函数): 输入值, 返回值**
* 模式匹配中的模式为值构造子

#### 类型导出

```haskell
module Shapes
( Point(..)		-- .. 表示所有值构造子
, Shape(..)
, ...
) where
```

* **TypeName** 只导出类型
* **TypeName(..)** 导出类型及所有值构造子
* **TypeName(Constructor1, Constructor2)** 导出类型及指定值构造子

#### Record Syntax

**{}**

* 按项取值
* 自动生成取参函数

```haskell
data Person = Person {	name :: String,
					   	age :: Int,
					   	...
					 }
```

#### Type Constructor 

**类型构造子: 输入类型, 返回类型**

**data Maybe a = Nothing | Just a**

* 类型参数 **a**

* **Nothing**  类型 **Maybe a**

#### Derived instance

**data Type = Type {} deriving (Eq, Show, Read)**

* 类型构造子的所有类型参数都在 `Eq` 中, Type 才能被划分到 `Eq` 中

* `False` 在 `True` 前, 比 `True` 小

* `Maybe a` 中, `Nothing` 小于 `Just something`

#### 类型别名

**type String = [Char]**

**type AssocList k v = [(k,v)]**

**type IntMap = Map.Map Int**

* 类型构造子 `AssocList`, 类型参数 k v 

* **类型构造子不全调用**得到新类型构造子
* 类型别名只能用在类型部分

#### Either a b

* left 类型为 a, right 类型为 b
* a 错误类型, b 成功类型

```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

#### 递归定义数据结构

**List**

```haskell
data List a = Empty | Cons a (List a) 
data List a = Empty | Cons { listHead :: a, listTail :: List a}
```

**结合性优先级**

定义函数为 operator 时可指定 fixity

`infixl 7 *`

`infixl 6 +`



**二元搜索树**

定义

```haskell
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

插入

```haskell
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
      | x == a = Node x left right
      | x < a  = Node a (treeInsert x left) right
      | x > a  = Node a left (treeInsert x right)
```

检查

```haskell
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
```

### Typeclass

Typeclass 行为由函数描述

Type 定义 instance 

#### class

**class Eq a where**

```haskell
class (TypeClass a) => Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

* /= 与 == 相互定义
* 最小完整定义: 让类型符合 class 行为的**最小实作函数数量**

#### instance

**instance Eq Light where**

类型 Light (deriving Eq)

```haskell
data Light = Red | Yellow | Green

instance Eq Light where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```

**instance 含类型构造子**

```haskell
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

$~$

### Functor typeclass

可被 map over

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

* **一类型参数的类型构造子 f**

```haskell
instance Functor [] where
    fmap = map
```

```haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

二类型参数类型构造子 **Either**

```haskell
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

### Kind

$值 \xrightarrow{标签} 类型 \xrightarrow{标签} kind$ 

`Int :: *`

`Maybe :: * -> *`

`Either :: * -> * -> *`

#### 类型构造子curry

**class**

```haskell
class Tofu t where
    tofu :: j a -> t a j
```

* `j :: * -> *`
* `t :: * -> (* -> *) -> *`

**type**

`* -> (* -> *) -> *`

```haskell
data Frank a b  = Frank {frankField :: b a} deriving (Show)
```

* `b a :: *`
* `b :: * -> *`
* `Frank :: * -> (* -> *) -> *`

**instance**

* type Frank 带入 class Tofu t

```haskell
instance Tofu Frank where
    tofu x = Frank x
```

## 输入与输出

### IO action

#### Hello world

`main = putStrLn "hello, world"`

`putStrLn :: String -> IO ()`

* `IO` I/O action
* `()` 返回类型
* do 绑定多个 IO action 为一个 IO action, 返回类型为最后一个 IO action 的返回类型
* main 类型为 IO something

```C++
getLine :: IO String
name <- getLine
```

* 执行 IO action getLine, 将返回值绑定到 name 名称(String)
* `<-` 从 IO action 中取出包含的返回值
* I/O action 才能获取 I/O action 的数据

* do block 最后一个 action 不能绑定名字

* **return :: Monad m => a -> m a**  将 pure value 包装为 IO action

#### IO 函数

**putStrLn :: String -> IO ()**

**putStr :: String -> IO ()** 边界条件为空字串

**putChar :: Char -> IO ()**

**print :: Show a => a -> IO ()**

**getChar :: IO Char**

由于缓冲区, 只有按下 Enter 时才会触发读取字符的行为

**when :: Applicative f => Bool -> f () -> f ()** 封装 `if something then do some I/O action else return () `

**sequence :: [IO a] -> IO [a]**  [a] IO action 返回值的 List

```haskell
rs <- sequence [getLine, getLine, getLine]
```

**mapM mapM_** mapM 执行 IO action, 返回结果, mapM_ 只执行 IO action, 丢弃结果

**forever** 无限循环 IO action

**forM** 参数先 List,  

### 文件与字符流

**getContents :: IO String** 行读入, 读到 EOF, Lazy IO

**interact :: (String -> String) -> IO ()**

**openFile :: FilePath -> IOMode -> IO Handle** 

```haskell
type FilePath = String
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```

**hGetContents :: Handle -> IO String**

**hClose :: Handle -> IO ()**

**withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a**

**hGetLine**、**hPutStr**、**hPutStrLn**、**hGetChar**

**readFile :: FilePath -> IO String**

 **writefile :: FilePath -> String -> IO ()**

**appendFile :: FilePath -> String -> IO ()** 

> 默认情况下:
>
> 文本文件 line-buffering
>
> 二进制文件 block-buffering

**hSetBuffering :: Handle -> BufferMode -> IO ()** 设置 buffer 模式

```haskell
data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
```

* NoBuffering: char
* LineBuffering: line
* BlockBuffering: Nothing(操作系统) Just Int byte

**hFlush :: Handle -> IO ()**

**openTempFile :: FilePath -> String -> IO (FilePath, Handle)**

**removeFile :: FilePath -> IO ()**

**renameFile :: String -> FilePath -> IO ()**

### 命令行引数

#### System.Environment

**getArgs :: IO [String]** 获取命令行参数

**getProgName :: IO String** 获取程序名称

```haskell
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]       
            
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
  
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
```

### 随机数

#### System.Random

**random :: (RandomGen g, Random a) => g -> (a, g)**

* RandomGen 随机数源类型类
   * **StdGen**
* Random 随机数类型类

**mkStdGen :: Int -> StdGen** 

```haskell
random (mkStdGen 100) :: (Int, StdGen)
```

**randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)**

**randomRs** 

**newStdGen** 

### Bytestring

#### Strict bytestring

* Data.ByteString
* 无惰性
* 一次性计算整个 bytestring

#### Lazy bytestring

* Data.ByteString.Lazy
* 惰性
* 每 chunk 读

**pack :: [Word8] -> ByteString**

**unpack :: ByteString -> [Word8]**

**fromChunks** strict bytestring 转 lazy bytestring

**toChunks** lazy bytestring 转 strict bytestring

**empty** 空 bytestring

### Exception

* pure code 和 IO code 都能抛出 Exception
* 只有 IO code 才能接收 Exception

```haskell
System.Directory
doesFileExist :: FilePath -> IO Bool 
```

#### System.IO.Error

**catch :: IO a -> (IOError -> IO a) -> IO a**

* **IO a** try block
* **(IOError -> IO a)** catch block

```haskell
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"
```

**isDoesNotExistError :: IOError -> Bool**

**ioError :: IOException -> IO a**

## 函数式思考问题

### 逆波兰表示法

`"10 4 3 + 2 * -" -> -4`

```haskell
import Data.List  

solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs
```

### 路径规划

#### 定义类型

```haskell
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]
```

#### step 二元函数

```haskell
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)
```

#### 路径规划

```haskell
optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
                then reverse bestAPath  
                else reverse bestBPath
```

#### 输入输出处理

```haskell
import Data.List  

main = do  
    contents <- getContents  
    let threes = groupsOf 3 (map read $ lines contents)  
        roadSystem = map (\[a,b,c] -> Section a b c) threes  
        path = optimalPath roadSystem  
        pathString = concat $ map (show . fst) path  
        pathPrice = sum $ map snd path  
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice

groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)
```

## Functor Applicative Functor 与 Monoid

### Functor

#### Functor fmap

可被 map over, list, Maybe, tree ...

```haskell
Functor :: (* -> *) -> Constraint
f :: * -> * 
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

* 类型构造子 f
* `fmap :: (a -> b) -> f a -> f b`
* 用函数 `(a -> b)` 将 `(f a)` 映射为 `(f b)`
* functor 类比为算法, f a 为算法1, f b 为算法2,  a->b 是map over 的函数, 用来映射算法1->算法2
* 函数 fmap 接受一个函数, 并把函数 lift 到 functor 上
* lift: 将函数"提升"到另一个上下文中 

#### Instance

**Either a**

```haskell
instance Functor (Either a) where
	fmap _ (Left x) = Left x
	fmap f (Right y) = Right (f y)
```

> Either a 是类型构造子 f
>
> Left x 和 Right y 是类型 f a 的模式(数学变量/形参)
>
> f 是类型 a->b 的模式(数学变量)

**IO**

```haskell
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

**`(->) r`**

```haskell
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))
    fmap = (.)
```

* 类型构造子 `(->) r`
* 类型 `(a->b)` 模式 f
* 类型 `(r->a)` 模式 g
* `fmap f g` 类型 `(r->b)`

#### Functor laws

函子定律

**恒等律**

**fmap id = id**

恒等函数映射 = 恒等函数

**组合律**

**fmap (f . g) = fmap f . fmap g**

组合映射 = 映射组合

### Applicative functor

#### 定义

module **Control.Applicative**

typeclass **Applicative**

function **pure <*>**

```haskell
class (Functor f) => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```

**pure**

`pure :: a -> f a`

* f 为 applicative functor instance
* pure 接受值, 返回包含值的 applicative functor

**<*>**

`(<*>) :: f (a -> b) -> f a -> f b`

**<*>** 接受一个包含函数的 functor 和另一个 functor, 取出 functor 中的函数对另一个 functor 中的值做 map

```haskell
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something
```

**<$>**

**pure f <*> x**  $\Leftrightarrow$  **fmap f x**  $\Leftrightarrow$  **f <\$> x**

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x
```

#### Instance

**Maybe**

```haskell
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something
```

**List**

```haskell
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]
```

* List 是非确定性计算, 代表所有可能

**IO**

```haskell
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)
```

**`(->) r`**

装着结果的盒子 **`f a = (r->a)`**

```haskell
instance Applicative ((->) r) where
	pure x = (\_ -> x)
	f <*> g = \x -> f x (g x)
```

**ZipList**

```haskell
instance Applicative ZipList where
	pure x = ZipList (repeat x)
	ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> fx) fs xs)
```

#### 函数

**getZipList :: ZipList a -> [a]** 从 zip list 取出 list

**liftA2**

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b
```

**sequenceA**

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA [] = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])
```

#### 定律

```haskell
pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
```

### 关键字 newtype

#### 定义

**newtype** 以现有类型定义新类型

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }
```

* 轻量级封装, 成本低
* 只能定义一个单字段值构造子

**deriving**

```haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
```

值构造子  `CharList :: [Char] -> CharList`

函数 `getCharList :: CharList -> [Char]`

#### newtype 定制 instance

需求: tuple 为 Functor 的 instance, 用 fmap 来 map over tuple 时会使用二参数 tuple 的第一个元素

例: `fmap (+3) (1, 1)` 得到 `(4, 1)`

**newtype 定制 instance tuple**

```haskell
newtype Pair b a = Pair { getPair :: (a, b) }
```

```haskell
instance Functor (Pair c) where
	fmap f (Pair (x, y)) = Pair (f x, y)
```

#### lazy

newtype 唯一一个单参数值构造子, 在模式匹配时编译器直接确定具体值构造子

**data 定义 CoolBool**

```haskell
data CoolBool = CoolBool { getCoolBool :: Bool }
```

模式匹配

```haskell
helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"
```

调用

```haskell
ghci> helloMe undefined
"*** Exception: Prelude.undefined  "
```

出错

data 允许多个值构造子, 所以需要通过传入值计算哪个值构造子被用到(即使只有一个), 计算 undefined 会抛出 Exception

**newtype 定义 CoolBool**

```haskell
newtype CoolBool = CoolBool { getCoolBool :: Bool }
```

调用

```haskell
ghci> helloMe undefined  
"hello"
```

正常

newtype 只允许一个值构造子, 无需计算值构造子, 跳过 undefined 传入值

#### type vs newtype vs data

type 类型别名, 更易理解

newtype 简单包装, 单字段单值构造子

data 自定义类型, 任意数量字段和值构造子

### Monoid

#### 定义

二元函数满足:

* 结合律
* identity

```haskell
class Monoid m where
	mempty :: m
	mappend :: m -> m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty
```

* mempty 即 1
* mappend 即二元函数

定律

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

#### List

```haskell
instance Monoid [a] where
	mempty = []
	mappend = (++)
```

#### Product 和 Sum

Num 是一种 monoid:

* 二元函数 `*`  identity `1`
* 二元函数 `+`  identity `0`

newtype 包装现有类型定义两种方式

**Product**

```haskell
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)
```

包裹 **Product a**

解开 **getProduct (Product a)**

Monoid instance

```haskell
instance Num a => Monoid (Product a) where
	mempty = Product 1
	Product x `mappend` Product y = Product (x * y)
```

**Sum**

```
newtype Sum a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Num a => Monoid (Sum a) where  
    mempty = Sum 0  
    Sum x `mappend` Sum y = Sum (x + y)
```

#### Any 和 All

Bool 是一种 Monoid:

* 二元函数 `||`  identity `False`
* 二元函数 `||`  identity `False`

**Any**

```haskell
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Monoid Any where  
    mempty = Any False  
    Any x `mappend` Any y = Any (x || y)
```

**All**

```haskell
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)
```

#### Ordering

```haskell
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT
```

#### Maybe

Monoid a => Monoid (Maybe a)

```haskell
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

丢掉第二个值

```haskell
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)
```

```haskell
instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x
```

保留第二个值 Last a

### Foldable

使用 Monoid fold 数据结构

Foldable typeclass 可 flod 结构

```haskell
import qualified Foldable as F
```

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
```

实作 `flodr` 或 `foldMap`

```haskell
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
```

Tree

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

```haskell
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                                f x           `mappend`  
                                F.foldMap f r
```

 
