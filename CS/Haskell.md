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

## 自定义 Types 和 Typeclasses

### Algebraic Data Types 入门

#### data

**data Type = Value Constructor | ...**

`data Bool = False | True`

```haskell
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

* **Value Constructor** 值构造子为函数, 返回 Type 类型的值

* 模式匹配的对象是**Value Constructor**
* 匹配过的 `[]` `False` `5` 是不含参数的值构造子
* `deriving (Show)` 能让 Shape 被输出到控制台
* 类型只有一个 **Value Constructor** 时, 最好与类型重名

导出类型

`Shape(..) ` 表示到处所有 **Value Constructor**

```haskell
module Shapes
( Point(..)
, Shape(..)
, ...
) where
```

* 不导出类型的 **Value Constructor** 也就不能用该 **Value Constructor** 进行模式匹配

### Record Syntax

```haskell
data Person = Person{	a :: type,
					   	b :: type,
					   	...
					} deriving (Show)
```

```haskell
Car {company="Ford", model="Mustang", year=1967}
```

* 自动生成取值函数
* 无需关心项顺序

### Type parameters

#### Type Constructor 

**Type Constructor** 类型构造子: 类型为参数, 返回新类型

`data Maybe a = Nothing | Just a`

* `a` 类型参数
* List 是类型构造子

* `Nothing`  类型 `Maybe a`
* `[]` 类型 `[a]`
* 不要在 `data` 声明中加类型约束

```haskell
data Type a b .. = Type {	x :: a,
							y :: b,
							...
						} deriving (Show)
```

### Derived instance

`data Type = Type {} deriving (Eq, Show, Read)`

将 Type 划分到 Eq 里, 然后就能用 `==` 和 `/=` 判定相等性

* 类型构造子的所有类型参数都在 `Eq` 中, Type 才能被划分到 `Eq` 中, 即成为 `Eq` 的 instance

`Ord` 类型类 derive instance

先判断值构造子是否一致

再判断参数(参数都是 `Ord` 的 instance)

`data Bool = False | True deriving (Ord)`

`False` 在 `True` 前, 比 `True` 小

`Maybe a` 类型中, `Nothing` 小于 `Just something`

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

### Type synonyms

类型别名

`type String = [Char]`

`type AssocList k v = [(k,v)]`

`AssocList` 为类型构造子, 类型 k v 为参数, 生成具体类型

不全调用得到新的类型构造子

`type IntMap v = Map Int v`

`type IntMap = Map Int`

qualified import 导入 Data.Map

`type IntMap = Map.Map Int`

* 类型构造子只能用在类型部分, 不能用在值部分, 不能当作值构造子使用

`Either a b`

```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

### Recursive data structures

递归定义数据结构

List 定义: `[]` 或 x : List

```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

record syntax

```haskell
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
```

结合性优先级

fixity 指定结合性(left/right-associative)和优先级

`*` fixity `infixl 7 *`

`+` fixity `infixl 6 +`

```haskell
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
```

```haskell
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
```

二元搜索树

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



```haskell
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
```

### 自定义 Typeclasses

* typeclass 看作 interface, 定义一些行为(函数)
* 满足所有行为的类型为 typeclass 的 instance

#### Prelude  Eq 定义

`class TypeClassName typeVar where`

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

* class 定义新 typeclass Eq, a(类型变量) 为 Eq 的 instance
* 交叉递归定义 class 行为函数

#### 定义 typeclass 的 instance

`instance TypeClassName TypeName where `

```haskell
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```

`==` 由 `/=` 定义

`/=` 由 `==` 定义

只需在 instance 定义中实现其中一个

最小完整定义 minimal complete definition: 能让 type 符合 typeclass 行为的最少函数实现数量

**Show instance**

```haskell
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

derive 生成 show 会将 值构造子转换为 String

**定义为 subclass**

```haskell
class (Eq a) => Num a where
   ...
```

* (Eq a) 类型约束 Num a 
* subclass: 定义 Num 的 instance a 前 a 首先是 Eq 的 instance
* Maybe 不能作为 class 中的类型, 因为 Maybe 不是类型, 而是类型构造子, 但 Maybe m 却可以
* `:info TypeClass` 查看 instance

```haskell
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

### yes-no typeclass

typeclass

```haskell
class YesNo a where
    yesno :: a -> Bool
```

instance

```haskell
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
```

```haskell
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
```

```haskell
instance YesNo Bool where
    yesno = id	-- id 返回参数
```

```haskell
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
```

```haskell
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
```

### Functor typeclass

可被 map 的类型

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

f 类型构造子, 接受类型 a 返回类型, 接受类型 b 返回类型

`map :: (a -> b) -> [a] -> [b]`

map 就是对 List 的 fmap

```haskell
instance Functor [] where
    fmap = map
```

能做容器的类型可能就是 functor(Functor 的 instance)

```haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

### Kind

king 类型的标签, 类型的类型

`:k` 

`*` 具体类型

类型构造子也可 curry, 能 partially apply

## 输入与输出

### Hello, world!

`main = putStrLn "hello, world"`

**编译**

`ghc --make test.hs`

```haskell
:t putStrLn
putStrLn :: String -> IO ()
```

* `IO` 表 I/O action, 绑定到 main 后能执行 IO 操作
* `()` 为 I/O action 的返回值
* do 将所有 I/O action 绑定为一个

```haskell
main :: IO something
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

**getLine** 类型 IO String, 执行 I/O action 将结果绑定到名字中

* 只有在 I/O action 中才能拿到某一个 I/O action 的数据
* I/O code: 依赖 I/O 的程序

* do block 最后一个 action 不能绑定名字

let bindings 能在 do block 中使用(不需要 in)

```haskell
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

**return** 将 pure value 包装为 IO action, return 不会结束执行

**putStr** 不换行 type signature `putStr :: String -> IO ()`

是一个包在 IO action 的 unit, 即空值, 不能绑定

**putChar**

**print** 相当于 `putStrLn . show` 但能输出 `""`

**getChar** type signature `getChar :: IO Char`

由于缓冲区, 只有按下 Enter 时才会触发读取字符的行为

**when**(`import Contorl.Monad`)

True 返回传入 IO action, False 返回 return ()

封装 `if something then do some I/O action else return ()`

**sequence** `sequence :: [IO a] -> IO [a]`

```haskell
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
    
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
```

**mapM mapM_** 返回 IO action 的函数  List,  mapM_ 丢弃运算结果(例 return () 中的 ())

**forever** 无限循环一个 IO action

**forM**  Control.Monad 跟 mapM 作用一样, 但参数顺序相反

### 文件与字符流

**getContents** 从标准输入读取直到 EOF `getContents :: IO String` 惰性 IO(按行读取), 用作重导输入输出

**interact** 接受 `String -> String` 函数, 返回 IO action

**openFile** `openFile :: FilePath -> IOMode -> IO Handle` 文件路径 IOMode 返回 IO action

`type FilePath = String`

`data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode`

`IO Mode` 表示一个 IO action 包含一个类型为 Mode 的值

**hGetContents** 从文件读取 Contents

**hClose** 关闭文件

**withFile** `withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`

**hGetLine**、**hPutStr**、**hPutStrLn**、**hGetChar**

**readFile**  `readFile :: FilePath -> IO String`

 **writeFile**  `writefile :: FilePath -> String -> IO ()`

**appendFile** 

**hFlush**

**openTempFile**

**removeFile**

**renameFile**

### 命令行引数

**getArgs** `System.Environment` `getArgs :: IO [String]`

**getProgName**

### 随机数

**System.Random**

**random** `random :: (RandomGen g, Random a) => g -> (a, g)`

### Bytestrings

**Data.ByteString** Strict bytestrings

**Data.ByteString.Lazy** Lazy bytestrings

**pack**

**unpack**

**fromChunks**

**empty**

### Exceptions

Exception 只能在 I/O section 中被接到

**catch**
