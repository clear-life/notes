# Haskell 体系

## 程序结构

* module 模块
   * declaration 声明
      * expression 表达式
         * lexical structure 词法结构

### 模块

#### 导入模块

**import Data.List**

**import Data.List (f, g)**

**import Data.List hiding (f)**

**import qualified Data.List**

**import qualified Data.List as L**

#### 自定义模块

```haskell
module MyModule
( f1
, f2
, f3
, ...
) where
```



## 基础概念

side effect 改变函数外状态

referential transparency引用透明: 同参数多次调用函数, 结果相同

## 核心

### 概念

**值 类型、名称 绑定、声明 表达式**

$值 \leftarrow 类型 \leftarrow kind$

$名称 \xleftarrow{绑定} 表达式$

$~$

### 值系统

#### 值构造子

值层的构造函数

* Record

**List 生成**

* Range 

   **`[a1, a2 .. n]` **

   * 默认步长为 1
   * 避免使用浮点数

* List Comprehension

   `[ 输出函数 | 限制条件 ]`



$~$

### 类型系统

#### 类型构造子

类型层的构造函数

类型值 $\xleftarrow{构造}$ 类型构造子

* `:k *` 零参类型构造子
* `:k * -> * -> ..` 有参类型构造子 
   * **`->`**  `infixr -1 ->` `*->*->*`

**Curry**

类型构造子可部分应用类型参数

#### 类型类

通过函数接口定义类型行为, 对类型分类

| Typeclass   |    作用     |      抽像函数      |  示例  |
| :---------- | :---------: | :----------------: | :----: |
| **Eq**      |   相等性    |     `==` `/=`      |  Bool  |
| **Ord**     |    比较     | `<=` `>` `compare` |  Char  |
| **Show**    |  toString   |       `show`       | [Char] |
| **Read**    | fromString  |       `read`       |  Bool  |
| **Num**     |  数值运算   |   `+` `*` `abs`    | Double |
| **Functor** | 可 map over |       `fmap`       | Maybe  |

* 类型约束

#### Kind

类型的标签

零参类型构造子 $\xrightarrow{kind}$ `*` 

有参类型构造子 $\xrightarrow{kind}$ `* -> * -> ..` 

$~$

### 名称

**六种名称**: 

* 值命名空间
   * 变量
      * 函数
   * 值构造子
* 类型命名空间
   * 类型变量
   * 类型构造子
   * 类型类
* 模块命名空间
   * 模块

**限制**:

* **变量名 类型变量名** 小写字母/_开头

   **值构造子 类型构造子 类型类 模块** 大写字母开头

* 同作用域, **类型构造子和类型类不能重名**

$~$

### 绑定

名称与表达式关联

* 模式匹配解构传入值

* let 表达式绑定

* where 子句绑定

* do 表达式绑定

#### 模式匹配

`xs@pattern` as 模式对整体的引用

$~$

### 声明

* 类型声明
* 绑定声明

* 模块/导入声明

$~$

### 表达式

* 名称

   变量名/函数名

* 值构造

   值构造子调用/记录/列表推导式/算术序列

* if case let do

   * **let bindings in expression**
   * **case expression of pattern -> expression**



## 函数

### 语法

**前缀函数**

* 前缀式调用 `()`
* 10级, 左结合

**中缀函数**

* 中缀式调用 ```  ``  ```
* `[infixl/infixr/infix] [0-9] f`  0-9级, 左/右/无 结合

**Guard**

```haskell
f pattern
	| exp1 = 
	| exp2 = 
    | otherwise = 
```

**Where**

作用域: **函数或模式匹配分支**

```haskell
f x = 
	where
```

**Lambda**

`\ x y .. -> ...`

* 一种模式

**复合函数**

```haskell
(.) :: (b->c) -> (a->b) -> a -> c
f . g = \x -> f (g x)
```



### Curry

允许函数部分调用

* 前缀函数按参数顺序 curry, 中缀函数可不按顺序
* 函数是一等公民, 像值一样传递

* 部分调用, 复用函数
* 类型系统支持 curry `a -> b -> c` $\Leftrightarrow$ `a -> (b -> c)`
* 函数组合 `.`
* 高阶函数抽像能力

### 重要函数

**map 映射**

```haskell
map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

**filter 过滤**

```haskell
filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
	| f x = x : filter f xs 
	| otherwise = filter f xs
```

**fold**

**fold f c list**

`foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b`

* 从左到右, 初值在左, 不能处理无限 List

`foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b`

* 从右到左, 初值在右, 能处理无限 List

`foldl1 与 foldr1`

* 首尾元素为初值

**scan**

scanl scanl1 scanr scanr1 记录累加值的所有状态到 list

**zipWith 打包**

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

### 常用函数

**运算符**

| operator | 优先级结合性 |            作用            |
| :------: | :----------: | :------------------------: |
|  **$**   | `infixr 0 $` |     相当于在右侧加括号     |
| **`.`**  | `infixr 9 .` | 复合函数(point free style) |

```haskell
($) :: (a -> b) -> a -> b  
f $ x = f x

map ($ 3) [(4+),(10*),(^2),sqrt]  -- $ 可将值作为函数使用
```



**Num**

| **fromIntegral** | 整数换为通用数 |
| :--------------: | :------------: |
|                  |                |
|                  |                |
|                  |                |
|                  |                |

**List**

取元素

|   **!!**    | `[a] !! i` |
| :---------: | :--------: |
|  **take**   | 前n个元素  |
|  **head**   |    头部    |
|  **tail**   |    尾部    |
|  **last**   |   尾元素   |
|  **init**   |    非尾    |
| **maximum** |  最大元素  |
| **minimum** |  最小元素  |

查信息

|   length    |     长度      |
| :---------: | :-----------: |
|  **null**   |     判空      |
|  **elem**   | 判存 $a\in A$ |
|   **sum**   |    元素和     |
| **product** |    元素积     |

变换

|    **:**    |       前插        |
| :---------: | :---------------: |
|   **++**    | 合并, 遍历左 List |
| **reverse** |       反转        |
|  **drop**   |  删除前 n 个元素  |

List 生成

|   **cycle**   |   重复 List    |
| :-----------: | :------------: |
|  **repeat**   |    重复元素    |
| **replicate** | 重复元素 n 次  |
|    **zip**    | 生成 pair list |
|  **zipWith**  |  二元函数 zip  |

### 模块函数

#### Data.List

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

#### Data.Char

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

#### Data.Map

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

#### Data.Set

**import qualified Data.Set as Set**

**fromList** List -> Set

**difference** $A - A \bigcap B$

**union** $A \bigcap B$

**null  size  member  empty  singleton  insert  delete**

**isSubsetOf isProperSubsetOf** 子集 真子集

**map filter**

**toList** Set->List

$~$

## 数学

#### 函数

$f(x,y...)=...$

$f(x,y...)$

#### List

数列 ${a_1, a_2\ ...}$

$\{x\in X|f(a,b...), a\in A,b\in B.. \}$ 列表推导式

`zip :: [a] -> [b] -> [(a, b)]`
$$
A = a1,a2\dots, B=b1,b2\dots 	\\
\{(a_i,b_i)|a_i\in A,b_i\in B\}
$$

#### Curry

$f(x,y,z) = g(x)*h(y)*j(z)$

每次带入一个变量的值

$f(a,y,z) = g(a)*h(y)*j(z)$

$f(a,b,z) = g(a)*h(b)*j(z)$

$f(a,b,c) = g(a)*h(b)*j(c)$



fold f c0 x

$c1 = f(x_0, c_0)$

$c2 = f(x_1, c_1)$

$...$

$c_{n+1} = f(x_{n}, c_n)$

$(f\circ g) = f(g(x))$