# Haskell 函数

## 语法

### 作用域

$函数 \rightarrow 模式 \rightarrow Guard$

#### Where

局部绑定, 作用域: **模式**

```haskell
f x = 
	where
```

### 函数分类

#### 前缀函数

前缀式调用 `()`

10级, 左结合

#### 中缀函数

中缀式调用 ```  ``  ```

**优先级结合性**

* `[infixl/infixr/infix] [0-9] op`  0-9级, 左/右/无 结合
* **优先级**: 分组方式(隐式括号), 不代表计算顺序

* **结合性**: 相同优先级的分组方向

#### Lambda

**匿名函数调用**

`\ x y .. -> ...`

#### 值构造子

`ValueConstructor Type1 Type2 ..`

### 分段函数

#### 模式匹配

```haskell
f pattern1 =
f pattern2 =
..
f patternn =
```

#### Guard

```haskell
f pattern
	| exp1 = 
	| exp2 = 
    | otherwise = 
```

### 复合函数

`infixr 9 .`

* 右侧输出 $\Leftrightarrow$ 左侧输入
* 右侧输入 `->` 左侧输出

```haskell
(.) :: (b->c) -> (a->b) -> a -> c
f . g = \x -> f (g x)
```

### 高阶函数/Curry

函数类型都是 **`a->b`**

* 数学: 带入变量值, 编程: 函数部分调用
* 前缀函数按顺序 curry, 中缀函数可不按顺序
* 类型系统支持 curry `a -> b -> c` $\Leftrightarrow$ `a -> (b -> c)`

```haskell
f x y z = x + y + z
等价于
f = \x -> \y -> \z -> x + y + z
```

### 重要函数

#### 运算符

| operator  |  优先级结合性  |           作用           |
| :-------: | :------------: | :----------------------: |
|   **$**   |  `infixr 0 $`  | 分组, 相当于在右侧加括号 |
|  **`.`**  |  `infixr 9 .`  | 水管, 右侧输出接左侧输入 |
| **`<$>`** | `infixl 4 <$>` |        中缀 fmap         |
| **`<*>`** | `infixl 4 <*>` |    context 值相互作用    |

```haskell
($) :: (a -> b) -> a -> b  
f $ x = f x

map ($ 3) [(4+),(10*),(^2),sqrt]  -- $ 可将值作为函数使用
```

```haskell
(.) :: (b->c) -> (a->b) -> a -> c
f . g = \x -> f (g x)
```

#### map

**映射元素**

```haskell
map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

#### filter

**过滤元素**

```haskell
filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
	| p x = x : filter p xs 
	| otherwise = filter p xs
```

#### fold

**迭代**

**fold f c list**

`foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b`

* 从左到右, 初值在左, 不能处理无限 List

`foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b`

* 从右到左, 初值在右, 能处理无限 List

`foldl1 与 foldr1`

* 首尾元素为初值

**scan**

scanl scanl1 scanr scanr1 记录累加值的所有状态到 list

#### zip

**组装**

```haskell
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys
```

**zipWith**

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
```

#### flip

调换参数位置

```haskell
flip' :: (a->b->c) -> b -> a -> c
flip' f y x = f x y
```



$~$

## 函数库

### 常用功能

#### Num

| **fromIntegral** | 整数换为通用数 |
| :--------------: | :------------: |
|                  |                |

#### List

**取元素**

|   **!!**    | 第 i 个元素 |
| :---------: | :---------: |
|  **take**   |  前n个元素  |
|  **head**   |    头部     |
|  **tail**   |    尾部     |
|  **last**   |   尾元素    |
|  **init**   |    非尾     |
| **maximum** |  最大元素   |
| **minimum** |  最小元素   |

**查信息**

|   length    |     长度      |
| :---------: | :-----------: |
|  **null**   |     判空      |
|  **elem**   | 判存 $a\in A$ |
|   **sum**   |    元素和     |
| **product** |    元素积     |

**变换**

|    **:**    |       前插        |
| :---------: | :---------------: |
|   **++**    | 合并, 遍历左 List |
| **reverse** |       反转        |
|  **drop**   |  删除前 n 个元素  |

**List 生成**

|   **cycle**   |   重复 List    |
| :-----------: | :------------: |
|  **repeat**   |    重复元素    |
| **replicate** | 重复元素 n 次  |
|    **zip**    | 生成 pair list |
|  **zipWith**  |  二元函数 zip  |

语法糖

* Range  `[a1, a2 .. an]`
   * `enumFromTo a1 an`
   * `enumFromThenTo a1 a2 an`

* List Comprehension `[ 输出函数 | 限制条件 ]`

   * `concatMap (\输入值 -> if 限制条件 then 输出函数 else []) 输入集合`



#### IO

长了脚的盒子, 包裹普通类型的值

IO action 绑定到 main 时触发

`return` 与 `<-` 作用相反

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

**mapM mapM_** mapM map 后 sequence, mapM_ map 后 sequence 并丢弃结果

**forever** 无限循环 IO action

**forM** 



#### 文件

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

#### System.Environment

**getArgs :: IO [String]** 获取命令行参数

**getProgName :: IO String** 获取程序名称



#### 字节流

单位 byte(8 bit)

**Strict bytestring**

* Data.ByteString
* 无惰性
* 一次性计算整个 bytestring

**Lazy bytestring**

* Data.ByteString.Lazy
* 惰性
* 每 chunk 读

**pack :: [Word8] -> ByteString**

**unpack :: ByteString -> [Word8]**

**fromChunks** strict bytestring 转 lazy bytestring

**toChunks** lazy bytestring 转 strict bytestring

**empty** 空 bytestring



#### Exception

* pure code 和 IO code 都能抛出 Exception
* 只有 IO code 才能接收 Exception

```haskell
System.Directory
doesFileExist :: FilePath -> IO Bool 
```

**System.IO.Error**

**catch :: IO a -> (IOError -> IO a) -> IO a**

* **IO a** try block
* **(IOError -> IO a)** catch block

**isDoesNotExistError :: IOError -> Bool**

**ioError :: IOException -> IO a**

#### Bounded

**minBound maxBound** 



### 模块函数

#### Data.List

**intersperse**  item 穿插 []

**intercalate** [] 穿插 [[]] 后 []

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



#### System.Random

**random :: (RandomGen g, Random a) => g -> (a, g)**

* RandomGen 随机数生成
   * **StdGen**
* Random 随机数值

**mkStdGen :: Int -> StdGen** 

```haskell
random (mkStdGen 100) :: (Int, StdGen)
```

**randoms**

**randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)**

**randomRs** 

**getStdGen**

**newStdGen** 



### 类型类

| Typeclass   |    作用     |      抽像函数      |  示例  |
| :---------- | :---------: | :----------------: | :----: |
| **Eq**      |   相等性    |     `==` `/=`      |  Bool  |
| **Ord**     |    比较     | `<=` `>` `compare` |  Char  |
| **Show**    |  toString   |       `show`       | [Char] |
| **Read**    | fromString  |       `read`       |  Bool  |
| **Num**     |  数值运算   |   `+` `*` `abs`    | Double |
| **Functor** | 可 map over |       `fmap`       | Maybe  |

