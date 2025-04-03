# Haskell 语法糖

## 非表达式

### 模式匹配

case

```haskell
(x:xs) = x
```

脱糖

```haskell
case list of
	[]     -> error "empty list"
  	(x:xs) -> x
```

### Guard

if-else

```haskell
f :: [a] -> String
f xs 
	| null xs   = "Empty"
	| length xs > 5 = "Long"
  	| otherwise = "Medium"
```

脱糖

```haskell
f xs = 	if null xs 
		then "Empty"
        else if length xs > 5 
        then "Long"
    	else "Medium"
```

### Record

函数

```haskell
data Person = Person { name :: String, age :: Int }
```

脱糖

```haskell
data Person = Person String Int

name :: Person -> String
name (Person n _) = n

age :: Person -> Int
age (Person _ a) = a
```

$~$

## 表达式

### List Comprehension

concatMap

```haskell
[x^2 | x <- [1..10], even x]
```

脱糖

```haskell
concatMap (\x -> if even x then [x^2] else []) [1..10]
```

### do

`>>=`

```haskell
do
	name <- getLine
	putStrLn ("Hello, " ++ name)
```

脱糖

```haskell
getLine >>= \name -> putStrLn ("Hello, " ++ name)
```

### 中缀函数

前缀函数

```haskell
1 + 2
```

脱糖

```haskell
(+) 1 2
```

### 字符串

```haskell
"Hello World"
```

脱糖

```haskell
['H','e','l','l','o',' ','W','o','r','l','d']
```

### Let

```haskell
let a = 0; b = 1 in a + b
```

脱糖

```haskell
(\a b -> a + b) 0 1
```

### If-else

```haskell
if x >= 0 then x else -x
```

脱糖

```haskell
case x >= 0 of
	True  -> x
	False -> -x
```

### Range

enumFromTo enumFromThenTo

```haskell
[1..5]        -- 枚举
[1,3..10]     -- 指定步长
```

脱糖

```haskell
enumFromTo 1 5          -- [1,2,3,4,5]
enumFromThenTo 1 3 10   -- [1,3,5,7,9]
```

### 运算符段

lambda

```haskell
(+1)     -- 等价于 \x -> x + 1
(1+)     -- 等价于 \x -> 1 + x
```

脱糖

```haskell
\x -> (+) x 1
\x -> (+) 1 x
```

