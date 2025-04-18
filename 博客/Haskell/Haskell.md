

## Monad

**动态组合**

**context 值用在 `a->m b` 函数上**

### >>=

**抽出 context1 中的值1 用在 `a->m b` 函数上, 生成 context2 中的值2**

```haskell
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

### Monad

**`>>=`  context 值  `a->m b`函数  context值**

**`>>`** context值1 `>>=` 丢进返回 context值2 的函数, 维护 context

```haskell
class Applicative m => Monad m where  
	return :: a -> m a
	return = pure
    
	(>>=) :: m a -> (a -> m b) -> m b  

	(>>) :: m a -> m b -> m b  
	m >> k = m >>= \_ -> k
```

**Maybe instance**

```haskell
instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
```

### do 表示法

#### 定义

* 语法糖 将 `.. >>= .. >>= ..` 拆分为 `do .. <- .. <- ..`
* 每行都是一个单子值
* 最后一值是 do 动态组合的总结果
* `>>`:  没有用 `<-` 绑定值

#### 脱糖

```haskell
do
  x1 <- m1      ⇒   m1 >>= \x1 -> ...
  x2 <- m2      ⇒   m2 >>= \x2 -> ...
  x3 <- m3      ⇒   m3 >>= \x3 -> ...
  ...
  xn <- mn      ⇒   mn >>= \xn -> ...
  return result ⇒   return result
```

例

```haskell
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)		return (0,0) >>= \start ->
    first <- landLeft 1 start  	landLeft 1 start >>= \first ->
    Nothing  					Nothing >>= \_ -> 
    second <- landRight 2 first landRight 2 first >>= \second ->
    landLeft 3 second			landLeft 3 second
```

```haskell
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)
    first <- landLeft 1 start
    Nothing
    second <- landRight 2 first
    landLeft 3 second
```

对比

```
return (0,0) >>= landLeft 1 >>= \first -> Nothing
>>= \_ -> landRight 2 first >>= landLeft 3
```

### List Monad

非确定性计算

#### 定义

```haskell
instance Monad [] where
	return x = [x]
	xs >>= f = concat (map f xs)
```



```haskell
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
	n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)
```

`n <- [1,2]` n 代表 [1, 2] 任一值

`ch <- ['a','b']` ch 代表 ['a','b'] 任一值

`return (n,ch)` 表示结果为 `[(n, ch)]` 由于 `[]` context 为非确定性计算, 所以结果为 `[(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]`

#### MonadPlus

既是 monad 又是 monoid

```haskell
class Monad m => MonadPlus m where
	mzero :: m a
	mplus :: m a -> m a -> m a
```

**mzero** 幺元/单位元 mempty

**mplus** 二元函数 mappend

```haskell
instance MonadPlus [] where  
    mzero = []  
    mplus = (++)
```

guard

```haskell
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero
```

```haskell
[1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)  
[7,17,27,37,47]
```

```haskell
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x
```

### 单子律

**单位元 return**

**结合操作 `>>=`** 

```haskell
return a >>= k = k a	-- left identity
m >>= return = m		-- right identity
m >>= (\x -> k x >>= h) = (m >>= k) >>= h	-- associativity
```

**复合函数**

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)  
f . g = (\x -> f (g x))
```

结合律相当于 `h.k m = h (k m) ` 

**单子函数组合**

**`<=<`**  **`f <=< g`**  

g 的输出 >>= 到 f 的输入(管道)

g 的输入 -> f 的输出 

```haskell
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)
```

**单子律**

`k <=< return = k`

`return <=< m = m`

`f <=< (g <=< h) = (f <=< g) <=< h`

普通函数

`k . id = k`

`id . m = m`

`f . (g . h) = (f . g) . h`

### Writer Monad

#### 引入

context: 附加值(log)

monoid 作为附加值

```haskell
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)
```

#### 定义

Module `Control.Monad.Writer` 

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

```haskell
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

#### do

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f (Writer (x, log)) = Writer (f x, log) 

instance (Monoid w) => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    (<*>) :: Monoid w => Writer w (a -> b) -> Writer w a -> Writer w b
    Writer (f, log1) <*> Writer (x, log2) = Writer (f x, log1 `mappend` log2)

instance (Monoid w) => Monad (Writer w) where  
    return = pure   
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')


logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)
```

`a->m b` 函数 logNumber 处理 context 逻辑

```haskell
a <- logNumber 3	  logNumber 3 >>= \a ->
b <- logNumber 5  	  logNumber 5 >>= \b ->
return (a*b)		  return (a*b)
```

对应

```haskell
logNumber 3 >>= \a -> logNumber 5 >>= \b -> return (a*b)
```

### list

#### right-associate

++ right-associate 效率比 left-associate 高

**difference list**

**prepend 前插**

`\xs -> [..] ++ xs` 输入 list 前插 list, 使其 right-associate 效率高

```haskell
f :: [a] -> [a]
g :: [a] -> [a]
f `append` g = \xs -> f (g xs)

\xs -> "1" ++ ("2" ++ xs)
```

```haskell
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
```

```haskell
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  

fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []
```

```haskell
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
```



```haskell
ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])  
[1,2,3,4,1,2,3]
```

```haskell
import Control.Monad.Writer  

gcd' :: Int -> Int -> Writer (DiffList String) Int  
gcd' a b  
  | b == 0 = do  
      tell (toDiffList ["Finished with " ++ show a])  
      return a  
  | otherwise = do  
      result <- gcd' b (a `mod` b)  
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
      return result
```

```haskell
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34  
Finished with 2  
8 mod 2 = 0  
34 mod 8 = 2  
110 mod 34 = 8
```

测试性能

```haskell
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])
```

```haskell
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000  
0  
1  
2
```

```haskell
finalCountDown :: Int -> Writer [String] ()  
finalCountDown 0 = do  
    tell ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell [show x]
```

### Reader Monad

```haskell
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w
```

```haskell
import Control.Monad.Instances  

addStuff :: Int -> Int  
addStuff = do  
  a <- (*2)  
  b <- (+10)  
  return (a+b)
```

```haskell
ghci> addStuff 3  
19
```



### State Monad

```haskell
newtype State s a = State { runState :: s -> (a,s) }
```

```haskell
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState
```

```haskell
import Control.Monad.State  

pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)
```

### Error Monad

```haskell
instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg)
```

### 单子函数

Monadic functions

#### liftM

```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b
```

```haskell
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x))
```

```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = do  
    x <- m  
    return (f x)
```

```haskell
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
```

```haskell
ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap mf m = do  
    f <- mf  
    x <- m  
    return (f x)
```

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f x y = f <$> x <*> y
```

#### join

```haskell
join :: (Monad m) => m (m a) -> m a 
join mm = do  
    m <- mm  
    m
```

#### filterM

```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

```haskell
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
```

```haskell
powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs
```

#### foldM

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
```

```haskell
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```

### 自定义 Monad

有理数 `Rational` 分子跟分母用 `%` 分隔

```haskell
import Data.Ratio

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show
```

```haskell
instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs
```

```haskell
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []
```



## 函子 应用函子 单子 对比

### context

**context: 包裹值的容器或计算环境**

* `Maybe a` 结果可能失败

* `[a]` 非确定性结果

* `IO a` 副作用

#### 函子

**独立映射**

context 容器类型及**结构不变**, 仅包裹值改变

* Maybe: Just 仍为 Just, Nothing 仍为 Nothing

   ```haskell
   fmap (+1) (Just 2) = Just 3
   fmap (+1) Nothing  = Nothing
   ```

* []: 长度和次序不变

   ```haskell
   fmap (*2) [1,2,3] = [2,4,6]
   ```

* IO: 副作用流程

   ```haskell
   fmap (+1) getLine
   ```

#### 应用函子

**静态组合**

context 容器类型不变**结构静态改变**

* Maybe: 存在 Nothing 结果就是 Nothing, 否则为 Just

   ```haskell
   pure (+) <*> Just 1 <*> Just 2 = Just 3
   pure (+) <*> Just 1 <*> Nothing = Nothing
   ```

* []: 长度, 次序可能改变

   ```haskell
   [(+1), (*2)] <*> [1,2] = [2,3,2,4]
   ```

* IO: 副作用流程不变

   ```haskell
   action :: IO Int
   action = pure (+) <*> readLn <*> readLn
   -- 副作用流程：依次读取两个输入，再返回它们的和
   ```

#### 单子

**动态组合**

context 容器类型不变**结构动态改变**

`>>=` 后续计算依赖前一步结果, 容器结构动态改变

* Maybe: 分支动态改变

   ```haskell
   Just 1 >>= \x -> Nothing >>= \y -> Just (x+y) = Nothing
   ```

* []: 长度, 次序动态改变

   ```haskell
   [1,2] >>= \x -> [x, x*10] = [1,10,2,20]
   ```

* IO: 副作用流程动态改变

   ```haskell
   readFile "config.txt" >>= \content -> if content == "debug" then print "Debug mode" else return ()
   ```

### 核心函数

函数对 context 值操作后再包裹回去

#### 函子

`fmap :: Functor f => (a -> b) -> f a -> f b`

函数  context值  context值

#### 应用函子

`<*> :: Applicative f => f (a -> b) -> f a -> f b `

context函数  context值  context值

#### 单子

`>>= :: Monad a => m a -> (a -> m b) -> m b`

context值  `a->m b` 函数  context值



$~$

## 函数

**getZipList :: ZipList a -> [a]** 从 zip list 取出 list

`(,,)` 函数与 `\x y z -> (x,y,z)` 等价 

`(,)` 与 `\x y -> (x,y)` 等价

**zipWith3** **zipWith4** .. **zipWith7**

**liftA2**

二元函数版 fmap

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b
```

**sequenceA**

**sequenceA: 将包裹的值串为 list**

多个 applicative a 按序应用 `:` 函数绑为一个 applicative [a]

`sequenceA f a1 : [f a] = f (x:) <*> f [xs]`

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a] 
sequenceA [] = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a] 
sequenceA = foldr (liftA2 (:)) (pure [])
```

> 注意前缀函数调用优先级无限高, 下面练习时需注意这一点

理解 `sequenceA [Just 1, Just 2]`

`(:) <$> Just 1 <*> sequenceA [Just 2]`

`(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])`

`(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])`

`(:) <$> Just 1 <*> Just [2]`

`Just (1:) <*> Just [2]`

`Just [1, 2]`

理解 `sequenceA [Just 1, Nothing, Just 2]`

`Just (1:) <*> sequenceA [Nothing, Just 2]`

`Just (1:) <*> ((:) <$> Nothing <*> sequenceA [Just 2])`

`Just (1:) <*> (Nothing <*> Just [2])`

`Just (1:) <*> Nothing`

`Nothing`

理解 `sequenceA [(+3), (*2), (/2)]`

`Applicative (-> r)`

`sequenceA [r->a, r->a, r->a]`

`r->(a:) <*> sequenceA [r->a, r->a]`

`r->(a:) <*> (r->(a:) <*> sequenceA [r->a])`

`r->(a:) <*> (r->(a:) <*> r->[a])`

`r->(a:) <*> r->[a,a]`

`r->[a,a,a]`

`\x -> [x+3, x*2, x/2]`

理解 `sequenceA [[1,2], [3,4]`

`Applicative []`

`sequenceA [[1,2], [3,4]`

`(:) <$> [1,2] <*> sequenceA [[3,4]]`

`[(1:), (2:)] <*> [[3], [4]]`

`[[1,3], [1,4], [2,3], [2,4]]` 外层 `[]` 为 `f`, 内层 `[]` 为 `[a]`



## Instance

### Functor

**`[]`**

* pure value: 所有可能的值
* `[]`: 可能的多个或零个值

`[]` 包裹可能的结果值

fmap 将纯函数 f lift 到 `[]` functor 上, 对包裹的所有可能结果应用纯函数 f, 生成新 `[]` functor

```haskell
instance Functor [] where
	fmap = map
```

**Maybe**

* pure value: 成功值
* `Maybe`: 可能失败的计算

`Maybe` 包裹可能失败的值

fmap 将纯函数 f lift 到 `Maybe` functor 上, 对包裹的可能失败值应用纯函数 f, 生成新 `Maybe` functor

```haskell
instance Functor Maybe where
	fmap _ Nothing = Nothing
	fmap f (Just a) = Just (f a)
```

**Either a**

* pure value : 可能的成功值
* Either a: 失败时的错误信息

Either a 包裹可能的成功值

fmap 将纯函数 f lift 到 Either a functor 上, 对包裹的 pure value 应用纯函数 f, 生成新 Either a functor

```haskell
instance Functor (Either a) where
	fmap _ (Left x) = Left x
	fmap f (Right y) = Right (f y)
```

**IO**

IO a

* pure value : 通过副作用获取的值
* IO: 描述如何与外部世界交互(输入输出, 带有副作用)

IO 包裹通过副作用获取的值

fmap 将纯函数 f lift 到 IO functor 上, 对包裹的 pure value 应用纯函数 f, 生成新 IO functor

```haskell
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

**`(->) r`**

`(->) r a`

* pure value: 函数调用的结果值
* `(->) r`: 函数输入类型为 r

`(->) r` 包裹函数调用的结果值

fmap 将纯函数 f lift 到 `(->) r` functor 上, 对包裹的函数调用结果值应用纯函数 f, 生成新 `(->) r` functor

```haskell
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))
    fmap = (.)
```

* 类型构造子 `(->) r`
* f 类型 `a -> b`
* g 类型 `r -> a`
* `fmap f g` 类型 `(r->b)`









### Applicative functor

**Maybe**

context: 可能失败的运算

```haskell
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something
```

**List**

context: 所有可能结果

```haskell
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]
```

**IO**

context: 输入输出, 副作用

sequence: do 有先后顺序

```haskell
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)
```

**`(->) r`**

包裹函数调用结果

context: 函数带有环境进行计算, `(->) r` 共享环境 `r`, 即共享同样的输入, 其类型为 `r` 

```haskell
instance Applicative ((->) r) where
	pure x = (\_ -> x)
	f <*> g = \x -> f x (g x)
```

```haskell
f <*> g = \x -> f x (g x)

r->a->b <*> r->a = r->b
将 r->a->b 包裹的 a->b 函数应用在 r->a 包裹的 a 上, 得到 r->b 包裹的 b, 共享环境 r

f <*> g = \x -> f x (g x)
```

**理解 `(+) <$> (+3) <*> (*100)`**

`(+) <$> (+3)`  将 + 应用在 `r->a` 包裹的 `a` 上, 得到 `r->a->b` `\x -> (+) (x + 3)` 

`*100 :: r->a`

`(+) <$> (+3) <*> (*100)` 将 `r->a->b` 包裹的函数 `a->b` 应用在 `r->a` 包裹的 `a` 上, 得到 `r->b` 包裹的 `b`

context: 共享环境 r

设类型 r 的值为 x

最终结果 `r->b` 为 `\x -> (x+3) + (x*100)`

**练习**: `(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5`

lambda 函数类型为 `a -> b -> c -> d`

<$> 第一个函数后 `r -> b -> c -> d`

<*> 第二个函数后 `r -> c -> d`

<*> 第三个函数后 `r -> d` 表达式 `[x+3, x*2, x/2]`

**ZipList**

```haskell
instance Applicative ZipList where
	pure x = ZipList (repeat x)
	ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> fx) fs xs)
```

## Monoid

#### 定义

对类型 m, 有二元函数满足**结合律**和有**幺元(单位元)**

```haskell
class Monoid m where
	mempty :: m
	mappend :: m -> m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty
```

* mempty 即 1
* mappend 即二元函数

#### 定律

幺元 结合律

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

#### Instance

**List**

`++` `[]`

```haskell
instance Monoid [a] where
	mempty = []
	mappend = (++)
```

数字是一种 Monoid

* 函数 `*` 幺元 `1`
* 函数 `+`  幺元  `0`

**Product**

```haskell
newtype Product a =  Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Num a => Monoid (Product a) where
	mempty = Product 1
	Product x `mappend` Product y = Product (x * y)
```

**Sum**

```haskell
newtype Sum a =  Product { getSum :: a } deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Num a => Monoid (Sum a) where  
    mempty = Sum 0  
    Sum x `mappend` Sum y = Sum (x + y)
```

Bool 是一种 Monoid

* 函数 `||`  幺元 `False`
* 函数 `&&`  幺元 `True`

**Any**

```haskell
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Monoid Any where  
    mempty = Any False  
    Any x `mappend` Any y = Any (x || y)
```

**All**

```haskell
newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Monoid All where  
	mempty = All True  
	All x `mappend` All y = All (x && y)
```

**Ordering**

函数: 短路逻辑 幺元 `EQ`

链式优先级比较

```haskell
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT
```

幺元: `EQ` 表示相等, 继续右侧比较

函数: 

* 左侧为 EQ, 继续右侧比较
* 左侧不为 EQ, 左侧为结果, 右侧短路

结合律证明

```haskell
(x <> y) <> z = x <> (y <> z)
```

* 若 x 为 `EQ`, 两边均为 `y <> z`
* 若 x 不为 `EQ`, 两边短路返回 `x`

```haskell
compare (a1, b1, c1) (a2, b2, c2) = 
	compare a1 a2 <> compare b1 b2 <> compare c1 c2
```

**Maybe**

Maybe a 中的 a 必须是 Monoid

函数: Monoid a 的 mappend 函数

幺元: Nothing

```haskell
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

**First**

确定一堆 Maybe 中是否有 Just

```haskell
newtype First a = First { getFirst :: Maybe a } deriving (Eq, Ord, Read, Show)
```

```haskell
instance Monoid (First a) where  
	mempty = First Nothing  
	First (Just x) `mappend` _ = First (Just x)  
	First Nothing `mappend` x = x
```

**Last a** 保留最后一个非 Nothing 值

### Foldable

可 flod

```haskell
import qualified Foldable as F
```

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b

F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
```

foldMap 应用函数对 foldable 中的元素进行应用, 结果为 monoid 类型

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
	foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r
```

理解 `F.foldMap (\x -> Any $ x == 3) testTree`

monoid `Any `

foldable `Tree`

将 testTree 中元素带入 `(\x -> Any $ x == 3)` 得到 `Any True` 或 `Any False` 再用 `mappend` 运算为一个 `Any` 值, 只要树中有 3, 结果就是 `Any True`, 否则就是 `Any False`



