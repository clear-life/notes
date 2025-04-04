## Functor Applicative Functor 与 Monoid

### Functor

Functor: **纯函数**应用在 **context 包裹的值**, 得到**新 context **, 保持**context 语义不变**

**本质: 函数 f 应用在 context1 中的值1, 维护 context 语义不变, 得到 context2 中的值2**

functor(值系统) 看作输出具有 context 的值

* `Just 3` 输出3, 带有可能无值的 context
* `[1, 2, 3]` 输出 1, 2, 3, 带有可能多个或零个值的 context
* `(+3)` 输出 x + 3, 带有依赖参数的 context

#### Functor fmap

**fmap: 函数 f 应用在 context1 中的值1, 维护 context 语义不变, 得到 context2 中的值2**

```haskell
Functor :: (* -> *) -> Constraint
f :: * -> * 
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

* 类型构造子 f 包裹 pure value

* 纯函数与 context 分离: fmap 把纯函数 lift 到 functor 上, 对functor 里的 pure value 应用纯函数, 保持 context 语义不变生成新 functor

   > lift: 将函数"提升"到另一个 context 中 

**<$>** 中缀版 fmap

**pure f <*> x**  $\Leftrightarrow$  **fmap f x**  $\Leftrightarrow$  **f <\$> x**

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x
```

#### Instance

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

#### Functor laws

**恒等律**

**fmap id = id**

fmap 恒等函数 = 恒等函数

**组合律**

**fmap (f . g) = fmap f . fmap g**

fmap 组合函数 = fmap 函数后组合

### Applicative functor

Applicative functor: 包裹在 context 中的纯函数, 应用到包裹在 context 中的值, 生成新的 context

* 应用 context 中的函数
* 支持多参函数
* **本质: context1 中的值1(函数), 与 context2 中的值2 作用, 维护 context 语义不变, 生成 context3 中的值3**

#### 定义

`module Control.Applicative`

```haskell
class (Functor f) => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```

**pure**: 将值包裹进 context f 中

**<*>**: 从 context1 中取出值1(函数 `a->b`), 应用在 context2 中的值2, 返回 context3 中的值3

* <*> 加强版 fmap, 读作 apply



#### Instance

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

理解 `(+) <$> (+3) <*> (*100)`

`(+) <$> (+3)`  将 + 应用在 `r->a` 包裹的 `a` 上, 得到 `r->a->b` `\x -> (+) (x + 3)` 

`*100 :: r->a`

`(+) <$> (+3) <*> (*100)` 将 `r->a->b` 包裹的函数 `a->b` 应用在 `r->a` 包裹的 `a` 上, 得到 `r->b` 

context: 共享环境 r

设类型 r 的值为 x

最终结果 `r->b` 为 `\x -> (x+3) + (x*100)`



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

## Monad

### 引进

#### 需求

Functor 可被 map over 类型, 带有 context(包裹部分)

```haskell
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

Applicative Functor 函数带有 context 的 Functor

```haskell
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
```

Monad 加强版的 Applicative Functor

`Maybe a` 表示可能会失败的 computation

`[a]` 表示非确定性 computation

`IO a` 表示有 side-effect 的 computation

需求: 对于具有 context 的值 `m a`, 将其带入 `a -> m b` 函数

monad 是支持 `>>=` 的 applicative functor

`>>=` 称为绑定

```haskell
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

#### Maybe Monad

Maybe 实现 `>>=` `applyMaybe`

```haskell
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x
```

### Monad typeclass

#### 定义

```haskell
class Applicative m => Monad m where  
    return :: a -> m a  

    (>>=) :: m a -> (a -> m b) -> m b  

    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  

    fail :: String -> m a  
    fail msg = error msg
```

* `return` 将普通值包裹进 monad 中, 等价于 `pure`
* bind: `>>=` 接收 monadiv value(单子值), 喂给接收普通值的函数, 返回 monad value

#### Maybe instance

```haskell
instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing
```

#### Maybe 应用

```haskell
type Birds = Int  
type Pole = (Birds,Birds)
```



```haskell
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  

landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing
```



```haskell
ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
Just (2,4)
```



```haskell
ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1  
Nothing
```

### do 表示法

#### 定义

* 表达式

* `do` 是 `>>=`  的语法糖, 将 `.. >>= .. >>= ..` 拆分为 `do .. <- ..  .. <- ..`
* `do` 中每行都是一个单子值
* 串行, 每一步的值都依赖前一步的结果, 并带着 context 继续下去
* `do` 中最后一个值不能用 `<-` 绑定
* `do` 中若一行运算没有用到 `<-` 绑定值, 则实际上使用了 `>>`

重写走钢丝

```haskell
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    landLeft 1 second
```

#### 模式匹配

`<-` 绑定使用模式匹配

模式匹配失败调用 `fail` 函数

默认实现

```haskell
fail :: (Monad m) => String -> m a  
fail msg = error msg
```

Maybe 实现

```haskell
fail _ = Nothing
```

### List Monad

#### 定义

```haskell
instance Monad [] where
	return x = [x]
	xs >>= f = concat (map f xs)
	fail _ = []
```

#### 不确定性

```haskell
ghci> [3,4,5] >>= \x -> [x,-x]  
[3,-3,4,-4,5,-5]

ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

do 重写

```haskell
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)
```

#### list comprehension

```haskell
ghci> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

**filter**

```haskell
ghci> [ x | x <- [1..50], '7' `elem` show x ]  
[7,17,27,37,47]
```

**guard** 函数 和 **MonadPlus** typeclass (即是monad 又是 monoid)实现 filter

```haskell
class Monad m => MonadPlus m where
	mzero :: m a
	mplus :: m a -> m a -> m a
```

mzero 对应 mempty, mplus 对应 mappend

```haskell
instance MonadPlus [] where  
    mzero = []  
    mplus = (++)
```

mzero 表示不产生结果的非确定性值, 即失败结果

mplus 将两个非确定性值结合为一个

```haskell
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero
```

```haskell
ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)  
[7,17,27,37,47]
```

```haskell
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x
```

#### 不确定性问题

```haskell
type KnightPos = (Int,Int)
```

```haskell
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
                ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')
```

```haskell
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]
```

```haskell
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second
```

```haskell
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
```

### 单子律

#### 同一律

**单位元 return**

**Left identity** 

`return x >>= f` $\Leftrightarrow$ `f x` 

**Right identity**

`m >>= return` $\Leftrightarrow$ `m` 

#### 结合律

`(m >>= f) >>= g`  $\Leftrightarrow$  `m >>= (\x -> f x >>= g)` 

#### 单子函数组合

**函数组合**

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)  
f . g = (\x -> f (g x))
```

**单子函数组合**

`g :: a -> m b`

`f :: b -> m c`

定义 `<=<`

```haskell
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)
```

单子律

`f <=< return` $\Leftrightarrow$ `f`

`return <=< f` $\Leftrightarrow$ `f`

`(f <=< g) <=< h` $\Leftrightarrow$ `f <=< (g <=< h)`

普通函数

`f . id` $\Leftrightarrow$ `f`

`id . f` $\Leftrightarrow$ `f`

`(f . g) . h` $\Leftrightarrow$ `f . (g . h)`

## Monad

Package 是分发和依赖的单位, 控制模块可见性

Module 是代码组织单位, 控制名称(函数, 类型)可见性

```shell
my-package/
  ├── MyPackage.cabal    # 包配置文件
  ├── src/
  │   ├── Utils.hs       # 模块1: MyPackage.Utils
  │   └── Main.hs        # 模块2: MyPackage.Main
  └── test/              # 测试模块
```

### Writer Monad

#### 引入

context: 附加值(log)

`applyLog`

```haskell
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)
```

#### Monoid

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
import Control.Monad.Writer  

logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)
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



