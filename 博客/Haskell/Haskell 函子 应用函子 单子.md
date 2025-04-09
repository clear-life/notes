# Haskell 函子 应用函子 单子.md

## Functor Applicative Monad

### Context

#### 包裹

context: 通过类型构造子为值附加计算逻辑, 将值包裹在上下文(情景/情况)中 

Maybe 可能存在可能不存在

IO 输入输出

[] 非确定性

Either 可能失败 携带错误信息

#### 必要性

保持纯函数的同时处理现实问题, 通过类型系统将现实问题的处理隔离在特定的上下文中

#### 操作值

`<-` 提取值

`fmap` `<$>` `<*>` `>>=` `>>` 维护 context

### Functor

可被 map over

类型构造子 f

f a 盒子f 包裹普通类型 a

f b 盒子 f 包裹普通类型 b

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```



**instance**

```haskell
instance Functor [] where
    fmap = map
```

```haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

```haskell
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

### Monad


