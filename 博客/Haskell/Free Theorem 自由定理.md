# Free Theorem 自由定理

### 理论

#### 参数多态性

**参数多态性**: 多态函数的行为对 every type 一致, 行为不能依赖具体类型

#### 自由定理

**自由定理**: 通过函数类型签名中的**类型变量(every type)**, 可以推导出**函数必须满足的等式或行为约束**

#### Haskell 限制

理想化数学模型中, 自由定理严格成立

但 Haskell 的某些特性会破坏自由定理

* **纯函数假设**

   自由定理基于纯函数假设, 副作用函数将失效

* **Monad**

   纯 Monad (Maybe, Reader) 自由定理完全有效

   副作用 Monad (IO, State) 自由定理部分受限

* **高级类型特性**

   类型类, GADTs(广义代数数据类型)等引入特定类型约束, 部分限制自由定理. `Eq a` 约束赋予类型 `a` 相等性语义

* **seq**

   `seq` 强制求值, 可能使多态函数的行为依赖未定义值 `⊥`

   ```haskell
   f :: a -> a
   f x = x `seq` x  -- 若 x = ⊥，则 f x = ⊥，但这不影响定理的“理想”形式
   ```

* **类型系统扩展**

   某些 GHC 扩展(`Typeable`)允许绕过参数多态性, 破坏自由定理

### 实践

#### `f :: a -> a`

自由定理推导出 **`f = id`**

增: 不能构造 a 类型值

删: 由于需要返回 a 类型值, 本质还是需要构造 a 类型值 

改: 除了直接返回外，没有操作能作用在任意类型上

* `f` 无法构造特定于 `a` 的值, 也不能删除或改变输入值, 只能返回输入值本身

```haskell
f :: a -> a
f = id
```

#### `f :: [a] -> [a]`

自由定理推导出 f 是**列表结构操作**

增: 不能构造 a 类型值, 但能根据已有 a 类型值扩充, 要考虑[]的情

删: 不能根据值本身的信息删除, 只能根据位置信息删除，也就是列表

结构减少

改∶不能根据元素信息改，因为要适配任意类型

* `f` 无法构造特定于 `a` 的值, 只能进行**列表结构操作**

[David Luposchainsk](https://github.com/quchen/articles/blob/master/second_functor_law.md)

```haskell
f :: [a] -> [a]
map g . f  = f . map g
```

#### ` fmap :: (a->b) -> f a -> f b`

自由定理推导出 **`fmap (f . g) = fmap f . fmap g`**

```haskell
fmap :: (a->b) -> f a -> f b
```

由 `fmap :: (a->b) -> f a -> f b`

自由定理表明:

* 若 `f . g = p . q`
* 则 `fmap f . fmap g = fmap p . fmap q`

若函子第一定律 `fmap id = id` 成立

设 `p = id, q = f . g`

则

```haskell
  fmap f . fmap g 
= fmap id . fmap (f . g)
= id . fmap (f . g)
= fmap (f . g)
```

也就是函子第二定律

```haskell
fmap f . fmap g = fmap (f . g)
```

[David Luposchainsk](https://github.com/quchen/articles/blob/master/second_functor_law.md)

[School of Haskell](https://www.schoolofhaskell.com/user/edwardk/snippets/fmap)
