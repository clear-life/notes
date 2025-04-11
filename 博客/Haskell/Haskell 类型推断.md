## Haskell 类型推断

## 理论

### 高阶类型变量

|       术语       |    kind    |   示例    |
| :--------------: | :--------: | :-------: |
|   **类型变量**   |  **`*`**   |   a, b    |
| **高阶类型变量** | **`*->*`** | Functor f |

### Ambiguous Type

**Ambiguous Type 错误**: type variable 无法通过 context 推断出**具体类型**, 且**未被约束**

例: 高阶类型变量

类型变量 f 确定具体类型, 类型变量 a 虽然未推断出具体类型, 但有类型约束, 相应函数(fmap 和 id)满足约束条件

```haskell
fmap :: Functor => (a->b) -> f a -> f b
fmap :: (Functor f, Num a) => (a->a) -> [] a -> [] a
ghci> fmap id [1,2,3]
[1,2,3] :: Num a => a
```

**类型推断**

fmap 类型 `Functor f => (a->b) -> f a -> f b`

id 类型 `c -> c`, 与 `a->b` 统一, 则 `a = b = c`

[1, 2, 3] 类型 `Num d => [d]`, 与 `f a` 统一, 则 `f = [], a = d`

fmap 类型 `Num a => (a->a) -> [] a -> [] a`

**最终多态表达式 `fmap id [1,2,3]` 类型 `Num a => [a]`, 正确** 

**fmap id**

`fmap id` 类型为 `Functor f => f a -> f a`

**类型变量 f 不是具体类型, 也无类型约束, 即 Ambiguous Type 错误**

## 案例

### ghci> fmap id == id

```haskell
fmap :: Functor f => (a->b) -> f a -> f b
```

**错误1 Ambiguous Type variable 未明确的类型变量**

> **Ambiguous type variable ‘f0’ arising from a use of  ‘fmap’** prevents the constraint ‘(Functor f0)’ from being solved.
>
> In the first argument of ‘(==)’, namely ‘**fmap id**’
>
> In the expression: fmap id == id
>
> In an equation for ‘it’: it = fmap id == id

`fmap id` 表达式不能明确 `fmap :: Functor f => (a->b) -> f a -> f b` 中**高阶类型变量 f** (错误信息中的 f0)的具体类型

**fmap id 类型推断**

fmap 类型 `Functor f => (a->b) -> f a -> f b`

id 类型 `c -> c`

统一类型, `a = b = c`

fmap id 类型为 `Functor f => f a -> f a`

**类型变量 f 为 ambiguous type, 错误**

**解决方案**: 类型标注, 明确 f 类型, 消除类型歧义

$~$

**错误2 Eq 类型类无函数 instance**

> **No instance for (Eq (f0 b0 -> f0 b0)) arising from a use of ‘==’**
>
> In the expression: fmap id == id
>
> In an equation for ‘it’: it = fmap id == id

`fmap id == id` 表达式两侧都是函数, 类型推断为 `f0 b0 -> f0 b0`, 其 不是 Eq 类型类的 instance, 不能使用 `==`

**== 类型推断**

fmap id 类型 `Functor f => f a -> f a`

id 类型 `b->b`, 统一类型, 则 `b = f a`

== 类型 `Eq c => c -> c -> Bool`

统一类型 `c = f a -> f a`, 则 `f a -> f a` (错误信息中的 f0 b0 -> f0 b0)不是 Eq 类型类 instance, **错误**

**解决方案**: 函数类型不进行比较