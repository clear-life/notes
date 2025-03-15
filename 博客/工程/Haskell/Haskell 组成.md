# Haskell 声明与表达式

### 值

* 函数名: 函数名与函数体绑定, 是个表达式

### 声明

结构定义

* 函数定义 `f x = 1`
* 类型定义 `data Type = ..`
* 类型标注 `f :: Int -> Int`
* 类型类定义 `class Eq a where (==) :: a -> a-> Bool`
* 实例定义 `instance Eq Bool where x == y = ...`
* 模块定义 `module Main where`

### 表达式

计算过程

* 函数调用

* `let x = 5 in x * 2`

* `case xs of [] -> 0; (x:xs) -> x`

   * `if .. then .. else ..`

      ```
      case .. of
      	True ->
      	False ->
      ```

      