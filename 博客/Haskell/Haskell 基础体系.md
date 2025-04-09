# Haskell 体系

## Package

**Package 集合**

Hackage, Stackage, GHC自带, Haskell Platform(废弃)

* **Package** 分发和依赖单位
   * **Library** 功能单位
      * **Module** 代码组织单位

```shell
my-package/
  ├── MyPackage.cabal    # 包配置文件
  ├── src/
  │   ├── Utils.hs       # 模块1: MyPackage.Utils
  │   └── Main.hs        # 模块2: MyPackage.Main
  └── test/              # 测试模块
```



## 程序结构

* module 模块

   * declaration 声明

      * expression 表达式

         * lexical structure 词法结构

            关键字, 运算符, 字面量, 缩进规则, 命名规则

![程序结构](../image/%E7%A8%8B%E5%BA%8F%E7%BB%93%E6%9E%84.png)

### 模块

**导入模块**

`import [qualified] <模块名> [as <别名>] [(<函数/类型列表>)] [hiding (<排除项列表>)]`

```haskell
import Data.List 
import Data.List (f, g) 
import Data.List hiding (f)
import qualified Data.List 
import qualified Data.List as L 
```

**自定义模块**

```haskell
module MyModule
(	name1
	name2
) where
```

**模块导出**

本质: 导出名称, 选择性导出已定义名称

* 函数

* 类型

   * Type 类型
   * Type(..) 类型及全部值构造子
   * Type(Constructor1, Constructor2) 类型及指定值构造子

* 类型类

* 模块导出

* 别名

   

### 基础概念

side effect 改变函数外状态

referential transparency引用透明: 同参数多次带入函数, 结果相同

惰性计算 lazy: 真正需要时才计算

* 无限 List 参与计算

## 值 类型  名称 绑定  声明 表达式

$值 \xleftarrow{标签} 类型 \xleftarrow{标签} kind$ 

$名称 \xleftarrow{绑定} 表达式$

$haskell 程序 = 声明 + 表达式$

$~$![值 类型  名称 绑定  声明 表达式](../image/%E5%80%BC%20%E7%B1%BB%E5%9E%8B%20%20%E5%90%8D%E7%A7%B0%20%E7%BB%91%E5%AE%9A%20%20%E5%A3%B0%E6%98%8E%20%E8%A1%A8%E8%BE%BE%E5%BC%8F.png)

### 值系统

#### 值构造子

值构造子: 值层构造函数, **输入值, 返回值**

* Record 自动生成取值函数
* 前值构造子小于后值构造子

#### 函数

函数是一等公民, 函数名是个值

$~$

### 类型系统

#### 类型定义

**类型定义data**

> Algebraic Data Type

```haskell
data TypeName = Value Constructor | ... [deriving (TypeClass)]
```

* 类型构造子类型参数都为 TypeClass 的 instance
* 不要在 data 声明中加类型约束

**类型定义 newtype**

单参数单值构造子

```haskell
newtype TypeName = Value Constructor { getType} [deriving (TypeClass)]
```

* 模式匹配时直接确定要匹配的值构造子, 跳过可能的 undefined 传入值(配合惰性计算)

**类型别名 type**

* 类型别名只用在类型系统

**类型相关**

* 类型签名 type signature `::`

* 类型约束 type constraint `=>`

* 多态 polymorphism
   * `a -> [a]`
   * `Nothing` 类型 `Maybe a`


#### 类型构造子

类型构造子: 类型层构造函数, **输入类型, 返回类型**

* 零参类型构造子 Int

* 多参类型构造子 Maybe Either

   * **`->`** **两参中缀类型构造子**  `infixr -1 ->` 

* **Curry**

   类型构造子可部分应用类型参数

#### 类型类

函数接口定义行为, 所属类型重载实现

* 最小完整定义: 最小实现函数数量

```
class C c where
	f :: 
instance C T where
	f = 
```

#### Kind

类型的标签

零参类型构造子 kind:  `*` 

有参类型构造子 kind:  `* -> * -> ..` 

$~$

### 名称

* 值
   * **变量**
   * **值构造子**
* 类型
   * **类型变量**
   * **类型构造子**
   * **类型类**
* 模块
   * **模块**

**限制**:

* **变量 类型变量** 小写字母/_开头, 其余大写字母开头

* 同作用域**类型构造子和类型类不能重名**

$~$

### 绑定

名称与表达式永久关联

* **普通绑定**
   * let 局部绑定
   * where 局部绑定
* **Monad 绑定**
   * Monad 绑定 `>>=`
   * do 绑定 `<-`
* **模式匹配绑定**
   * 函数模式匹配
   * case 表达式

#### 模式匹配

数学: **值的结构**分情况讨论

匹配**值构造子**并对输入值解构

**递归值构造子**通过**递归模式匹配**逐层解析

`xs@pattern`  as 模式对整体的引用

$~$

### 声明

* 模块/导入声明
* 类型声明(类型系统)
* 绑定声明(值系统)

$~$

### 表达式

* **名称**
* **值构造子**
* **if case let do**
   * if-then-else 脱糖 case
   * let bind in exp 脱糖 lambda
   * case var of p -> exp
   * do 脱糖 `>>=`
* **Lambda**
* **List 生成**
   * list comprehension 脱糖 `>>=`
   * range 脱糖 `enumFromTo`

