# std::memory_order

## 术语

### 中英术语对照表

|    中文    |         英文          |
| :--------: | :-------------------: |
|    访问    |        access         |
|  内存模型  |     memory model      |
|  内存位置  |    memory location    |
|  内存次序  |     memory order      |
|  内存访问  |     memory access     |
|  内存次序  |     memory_order      |
|   运算符   |       operator        |
|   操作数   |        operand        |
|   值类别   |    value category     |
|   字面量   |        literal        |
|   表达式   |      expression       |
| 初等表达式 |  primary expression   |
| 表达式求值 | expression evaluation |
|  求值顺序  |  order of evaluation  |
| 按顺序早于 |   sequenced before    |
|   值计算   |   value computation   |
|   副作用   |      side-effect      |
| 未定义行为 |  undefined behavior   |
|            |                       |
|            |                       |
|            |                       |
|            |                       |

### 访问

**access**: **read** 和 **write**

## 内存模型

**memory model**: 为 C++ 抽象机定义了**计算机内存存储**的语义

C++ 程序使用的内存是**连续字节序列**, 内存中的**每个字节**都有**唯一地址**

### 内存位置

**memory location**:

* 标量类型对象
* 非零位域的最大连续序列

```C++
struct S
{
    char a;		// 内存位置 #1
    int b : 5;	// 内存位置 #2
    int c : 11;	// 内存位置 #2
    	  : 0;	
    	d : 8;	// 内存位置 #3
    struct
    {
        int ee : 8;	// 内存位置 #4
    }e;
} obj;	// 对象 obj 由 4 个分离的内存位置组成
```

### 线程与数据竞争

线程的执行是程序中的**控制流**, 始于 `std::thread::thread`, `std::async` 或其他方式 调用的**顶层函数**

同一内存位置, 当表达式 A **write**, 表达式 B **read** 或 **modify**, 就称表达式 A 和 B **冲突**

存在表达式冲突的程序就有**数据竞争**, 除非满足任一条件:

* A 和 B 在同一线程内, 或在同一信号处理函数中
* A 和 B 是原子操作
* A **happen-before** B 或 **B happen-before** A

若出现数据竞争, 则行为未定义:

```C++
int x = 0;
auto f = [&]{x++;};
thread t1(f), t2(f), t3(f);	// 未定义行为
```

```C++
atomic<int> x = 0;
auto f = [&]{x++;};
thread t1(f), t2(f), t3(f);	// 正常行为
```

### 内存访问

**memory access**: 

* 从 memory **read**
* 向 memory **write**

> memory 通常指内存, 即**内存条**

> **存储器分类**:
>
> * 只读存储器 ROM
>   * BIOS
> * 随机存储器 RAM
>   * 内存条
> * 缓冲存储器 cache
>   * 三级缓存

## 求值顺序

### 运算符

>理解: 所有的运算符都看作特定的函数, 即**把常见的操作固化为特定的符号**
>
>输入为**操作数**, 输出为 **value** 和 **side-effect**
>
>如 a + b 中的 + 运算符, 就是把函数 
>
>```C++
>T operator+(T a, T b)
>{
>    return a + b;
>}
>```
>
>固化为符号 +

**operator**:

*  常见

  * 赋值, 算术, 逻辑, 比较, 位运算

    > a = b, a + b, a && b, a == b, a & b

* 非常见

  * 成员访问

    > a[b], *a, a.b, a->b

  * 函数调用

    > func(...)

  * 逗号

    > a, b

* 特殊

  * 类型转换运算符

    > static_cast

  * new 和 delete

### 操作数

**operand**

运算符的操作数: 

* **子表达式**
* **初等表达式**

> 表达式 1 + 2 * 3 的操作数为**子表达式** 2 * 3 和**初等表达式** 1

### 值类别

**value category**

每个**表达式**都可以按**类型**和**值类别**区分

**基本类别**:

> primary category

* **lvalue** 左值
* **prvalue** 纯右值
* **xvalue** 亡值

### 字面量

**literal**: C++ 程序中的 **token**, 表示源代码中的常量值

字面量:

* 整数字面量

* 浮点字面量

* 布尔字面量

* 字符字面量

* 字符串字面量

  > const char[], 本质是个指针

* nullptr 字面量

* 自定义字面量

### 表达式

**expression**: 表达式是**运算符**及其**操作数**的序列, 指定了一个计算

* 实参和子表达式的**求值顺序**, 指定了得到中间结果的顺序

### 初等表达式

**primary expression**: **表达式**的**"原子"**单位

初等表达式:

* this

* 字面量

  > 如 2 或 "abc"

* 标识符 identifier

* lambda 表达式

* **`()` 内的表达式**

  > 这保证了 `()` 比任何运算符优先级高

### 表达式求值

**expression evaluation**: 执行表达式操作(看作一个函数)的过程

**evaluation**:

* **value computation** 值计算: 计算表达式返回值
* **side-effect** 副作用 的启动:
  * 访问 **volatile glvalue** 对象
  * write 对象
  * 调用库 I/O 函数
  * 其他类似函数

### 求值顺序  

**order of evaluation**: 表达式的任意部分的求值顺序都是 **unspecified** (下面的规则例外), 编译器能以任意顺序计算**操作数**和**子表达式**

$~$

C++ 没有**从左到右**或**从右到左**求值的概念, 这不与**结合性的概念**混淆:

表达式 `a() + b() + c()` 根据 `operator+` 的结合性被分析为 `(a() + b()) + c()`

但在运行时, `a()`,  `b()` , `c()` 的求值顺序是任意的

```C++
int a()
{
    return puts("a");
}

int b()
{
    return puts("b");
}

int c()
{
    return puts("c");
}

void z(int, int, int) {}

int main()
{
    z(a(), b(), c());	
}
```

结果一共有 6 种情况

$~$

**表达式求值的顺序**

**sequenced before** 是线程内两个**求值**间的**非对称性**, **传递性**的**配对关系**

* 若 **A sequence before B**
  * 则**求值 A 会在求值 B 开始前完成**
* 若 **A not sequenced before B** 且 **B not sequenced before A**
  * **求值 A** 和**求值 B** 为 **unsequenced**: A 和 B 的指令可以任意混杂
  * **求值 A** 和**求值 B** 为 **indeterminately sequenced**:
    * 要么 **A sequence before B**
    * 要么 **B sequence before A**

### 按顺序早于

**sequenced before**

1. **完整表达式**的**值计算**和**副作用**都 **sequenced before** **下一完整表达式**的**值计算**和**副作用**

2. **运算符操作数**的**值计算** **sequenced before** **运算符结果**的**值计算**

3. 函数调用中, **参数表达式**和**指定被调函数的后缀表达式**的**值计算**和**副作用**都 **sequenced before** 被调函数体中的**表达式和语句**的**执行**

4. 内置 `x++` 和 `x--` 运算符的**值计算** **sequenced before** 它的**副作用**

5. 内置 `++x` 和 `--x` 运算符的**副作用** **sequenced before** 它的**值计算**

6. 内置 `&&` 和 `||` 运算符**左操作数**的**值计算**和**副作用**都 **sequenced before** **右操作数**的**值计算**和**副作用**都 **sequenced before** 

7. `?:` 运算符第一表达式的**值计算**和**副作用**都 **sequenced before** 第二或第三表达式的**值计算**和**副作用**

8. 内置赋值运算符和内置复合赋值运算符中, 左右参数的**值计算**都 **sequenced before** 运算符的**副作用**, 运算符的**副作用** **sequenced before** 运算符的**值计算**

9. 内置 `,` 运算符第一参数的**值计算**和**副作用**都 **sequenced before** 第二参数的**值计算**和**副作用**

10. 列表初始化中大括号内用 `,` 分隔的初始化子句, **逗号运算符前**初始化子句的**值计算**和**副作用**都 **sequenced before** **逗号运算符后**初始化子句的**值计算**和**副作用**

11. 若函数调用 A 与函数外另一表达式求值 B 的关系为: **A not sequenced before B** 且 **B not sequenced before A**

    则 A 和 B 的关系为 **indeterminately sequenced**

    > `std::execution::par_unseq` 策略下的标准库算法中, 函数调用的关系是 **unsequenced**(C++17起)

12. **allocation** 函数调用 **indeterminately sequenced**(C++17前) **sequenced before**(C++17起) **new** 表达式中**构造函数参数**的**求值**(evaluation)

13. 函数返回时, 作为**函数调用求值**结果的**临时变量**的**拷贝初始化** **sequenced before** **return 语句**操作数的**最后时刻**对**所有临时变量**的**销毁**

14. 在**函数调用表达式**中, **命名该函数**的表达式 **sequenced before** **所有参数表达式和默认参数**

15. 函数调用中, 每个**参数初始化**的**值计算**和**副作用**与**任何其他参数初始化**的**值计算**和**副作用**都是 **indeterminately sequenced**

16. 用运算符写法进行调用时, **重载运算符**会遵循其重载的**内置运算符**的**排序规则**

17. 下标表达式 `E1[E2]` 中,  `E1` 的**值计算**和**副作用**都 **sequenced before** `E2` 的**值计算**和**副作用**

18. 成员指针表达式 `E1.*E2` 和 `E1->*E2` 中, `E1` 的**值计算**和**副作用**都 **sequenced before** `E2` 的**值计算**和**副作用**

    > 除非 E1 的动态类型不包含 E2 指向的成员

19. 移位运算符表达式 `E1 << E2` 和 `E1 >> E2`, `E1` 的**值计算**和**副作用**都 **sequenced before** `E2` 的**值计算**和**副作用**
20. 简单赋值表达式 `E1 = E2` 和简单复合赋值表达式 `E1 @= E2` 中, `E2` 的**值计算**和**副作用**都 **sequenced before** `E1` 的**值计算**和**副作用**
21. **括号初始化器**中, **表达式列表中以逗号分隔的每个表达式**, 像函数调用一样求值(**indeterminately sequenced**)

### 未定义行为

**undefined behavior(UB)**: 对程序的行为没有任何限制, 即**任何情况都可能发生**

1. 同一**内存位置**上, **副作用 A** 与**副作用 B** **unsequenced**, 则行为未定义

   ```C++
   i = ++i;	// 正确行为, 规则 5 和 8
   
   ++i 值计算		a
   ++i 副作用		b
   i = ++i 值计算	c
   i = ++i 副作用 d
       
   ++i 中, 副作用 sequenced before 值计算, 即 b < a
   i = ++i 中, 左右参数(++i)的值计算 sequenced before  i = ++i 的副作用, 即 a < d
   i = ++i 中, 副作用 sequenced before 值计算, 即 d < c
   根据传递性, b < a < d < c
   ```

   ```C++
   i = 0;
   n = ++i + i++;	// 未定义行为
   
   ++i 值计算		a
   ++i 副作用		b
   i++ 值计算		c
   i++ 副作用		d
   
   ++i 	b < a
   i++		c < d
   但 b 和 d unsequenced
   即  ++i 副作用  与  i++ 副作用  unsequenced, 
   结果可能为 i == 1 和 i == 2, 于是行为未定义
   ```

   ```C++
   i = ++i + 2;	// 正确行为
   
   ++i 值计算			a
   ++i 副作用			b
   i = ++i + 1	值计算	c
   i = ++i + 1	副作用	d
   
   ++i, b < a
   i = ++i + 1, d < c, 且 a < d
   所以 b < a < d < c
       
       
   i = 0;    
   i = i++ + 2;	// 未定义行为
   
   i++ 值计算			a
   i++ 副作用			b
   i = i++ + 1	值计算	c
   i = i++ + 1	副作用	d
       
   i++, a < b
   i = i++ + 1, d < c, 且 a < d
   但 b 和 d 的关系 unsequenced
   于是 i 上的两个副作用 unsequenced, 结果可能为 i == 1 和 i == 2, 结果未定义
   ```

2. 同一**内存位置**上, **副作用 A** 与使用该内存位置的任何对象的**值计算 B** **unsequenced**, 则行为未定义

   ```C++
   i = 0;
   x = ++i + i;	// 未定义行为
   
   ++i 值计算		a
   ++i 副作用 	b
   i 值计算		c
   
   ++i 中, 副作用 sequenced before 值计算, 即 b < a
   然而 a 和 c, b 和 c 的关系却是 unsequenced, 即编译器可以任意重排指令
       
   副作用 b 和值计算 c 作用于同一内存位置, 但却是 unsequenced
   结果有两种情况 c 计算的值为: 0 或 1, 于是行为未定义
   ```


## 内存次序

### memory_order

```C++
#include <atomic>

// C++11
enum memory_order 
{
    memory_order_relaxed,
    memory_order_consume,
    memory_order_acquire,
    memory_order_release,
    memory_order_acq_rel,
    memory_order_seq_cst
};

// C++20
enum class memory_order
{
    relaxed,
    consume,
    acquire,
    release,
    acq_rel,
    seq_cst
};
```

`std::memory_order` 指定**内存访问**(包括**非原子内存访问**)**在原子操作上的次序**

**定义**:

线程间**同步**和**内存次序**决定了**表达式**的 **evaluation** 和 **side-effect**

