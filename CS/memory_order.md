# std::memory_order

## 术语

### 中英术语对照表

|    中文    |           英文            |
| :--------: | :-----------------------: |
|    访问    |          access           |
|  内存模型  |       memory model        |
|  内存位置  |      memory location      |
|  内存次序  |       memory order        |
|  内存访问  |       memory access       |
|  内存次序  |       memory_order        |
|   运算符   |         operator          |
|   操作数   |          operand          |
|   值类别   |      value category       |
|   字面量   |          literal          |
|   表达式   |        expression         |
| 初等表达式 |    primary expression     |
| 表达式求值 |   expression evaluation   |
|  求值顺序  |    order of evaluation    |
|   先序于   |     sequenced before      |
|   值计算   |     value computation     |
|   副作用   |        side-effect        |
| 未定义行为 |    undefined behavior     |
|   值计算   |     value computation     |
|   副作用   |        side-effect        |
|  携带依赖  |     carry dependency      |
|  改动序列  |    modification order     |
|  先发生于  |       happen-before       |
|  释放序列  |     release sequence      |
| 依赖先序于 | dependency-ordered before |
|    同步    |        synchronize        |
|  先发生于  |       happen-before       |
| 连贯先序于 | coherence-ordered-before  |
|            |                           |

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

## 表达式求值顺序

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

### 先序于

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


# 内存次序

## 相关术语

### memory_order

`std::memory_order` 指定**内存访问**(包括**非原子内存访问**)如何**围绕原子操作排序**

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

**描述**:

线程间**同步**和**内存次序**决定了不同执行线程**表达式**的**求值**和**副作用**

### 先序于

**sequenced before**

线程内, 求值 A 于求值 B 可能存在**先序于**的关系, 见**表达式求值顺序**

### 携带依赖

**carry dependency**: 

线程内, 求值 A **先序于** 求值 B, 若下列任一条件满足, 则 **B 依赖于 A**:

* A 的值作为 B 的操作数, 除非
  * B 调用 `std::kill_dependency`
  * A 是内置 `&&`, `||`, `?:`, `,` 的左操作数
* A 写入标量对象 M, B 从 M 读
* B 依赖于 X,  X 依赖于 A

### 改动序列

**modification order**: 在原子变量上的**修改操作的全序序列**, 称为**该原子变量的改动序列**

所有原子操作满足:

1. **写写一致性**: 原子对象 M 上的**求值 A 和求值 B**(**写写**), 若 **A 先发生于 B**, 则在 M 的改动序列中 **A 比 B 早**
2. **读读一致性**: 原子对象 M 上的**值计算 A 和值计算 B**(**读读**), 若 **A 先发生于 B**, A 读到 M 的值来自写操作 X, 则 B 读到 M 的值来自 **M 改动序列中 X 及之后的部分**
3. **读写一致性**: 原子对象 M 上的**值计算 A 和操作 B**(**读写**), 若 **A 先发生于 B**, 则 A 读到 M 的值来自 **M 改动序列中 B 之前的部分**
4. **写读一致性**: 源自对象 M 上的**副作用 A 和值计算 B**(**写读**), 若 **A 先发生于 B**, 则 B 读到 M 的值来自 **M 改动序列中 A 及之后的部分**

### 释放序列

**release sequence**: 原子对象 M 上的 `release` 操作 A 执行后, M 的改动序列中包含下列内容的最长连续子序列:

* A 所在线程的写操作
* 任何线程对 M 的原子**读-改-写**操作

被称为以 **A 为开头的释放序列**

### 同步

**synchronize with**

线程 1 的**原子 `release` store** 操作 A, 线程 2 的**原子 `acquire` load** 操作 B, 若 B 读自 A 存入的值, 则 **A 同步于 B**

### 依赖先序于

**dependency-ordered before**

线程间, 若下列任一为真, 则 **A 依赖先序于 B**:

* 原子对象 M, 线程 1 的求值 A 是个 `release` 操作, 线程 2 的求值 B 是个 `consume` 操作, 且 B 读到的值来自 A
* A 依赖先序于 X 且 X 携带依赖给 B (即 B 依赖于 X)

### 线程间先发生于

**inter-thread happen-before**

线程间, 若下列任一为真, 则 **求值 A 线程间先发生于求值 B**:

* A 同步于 B
* A 同步于 X 且 X 先序于 B
* A 依赖先序于 B
* A 先序于 X 且 X 线程间先发生于 B
* A 线程间先发生于 X 且 X 线程间先发生于 B

### 先发生于

**happen-before**

无论线程, 若下列任一为真, 则 **求值 A 先发生于求值 B**:

* A 先序于 B
* A 线程间先发生于 B

### 简单先发生于

**simply happen-before**

无论线程, 若下列任一为真, 则 **求值 A 简单先发生于求值 B**:

* A 先序于 B
* A 同步于 B
* A 简单先发生于 X 且 X 简单先发生于 B

> 没有 `consume` 的情况下, 简单先发生于 与 先发生于 是相同的

### 强先发生于

**strongly happen-before**

无论线程, 若下列任一为真, 则 **求值 A 强先发生于求值 B**:

* A 先序于 B
* A 同步于 B 且 A 与 B 均为序列一致原子操作
* A 先序于 X, X 简单先发生于 Y, Y 先序于 B
* A 强先发生于 X 且 X 强先发生于 B


> 强先发生于不包括 `consume` 操作

### 可见副作用

**visible side-effect**

标量 M 上的副作用 A 和 值计算 B, 若下列都为真, 则 A 对 B 是可见的:

1. A 先发生于 B
2. 没有其余 M 的副作用 X 满足 A 先发生于 X 且 X 先发生于 B

### 连贯先序于

**coherence-ordered-before**

原子对象 M 上的原子操作 A 和原子操作 B, 若下列任一为真, 则 **A 连贯先序于 B**:

* A 是修改动作, B 读自 A 存储的值
* 在 M 的改动序列中 A 比 B 早
* A 读自 X 存储的值, X 在改动序列中早于 B, 且 A 和 B 不是同一 **read-modify-write(RMW)** 操作
* A 连贯先序于 X, X 连贯先序于 B

## 基本操作

### `consume ` 操作

带 `memory_order_consume` 或更强 `tag` 的 **atomic load** 是 `consume` 操作

### `acquire` 操作

带 `memory_order_acquire` 或更强 `tag` 的 **atomic load** 是 `acquire` 操作

> mutex 上的 lock() 操作是一个 `acquire` 操作

### `release` 操作

带 `memory_order_release` 或更强 `tag` 的 **atomic store** 是 `release` 操作

> mutex 上的 unlock() 操作是一个 `release` 操作

> `std::atomic_thread_fence` 施加比 `consume`, `acquire`, `release` 更强的同步要求

## 内存次序

### relaxed ordering

**宽松次序**: **带 `memory_order_relaxed` tag 的原子操作不是同步操作**, 并不会为并发的内存访问施加次序约束

只能保证**原子性**和**改动序列的一致性**

例:

```C++
atomic<int> x = 0;
atomic<int> y = 0;

A:
{
	r1 = y.load(memory_order_relaxed);	a1
    x.store(r1);						a2
}

B:
{
	r2 = x.load(memory_order_relaxed);	b1
    y.store(42);						b2
}
```

结果可能为 `r1 == 42 && r2 == 42`

尽管 a1 先序于 a2, b1 先序于 b2

但因为宽松次序只保证**原子性**和**改动序列的一致性**

> 原子性: 操作时原子操作
>
> 改动序列一致性: 在线程 A 和线程 B 看来, x 和 y 的改动序列都相同, 不会出现改动序列不同的情况

所以有可能出现:

* 在 y 的改动序列中, b2 早于 a1
* 在 x 的改动序列中, a2 早于 b1

所以由于**编译器重排指令**或**运行时 b2 完成地更快**, 于是出现 b2 在 b1 之前完成的情况 `b2  a1  a2  b1`

**宽松次序的典型应用是计数器**

```C++
atomic<int> cnt = 0;

cnt.fetch_add(1, memory_order_relaxed);
```

### release-acquire ordering

**定义**: 线程 1 中带 tag `memory_order_release` 的 **atomic store A**, 线程 B 中同一变量上带 tag `memory_order_acquire` 的 **atomic load B**, 若 B 读自 A 写入的值, 则 **A 同步于 B**

* 在线程 1 看来, 先发生于 A 的所有内存写入(**非原子**和 **relaxed 原子**), 在线程 2 中是**可见副作用**

> 前提是 B 返回 release 序列中 A 及之后写入的值

> 同步关系仅在 **release** 和 **acquire** 相同原子变量的线程间建立, 其余线程可能会看到完全不同的内存访问顺序

**互斥锁**是 **release-acquire** 同步的典型例子, 当线程 A 释放锁线程 B 获取锁, 则 A 中临界区的部分(release 之前) 对线程 B 可见(acquire 之后)

```C++
#include <thread>
#include <atomic>
#include <string>
#include <cassert>

using namespace std;

atomic<string*> ptr;
int x = 0;

void producer()
{
	string* p = new string("Hello");
	x = 42;
	ptr.store(p, memory_order_release);
}

void consumer()
{
	string* p2;
	while (!(p2 = ptr.load(memory_order_acquire)));

	assert(*p2 == "Hello");
	assert(x == 42);
}

int main()
{
	thread t1(producer);
	thread t2(consumer);
	t1.join();
	t2.join();
}
```

```C++
#include <thread>
#include <atomic>
#include <cassert>
#include <vector>


using namespace std;
vector<int> x;
atomic<int> flag = { 0 };

void thread_1()
{
    x.push_back(42);
    flag.store(1, memory_order_release);
}

void thread_2()
{
    int expected = 1;
    while (!flag.compare_exchange_strong(expected, 2, memory_order_relaxed)) 
    {
        expected = 1;
    }
}

void thread_3()
{
    while (flag.load(memory_order_acquire) < 2);
    assert(x.at(0) == 42); 
}

int main()
{
    thread a(thread_1);
    thread b(thread_2);
    thread c(thread_3);
    a.join(); b.join(); c.join();
}
```

### release-consume ordering

**定义**: 线程 1 中带 tag `memory_order_release` 的 **atomic store A**, 线程 B 中同一变量上带 tag `memory_order_consume` 的 **atomic load B**, 若 B 读自 A 写入的值, 则 **A 依赖先序于 B**

* 在线程 1 看来, 先发生于 A 的所有内存写入(**非原子**和 **relaxed 原子**), 在**线程 2 中 B 携带依赖的操作**看来是**可见副作用**, 即一旦 B 完成, 则**线程 2 中使用 B 获取值的运算符和函数**能看到 **A 及之前写入的内容**

> 同步关系仅在 **release** 和 **consume** 相同原子变量的线程间建立, 其余线程可能会看到完全不同的内存访问顺序

典型例子: 少写入的数据结构读取, 有指针的发布-订阅模式

```C++
#include <thread>
#include <atomic>
#include <cassert>
#include <string>

using namespace std;

atomic<string*> ptr;
int x;

void producer()
{
    string* p = new string("Hello");
    x = 42;
    ptr.store(p, memory_order_release);
}

void consumer()
{
    string* p2;
    while (!(p2 = ptr.load(memory_order_consume)));
    
    assert(*p2 == "Hello"); // 不会出错: *p2 依赖于 ptr 
    assert(x == 42); // 可能出错也可能不出错: x 不依赖于 ptr
}

int main()
{
    thread t1(producer);
    thread t2(consumer);
    t1.join(); t2.join();
}
```

### sequentially-consistent ordering

**定义**: 带 tag `memory_order_seq_cst` 的 **atomic** 操作不仅有与 **release-acquire ordering** 的同步方式, 而且会将所有带此 tag 的原子操作形成一个**全序序列**

**正式定义**: 包含所有 `memory_order_seq_cst` 操作和栅栏的**全序序列 S**, 满足下列约束:

1. 若 A 和 B 是 `memory_order_seq_cst` 操作, 且 A 强先发生于 B, 则在 S 中 A 前于 B
2. 对象 M 上的原子操作 A 和 B, 若 A 连贯先序于 B:
   1. 若 A 和 B 都是 `memory_order_seq_cst` 操作, 则 S 中 A 前于 B
   2. 若 A 是  `memory_order_seq_cst` 操作, B 先发生于  `memory_order_seq_cst` 栅栏 X, 则 S 中 A 前于 X
   3.  若 `memory_order_seq_cst` 栅栏 X 先发生于 A, B 是  `memory_order_seq_cst` 操作, 则 S 中 X 前于 B
   4. 若 `memory_order_seq_cst` 栅栏 X 先发生于 A,  B 先发生于  `memory_order_seq_cst` 栅栏 Y, 则 S 中 X 前于 Y

正式定义确保了:

* 全序序列与所有原子对象的改动序列一致
* `memory_order_seq_cst` **load** 要么从最后一个 `memory_order_seq_cst` 修改操作获取值, 要么从不先发生于 `memory_order_seq_cst` 修改操作的 `non-memory_order_seq_cst` 修改操作中获取值

> **全序序列**可能与**先发生于**不一致

```C++
#include <thread>
#include <atomic>
#include <cassert>
 
std::atomic<bool> x = {false};
std::atomic<bool> y = {false};
std::atomic<int> z = {0};
 
void write_x()
{
    x.store(true, std::memory_order_seq_cst);
}
 
void write_y()
{
    y.store(true, std::memory_order_seq_cst);
}
 
void read_x_then_y()
{
    while (!x.load(std::memory_order_seq_cst));
    
    if (y.load(std::memory_order_seq_cst))
        ++z;
}
 
void read_y_then_x()
{
    while (!y.load(std::memory_order_seq_cst));
    
    if (x.load(std::memory_order_seq_cst)) 
        ++z;
}
 
int main()
{
    std::thread a(write_x);
    std::thread b(write_y);
    std::thread c(read_x_then_y);
    std::thread d(read_y_then_x);
    a.join(); b.join(); c.join(); d.join();
    assert(z.load() != 0);  	// 不会发生
}
```

