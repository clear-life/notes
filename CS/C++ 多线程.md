# 通用

### RAII

RAII(**R**esource **A**cquisition **I**s **I**nitialization) 资源获取即初始化

**局部对象管理资源**: 局部对象指**栈中对象**, 资源即操作系统**有限的资源**如内存, 网络套接字

**利用局部对象自动销毁的特性防止忘记释放资源**: 

1. 获取: **构造函数获取**
2. 使用
3. 销毁: **析构函数释放**

### 移动操作

资源的所有权**转移**

* 数据从一个对象转移到另一个对象内, **源对象被"搬空"**
* 若源对象是右值, 移动操作会自动发生
* 若源对象是左值, 则必须通过 `std::move()` 转化为右值后转移

> 例:
> unique_ptr



# 并发

### 并发概念

**并发**: 多个活动同时独立运行

上下文切换: 保存 CPU 状态和 PC(程序计数器)

**并发方式**: 多进程和多线程

**并发作用**: **分离关注点**和**性能提升**

* 分离关注点(业务分离): 合并相关代码, 隔离无关代码

  > 一个线程负责用户界面管理, 另一个负责播放视频

* 性能提升: 多任务并行运行

  > 单个任务本身并没有提升性能

**增强性能的并发方式**:

1. **单一任务分解为多个部分**并行执行
2. 数据并行, 即**单一算法对多份数据**并行执行

**何时不用并发**: 

1. **代码更难理解**, 编写维护成本高, 容易出错
2. **性能提升不大**
3. **线程的资源**(内核资源和栈空间)消耗
4. **上下文切换**的消耗

### Hello Concurrent World

```C++
#include <iostream>
#include <thread>

void fun()
{
    std::cout << "Hello Concurrent World" << std::endl;
}

int main()
{
    std::thread t(fun);
    t.join();
}
```

**起始函数**: 每个线程从起始函数开始执行

> 主线程的起始函数是 main 函数
>
> main 函数结束后, 整个进程结束, 未执行完的线程直接结束 

**`std::thread t(fun)`** : 构造 thread 对象 t 并关联线程, 起始函数为 `fun`, `fun` 应为**可调用对象**, 线程开始执行

**join**: 线程与主线程**汇合**

# 线程管控

### main 线程

由 C++ 运行时系统启动

### 线程的启动与结束

线程开始: 以起始函数为入口, 构建 `std::thread` 对象时启动

```C++
std::thread t(fun);		// 启动线程, fun: 任何可调用对象
```

线程结束: 函数返回时, 对应的线程结束

### std::abort()

异常终止进程, 不调用析构函数

### 线程的汇合与分离

* 线程汇合: join

  > 阻塞调用线程,  被调线程执行完后, join 才返回

* 线程分离: detach

  > 分离的线程所有权转移给 C++ 运行时库(runtime library)
  >
  > C++ 运行时库保证了, 线程结束时正确回收资源

> 无论是 join 还是 detach, 运行后都会切断线程与 thread 对象的关联

**已执行完代码**但尚未 `join` 的线程**仍被视为正在执行的线程**，因此是 **`joinable`** 的

因为可能会手动调用 `join`, 而 `!joinable()` 状态的线程是不能 `join` 的

虽然可以这样写:

```C++
if(t.joinable())
    t.join();
```

但若在 `t.joinable()` 返回 `true` 之后, `t.join()` 调用前, 线程执行完, 导致 `!joinable` 就会引发异常

### 向线程函数传递参数

在 thread 的构造函数中传递线程函数的参数, **值传递**方式

然后 thread 构造函数**值传递的副本**被当成**临时变量**, 按照线程函数的方式传递参数

```C++
void fun(int, char&);
std::thread t(fun, 1, 'a');		// 构造函数默认值传递
// fun(1, 'a');	线程函数按照 fun 的方式传递临时变量
```

**类成员函数**

除了要传递**函数指针**外, 还要传递类的**对象指针**

```C++
class A
{
public:
    void fun();
};

A a;
std::thread t(&A::fun, &a);
```



### std::ref()

生成一个 `reference_wrapper` 对象, 造成**引用传递**

```C++
void fun(int &a)
{
    cout << a << endl;
    a = 2;
}

int main()
{
    int a = 1;
    std::thread t( fun, std::ref(a) );
    t.join();
    cout << a << endl;
}
```



### std::thread

```C++
~thread()
{
    if(joinable())
        std::terminate();	// 终止线程
}
```

若 std::thread 对象销毁时是 joinable 的,  则 thread 的析构函数会调用 std::terminate() 终止线程

### std::terminate

异常处理失败时, 调用 std::terminate() 终止程序

std::terminate() 默认调用 std::terminate_handler()

std::terminate_handler() 默认调用 std::abort()

### std::move

将**左值转化为右值**方便进行移动操作

> move 本身并不进行移动操作, 只是将左值转化为右值, 这是移动操作的前提

**移后源对象**应是**未定义值**, 但**析构是安全的**, 也最好这么做

```C++
// 极端情况下, 他们可能是同一份值

class A
{
public:
    int x;
    A(int _x = 1) : x(_x) {}
};

void fun(A&& a)
{
    a.x *= 2;
    cout << a.x << endl;
}

int main()
{
    A a;
    cout << a.x << endl;
    fun(std::move(a));
    fun(std::move(a));		// 不能使用移后源对象
    cout << a.x << endl;	// 不能使用移后源对象
}
```



### 可调用对象与 std::function

[C++ 函数指针 与 std::function](https://zhuanlan.zhihu.com/p/547484498)

### 不能 joinable 的情况

std::thread 对象**没有关联线程**

1. std::thread 对象默认构造
2. std::thread对象被 move 过, 所有权转交给别的对象了
3. 线程被 join 或 detach 过

### 定义线程后不执行 join 或 detach 会出错

根本原因在于 **std::thread 不完全符合 RAII 原则**

thread 构造时符合**资源获取**, 但**析构时不能自动进行资源销毁**

**因为 join 和 detach 都有其缺点**, 需要手动选择其中一个执行, 不然就出错

* join 可能造成死锁
* detach 可能会指向销毁的变量

### RAII 实现 thread 

保证 thread 析构时能够释放资源(执行完线程函数)

```C++
class j_thread
{
    std::thread& t;
public:
    j_thread(std::thread& _t) : t(_t) {}	// RAII 获取资源
    ~j_thread()								// RAII 释放资源
    {
        if(t.joinable())
            t.join();
    }
    
    // 禁用拷贝构造和赋值操作
    j_thread(const j_thread &)=delete;
    j_thread& operator=(const j_thread &)=delete;
}
```



### 线程创建时的二义性

存在二义性的 C++ 语句, 只要能被解释为函数声明, 编译器就一定解释为**函数声明**

```C++
class A
{
public:
    void operator()();
};

int main()
{
    A a;
    thread t(a());
    t.join();
}
/*
thread t(a()); 被解释为函数声明
thread: 返回类型
t: 函数名
a(): 函数指针类型, 指向的函数没有参数, 返回 A 类型对象

a() 原意是生成临时的匿名函数对象
解决办法:
1. 为临时函数对象命名
thread t((a()))
2. 统一初始化语法
thread t{a()}
*/
```

### 线程有引用或指针

线程持有**主线程局部变量的引用或指针**时, 会访问已经销毁的变量

```C++
#include <iostream>
#include <thread>

using namespace std;

struct A
{
    int& a;
public:
    A(int& _a) : a(_a) {}
    void operator()()
    {
         cout << a << endl;
    }
};

int main()
{
    int x = 1;

    A a(x);
    std::thread t(a);

    t.detach();
}
```

**解决办法**:

1. 线程完全自含

   通过数据复制, 使得线程拥有完整的数据

2. 线程汇合

   join 确保主线程结束前, 执行完该线程
