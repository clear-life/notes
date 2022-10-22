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

### noexcept

表明函数不会抛出异常, 编译器直到不会抛出异常后, 会**更加进行代码优化**

### delete 两次指针

第二次 delete 可能会释放另一个变量或指针申请的内存空间

### 左值, 纯右值, 将亡值

### explicit

* explicit 显式的

* **禁用隐式类型转换**

### 迭代器

* 前向迭代器

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

**std::thread 支持移动操作**

只要**赋值运算符右边是右值**, 且**左边对象支持移动操作**, 就能自动进行**移动赋值**操作

```C++
void fun();

std::thread t1 = std::thread(fun);	// 临时对象, 移动赋值
std::thread t2 = std::move(t1);		// 移动赋值, 线程转移给 t2
t2 = std::thread(fun);	// 转移前, t2 已关联线程, 因此 t2 的析构函数中会调用 std::terminate() 终止线程
```

### 移动赋值运算符

```C++
template<typename T>
T& operator=(T&& t) noexcept {};
```

### join 和 detach

### std::terminate

异常处理失败时, 调用 std::terminate() 终止程序

std::terminate() 默认调用 std::terminate_handler()

std::terminate_handler() 默认调用 std::abort()

**线程的堆栈不会被销毁**, 造成内存泄漏

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
    std::thread t;
public:
    j_thread() noexcept=default;
    
    // 构造函数 RAII 获取资源
    j_thread(j_thread&& j) noexcept: t(std::move(j.t)) {}
    
    explicit j_thread(std::thread _t) noexcept: t(std::move(_t)) {}
    
    template<typename Callable, typename Args>
    explicit j_thread(Callable&& fun, Args&& args) : 	
    	t(std::forward<Callable>(fun), std::forward<Args>(args)) {}
    
    // 线程转移
    j_thread& operator=(j_thread&& j) noexcept
    {
        if(joinable())
            join();
        t = std::move(j.t);
        
        return *this;
    }
    
    j_thread& operator=(std::thread _t) noexcept
    {
        if(joinable())
            join();
        t = std::move(_t);
        
        return *this;
    }
    
    // 析构函数 RAII 释放资源
    ~j_thread()	noexcept							
    {
        if(joinable())
            join();
    }
    
    // 禁用拷贝构造和赋值操作
    j_thread(const j_thread &)=delete;
    j_thread& operator=(const j_thread &)=delete;
    
    // 常用操作
    void swap(j_thread& j) noexcept
    {
        t.swap(j.t);
    }
    
    std::thread::id get_id() const noexcept
    {
        return t.get_id();
	}
    
    // 模仿 thread 操作
    bool joinable() const noexcept
    {
        return t.joinable();
	}
    
    void join()
    {
        t.join();
	}
    
    void detach()
    {
        t.detach();
	}
    
    // 模仿 thread
    std::thread& as_thread() noexcept
    {
        return t;
    }
    
    const std::thread& as_thread() const noexcept
    {
        return t;
    }
}
```

### 多线程数量

由**硬件支持的最大数量**和**算法要求的最大数量**共同决定, 取二者的较小值, 注意主线程本身占一个名额

**std::thread::hardware_concurrency()** 硬件支持的最大并发线程数

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

### 识别线程

线程 ID 唯一识别线程, **全序关系**

**std::thread::id** 线程 ID

**std::this_thread::get_id()** 获取当前线程的 ID

**std::thread::get_id()** 获取 thread 对象关联的线程 ID, 0 表示无关联

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

# 线程间共享数据

## mutex

### std::mutex

### std::recursive_mutex

### std::time_mutex

### std::recursive_timed_mutex

## lock

### std::lock_guard

### std::unique_lock

### 使用互斥

```C++
#include <mutex>

std::list<int> l;
std::mutex m;

void add(int x)
{
    std::lock_guard<std::mutex> guard(m);

    l.push_back(x);
}

bool find(int x)
{
    std::lock_guard<std::mutex> guard(m);
    
    return std::find(l.begin(), l.end(), x) != l.end();
}
```

### 不得向锁外传递受保护数据的指针和引用

显而易见, **函数返回值不能**是**受保护数据的指针和引用**

同样地, **函数内不能向锁外**传递**受保护数据的指针和引用**

```C++
#include <iostream>
#include <mutex>

using namespace std;

class A
{
public:
    int a;
    A(int x = 1) : a(x) {}
};

class Warp
{
private:
    A data;
    std::mutex m;

public:
    template<typename Fun>
    void test(Fun fun)
    {
        std::lock_guard<std::mutex> l(m);
        fun(data);			// 锁内向锁外传递指针
    }
};

A* unprotected;				// 指向受保护的数据
void fun(A& data)			// 恶意函数
{
    unprotected = &data;
}

int main()
{
    Warp w;
    w.test(fun);

    cout << unprotected->a << endl;	// 访问受保护的数据
}
```

### std::mutex

* 互斥量, 保护共享数据

* **非递归互斥**:

  1. 调用线程从 `lock` 开始, 到 `unlock` 为止占有 `mutex`

  2. 线程占有 `mutex` 时, 其他线程若试图要 `mutex` 的所有权, 则将阻塞/返回

     > mutex 相当于临界区, 占有 mutex 就是进入临界区, 没有 mutex 就是出临界区

  3. 调用 `lock` 前必须不占有 `mutex`

     > 递归下一层后, 由于已经占有 `mutex`, 会被阻塞, 所以不能保证递归的正常进行
     >
     > 只能锁一层

* std::mutex **不能复制, 也不能移动**

**方法:**

* `lock` 加锁, 若不能则阻塞
* `try_lock` 尝试加锁, 若不能就返回 `false`
* `unlock` 解锁

### std::recursive_mutex 

递推锁

> 多把相同锁, 多层锁

```C++
std::recursive_mutex m;

void fun(int u)
{
    std::lock_guard<std::recursive_mutex > guard(m);
    
    if (u == 0)
        return;
    cout << u << endl;
    fun(u - 1);
}

int main()
{
    std::thread t(fun, 3);
    t.join();
}
```

### std::lock_guard<>

实现 mutex RAII 思想, 构造时加锁, 析构时解锁

> 增强版: std::scoped_lock<>

```C++
std::mutex m;

void fun(int u)
{
    std::lock_guard<std::mutex> guard(m);
    ...
}
```



