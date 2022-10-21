## 并发

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

## 线程管控

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

**已执行完代码**但尚未 `join` 的线程**仍被视为正在执行的线程**，因此是 **`joinable`** 的

因为可能会手动调用 `join`, 而 `!joinable()` 状态的线程是不能 `join` 的

虽然可以这样写:

```C++
if(t.joinable())
    t.join();
```

但若在 `t.joinable()` 返回 `true` 之后, `t.join()` 调用前, 线程执行完, 导致 `!joinable` 就会引发异常

### 可调用对象与 std::function

[C++ 函数指针 与 std::function](https://zhuanlan.zhihu.com/p/547484498)

### 不能 joinable 的情况

thread 对象**没有关联线程**

1. thread 对象默认构造
2. thread 对象被 move 过, 所有权转交给别的对象了
3. 线程被 join 或 detach 过

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
a(): 函数指针
*/
```

