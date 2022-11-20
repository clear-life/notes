# 通用

### RAII

RAII(**R**esource **A**cquisition **I**s **I**nitialization) 资源获取即初始化

**栈中局部对象管理资源**: 资源即操作系统**有限的资源**如内存, 网络套接字

利用**局部对象自动销毁**的特性**防止忘记释放资源**: 

1. 获取: **构造函数获取**
2. 使用
3. 销毁: **析构函数释放**

### 移动操作

资源的所有权**转移**

* 数据从一个对象转移到另一个对象内, **源对象被"搬空"**
* 若源对象是右值, 移动操作会自动发生
* 若源对象是左值, 则必须通过 `std::move()` 转化为右值后转移

### 支持移动操作

* **unique_ptr**
* **thread**

### delete 两次指针

第二次可能会释放另一变量申请的空间

### 迭代器

* 前向迭代器

### STL 哪些容器是线程安全的



### 类模板参数推导

### 函数指针

### 可变模版参数

```C++
template<typename... T>		// 0~n 个独立模板参数
void fun(T... args)			// 参数包: 带省略号的参数
```

### std::move

将**左值转化为右值**方便进行移动操作

> move 本身并不进行移动操作, 只是将左值转化为右值, 右值是移动的前提

**移后源对象**应是**未定义值**, 但**析构是安全的**, 也最好这么做

### std::ref()

生成一个 `reference_wrapper` 对象, 使被封装的对象**引用传递**给线程函数

### 函数签名

* 函数名
* 参数列表
* 所在类和命名空间

## 函数式编程

输出只跟输入有关, 跟外部状态无关

### 纯函数

* 输入确定后, 输出就确定了
* 数据自包含(不使用全局变量)
* 不修改程序状态, 不引起副作用(不修改全局变量)

## 异常

### 自定义异常

```C++
class Myexcept
{
private:
    string mes;

public:
    Myexcept(string str = "A problem") : message{str} {}
    string what() const { return message; }
};
```

### 异常处理顺序

```C++
int main(void)
{
    A a;	// 1.构造
    try {
        throw(std::string("error!"));  // 2. 抛出异常, 跳过 try 块后面的代码, 跳到对应的 catch
        a.fun();						
    }
    catch (string s) {    
        cout << s << endl;  // 3. 异常处理   
    }
    catch (...) {          // 其他类型异常
        cout << "..." << endl;
    }

    cout << "return 0!" << endl;    // 4. 执行后面的代码
    return 0;				// 5. 析构
}
```

## C++11, C++14, C++17, C++20

### C++11

**1. 类型推导**

**2. 右值引用**

用于**移动语义**和**完美转发**

**3. 智能指针**

**4. 范围 for 循环**

**5. nullptr**

解决二义性

null 就是 0, nullptr 表示空指针

**6. 列表初始化**

**std::initializer_list**

好处: 解决 thread 构造时的二义性

**7. 可变模板参数**

### C++17

**register**

存储说明符, 尽可能存储到寄存器, **C++17弃用**

**nodiscard**

### C++20

**std::jthread**

RAII 方法析构 join 的 thread

## 关键字

### constexpr

`constexpr` 修饰的目标必须具有在**编译阶段就计算出结果**的能力

### [[nodiscard]]

修饰函数返回值, 表明该值不能被忽略

```C++
std::thread t(fun);
t.joinable();	// warning:放弃具有 "nodiscard" 属性的函数的返回值
```

### noexcept

* 表明函数不会抛出异常, **阻止异常的传播和扩散**
* 使库能够使用移动构造函数

* 编译器会优化的更好

```C++
/*
noexcept:
1. 修饰函数表示不抛出异常
2. 接收常量表达式, 根据结果表明会不会抛出异常
*/

void func() noexcept;               // 不抛出异常
void func() noexcept(常量表达式);    // 常量表达式为 true, 函数为 noexcept, 否则为 non-noexcept

template<>
void func() noexcept(noexcept(T()));
// noexcept(T()) 的结果由 T() 是否抛出异常决定, 不抛出为 true, 否则为 false
// func 是否为 noexcept 由 noexcept(T()) 决定

```



### explicit

禁用隐式类型转换

### mutable

mutable 修饰的变量永远可变, 不受 const 函数的限制

### throw

**1. 抛出异常**

```C++
if(...)
    throw 异常类对象;
```

**2. 表明抛出的异常类型**

```C++
void func() throw(char, int);	// 表明只抛出 char 和 int 类型的异常, 其余的异常即使抛出, try 也无法捕获, 直接终止程序 
void func() throw();	// 不抛出异常, 即使抛出异常, try 也不捕获
```

# 并发

### 并发概念

**并发作用**: **分离关注点**和**性能提升**

* 分离关注点(业务分离)

  > 一个线程负责用户界面管理, 另一个负责播放视频

* 性能提升

  > 多任务并行运行, 单个任务本身并没有提升性能

**增强性能的并发方式**:

1. 单一任务分解为多个部分
2. 单一算法对多份数据执行

**并发的缺点:**

1. **代码更难理解**, 写和维护成本高, 容易出错
2. **有时性能提升不大**
3. **线程的资源**(内核资源和栈空间)消耗
4. **上下文切换**的消耗

### 存储周期, 链接和作用域

* 存储周期: 变量的创建和销毁
* 链接: 变量函数的内存位置
* 作用域: 变量函数的可见范围

### 存储说明符

**auto** 

> 局部变量, 栈

* 存储周期: **automatic**, **代码块的开始和结束**

**static**

> 静态变量, 静态区

* 存储周期: **static 或 thread**
* 内部链接

**extern**

* 存储周期: **static 或 thread**
* 外部链接

**thread_local**

* 存储周期: **thread**, **线程的开始和结束**

**dynamic**

> 堆

* 存储周期: new 和 delete

**mutable**

* 变量永远可修改

# 一. 线程管控

## std::thread

### 构造

**`std::thread t(fun)`**

* **可调用对象** fun: 起始函数
* 构建 thread 后, 线程从起始函数开始执行, 函数执行完后, 线程结束

### 析构

thread 析构前必须不再关联线程, 不然就出错

```C++
~thread()
{
    if(joinable())
        std::terminate();	
}
```

### RAII 析构

**std::thread** 不完全符合 RAII 原则:

* **析构时不能自动进行资源销毁**

因为 **join 和 detach** 都有其**缺点**:

* join 可能造成死锁
  * A 的 join 放 B 中, B 的 join 放 A 中, 二者都在等待对方完成
  * A 的 join 放 A 内, A 阻塞 A, A 等 A 执行完

* detach 可能会指向销毁的变量

**简单封装 `thread`**

```C++
class thread_guard
{
	std::thread& t;		// 引用类型
public:
	explicit thread_guard(std::thread& _t) : t(_t) {}
	~thread_guard()
	{
		if (t.joinable());
			t.join();
	}

	thread_guard(thread_guard const&) = delete;
	thread_guard& operator=(thread_guard const&) = delete;
};
```

### join

**线程汇合**

A 调用 B.join() 后, A 被阻塞, 直至 B 执行完, 同时 B 的**线程 id 和句柄**设为 0 

### detach

**线程分离**

分离线程的所有权转移给 C++ 运行时库(runtime library), 保证线程结束时正确回收资源

同时**线程 id 和句柄**设为 0

### 移动

std::thread 支持**移动**操作

> 只要**赋值运算符右边是右值**, 且**左边对象支持移动操作**, 就能自动进行**移动赋值**操作

```C++
void fun();

std::thread t1 = std::thread(fun);	// 临时对象
std::thread t2 = std::move(t1);		// std::move 将左值转化为右值
t2 = std::thread(fun);	// 移动赋值, 转移前, t2 已关联线程, 所以 t2 是 joinable, 出错! 
```

**参数与返回值的移动**

```C++
// std::thread 做参数

void f(std::thread t);	// 函数原型

void fun();
f(std::thread(fun));	// 传入临时对象

std::thread t(fun);
f(std::move(t));		// 左值 move 后传入, t 为移后源对象


// std::thread 做返回值
std::thread f()
{
	void fun();
	return std::thread(fun);    // 临时对象
}

std::thread f()
{
	void fun();
	std::thread t(fun);
	return t;                   // 即将消亡的对象
}
```

**移动赋值运算符**

```C++
T& operator=(T&& t) noexcept {};
```

### std::thread::id

线程 ID 唯一标识线程, **全序关系**

> 0 表示无关联

**std::thread::id** 线程 ID

**std::thread::get_id()** thread 对象关联线程的 ID

### thread 源码

```C++
class thread
{
private:
    struct identifier
    { 
        void* handle;     
        unsigned int id;
    } thr;

public:
    thread() : thr{} {}                   

    template<class Fn, class... Args>
    explicit thread(Fn&& func, Args&&... args)      
    { 
        _Start(std::forward<Fn>(func), std::forward<Args>(args)...);
    }

    ~thread()  
    {
        if (joinable()) 
        {
            std::terminate();
        }
    }


    thread(thread&& other)  : thr(std::exchange(other.thr, {})) {}  
    thread& operator=(thread&& other)  
    {
        if (joinable()) 
        {
            std::terminate();
        }

        thr = std::exchange(other.thr, {});
        return *this;
    }


    thread(const thread&) = delete;
    thread& operator=(const thread&) = delete;


    bool joinable() const  
    {
        return thr.id != 0;
    }

    void join() 
    {
        if (!joinable())                                        // 1. 是否 joinable
        {
            _Throw_Cpp_error(_INVALID_ARGUMENT);
        }

        if (thr.id == _Thrd_id())                               // 2. 是否是当前线程
        {
            _Throw_Cpp_error(_RESOURCE_DEADLOCK_WOULD_OCCUR);   // 资源死锁
        }

        if (_Thrd_join(thr, nullptr) != _Thrd_success)          // 3. 是否执行成功     
        {       
            _Throw_Cpp_error(_NO_SUCH_PROCESS);                 // 没有该线程
        }

        thr = {};
    }

    void detach() 
    {
        if (!joinable()) 
        {
            _Throw_Cpp_error(_INVALID_ARGUMENT);
        }

        _Check_C_return(_Thrd_detach(thr));
        
        thr = {};
    }


    void swap(thread& other)  
    {
        std::swap(thr, other.thr);
    }

    unsigned int get_id() const 
    {
        return thr.id;
    }

    static unsigned int hardware_concurrency();
};
```

## 线程函数传参

### 参数传递

thread 构造函数按**值传递**方式传递**线程函数的参数**, 与**线程函数本身的形参类型**无关

* thread 构造函数**值传递的副本**被当成**临时变量**, 向线程函数传递参数

```C++
// 传递引用类型
void fun(int, char&);
std::thread t(fun, 1, 'a');		// 构造函数默认值传递
// 编译不通过, 因为值传递的副本不能作为左值被 fun 接收
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

### 可变模板参数

thread 的构造函数是**可变模板参数**

可变模板参数的用法:

**1. 递归函数**

```C++
// 递归终止函数 1
void fun()
{
}

// 递归终止函数 2
template<typename T>
void fun(T t)
{
	cout << t << endl;
}

// 展开函数
template<typename T, typename ...Args>
void fun(T head, Args... rest)
{
	cout << head << endl;
	fun(rest...);
}

int main()
{
	fun(4, 3, 2, 1);
	return 0;
}
```

**2. 逗号表达式**

```C++
template<typename T>
void print(T t)         // 处理参数包的参数
{
    cout << t << endl;
}

template<typename ...Args>
void fun(Args... args)  // 访问参数包的每个参数
{
    int arr[] = {(print(args), 0)...};  // 利用逗号和初始化列表的特性
}

int main()
{
	fun(4, 3, 2, 1);
	return 0;
}
```

### 可调用对象与 std::function

[C++ 函数指针 与 std::function](https://zhuanlan.zhihu.com/p/547484498)

## joinable

### join, deatch 和 joinable

**已执行完代码**但尚未 `join/deatch` 的线程**仍被视为正在执行的线程**，因此是 **`joinable`** 的

* 因为可能会手动调用 join, 就造成**非 joinable 调用 join**

* 虽然可以这样写:

  ```C++
  if(t.joinable())
      t.join();
  ```

  但若在 `t.joinable()` 返回 `true` 之后, `t.join()` 调用之前, 线程执行完使 t 变成非 joinable

  再调用 `join` 就会引发异常

### RAII 实现 thread 

把 `j_thread` 当成具有自动析构 `join` 的  `thread ` 看待

```C++
class j_thread      
{
    std::thread t;
public:
    j_thread();    
    
    j_thread(j_thread && j): t(std::move(j.t)) {}           
    
    explicit j_thread(std::thread &&_t): t(std::move(_t)) {} 
    
    template<typename Callable, typename Args>
    explicit j_thread(Callable&& fun, Args&& args) : 	            
    	t(std::forward<Callable>(fun), std::forward<Args>(args)) {}
    
    j_thread& operator=(j_thread&& j)
    {
        if(joinable())
            join();
        t = std::move(j.t);
        
        return *this;
    }
    
    j_thread& operator=(std::thread _t) 
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
    
    j_thread(const j_thread &)=delete;
    j_thread& operator=(const j_thread &)=delete;
    
    void swap(j_thread& j) noexcept
    {
        t.swap(j.t);
    }
    
    std::thread::id get_id() const 
    {
        return t.get_id();
	}
    
    bool joinable() const
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

## 异常情况

### std::terminate

**异常处理失败**时, 调用 std::terminate() 终止程序

* std::terminate() 默认调用 std::terminate_handler()

* std::terminate_handler() 默认调用 std::abort()

* **线程的堆栈不会被销毁**, 造成内存泄漏

### std::abort()

异常终止进程, 不调用析构函数, 造成内存泄漏

### thread 构造时的二义性

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

### 线程中的引用和指针

线程持有**主线程局部变量的引用或指针**时, 可能会访问已经销毁的变量

```C++
void fun(int& x);	// 线程函数有指针和引用参数, 隐患: 线程内可能会访问已经销毁的局部变量

int main()
{
	int x;
	std::thread t(fun, std::ref(x));	
	t.detach();
}
```

**解决办法**:

1. 线程**完全自含**

   通过数据复制, 使得线程拥有完整的数据

2. 线程**汇合**

   join 确保主线程结束前, 执行完该线程

# 二. 线程间共享数据

问题: **条件竞争与死锁**

原因: 推进顺序与个数

## 条件竞争

### 不变量

**不变量**类似循环不变量, 是**数据的断言**

* 多线程的问题是破坏**不变量**, 典型场景是需要**改动多份数据**

```C++
// 双向链表 A ↔ B ↔ C
// 不变量: 若 A -> B, 则 B -> A
void delete_node(Node B)
{
    n->pre->next = n->next;     // A -> C
                                // 此时A B C 之间的不变量被破坏, 其余线程访问 A B C 时就出问题了
    n->next->pre = n->pre;      // C -> A
    delete n;
}
```

### 条件竞争

**两个线程的操作产生了冲突**

* 条件竞争跟顺序有关, **读写的顺序不同, 结果不同**

  > 假定文件一共能存 10 行数据

  * **A 想读文件, B 想写文件**, 二者同时进行, **中间过程纠缠在一起**


**数据竞争**

* 竞争的是数据

例:

```C++
int a = 0;

void fun()
{
    a++;
}

std::thread t1(fun);
std::thread t2(fun);
```

`a++` 分三步:

1. 从**内存读**数据到寄存器

2. 寄存器中的数据**加 1**

   > 破坏了不变量: a 内存的数据表示实际的数据

3. 寄存器的数据**存回内存**

两个线程同时执行三个步骤, 导致最后的结果为 `a = 1`, 并不是 `a = 2`

### 解决办法

**1. 无锁编程**

修改**数据结构**的设计及其**不变量**

由**一系列原子操作**完成数据变更, 每个操作都**维持不变量不被破坏**

**2. 加锁**

**3. 事物** 

## std::mutex

**互斥锁**, 0 表示已被线程占有, 1 表示空闲

### std::mutex

**非递归互斥**:

1. 调用线程从 `lock` 开始, 到 `unlock` 为止占有 `mutex`, 此时其他线程会被阻塞/返回false

   > mutex 相当于临界区, 占有 mutex 就是进入临界区, 没有 mutex 就是出临界区

2. 调用 `lock` 前必须不占有 `mutex` (**非递归**)

   > 两次 lock 会死锁
   >
   > 递归下一层后, 由于已经占有 `mutex`, 会被阻塞, 所以不能保证递归的正常进行
   >
   > 只能锁一层

### lock() 阻塞

1. 若 mutex 为 0, 则置为 1 后正常返回

2. 若 mutex 为 1, 则被**阻塞**, 等待 mutex 为 0

> 同一线程 lock 两次, 会导致死锁: 自己阻塞自己, 自己等待自己解锁

```C++
std::mutex m;

m.lock();
m.lock();		// 死锁
cout << "test" << endl;
m.unlock();
m.unlock();
```

### try_lock() 非阻塞

1. 若 mutex 为 0, 则置为1 后返回 true
2. 若 mutex 为 1, 则返回 false, **不被阻塞**

```C++
std::mutex m;

m.try_lock()	// 尝试加锁成功

m.try_lock())	// 尝试加锁失败
    
cout << "test" << endl;

m.unlock();
```

### unlock

1. 若 mutex 为 1, 则置为 0
2. 若 mutex 为 0, 则返回

```C++
// unlock 两次对本身而言没有问题
// 但对别的线程而言, 其本应被阻塞, 却因为多 unlock 了一次而不被阻塞进入临界区
// 所以第二次 unlock 不会成功
std::mutex m;

m.lock();
cout << "test" << endl;
m.unlock();
m.unlock();     // 不会成功

m.lock();
m.lock();       // 依旧会阻塞
cout << "test" << endl;
m.unlock();
```



### 例1: 一个 mutex 锁住两个操作

```C++
#include <mutex>

std::list<int> l;
std::mutex m;		// 使 add 和 find 操作互斥 

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

### 例2: 不得向锁外传递受保护数据的指针和引用

显而易见, **函数返回值不能**是**受保护数据的指针和引用**

同样地, **函数内不能向锁外**传递**受保护数据的指针和引用**

```C++
void protect(函数指针 fun)
{
    std::lock_guard<std::mutex> l(m);	// 进入临界区
    fun(data);	// 传入保护的数据 data 的引用给外界函数 fun
}
```

### mutex 类型

**std::mutex** 普通锁

**std::recursive_mutex** 递归锁

**std::time_mutex** 延迟锁

**std::recursive_timed_mutex** 递归延迟锁

### std::recursive_mutex 

允许**同一线程**对**某互斥的同一实例多次加锁** 

```C++
std::recursive_mutex m;

void fun(int u)
{
    std::lock_guard<std::recursive_mutex > guard(m);
    
    cout << u << endl;
    if (u == 0)
        return;
    fun(u - 1);
}

int main()
{
    std::thread t(fun, 3);
    t.join();
}
```

### 锁的粒度

**粒度大, 效率低**

**粒度小, 容易出问题**

1. 锁的**顺序**

   加锁的顺序不确定, 会造成**条件竞争**

2. 锁的**个数**

   锁的个数不满足条件, 会造成**死锁**

## RAII 封装 mutex

构造时 `lock` / `try_lock`, 析构时 `unlock`

### std::lock_guard<>

```C++
int main()
{
    std::mutex m;
    std::lock_guard<std::mutex> l(m);	// 构造时加锁
    
    cout << "test" << endl;

    return 0;		// 析构时解锁
}
```

### 空结构体

空结构体尽管定义一样, 但仍旧是不同的类型

> 编译器自动加一个 char, 使得空类对象大小为 1 字节

`std::adopt_lock`  假设已加锁

`std::try_to_lock`  尝试加锁, 不阻塞

`std::defer_lock`  不加锁

### 类模板参数推导

自动推断模板参数类型

```C++
int main()
{
    std::mutex m;
    std::lock_guard l(m);	// 自动推断出 m 的类型为 std::mutex
    
    cout << "test" << endl;

    return 0;
}
```

### 线程安全接口的设计

**线程安全的栈容器类**

```C++
#include <mutex>
#include <stack>

using namespace std;

struct empty_stack : std::exception
{
    const char* what() const throw();
};


template<typename T>
class threadsafe_stack
{
private:
    std::stack<T> stk;
    mutable std::mutex m;
public:
    threadsafe_stack();
    threadsafe_stack(const threadsafe_stack& other)
    {
        std::lock_guard<std::mutex> l(other.m);     // mutable 保证了 m 在任何情况下都能被修改
        stk = other.stk;                            // 函数体内复制在 mutex 的保护范围内
    }
    threadsafe_stack& operator=(const threadsafe_stack&) = delete;

    void push(T x)
    {
        std::lock_guard<std::mutex> l(m);
        stk.push(std::move(x));
    }

    void pop(T& x)      // 传入引用
    {
        std::lock_guard<std::mutex> l(m);
        if (stk.empty())
            throw empty_stack();
        x = stk.top();
        stk.pop();
    }

    std::shared_ptr<T> pop()    // 返回指针
    {
        std::lock_guard<std::mutex> l(m);
        if (stk.empty())
            throw empty_stack();
        std::shared_ptr<T> const res(std::make_shared<T>(stk.top()));
        stk.pop();
        return res;
    }

    bool empty() const;
};
```

### std::lock_guard 源码

```C++
template <class T>
class lock_guard 
{ 
public:
    // 构造加锁
    explicit lock_guard(T& _Mtx) : m(_Mtx)   // 参数为 mutex 
    {
        m.lock();
    }

    // 不上锁
    lock_guard(T& _Mtx, adopt_lock_t) : m(_Mtx) {}   // std::adopt_lock 是 std::adopt_lock_t 类的实例

    // 析构解锁
    ~lock_guard() noexcept 
    {
        m.unlock();
    }

    lock_guard(const lock_guard&) = delete;
    lock_guard& operator=(const lock_guard&) = delete;

private:
    T& m;
};
```

### std::lock

一次性锁住多个互斥, 不发生死锁 

> 仅仅是一个模板函数, 功能是一次性加锁
>
> 解锁仍需手动解锁

* 要么全部成功加锁
* 要么未成功加锁, 那么已经加锁的会解锁, 然后抛出异常

```C++
void swap(int& l, int& r);

class A
{
private:
    int x;
    std::mutex m;

public:
    A(int const &_x) : x(_x) {}

    friend void swap(A& l, A& r)
    {
        if(&l == &r)
            return;
        
        std::lock(l.m, r.m);
        std::lock_guard<std::mutex> lock_a(l.m, std::adopt_lock);
        std::lock_guard<std::mutex> lock_b(r.m, std::adopt_lock);

        std::swap(l.x, r.x);
    }
};
```

### std::scoped_lock<>

* `std::lock()` 与 `std::lock_guard<>` 的结合版
* 可变模板参数

```C++
friend void swap(A& l, A& r)
    {
        if(&l == &r)
            return;
    
        // 相当于 std::lock 和 std::lock_guard 结合
    	std::scoped_lock guard(l.m, r.m);		

        std::swap(l.x, r.x);
    }
```

* 类模板参数推导

  ```C++
  std::scoped_lock guard(l.m, r.m);
  等价于
  std::scoped_lock<std::mutex, std::mutex> guard(l.m, r.m);
  ```


### std::unique_lock

更灵活的管理互斥

```C++
std::mutex m;
std::unique_lock<std::mutex> l(m);						// lock, 相当于 lock_guard<std::mutex>
std::unique_lock<std::mutex> l(m, std::adopt_lock);		// 假定已 lock
std::unique_lock<std::mutex> l(m, std::defer_lock);		// 不 lock, 当作析构 unlock 的 mutex 使用
std::unique_lock<std::mutex> l(m, std::try_to_lock);	// 尝试 lock
```

**std::unique_lock 支持移动操作**

可以**转移互斥的归属权**

```C++
#include <mutex>

std::unique_lock<std::mutex> get_lock()
{
    extern std::mutex m;
    std::unique_lock<std::mutex> l(m);

    // prepare

    return l;
}

void fun()
{
    std::unique_lock<std::mutex> l(get_lock());
    
    // do something, 持有互斥
}
```

可以**灵活加锁解锁**

```C++
void fun()
{
    std::mutex m;
    std::unique_lock<std::mutex> l(m);  // 构造时 lock
    // 访问共享数据
    l.unlock();     // 手动解锁
    // 不访问共享数据
    l.lock();       // 手动加锁
    // 访问共享数据
}   // 析构解锁
```

## 死锁

### 死锁的四个必要条件

对**资源**来说:

* 互斥占有
* 不可剥夺

对**线程/进程**来说:

* 请求并保持
* 循环等待

### 死锁的原因

* 锁操作
* 进程推进顺序不当

### 死锁实例

A 等待 B, B 等待 A

**1. 互相 lock**

线程 A 和 B 都需要对互斥 A 和 B 上锁

若**线程 A 对互斥 A 上锁**, **线程 B 对互斥 B 上锁**, 二者都需要对对方上锁, **二者阻塞对方**

**2. lock 两次**

同一 mutex lock 两次会死锁: **自己阻塞自己**, **自己等待自己 unlock**

**3. 互相 join**

**thread A 调用 thread B.join**, **thread B 调用 thread A.join**, 二者都在等待对方结束, **二者阻塞对方**

**4. 自己 join 自己**

thread t 在内部调用 t.join(), 相当于**自己阻塞自己**, **自己等待自己执行完**

> thread A 调用 thread B.join 会阻塞 thread A, 直到 thread B 执行完

### 防范死锁的准测

只要**线程 B 有可能等线程 A**, 那么**线程 A 不能反过来等线程 B**

### 防范死锁

**1. 只用一个锁**

* 若已经持有一个锁, 则不要获取第二个锁

* 若需要获取多个锁, 尽量用 `std::lock` 一次性获取

**2. 避免调用用户接口**

* 用户接口功能任意, 可能会获取锁, 也可能将共享资源指针传递出去

**3. 按顺序加锁**

对互斥 A 和互斥 B, 永远先锁互斥 A 再锁互斥 B

* 仍会出问题: 调用顺序不同

  ```C++
  void swap(A, B);
  
  // 假设先锁第一个参数再锁第二个参数
  // 那么调用 swap(A, B) 和 swap(B, A) 会陷入死锁
  ```


**4. 按层级加锁**

从高层到底层加锁

> 本质上就是按顺序加锁

```C++
#include <mutex>
#include <stdexcept>
#include <climits>
class hierarchical_mutex    
{
    std::mutex m;  // 内部锁
    unsigned long const level;  // 实例的层级
    unsigned long this_thread_pre_level;    // 记录 this_thread 的上一层级
    static thread_local unsigned long this_thread_level;  // 当前线程最新层级

    void check()
    {
        // 此次加锁的层级必须比当前线程的上一次加锁层级低, 即 level < this_thread_level
        if(level >= this_thread_level)      
        {
            throw std::logic_error("mutex hierarchy violated");
        }
    }
    void update()
    {
        this_thread_pre_level = this_thread_level;  // 恢复现场对应的步骤
        this_thread_level = level;
    }
public:
    explicit hierarchical_mutex(unsigned long value):
        level(value),
        this_thread_pre_level(0)
    {}

    void lock()
    {
        check();
        m.lock();          // m.lock() 之后, m.unlock() 之前, 拥有互斥, 不必担心破坏不变量
        update();
    }

    void unlock()
    {
        this_thread_level = this_thread_pre_level;  // 类似于恢复现场
        m.unlock();
    }

    bool try_lock()
    {
        check();
        if(!m.try_lock())
            return false;
        update();
        return true;
    }
};
thread_local unsigned long hierarchical_mutex::this_thread_level(ULONG_MAX);     // 初始化为 ULONG_MAX 

int main()
{
    hierarchical_mutex m1(42);
    hierarchical_mutex m2(2000);
}

```

## 延迟初始化

### 1. 互斥

问题: 

* 第一次使用时没问题, 正常上锁并初始化
* 但初始化之后, 每次还需要上锁, 导致并发效率不高

```C++
std::shared_ptr<T> p;
std::mutex m;

void fun()		// 访问数据, 若未初始化则需要初始化
{
    std::unique_lock<std::mutex> l(m);
    if(!p)      
    {
        p.reset(new T);
    }
    l.unlock();

    p->do_something();	// 访问数据
}
```

### 2. 双重检验锁定

进行两次检验

```C++
std::shared_ptr<T> p;
std::mutex m;

void fun()
{
    if(!p)      // 保证了只会进行一次初始化
    {
        std::unique_lock<std::mutex> l(m);
        if(!p)
        {
            p.reset(new T);
        }
        l.unlock();
    }

    p->do_something();     
} 
```

### 3. std::once_flag 类和 std:: call_once() 函数

`std::once_flag` 实例存储同步数据, 一个实例对应一次初始化

`std:: call_once()` 函数在一个 `std::once_flag` 实例上只会调用一次 `init` 可调用对象

```C++
std::shared_ptr<int> p;
std::once_flag flag;

void init()	// 初始化函数
{
    p.reset(new int(0));
}
void fun()
{
    // std::call_once 只会执行一次
    std::call_once(flag, init);	// flag 相当于标志位, 表明是否调用过 init 函数
    
    // 访问数据
}
```

### 4. local static

C++11 标准保证了 `local static` 变量初始化的正确执行, 即

* 只有一个线程会执行初始化
* 初始化完成之前, 其余线程不会越过去

```C++
int& get_instance()
{
    static int x = 1;	// 线程安全且只执行一次
    return x;
}
```

## 保护甚少更新的数据结构

`std::shared_mutex`

**排他锁**: 排他写 `std::lock_guard<std::shared_mutex>`

**共享锁**: 共享读 `std::shared_lock<std::shared_mutex>`

```C++
#include <map>
#include <mutex>
#include <string>
#include <shared_mutex>

class A
{
	std::map<std::string, std::string> dns;
	mutable std::shared_mutex m;                        // 共享 mutex

public:
	std::string find(std::string& key) const
	{
		std::shared_lock<std::shared_mutex> l(m);       // 共享

		std::map<std::string, std::string>::const_iterator const it = dns.find(key);
		
		return (it == dns.end()) ? std::string() : it->second;
	}

	void update(std::string const& key, std::string const& val)
	{
		std::lock_guard<std::shared_mutex> l(m);        // 排他

		dns[key] = val;
	}
};
```

# 三. 线程同步

## 条件变量

**头文件** `<condition_variable>`

**条件变量**: **与某一条件关联**, 条件成立时, 通过条件变量**唤醒所有等待线程**继续执行

* **std::condition_variable**: 仅与 **std::mutex** 一起使用

* **std::condition_variable_any**: 任何互斥

### 使用条件变量

```C++
std::mutex m;									// 使两个函数互斥
std::queue<int> q;
std::condition_variable flag;

void data_prepare()
{
    for(int i = 0; i < 10; i++)
    {
        {
            std::lock_guard<std::mutex> l(m);	// 锁住数据的 入
            q.push(i); 
        }		// 解锁后才通知是为了效率
        flag.notify_one();
    }
}

void data_process()
{
    while(true)
    {
        std::unique_lock<std::mutex> l(m);		// 锁住数据的 出

        flag.wait(l, []{ return !q.empty(); });

        int t = q.front();
        q.pop();

        l.unlock();

        cout << t << endl;

        if(q.empty())
            break;
    }
}

int main()
{
    std::thread t1(data_prepare);
    std::thread t2(data_process);

    t1.join();
    t2.join();
}
```

### notify_one 和 notify_all

条件满足后, 通知阻塞线程

* **notify_one** 唤醒一个阻塞线程
* **notify_all** 唤醒所有阻塞线程

### wait

等待条件满足, **互斥的状态保持不变**, 即在 `wait` 内成对执行 `lock` 和 `unlock`

**condition_variable::wait(unique_lock<mutex\>& l, Func func)**

检测条件是否成立:

> func 返回 true 表示成立, false 表示不成立

* 若成立, `wait` 返回

* 若不成立

  1. `l.unlock()` 后阻塞线程

  2. `notify_one()` 通知条件变量, 阻塞解除

  3. `l.lock()` 后, 再次检测条件

```C++
template<typename Func>
void condition_variable::wait(unique_lock<mutex>& l, Func func)
{
    while(!func())
    {
        l.unlock();			// 必须使用 std::unique_lock 的原因: 灵活上锁解锁
    
        // 阻塞, 等待条件变量通知
        wait_for_cnd();

        l.lock();
    }
}
```

### 虚假唤醒

不是真正因为条件满足而唤醒

**原因1**: 多线程争抢

1. **一个生产者, 多个消费者**

2. 生产一个资源后, 多个消费者都被唤醒
3. 但只有一个消费者能得到资源, 其余线程的唤醒就是**虚假唤醒**

**原因2**: 系统原因

某些系统允许: 没有条件变量通知的情况下, `wait` 依然可以返回

**副作用**:

* 与条件判断无关的操作
* 不限次数和时机地产生

**问题**:

* **副作用**多次产生, 不限次数和时机

### 唤醒丢失

通知了, 但没人等, 被错过了

### 用条件变量构建线程安全队列

```C++
#include <mutex>
#include <condition_variable>
#include <queue>
#include <memory>

template<typename T>
class threadsafe_queue
{
private:
    mutable std::mutex m;	// 互斥必须用 mutable 修饰(const 对象)
    std::queue<T> q;
    std::condition_variable cond;

public:
    // 默认构造与拷贝构造
    threadsafe_queue() {}
    threadsafe_queue(threadsafe_queue const& other)
    {
        std::lock_guard<std::mutex> l(other.m);
        q=other.q;
    }

    void push(T val)
    {
        std::lock_guard<std::mutex> l(m);
        q.push(val);
        cond.notify_one();
    }

    // 引用和指针
    void wait_and_pop(T& val)
    {
        std::unique_lock<std::mutex> l(m);
        cond.wait(l,[this]{return !q.empty();});

        val = q.front();
        q.pop();
    }

    std::shared_ptr<T> wait_and_pop()
    {
        std::unique_lock<std::mutex> l(m);
        cond.wait(l,[this]{return !q.empty();});

        std::shared_ptr<T> res(std::make_shared<T>(q.front()));
        q.pop();

        return res;
    }

    bool try_pop(T& val)
    {
        std::lock_guard<std::mutex> l(m);
        if(q.empty())
            return false;

        val = q.front();
        q.pop();

        return true;
    }

    std::shared_ptr<T> try_pop()
    {
        std::lock_guard<std::mutex> l(m);
        if(q.empty())
            return std::shared_ptr<T>();

        std::shared_ptr<T> res(std::make_shared<T>(q.front()));
        q.pop();
        
        return res;
    }

    bool empty() const
    {
        std::lock_guard<std::mutex> l(m);
        return q.empty();
    }
};
```

## future

概念: **模拟一次性事件**, 事件发生后, **与事件关联的所有 `future` 实例会同时就绪**, 可以**访问与事件关联的数据**

理解: future 储存**一个异步操作的结果**

* **std::furure** 一个事件只能关联一个 `std::furure` 实例, 一个线程等待结果

* **std::shared_future** 一个事件可以关联多个 `std::shared_future` 实例, 多个线程等待结果

### future

**成员函数**

* `get()` 获取关联事件的值

  > 如果事件发生异常, 异常也会作为值保存在 future 实例中, future 同样会进入就绪状态
  >
  > get() 调用后, 存储的异常被重新抛出

* `valid()` 判断 `future` 对象是否有效
* `share()` 创建新的 `shared_future` 对象, 转移归属权

```C++
#include <iostream>
#include <future>

using namespace std;

int fun()
{
    return 1;
}

int main()
{
    std::future<int> f = std::async(fun);	// future 实例含有关联事件的数据

    cout << f.get() << endl;
}
```

### future 源码

```C++
template <class T>
class future
{ // class that defines a non-copyable asynchronous return object that holds a value
    // 定义不可复制的异步, 该异步返回一个拥有值的对象
public:
    T get() 
    {
        // 阻塞到事件完毕, 返回关联事件的数据
        future _Local{std::move(*this)};
        return std::move(_Local._Get_value());
    }

    shared_future<T> share() noexcept {
        return shared_future<T>(std::move(*this));
    }

    future(const future&) = delete;
    future& operator=(const future&) = delete;
};
```

### future 保存异常

`std::async()` 和 `std::packaged_task<>` 自动保存异常

`std::promise()` 通过成员函数 `set_exception()` 手动保存异常

1. `try/catch`块保存异常

```C++
try
{
    int x = 1;
    p.set_value(x);
}
catch(...)
{
    p.set_exception(std::current_exception());
}
```

2. 直接保存异常

   > 需要提前直到异常类型

```C++
p.set_exception(std::make_exception_ptr(std::logic_error("x < 0")));
```

> 销毁关联的 `std::packaged_task` 和 `std::promise` 对象, 会将异常 `std::future_error` **存储为异步任务的状态数据**, 值是错误代码 `std::future_errc::broken_promise`
>
> 这种方式会导致许诺(异步方式给出值或异常)被破坏, 等待的线程永远都等不到结果

## async()

### std::async 源码

```C++
template <class _Func, class... _Args>
future<_Invoke_result_t<decay_t<_Func>, decay_t<_Args>...>> 	// 返回类型
    async(_Func&& func, _Args&&... args) 
{
    // manages a callable object launched with default policy
    // 管理一个可调用对象
    return std::async(launch::async | launch::deferred, 		// 默认策略
                      std::forward<_Func>(func), 
                      std::forward<_Args>(args)...);
}
```

### std::async 传参

**运行策略**:

* `std::launch::deferred`  延后执行, 直至 `future` 调用 `wait()` 或 `get()`, **在二者的内部运行任务函数**

  > 有可能永远不执行任务函数

* `std::launch::async`  另起线程

* `std::launch::deferred | std::launch::async`  自行选择, default

```C++
#include <string>
#include <future>

struct X
{
    std::string fun(std::string const&);
};

X x;
auto f1 = std::async(&X::fun, &x, "hello");     // x.fun()
auto f2 = std::async(&X::fun, x, "goodbye");    // x 值传递生成副本 t, t.fun()


struct Y
{
    double operator()(double);
};

Y y;
auto f3 = std::async(std::launch::deferred, Y(), 1.0);                   // Y() 默认构造生成匿名变量 t, t.operator()
auto f4 = std::async(std::launch::async, std::ref(y), 1.0);           // 引用传递, y.operator()


X fun(X&);      // 函数 fun
auto f6 = std::async(fun,std::ref(x));      // fun()


class move_only // 只有移动操作的类
{
public:
    void operator()();
};
auto f5 = std::async(move_only());          // 默认构造生成副本, 然后移动构造在 std::sync() 内产生临时对象 t, t.operator()
```

## packaged_task<>

* 关联 **future 实例**和**函数**
* 异步运行函数, 将结果保存在 future 实例中

> 函数 代指 可调用对象

### std::packaged_task<>

类模板

**作用**

调用函数, 返回值保存在 future 实例中, 令 future 准备就绪

**模板参数**

模板参数是**函数类型**

* 函数类型**不必严格匹配**, 只要能进行**隐式类型转换**

```C++
void()		// 无输入, 无返回值
int(int, double)	// 输入 int, double , 输出 int 
```

**成员函数**

`get_future()` 返回关联的 `future` 实例

`operator()` 调用**关联的函数**

### std::packaged_task<> 接口

```C++
template <class _Ret, class... _Args>
class packaged_task<_Ret(_Args...)> {
    // class that defines an asynchronous provider that returns the result of a call to a function object
    // 提供异步函数调用返回值的类
public:
    packaged_task() : _MyPromise(0) {}

    template <class _Fty2, enable_if_t<!is_same_v<_Remove_cvref_t<_Fty2>, packaged_task>, int> = 0>
    explicit packaged_task(_Fty2&& _Fnarg) : _MyPromise(new _MyStateType(_STD forward<_Fty2>(_Fnarg))) {}

    ~packaged_task();

    future<_Ret> get_future();
    void operator()(_Args... _Args);
};
```

## promise<T\>

### std::promise<T\>

**作用**

在线程 A 中保存一个值, 关联的 `future` 实例就能在线程 B 获取到该值

**成员函数**

`get_future()` 返回关联的 `future` 实例

`set_value()` 设置值

`set_exception()` 设置异常

**使用**

```C++
void fun1(std::promise<int> &p)
{
    int x = 1;
    cout << "传入数据: " << x << endl; 
    p.set_value(x);
}

void fun2(std::future<int> &f)
{
    int x = f.get();
    cout << "收到数据: " << x << endl; 
}

int main()
{
    std::promise<int> p;
    std::future<int> f = p.get_future();    // f 与 p 关联

    std::thread t1(fun1, std::ref(p));
    std::thread t2(fun2, std::ref(f));

    t1.join();
    t2.join();

    return 0;
}
```

### std::promise<T\> 接口

```C++
template <class _Ty>
class promise 
{ // class that defines an asynchronous provider that holds a value
// 定义一个异步方式提供值的类
public:
    promise() : _MyPromise(new _Associated_state<_Ty>) {}

    template <class _Alloc>
    promise(allocator_arg_t, const _Alloc& _Al) : _MyPromise(_Make_associated_state<_Ty>(_Al)) {}

    promise(promise&& _Other);

    promise& operator=(promise&& _Other);

    ~promise();


    future<_Ty> get_future() {
        return future<_Ty>(_MyPromise._Get_state_for_future(), _Nil{});
    }

    void set_value(const _Ty& _Val) {
        _MyPromise._Get_state_for_set()._Set_value(_Val, false);
    }

    void set_value(_Ty&& _Val) {
        _MyPromise._Get_state_for_set()._Set_value(_STD forward<_Ty>(_Val), false);
    }

private:
    _Promise<_Ty> _MyPromise;
};
```

## shared_future

### shared_future 使用

`std::shared_future` 实例由 `std::future` 实例构造得到

**future 移动构造**

```C++
std::promise<int> p;
std::future<int> f(p.get_future());
assert(f.valid());		// 有效

std::shared_future<int> sf(std::move(f));
assert(!f.valid());		// 移动后无效
assert(sf.valid());	


std::shared_future<int> sf(p.get_future());	// 移动构造	


auto sf = p.get_tuture.share();		// 移动构造, 自动推断类型
```

### std::shared_future<> 源码

```C++
template <class _Ty>
class shared_future : public _State_manager<_Ty> 
{
public:
    shared_future() noexcept {}
    shared_future(const shared_future& _Other);
    shared_future(future<_Ty>&& _Other);
    shared_future(shared_future&& _Other);

    shared_future& operator=(const shared_future& _Right);
    shared_future& operator=(shared_future&& _Right);

    ~shared_future();

    const _Ty& get() const 
    {
        return this->_Get_value();
    }
};
```

## <chrono\>

时间的表示

### 时间类

* `std::chrono::steady_clock` 恒稳时钟
* `std::chrono::system_clock` 系统时钟
* `std::chrono::high_resolution_clock` 高精度时钟

**成员**

* `now()` 当前时刻
* `time_point` 时间类型
* `period` 计时单元
* `static bool is_steady`  是否恒稳

### 时长类

`std::chrono::duration<>` 模板类

**模板参数**

* 数据类型

  数据的存储类型

* 计时单元大小

  一个单位代表的**秒数**

```C++
// short 存储的分钟时长类, 一单位表示 60 秒
std::chrono::duration<short, std::ratio<60,1>>	
    
// double 存储的毫秒时长类, 一单位表示 1/1000 秒
std::chrono::duration<double,std::ratio<1,1000>>
```

**预设时长类**

**nanoseconds** 纳秒  

**microseconds** 微妙

**milliseconds** 毫秒

**seconds** 秒

**minutes** 分钟

**hours** 小时

**成员**

* `count()` 时间数值, 不考虑单位

### 时间点类

`std::chrono::time_point<>` 

**模板参数**

* 时钟类
* 计时单元类

**成员**

* `time_since_epoch()` 从时钟纪元到当前的时间长度

  > 时钟纪元, 时间开始的时刻, 如: 1970年1月1日0时0分0秒

**时间运算**

时间点 + 时长 = 时间点

时间点 - 时间点 = 时长

```C++
#include <iostream>
#include <chrono>

using namespace std;

int main()
{
	auto start = std::chrono::high_resolution_clock::now();
	
	fun();
		
	auto stop = std::chrono::high_resolution_clock::now();

	cout << std::chrono::duration<double, std::chrono::seconds>(stop - start).count() << endl;
}
```

## 线程同步实现

### 函数式编程

```C++
#include <future>
template<typename F,typename A>
std::future<result_type> spawn_task(F&& f,A&& a)        
{	// 返回与 packaged_task 关联的 future
    
    // packaged_task 关联函数 f , 函数 f 的返回值类型是 result_type
    std::packaged_task<result_type> task(std::move(f)); 
    
    // packaged_task 关联 future
    std::future<result_type> res(task.get_future());    

    // 新开线程异步运行 packaged_task
    std::thread t(std::move(task),std::move(a));        
    t.detach();

    return res;
}
```

### CSP 通信式串行编程

**有限状态自动机**, 不同状态间通过**消息队列**进行通信, 然后根据状态处理消息

总体是一个类, 每个状态都是一个成员函数, 接收一个消息后, 处理完后会进入下一状态

### std::experimental::future

如果 future 就绪, 可以进一步通过成员函数 `then()` 处理结果

```C++
std::experimental::future<int> f;
f = .get_future();

auto f2 = f.then(fun);
assert(!f.valid());
assert(f2.valid());
```

**后续函数的参数**是**准备就绪的 future**

```C++
void fun(std::experimental::future<int> f);	// f 可能含有正常结果, 也可能含有异常
```

**与 std::async() 等价的函数**

```C++
#include <experimental/future>
template<typename Func>
std::experimental::future<decltype(std::declval<Func>()())>
spawn_async(Func&& func){
    std::experimental::promise<decltype(std::declval<Func>()())> p;
    auto res = p.get_future();

    std::thread t(
        [p=std::move(p),f=std::decay_t<Func>(func)]()
            mutable{
            try{
                p.set_value_at_thread_exit(f());
            } catch(...){
                p.set_exception_at_thread_exit(std::current_exception());
            }
    });
    t.detach();
    
    return res;
}
```

**后续函数的连锁调用**

```C++
#include <experimental/future>
std::experimental::future<void> process_login(
    std::string const& username,std::string const& password)
{
    return backend.async_authenticate_user(username,password).then(	// 确定账密
        [](std::experimental::future<user_id> id){
            return backend.async_request_current_info(id.get());	// 返回信息
        }).then([](std::experimental::future<user_data> info_to_display){
            try{
                update_display(info_to_display.get());				// 信息展示
            } catch(std::exception& e){
                display_error(e);									// 异常处理
            }
        });
}

```

### std::experimental::when_all()

用 `std::experimental::when_all()` 函数等待 `future` 全部就绪，然后使用 `then()` 编排后续函数

### std::experimental::when_any()

运用 `std::experimental::when_any()` 函数等待多个 `future`，直到其中之一准备就绪

### std::experimental::latch

`std::experimental::latch` 的构造函数接收唯一一个参数，在构建该类对象时，我们需通过这个参数设定其计数器的初值

每当等待的目标事件发生时，我们就在线程闩对象上调用 `count_down()`，一旦计数器减到0，它就进入就绪状态

### std::experimental::barrier

只有在全部线程都完成各自的处理后，才可以操作下一项数据或开始后续处理，`std::experimental::barrier` 针对的就是这种场景

### std::experimental::flex_barrier

`std::experimental::flex_barrier`  是 `std::experimental::barrier`的灵活版本
