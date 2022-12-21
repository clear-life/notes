# 通用

### RAII

RAII(**R**esource **A**cquisition **I**s **I**nitialization) 资源获取即初始化

**栈中局部对象管理资源**: 资源即操作系统**有限的资源**如内存, 网络套接字

利用**局部对象自动销毁**的特性**防止忘记释放资源**: 

1. 获取: **构造函数获取**
2. 使用
3. 销毁: **析构函数释放**

### RVO

返回值优化(Return Value Optimization)

**场景**

```C++
class A
{
    
};

A fun()
{
    A a;
    ...
    return a;
}
```

**优化前**

先调用 A 的拷贝构造函数, 再将 **a 的拷贝**作为函数返回值返回

**优化后**

直接将 a 作为函数返回值返回, **避免了不必要的拷贝操作**

**RVO 使用**

类似移动右值的场景

* 一次返回
* 返回值非引用
* 可以用 `move` 手动启动 RVO

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

* 构建 thread 后, 线程从起始函数开始执行, 函数执行完后, 线程结束

### 析构

thread 析构前必须不再关联线程

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

**join 和 detach** 都有其**缺点**:

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

A 调用 B.join() 后, A 被阻塞, 直至 B 执行完

同时 B 的**线程 id 和句柄**设为 0 

### detach

**线程分离**

分离线程的所有权转移给 C++ 运行时库(runtime library), 保证线程结束时正确回收资源

同时**线程 id 和句柄**设为 0

### 移动操作

std::thread 支持**移动**操作

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
char c = 'a';
std::thread t(fun, 1, c);		// 构造函数默认值传递
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

1. 若 mutex 为 1, 则置为 0 后正常返回

2. 若 mutex 为 0, 则被**阻塞**, 等待 mutex 为 1

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

1. 若 mutex 为 1, 则置为 0 后返回 true
2. 若 mutex 为 0, 则返回 false, **不被阻塞**

```C++
std::mutex m;

m.try_lock()	// 尝试加锁成功

m.try_lock())	// 尝试加锁失败
    
cout << "test" << endl;

m.unlock();
```

### unlock

1. 若 mutex 为 0, 则置为 1
2. 若 mutex 为 1, 则返回

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

问题描述: `notify_one` 只会唤醒一个等待线程, 若该线程**抛出异常**或由于其他原因没有响应唤醒, 就浪费了一次 `notify_one` 的唤醒

**解决办法**

* 将 `notify_one` 改为 `notify_all`

  后果是增大开销

* 在异常处理中再次调用 `notify_one`

**唤醒丢失**

```C++
A:
{
    unique_lock<mutex> l(m);			
    cnd.wait(l, [](){return flag;});	
}
	cnd.wait()
    {
        while(!flag)	a1
            cnd.wait(l);a2
    }
B:
{
    flag = true;		b1
    cnd.notify_one();	b2
}
```

唤醒丢失序列: `a1  b1  b2  a2`

`notify_one` 的**唤醒信号在线程 A 未阻塞时发出, 然后丢失**

**解决办法**

加锁

```C++
A:
{
    unique_lock<mutex> l(m);			
    cnd.wait(l, [](){return flag;});	
}

B:
{
    {
        unique_lock<mutex> l(m);
        flag = true;
    }
    cnd.notify_one();	
}
```

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

# 四. 内存模型和原子操作

## 内存模型

### 位域

以位为单位定义对象的内存大小

* **bit_field** 位域结构名
* **x** 位域成员名
* **width** 成员所占位数

```C++
struct bit_field	
{
 type x : width;	
};
```

**使用**

```C++
struct A 
{
 	unsigned a: 30;
 	unsigned b: 4;
};
```

位域成员不能跨越**内存区域的边界**, 所以 b 在第二个内存区域, 共有两个内存区域

* a 占第一个内存区域的 30 位, 空余 2 位
* b 占第二个内存区域的 4 位, 空余 28 位

**宽度为 0 的匿名位域成员当作分隔符**

> 宽度为 0  的位域成员实际不占有内存区域

```C++
struct A
{
	unsigned a : 30;
	unsigned : 0;
	unsigned b : 2;
};
```

* a 占第一个内存区域的 30 位, 空余 2 位
* b 占第二个内存区域的 2 位, 空余 30 位

### 对象

**对象: 一个或多个 memory location**

举例: 

* 基本内置类型: int, double
* 自定义类型: class, struct

```C++
struct A
{
    int i;		// #1
    double d;	// #2
    unsigned bf1:10;// #3
    int bf2:25;
    int bf3:0;
    int bf4:9;	// #4
    int i2;		// #5
    char c1, c2;// #6,7
    string s;
}
```

**总结**

* 每个变量都是对象, 对象的数据成员也是对象(子对象) 
* 每个对象至少占用一块内存区域
* 基本内置类型, 不论大小, 都占用一块内存区域
* 相邻的位域属于同一内存区域

### 内存区域和并发

两个线程访问**同一内存区域**, 就有可能导致**条件竞争**

**解决办法:**

* 互斥
* 原子操作

如果没有强制多线程访问同一 **memory location** 服从一定的次序, 如果至少有一个是**非原子访问**,

且至少有一个写访问, 那么就有可能发生 **data race**, 造成**未定义行为**

### 改动序列

每个 C++ 对象上都有一个 **modification order**，从对象的**初始化**开始, 由所有线程在该对象上的 **write** 构成

* 一般情况下, 随着程序的每一次运行,  **modification order** 会不同

* 但在程序一次运行中, **所有线程**必须**就该序列达成一致**

* 如果对象不是原子类型, 那么就必须**通过某种同步方式**使得所有线程在每个变量的 **modification order** 上**达成一致**

* 如果不同线程在某一变量上看到不同的**值序列**, 说明发生了 **data race** 和**未定义行为**

  > 指令级并发, 指令级不变量被破坏

* 如果对象是原子类型, 那么编译器就会确保必要的同步操作来保证该变量上的 **modification order** 一致性

**speculative execution**

预测执行: 预测哪个分支最有可能要执行, **提前执行预测分支的指令**

为了保证**改动序列的一致性**, 需要禁止某些**预测执行**功能, 因为:

* 只要某线程在改动序列中看到某个变量的值, 那么后续的 **read** 操作必须看到之后的值
* 并且该线程后续的 **write** 操作必须出现在改动序列中的后面
* 只要一个 read 操作在一个  write 之后, 那么 read 读出来值要么是 write 写入的值, 要么是 write 改动序列之后的值

## 原子操作

**不可分割**, 要么全部完成, 要么全部不完成, 没有半完成状态

**非原子操作导致的条件竞争**:

```C++
int x = 0;

A:
{
    x++;
}

B:
{
    x++;
}

x 有可能为 1
```

### atomic<>

**特点:**

* **内置类型赋值**
* 支持**隐式转换**为内置类型
* 成员函数**返回右值,** 不返回左值
* **删除拷贝和移动操作**

  **拷贝和移动**操作都涉及两个独立对象, 先从 A 读, 再对 B 写, 读写之间有**间隙**, 所以**删除拷贝和移动操作**

**成员函数:**

* **is_lock_free()**

  是否不用锁

  * `true`: 原子指令实现

  * `false`: 内部锁实现

* **is_always_lock_free()**

  **编译期**判定是否永远不用锁

  * `true`: 在**所有硬件**上以**原子指令**实现

  * `false`: **运行时才能确定**是否不用锁

* **load()**: load 操作

* **store()**: store 操作

* **exchange()**: read-modify-write 操作

* **compare_exchange_weak()**: read-modify-write 操作

* **compare_exchange_strong()**: read-modify-write 操作

### memory_order

枚举类 **std::memory_order** 表示内存次序

```C++
enum memory_order 
{
    memory_order_relaxed,
    memory_order_consume,
    memory_order_acquire,
    memory_order_release,
    memory_order_acq_rel,
    memory_order_seq_cst
};
```

**原子操作划分为3类**: 

每类操作有独自能选用的 memory order

* **store** 操作
  * memory_order_relaxed
  * memory_order_**release**
  * memory_order_seq_cst
* **load** 操作
  * memory_order_relaxed
  * memory_order_**consume**
  * memory_order_**acquire**
  * memory_order_seq_cst
* **read-modify-write** 操作
  * memory_order_relaxed
  * memory_order_**consume**
  * memory_order_**acquire**
  * memory_order_**release**
  * memory_order_**acq_rel**
  * memory_order_seq_cst

### atomic_flag

最简单STD原子类型, 永远保证无锁

* 两个状态: 清除 false, 设置 true

**必须初始化为 false**

```C++
atomic_flag f = ATOMIC_FLAG_INIT;
```

**成员函数**

* **test_and_set** 返回旧值, 设置为 true
* **clear** 设置为 false

**源码**

```C++
struct atomic_flag 
{ 
    bool test_and_set(const memory_order order = memory_order_seq_cst)  
    {
        return storage.exchange(true, order) != 0;
    }

    void clear(const memory_order order = memory_order_seq_cst)  
    {
        storage.store(false, order);
    }

    atomic<bool> storage;
};
```

> != 对二值逻辑(true 和 false, 1 和 0)来说相当于异或操作
>
> 1 != 0    1
>
> 0 != 0    0

**自旋锁互斥**

0 表示未加锁, 1 表示加锁

```C++
atomic_flag flag = ATOMIC_FLAG_INIT;

void lock()
{
    while(flag.test_and_set(memory_order_acquire));
}

void unlock()
{
    flag.clear(memory_order_release);
}
```

### CAS

* **exchange()**  返回旧值, 设置新值

* **compare_exchange_strong()**

  比较后根据结果 exchange

* **compare_exchange_weak()**

  比较后根据结果 exchange

**compare_exchange_strong**

原子值为 x.val, 期望值为 rt, 设定值为 t

* 若 x.val == rt, 则 x.val = t, 返回 true
* 若 x.val != rt, 则 rt = x.val, 返回 false

```C++
bool compare_exchange_strong(T& rt, const T t)
{  
    if(x.val == rt)
    {
        x = t;
        return true;
    }
    else
    {
        rt = x.val;
        return false;
    }
}
```

**spurious failure**

`x.compare_exchange_weak(rt, t)` 中即使 `x == rt` 成立也有可能失败, 称为**佯败**spurious failure

原子在于**原子化的CAS操作**必须由一条指令完成, 然而在某些处理器上没有这种指令

无法保证CAS操作的原子性, 而是由一系列指令完成, 就有可能导致失败

> 在佯败情况下, 最终 x.val = t 失败, 而且返回了true

所以 `compare_exchange_weak` 往往配合循环使用

```C++
atomic<bool> b;

{
    bool rt = false;
    while(!b.compare_exchange_weak(rt, true) && !rt);	// 防止佯败
}
```



### atomic<T*>

指向 T 类型的原子指针

**成员函数**

* **fetch_add()**	返回旧值
* **fetch_sub()**    返回旧值
* += 和 -=    返回新值
* ++ 和 --

### 标准整数原子类型

* 常见原子操作

  ```C++
  load()
  store()
  exchange()
  compare_exchange_weak()
  compare_exchange_strong()
  ```

* 原子运算

  下列操作都是 **read-modify-write** 操作

  ```C++
  fetch_add()
  fetch_sub()
  fetch_and()
  fetch_or()
  fetch_xor()
  ```

* 复合赋值

  无法设置 memory order 因而都是 memory_order_set_cst

  ```C++
  +=
  −=
  &=
  |=
  ^=
  ```

* 前后缀自增和自减

  与一个相同

  ```C++
  ++x
  x++
  −−x
  x−−
  ```


### 原子操作非成员函数

* 前缀 `atomic_`
* 后缀 `_explicit` 指定内存次序
* 无后缀 不指定内存次序

## 内存顺序

**memory_order** 本质上是在讨论单线程内**指令执行顺序**对**多线程**影响的问题

### evaluation

* 值计算: **表达式的结果**
* 副效应: **值计算的过程**

  ```C++
  int x;
  
  int fun()
  {
      x = 2;
      return 0;
  }
  
  表达式: 
  fun() + 1
  值计算: 0 + 1 = 1
  副效应: 赋值 x = 2
  ```

**side-effect**

side-effect 是运算符, 表达式, 语句, 函数的结果, 在完成 value 的计算后仍然存在

多种解释:

* 计算表达式结果之外的操作, 在计算完之后仍然存在

* 任何数学意义之外的 **change**

  > A = B + C	main-effect: 计算 B + C 的值, side-effect: A 的值被改变

> 

* 更改对象的值
* 进行输入或输出

```C++
x = 1 + 2;	// main-effect: 计算 1 + 2 的值, side-effect: x 的值被修改
```



### 线程内 $<$

> 表达式 A 和 B 有三种关系:
>
> A 和 B 视为原子操作
>
> * A < B
> * B < A
>
> A 和 B 视为非原子操作
>
> * A $\nless$ B 且 B $\nless$ A

* 若 A < B, 则 **A 的求值**在 B 的**求值开始前**完成

* 若 B $<$ A, 则 **B 的求值**在 A 的**求值开始前**完成

* 若 A $\nless$ B 且 B $\nless$ A

  * A 和 B 的求值**顺序不确定**: A $<$ B 或 B $<$ A

  * A 和 B 的求值**无顺序**: A 和 B 的指令以任意方式混合

    ```C++
    A:
    {
        a1
        a2
    }
    
    B:
    {
        b1
        b2
    }
    
    A 的汇编指令为 a1 a2
    B 的汇编指令为 b1 b2
        
    A 和 B 的求值为任意情况:
    a1  a2  b1  b2      
    a1  b1  a2  b2  
    a1  b1  b2  a2  
    b1  a1  a2  b2  
    b1  a1  b2  a2  
    b1  b2  a1  a2      
    ```

### 线程间 $<$

线程间, 对求值 A 和求值 B, 最终混合序列中 A $<$ B

### $<$

包括: 线程内 $<$ 和线程间 $<$

### 依赖于 $\rightarrow$

依赖关系, 参考**拓扑排序**

概念: 若 A $<$ B, 且 **B 的求值依赖于 A**, 记为 **A $\rightarrow$ B**

1. A 的值作为 B 的操作数

   > 除了两种特殊情况

2. A 过程写入 X, B 过程需要读取 X
3. 若 A $\rightarrow$ X, X $\rightarrow$ B, 则 A $\rightarrow$ B

### 修改序列

程序对变量的**写操作序列** $w_1 w_2\dots w_n$

所有原子操作保证:

原子对象 M 的读操作 $w$, 写操作 $r$

1. $ww$连贯:

   若 $w_a < w_b$, 则在 M 的修改序列中 $w_a < w_b$

2. $rr$连贯:

   若 $r_a < r_b$, 且 $r_a$ 的值来自 $w_x$,  $w_x < w_y$, 则 $r_b$ 的值来自$w_x$ 或 $w_y$

3. $rw$连贯:

   若 $r_a < w_b$, 且 $w_x < w_b$, 则 $r_a$ 的值来自 $w_x$

4. $wr$连贯:

   若 $w_x < r_b$, 且 $w_x < w_y$, 则 $r_b$ 的值来自 $w_x$ 或 $w_y$

# 五. 基于锁的并发数据结构

## 设计并发结构

### 目的

* **并发访问**不出问题
* 提高**并发程度**

### 线程安全的数据结构

多线程环境下

1. 每个线程看到的**数据结构都是自洽**的
2. **数据不会被丢失和破坏**
3. **不变量始终成立**，**不会出现条件竞争**

### 方法

**总的思路**

保护的范围越小, 串行化操作越少, 并发程度越高

**串行化**

在**锁保护的范围串行执行**, 并非并发访问

> 强制排队访问

### 设计并发数据结构

减少串行操作, 提高并发程度, **只保留最必要的串行操作**

**原则**:

* **不存在接口上的条件竞争**
* **异常情况下不变量成立**

  > 异常情况下, 数据不出问题
* **限制锁的作用范围**

**使用者的限制**:

* 保证**构造函数完成前**和**析构函数开始后**, 不会访问数据结构
* 考虑与其他操作并发调用是否安全

**提高并发程度**:

* 使用**不同互斥**保护不同部分
* 每个操作需要**什么样的保护**
* 能否通过**简单的改动**增加并发程度

### 互斥的选择

**一个互斥**

* 实现简单
* 并发程度低

**多个互斥**

* 实现复杂
* 并发程度高
* **可能引发死锁**

## 线程安全的栈容器

### 代码

```C++
#include <mutex>
#include <stack>
#include <exception>

using namespace std;

struct empty_stack : exception
{
    const char* what() const throw();
};

template<typename T>
class threadsafe_stack
{
private:
    stack<T> stk;
    mutable mutex m;
public:
    threadsafe_stack();
    threadsafe_stack(const threadsafe_stack& other)
    {
        lock_guard<mutex> l(other.m); 
        stk = other.stk;                            
    }
    threadsafe_stack& operator=(const threadsafe_stack&) = delete;

    void push(T x)
    {
        lock_guard<mutex> l(m);
        stk.push(move(x));	
    }

    void pop(T& x)      	// 引用式返回
    {
        lock_guard<mutex> l(m);
        if (stk.empty())	
            throw empty_stack();
        
        x = move(stk.top());
        stk.pop();
    }

    shared_ptr<T> pop()    // 指针式返回
    {
        lock_guard<mutex> l(m);
        if (stk.empty())
            throw empty_stack();
        
        shared_ptr<T> const res(make_shared<T>(stk.top()));	
        stk.pop();			
        return res;
    }
	
    {
        lock_guard<mutex> l(m);	
        return stk.empty();
    }
};
```

### 接口分析

```C++
threadsafe_stack(const threadsafe_stack& other)
{
    lock_guard<mutex> l(other.m); 
    stk = other.stk;                            
}
```

* **互斥**: `other.m` 保护 `other.stk` 的读取过程, 没有使用 `this.m` 是因为**假定使用者在构造函数完成前没有访问数据结构**
* **条件竞争**: 构造函数不与其他接口并行执行, 不存在竞争
* **异常**: 
  * 加锁时可能异常: 此时数据并未改动, 所以异常安全
  * 解锁不可能失败, 且 `lock_guard` 保证不会遗漏解锁
  * `stack<>` 复制可能异常, 但 `stack<>` 保证不会出问题

```C++
void push(T x)
{
    lock_guard<mutex> l(m);
    stk.push(move(x));	// 异常: stk 保证自身异常安全
}
```

* **互斥**: `this.m` 恰好作用于 `this.stk` 的修改过程
* **条件竞争**: 条件竞争暂不明确
* **异常**: 
  * 加锁时数据未改动, 异常安全
  * 解锁不会失败, `lock_guard` 保证必定解锁
  * `stk.push()` 可能异常, 原因是拷贝/移动数据时可能异常, 但 `stack<>` 保证不会出问题

```C++
void pop(T& x)      	
{
    lock_guard<mutex> l(m);
    if (stk.empty())	
        throw empty_stack();
    
    x = move(stk.top());
    stk.pop();
}

shared_ptr<T> pop()    
{
    lock_guard<mutex> l(m);
    if (stk.empty())
        throw empty_stack();
    
    shared_ptr<T> const res(make_shared<T>(stk.top()));	
    stk.pop();	
    return res;
}
```

* **互斥**: `this.m` 作用于全程

* **条件竞争**: 原本的 `empty()` 和 `pop()`, `top()` 和 `pop()` 存在条件竞争, 现在的 `pop()` 包含了 `empty()` 和 `top()` 的功能, **独立自洽**

* **异常**:

  * 加锁安全

  * 解锁安全

  * `empty_stack` 异常: 数据未改动, 异常安全

  * `shared_ptr<T>` 创建异常:

    * 对象内存分配异常
    * 引用计数内存分配异常
    * 数据的拷贝/移动过程, **拷贝构造函数**和**移动构造函数**抛出异常

    `shared_ptr` 和 `make_shared` 保证该过程异常安全

  * `x = move(stk.top());` 拷贝/移动操作异常: 数据结构本身未改动, 所以异常安全

  * `stk.pop()` 异常: `stack<>` 保证异常安全

```C++
bool empty() const		
{
    lock_guard<mutex> l(m);	
    return stk.empty();
}
```

`empty()` 不改动数据, 异常安全

### 问题

**1. 使用者要避免死锁**: 

对于**用户自定义**的**拷贝操作**, **移动操作**和**重载 new 运算符**

在**栈容器插入或删除元素**时, 会调用上述函数, **上述函数可能再次调用栈容器的成员函数**, 再次获取同一互斥, 导致产生死锁

**2. 构造函数与析构函数**

使用者必须保证: **构造函数完成前**和**析构函数开始后**, 其他线程不能访问数据

**3. 并未提供 wait 和 try 操作**

没有提供 `wait_and_push` 和 `try_and_push` 等操作

**4. 并发程度低**

只是用一个锁, 本质上是全部串行化, 并发程度低

**5. 容器元素不是指针**

内存分配在锁的范围内进行, 效率低

## 简单线程安全的队列容器

###  条件变量实现线程安全队列

**代码**

```C++
#include <queue>
#include <mutex>
#include <condition_variable>

using namespace std;

template<typename T>
class threadsafe_queue
{
private:
	mutable mutex m;
	condition_variable cnd;
	queue<T> q;

public:
	threadsafe_queue(){}

	void push(T x)
	{
		lock_guard<mutex> l(m);
        
		q.push(move(x));
        
		cnd.notify_one();
	}

	void wait_and_pop(T& x)
	{
		unique_lock<mutex> l(m);
		cnd.wait(l, [this] {return !q.empty(); });
        
		x = move(q.front());
		q.pop();
	}

	shared_ptr<T> wait_and_pop()
	{
		unique_lock<mutex> l(m);
		cnd.wait(l, [this] {return !q.empty(); });

		shared_ptr<T> res(make_shared<T>(move(q.front())));
		q.pop();

		return res;
	}

	bool try_pop(T& x)
	{
		lock_guard<mutex> l(m);
		if (q.empty())
			return false;

		x = move(q.front());
		q.pop();
        
		return true;
	}

	shared_ptr<T> try_pop()
	{
		lock_guard<mutex> l(m);
		if (q.empty())
			return shared_ptr<T>();

		shared_ptr<T> res(make_shared(move(q.front())));
		q.pop();
		return res;
	}

	bool empty() const
	{
		lock_guard<mutex> l(m);
		return q.empty();
	}
};
```

**改进**

* 增加 `wait` 和 `try` 操作, **避免忙等**

**缺点**

* `notify_one` 只会唤醒一个等待线程, 若该线程**抛出异常**或由于其他原因没有响应唤醒, 就浪费了一次 `notify_one` 的唤醒
* 单一互斥
* 元素类型非指针

**解决办法**

* 将 `notify_one` 改为 `notify_all`, 但会增大开销

* `wait_and_pop` 在异常处理中再次调用 `notify_one`

* 容器元素改为 `shared_ptr<>`, `shared_ptr<>` 保证了在拷贝/移动时不会抛出异常

### 存储 `shared_ptr<>` 的线程安全队列

**代码**

```C++
#include <queue>
#include <mutex>
#include <condition_variable>

using namespace std;

template<typename T>
class threadsafe_queue
{
private:
	mutable mutex m;
	condition_variable cnd;
	queue<shared_ptr<T>> q;

public:
	threadsafe_queue() {}

	void push(T x)
	{
		shared_ptr<T> p(make_shared<T>(move(x)));	// 脱离锁的保护进行内存分配

		lock_guard<mutex> l(m);
		q.push(p);
		cnd.notify_one();
	}

	viod wait_and_pop(T& x)
	{
		unique_lock<mutex> l(m);
		cnd.wait(l, [this] {return !q.empty(); });
		
		x = move(*q.front());
		q.pop();
	}

	bool try_and_pop(T& x)
	{
		lock_guard<mutex> l(m);

		if (q.empty())
			return false;

		x = move(*q.front());
		q.pop();
		return true;
	}

	shared_ptr<T> wait_and_pop()
	{
		unique_lock<mutex> l(m);
		cnd.wait(l, [this] {return !q.empty(); });

		shared_ptr<T> res = q.front();
		q.pop();
		return res;
	}

	shared_ptr<T> try_pop()
	{
		lock_guard<mutex> l(m);
		if (q.empty())
			return shared_ptr<T>();

		shared_ptr<T> res = q.front();
		q.pop();
		return res;
	}

	bool empty() const
	{
		lock_guard<mutex> l(m);
		return q.empty();
	}
};
```

**改进**

* **元素类型改为指针**, 指针拷贝/移动不会异常
* **在锁的范围外分配内存**, 减少互斥的持有时间

**缺点**

* **单一互斥**

**接口分析**

```C++
void push(T x)
{
    shared_ptr<T> p(make_shared<T>(move(x)));

    lock_guard<mutex> l(m);
    q.push(p);
    cnd.notify_one();
}
```

* **互斥**: 单一互斥保护数据修改,  `notify_one()` 没必要在锁的范围内执行, 内存分配在锁的范围外进行(缺点)
* **条件竞争**: 貌似不存在条件竞争
* **异常**:
  * `shared_ptr` 和 `make_shared` 内存分配异常, 但数据未修改, 故异常安全
  * `lock_guard` 保证加锁解锁的异常安全
  * `q.push()` 异常, 由 `queue` 保证异常安全
  * `cnd.notify_one()` 由条件变量保证异常安全 

```C++
viod wait_and_pop(T& x)
{
    unique_lock<mutex> l(m);
    cnd.wait(l, [this] {return !q.empty(); });

    x = move(*q.front());
    q.pop();
}

bool try_and_pop(T& x)
{
    lock_guard<mutex> l(m);

    if (q.empty())
        return false;

    x = move(*q.front());
    q.pop();
    return true;
}
```

* **互斥**: 单一互斥保护数据修改, 作用于全程
* **条件竞争**: `pop()` 即判断是否为空, 又返回值, 独立自洽
* **异常**:
  * `lock_guard` 保证加锁解锁的异常安全
  * `if (q.empty())` 异常: 数据未改动, 异常安全
  * `x = move(*q.front())` 异常: 数据未改动, 异常安全

## 精细粒度锁的线程安全队列

### 单线程简单队列

**代码**

```C++
using namespace std;

template<typename T>
class queue
{
private:
	struct node
	{
		T data;
		unique_ptr<node> next;

		node(T x): data(move(x)) {}
	};

	unique_ptr<node> head;
	node* tail;		// tail 是额外的, 所以需要用原生指针

public:
	queue(): tail(nullptr) {}

	queue(const queue& other) = delete;
	queue& operator=(const queue& other) = delete;

	void push(T x)
	{
		unique_ptr<node> p(new node(move(x)));

		node* new_tail = p.get();

		if (tail)
		{
			tail->next = move(p);
		}
		else
		{
			head = move(p);
		}

		tail = new_tail;
	}

	shared_ptr<T> try_pop()
	{
		if (!head)
		{
			return shared_ptr<T>();
		}

		shared_ptr<T> const res(make_shared<T>(move(head->data)));
		
		unique_ptr<node> const old_head = move(head);
		head = move(old_head->next);

		if (!head)
			tail = nullptr;
		return res;
	}
};
```

**改进**

* `unique_ptr<>` 管理节点, 保证节点自动删除
* 头节点 `head` 和尾节点 `tail`, 方便插入删除

**问题**

* 元素类型非指针

* 两个互斥分别保护 `head` 和 `tail` 指针, 可能导致试图锁定同一互斥, 并发程度低

  `head` 和 `tail` 有三种位置关系:

  * head > tail : 不合法, 排除
  * head = tail : 需要修改 `head->next` 和 `tail->next`, 于是需要争夺同一互斥
  * head < tail : 不需要争夺互斥

### 单线程分离数据的队列

增加虚节点, 分离数据, 提高并发程度

* head > tail : 不合法, 排除
* head = tail : 此时队列为空, 不再需要访问 `head->next`, 不会争夺同一互斥
* head < tail : 不需要争夺互斥

>  虚节点位于尾部, 由 `tail` 指针指向

**代码**

```C++
template<typename T>
class queue
{
private:
	struct node
	{
		shared_ptr<T> data;
		unique_ptr<node> next;
	};

	unique_ptr<node> head;
	node* tail;

public:
	queue() : head(new node), tail(head.get()) {}

	queue(const queue& other) = delete;
	queue& operator=(const queue& other) = delete;

	void push(T x)
	{
		shared_ptr<T> new_data(make_shared<T>(move(x)));

		unique_ptr<node> p(new node);

		tail->data = new_data;
		node* const new_tail = p.get();
		tail->next = move(p);

		tail = new_tail;
	}

	shared_ptr<T> try_pop()
	{
		if (head.get() == tail)
		{
			return shared_ptr<T>();
		}

		shared_ptr<T> res(head->data);

		unique_ptr<node> old_head = move(head);
		head = move(old_head->next);

		return res;
	}
};
```

**改进**:

* `push()` 和 `pop()` **不再同时操作相同节点**, 即可以用**不同的互斥**分别保护 `head` 和 `tail`

  > 通过简单的设计, 使得争夺互斥的情况减少, 提高并发程度

  * head > tail : 不合法, 排除

  * head = tail : 此时队列为空, 不再需要访问 `head->next`, 不会争夺同一互斥

  * head < tail : 不需要争夺互斥

* **`push()` 只访问 `tail` 指针**, **`pop()` 访问 `head` 和 `tail` 指针, 但 `tail` 只用来做比较**

### 精细粒度锁的线程安全队列

**分析**

* 两种互斥, 分别锁住 `head` 和 `tail`

* 虚节点的设计让两个接口不容易产生冲突

  > 研究虚节点设计前后不变量的变化及对应锁的变化

```C++
template<typename T>
class threadsafe_queue
{
private:
	struct node
	{
		shared_ptr<T> data;
		unique_ptr<node> next;
	};

	mutex head_m;
	unique_ptr<node> head;

	mutex tail_m;
	node* tail;

	node* get_tail()
	{
		lock_guard<mutex> l(tail_m);
		return tail;
	}

public:
	threadsafe_queue(): head(new node), tail(head.get()) {}

	threadsafe_queue(threadsafe_queue const& other) = delete;
	threadsafe_queue& operator=(threadsafe_queue const& other) = delete;

	void push(T x)
	{
		shared_ptr<T> new_data(make_shared<T>(move(x)));
		unique_ptr<node> p(new node);

		node* const new_tail = p.get();

		lock_guard<mutex> tail_l(tail_m);

		tail->data = new_data;
		tail->next = move(p);

		tail = new_tail;
	}

	shared_ptr<T> try_pop()
	{
		unique_ptr<node> old_head;

		{
			lock_guard<mutex> head_l(head_m);
			if (head.get() == get_tail())
			{
				old_head = nullptr;
			}
			else
			{
				old_head = move(head);
				head = move(old_head->next);
			}
		}

		return old_head ? old_head->data : shared_ptr<T>();
	}
};
```

**不变量**

* ```C++
  tail→next == nullptr
  tail→data == nullptr
  ```

* `head == tail` 说明队列为空

* 对于节点 x, 若 `x != tail`, 则 `x->data` 指向 T 类型的实例, 且 `x->next` 指向后继节点

* `x->next == tail` 说明 x 是最后一个节点

* 从 `head` 出发, 沿着 `next` 指针访问后继节点, 最终会到达 `tail` 指向的节点

**接口分析**

```C++
void push(T x)
{
    shared_ptr<T> new_data(make_shared<T>(move(x)));
    unique_ptr<node> p(new node);

    node* const new_tail = p.get();

    lock_guard<mutex> tail_l(tail_m);

    tail->data = new_data;
    tail->next = move(p);

    tail = new_tail;
}
```

* **互斥**: `tail_m` 恰好作用于 `tail` 的修改过程

* **不变量**: `tail` 相关的**不变量在互斥外保持不变**, 在互斥内被破坏(但不被外界看到)

* **条件竞争**: `push` 独立自洽

* **异常**:

  * `shared_ptr`, `make_shared` 和 `new`, `unique_ptr` 异常

    数据未改动, `shared_ptr`, `make_shared` 和 `new`, `unique_ptr` 保证不会出现内存泄漏

  * 暂时认定指针拷贝/移动操作不发生异常


```C++
shared_ptr<T> try_pop()
{
    unique_ptr<node> old_head;

    {
        lock_guard<mutex> head_l(head_m);
        if (head.get() == get_tail())
        {
            old_head = nullptr;
        }
        else
        {
            old_head = move(head);
            head = move(old_head->next);
        }
    }

    return old_head ? old_head->data : shared_ptr<T>();
}
```

* **互斥**: 

  * `head_m` 保护除返回语句外的所有过程, `head_t` 保护 `tail` 的访问过程
  * `tail_m` 保护 `tail` 的读取, 使得 `push()` 中对 `tail` 的修改导致的 `tail` 不变量的破坏对 `tail` 的读取不可见, 要么还未修改, `get_tail` 读取到 `tail` 旧值, 要么已经修改, `get_tail` 读取到 `tail` 新值

* **不变量**:

  *  `get_tail` 在 `head_mutex` 的保护范围内, 这点很重要

    在 `head_mutex` 的保护范围内, 可以认为 `head` 不会被改变, 

    即参与比较的对象 `head` 不会被改变, 另一参与比较的对象 `tail` 虽然有可能改变, 

    但 `push()` 只会改变 `tail` 的值往右移动, 对当前读取到的 `tail` 而言, 可以看作不会被改变, 于是参与比较 `head` 和 `tail` 在**参与比较时的值可以认为是同一时刻**的

  * 但若 `get_tail` 在 `head_mutex` 的保护范围外的话, 参与比较时, `head` 的值与 `get_tail` 读取到的 `tail` 可能是不同时刻的, 就可能会判断错误, 导致出错

  * 错误示例

    ```C++
    A:
    {
        tail_m.lock();
        ...		// 修改 tail
        tail_m.unlock();
    }
    
    B:
    {
        tail_m.lock();
        old_tail = tail;	// 读取 tail  		时刻 a
        tail_m.unlock();
        
        head_m.lock();
        if(head == old_tail)// 比较 head 和 tail  时刻 b
        head_m.unlock();
        
    }
    ```

    参与比较的 head 和 tail 的值**可能属于不同时刻**, 导致得到错误的判断, 产生错误的结果, 破坏数据结构

    > 错误的判断和结果指: 
    >
    > 时刻 a : 队列有一个元素
    >
    > 时刻 a 和 b 之间: 另一 try_pop 线程删除队列的元素, 使得队列为空
    >
    > 时刻 b : head 和 old_tail 发生比较, 发现二者不同(尽管此时只有一个虚节点, 队列为空), 于是将 head 节点 pop 掉, 导致 head 为 nullptr, 破坏了不变量(虚节点队列至少含有一个节点, head 不可能为 nullptr)

  * 正确示例

    ```C++
    A:
    {
        tail_m.lock();
        ...		// 修改 tail
        tail_m.unlock();
    }
    
    B:
    {
        head_m.lock();
        if(head == get_tail())// 同一时刻比较 head 和 tail
        head_m.unlock();
        
    }
    ```

    参与比较时, `head` 的值不可能发生变化, `get_tail()`函数在**读取**和**参与比较**之间 `tail` 的值可能发生变化, 但 `tail` 的值虽然发生变化, `head` 的值却不会发生变化, 于是能够做出正确的判断

    > 顶多参与比较时判断为空队列, 实际为非空队列, 导致 try_pop 的结果为空指针, 但不会破坏数据结构本身

*  **条件竞争**: `pop()` 包含判空和返回值, 独立自洽

* **异常**:

  * 指针拷贝/移动操作可以认为不发生异常
  * return 语句可能发生异常

* **死锁**:

  * `try_pop()` 会获取两个锁, 但总是先锁 `head_m`, 再锁 `tail_m`, 所以死锁不会出现

* **并发程度**:

  * 锁的粒度更精细, 两个接口分别在两个互斥上持锁, 并发程度高

    > try_pop 对应 head_m, push 对应 tail_m

  * 内存分配在未持锁状态下进行

    > `shared_ptr<T> new_data(make_shared<T>(move(x)));`	内存分配
    > `unique_ptr<node> p(new node);`	内存分配

  * 内存释放在未持锁状态下进行

    > `unique_ptr<node> old_head;` 在函数返回时自动释放内存

### 支持等待操作的线程安全队列

* 增加 `wait` 操作

**接口分离代码**

```C++
#include <mutex>
#include <condition_variable>

using namespace std;

template<typename T>
class threadsafe_queue
{
private:
	struct node
	{
		shared_ptr<T> val;
		unique_ptr<node> next;
	};

	mutex head_m;
	mutex tail_m;
	condition_variable cnd;

	unique_ptr<node> head;
	node* tail;

	node* get_tail()
	{
		lock_guard<mutex> tail_l(tail_m);
		return tail;
	}

	unique_ptr<node> pop_head()
	{
		unique_ptr<node> old_head = move(head);
		head = move(old_head->next);
		return old_head;
	}

	unique_lock<mutex> wait_for_data()
	{
		unique_lock<mutex> head_l(head_m);
		cnd.wait(head_l, [&] {return head.get() != get_tail(); });
		return move(head_l);
	}

	unique_ptr<node> wait_pop_head()
	{
		unique_lock<mutex> head_l(wait_for_data());
		return pop_head();
	}

	unique_ptr<node> wait_pop_head(T& x)
	{
		unique_lock<mutex> head_l(wait_for_data());
		x = move(*head->val);
		return pop_head();
	}

	unique_ptr<node> try_pop_head()
	{
		lock_guard<mutex> head_l(head_m);
		if (head.get() == get_tail())
		{
			return unique_ptr<node>();
		}
		return pop_head();
	}

	unique_ptr<node> try_pop_head(T& x)
	{
		lock_guard<mutex> head_l(head_m);
		if (head.get() == get_tail())
		{
			return unique_ptr<node>();
		}
		x = move(*head->val);
		return pop_head();
	}
public:
	threadsafe_queue() : head(new node), tail(head.get()) {}

	threadsafe_queue(const threadsafe_queue& other) = delete;
	threadsafe_queue& operator=(const threadsafe_queue& other) = delete;

	void push(T x)
	{
		shared_ptr<T> new_val(make_shared<T>(move(x));
		unique_ptr<node> p(new node);

		{
			lock_guard<mutex> tail_l(tail_m);
			tail->val = new_val;
			node* const new_tail = p.get();
			tail->next = move(p);
			tail = new_tail;
		}

		cnd.notify_one();
	}

	shared_ptr<T> try_pop()
	{
		unique_ptr<node> old_head = try_pop_head();
		return old_head ? old_head->val : shared_ptr<T>();
	}
	bool try_pop(T& x)
	{
		unique_ptr<node> const old_head = try_pop_head(x);
		return old_head;
	}

	shared_ptr<T> wait_and_pop()
	{
		unique_ptr<node> const old_head = wait_pop_head();
		return old_head->val;
	}
	void wait_and_pop(T& x)
	{
		unique_ptr<node> const old_head = wait_pop_head(x);
	}

	bool empty();
};
```

**合并后代码**

```C++
#include <mutex>
#include <condition_variable>

using namespace std;

template<typename T>
class threadsafe_queue
{
private:
	struct node
	{
		shared_ptr<T> data;
		unique_ptr<node> next;
	};

	mutex head_m;
	unique_ptr<node> head;

	mutex tail_m;
	node* tail;

	condition_variable cnd;

	node* get_tail()
	{
		lock_guard<mutex> tail_l(tail_m);
		return tail;
	}

public:
	threadsafe_queue(): head(new node), tail(head.get()) {}

	threadsafe_queue(threadsafe_queue const& other) = delete;
	threadsafe_queue& operator=(threadsafe_queue const& other) = delete;

	shared_ptr<T> try_pop();
	bool try_pop(T& x);

	shared_ptr<T> wait_and_pop();
	void wait_and_pop(T& x);

	void push(T x);
	bool empty();
};

template<typename T>
void threadsafe_queue<T>::push(T x)
{
	shared_ptr<T> new_data(make_shared<T>(move(x)));
	unique_ptr<node> p(new node);
	
	node* const new_tail = p.get();

	{
		lock_guard<mutex> tail_l(tail_m);

		tail->data = new_data;
		tail->next = move(p);

		tail = new_tail;
	}

	cnd.notify_one();
}

template<typename T>
shared_ptr<T> threadsafe_queue<T>::wait_and_pop()
{
	unique_ptr<node> old_head;

	{
		unique_lock<mutex> head_l(head_m);
		cnd.wait(head_l, [&] {return head.get() != get_tail(); });

		old_head = move(head);
		head = move(old_head->next);
	}

	return old_head->data;
}

template<typename T>
void threadsafe_queue<T>::wait_and_pop(T& x)
{
	unique_ptr<node> old_head;

	{
		unique_lock<mutex> head_l(head_m);
		cnd.wait(head_l, [&] {return head.get() != get_tail(); });
		x = move(*head->data);

		old_head = move(head);
		head = move(old_head->next);

	}
}

template<typename T>
shared_ptr<T> threadsafe_queue<T>::try_pop()
{
	unique_ptr<node> old_head;

	{
		lock_guard<mutex> head_l(head_m);

		if (head.get() != get_tail())
		{
			old_head = move(head);
			head = move(old_head->next);
		}
	}

	return old_head ? old_head->data : shared_ptr<T>();
}

template<typename T>
bool threadsafe_queue<T>::try_pop(T& x)
{
	unique_ptr<node> old_head;

	{
		lock_guard<mutex> head_l(head_m);

		if (head.get() != get_tail())
		{
			x = move(*head->data);

			old_head = move(head);
			head = move(old_head->next);
		}
	}

	return old_head;
}

template<typename T>
bool threadsafe_queue<T>::empty()
{
	lock_guard<mutex> head_l(head_m);
	return head.get() == get_tail();
}
```

**改进**

* 将有可能抛出异常的拷贝/移动操作放在锁内
* 增加 `wait` 操作
* 控制锁的范围

**接口分析**

```C++
template<typename T>
void threadsafe_queue<T>::push(T x)
{
	shared_ptr<T> new_data(make_shared<T>(move(x)));
	unique_ptr<node> p(new node);
	
	node* const new_tail = p.get();

	{
		lock_guard<mutex> tail_l(tail_m);

		tail->data = new_data;
		tail->next = move(p);

		tail = new_tail;
	}

	cnd.notify_one();
}
```

* **互斥**: 
  * `tail_m` 仅保护 `tail` 的修改过程, 内存分配过程在锁外, `notify_one` 也在锁外, 使得唤醒线程没必要等待锁的释放
* **不变量**: 不变量的破坏被 `tail_m` 保护, 外界看不到
* **条件竞争**: 无限队列, `push()` 独立自洽
* **异常**: 
  * 内存分配过程异: 数据未改动, 不变量未破坏, STL 接口保证不会内存泄漏
  * `node* const new_tail = p.get()` 貌似有可能发生异常, 如果可能发生异常, 建议放入锁内(需要了解为什么异常要放进锁内)
  * `cnd.notify_one()` 异常: 改动已发生, 条件变量保证异常不出问题
* **死锁**: 一个互斥, 不发生死锁
* **并发程度**: 
  * 内存分配在未持锁状态下进行
  * 条件变量 `notify_one` 在未持锁状态下进行
  * 锁的粒度更精细, `push()` 仅持有 `tail_m` 互斥, 与 `pop()` 并发程度高

```C++
template<typename T>
shared_ptr<T> threadsafe_queue<T>::wait_and_pop()
{
	unique_ptr<node> old_head;

	{
		unique_lock<mutex> head_l(head_m);
		cnd.wait(head_l, [&] {return head.get() != get_tail(); });

		old_head = move(head);
		head = move(old_head->next);
	}

	return old_head->data;
}

template<typename T>
void threadsafe_queue<T>::wait_and_pop(T& x)
{
	unique_ptr<node> old_head;

	{
		unique_lock<mutex> head_l(head_m);
		cnd.wait(head_l, [&] {return head.get() != get_tail(); });
		x = move(*head->data);

		old_head = move(head);
		head = move(old_head->next);

	}
}

template<typename T>
shared_ptr<T> threadsafe_queue<T>::try_pop()
{
	unique_ptr<node> old_head;

	{
		lock_guard<mutex> head_l(head_m);

		if (head.get() != get_tail())
		{
			old_head = move(head);
			head = move(old_head->next);
		}
	}

	return old_head ? old_head->data : shared_ptr<T>();
}

template<typename T>
bool threadsafe_queue<T>::try_pop(T& x)
{
	unique_ptr<node> old_head;

	{
		lock_guard<mutex> head_l(head_m);

		if (head.get() != get_tail())
		{
			x = move(*head->data);

			old_head = move(head);
			head = move(old_head->next);
		}
	}

	return old_head;
}
```

* **互斥**: 
  * `head_m` 保护 `head` 的修改全程
  * `tail_m` 仅保护 `tail` 的读取过程
* **不变量**: `tail_m` 在 `head_m` 内使得 `head` 和 `tail` 的比较是同一时刻的, 在不变量未被破坏的情况下参与比较的, 能够得到正确的判断
* **条件竞争**: `pop()` 操作判断是否为空和返回结果, 独立自洽
* **异常**: 
  * 指针相关操作可以认为无异常
  * x 的拷贝/移动操作可能发生异常, 但在锁内
* **死锁**: 两个互斥, 但获取顺序确定, 不发生死锁
* **并发程度**: 
  * 返回值在锁外返回
  * 精细粒度的互斥, 两个互斥保护不同的部分, 分离数据的设计使得两个接口不会试图获取同一互斥, 也不会出现**两个互斥锁住同一变量并修改**的情况

## 线程安全的查找表

### 分析

* **通过迭代器访问容器元素会出现条件竞争**, 所以先删除迭代器
* 重新设计接口, 防止条件竞争, 常见手段为**接口融合**

### 接口设计

**get_value()**

* 预设返回值

  由 `key` 获取 `value`, 由于 `key` 可能不存在, 从**接口融合**的角度要兼顾 `key` 不存在的情况

  可以由用户预设一个值, `key` 不存在时返回该值

* 返回智能指针

  返回智能指针, 如果 `key` 存在, 返回其指针, 如果不存在就返回空指针

### 代码

```C++
#include <vector>
#include <list>
#include <map>
#include <mutex>
#include <shared_mutex>

using namespace std;

template<typename Key, typename Value, typename Hash = hash<Key>>
class threadsafe_lookup_table
{
private:
	class bucket
	{
	private:
		typedef pair<Key, Value> node;
		typedef typename list<node>::iterator bucket_iterator;	// typename 专门强调是类型

		list<node> data;
		mutable shared_mutex m;

		bucket_iterator find_entity(Key const& key) const
		{
			return find_if(data.begin(), data.end(),
				[&](node const& item) 
				{return item.first == key; });
		}

	public:
		Value get_value(Key const& key, Value const& default_value) const
		{
			shared_lock<shared_mutex> l(m);

			bucket_iterator const it = find_entity(key);

			return it == data.end() ? default_value : it->second;
		}

		void add_or_update_mapping(Key const& key, Value const& value)
		{
			lock_guard<shared_mutex> l(m);

			bucket_iterator const it = find_entity(key);

			if (it == data.end())
			{
				data.push_back(node(key, value));
			}
			else
			{
				it->second = value;
			}
		}

		void remove_mapping(Key const& key)
		{
			lock_guard<shared_mutex> l(m);

			bucket_iterator const it = find_entity(key);

			if (it != data.end())
			{
				data.erase(it);
			}
		}
	};

	vector<unique_ptr<bucket>> buckets;
	Hash hasher;

	bucket& get_bucket(Key const& key) const
	{
		size_t const idx = hasher(key) % buckets.size();
		return *buckets[idx];
	}

public:
	threadsafe_lookup_table(
		int num = 19, Hash const& hasher_ = Hash()
	) : buckets(num), hasher(hasher_)
	{
		for (int i = 0; i < num; i++)
			buckets[i].reset(new bucket);
	}

	threadsafe_lookup_table(threadsafe_lookup_table const& other) = delete;
	threadsafe_lookup_table& operator=(threadsafe_lookup_table const& other) = delete;

	Value get_value(Key const& key, Value const& default_value) const
	{
		return get_bucket(key).get_value(key, default_value);
	}

	void add_or_update_mapping(Key const& key, Value const& value)
	{
		get_bucket(key).add_or_update_mapping(key, value);
	}

	void remove_mapping(Key const& key)
	{
		get_bucket(key).remove_mapping(key);
	}

	map<Key, Value> get_map() const
	{
		map<Key, Value> res;
		{
			vector<shared_lock<shared_mutex>> locks;
			for (int i = 0; i < buckets.size(); i++)
				locks.push_back(shared_lock<shared_mutex>(buckets[i].m));

			for (int i = 0; i < buckets.size(); i++)
				for (bucket_iterator it = buckets[i].data.begin();
					it != buckets[i].data.end(); ++it)
					res.insert(*it);
		}
		return res;
	}
};
```

### 改进

* 桶的数目由构造函数设置, 桶的数目固定, 所以访问具体的桶不需要考虑该桶的不变量是否成立, 即不需要加锁, 但在访问桶的 data 时还是要加锁的
* 多个互斥保护不同部分
* 读写锁, 提高并发程度 n 倍
* 重设接口, 消除接口上的条件竞争

### 接口分析

**bucket 接口**

```C++
bucket_iterator find_entity(Key const& key) const
{
    return find_if(data.begin(), data.end(),
                   [&](node const& item) 
                   {return item.first == key; });
}
```

**private** 接口, 供外部接口访问, 不需要加锁

```C++
Value get_value(Key const& key, Value const& default_value) const
{
    shared_lock<shared_mutex> l(m);

    bucket_iterator const it = find_entity(key);

    return it == data.end() ? default_value : it->second;
}
```

读操作

* **互斥**: `shared_mutex` 保护读取过程, **包含返回语句**, 防止出现迭代器指向的节点失效的情况
* **不变量**: 返回语句也在锁的保护范围, **保护迭代器的不变量不被破化**
* **条件竞争**: 自行判断是否含有 `key`, 且由 `default_value` 表明是否查找到, 独立自洽, 没有条件竞争
* **异常**: 
  * 读操作不改动数据, 即使抛出异常也不影响数据结构, 可以认为无异常
  * 假定加锁解锁无异常
  * `find_entity()` 由 `std::find_if()` 保证无异常, 迭代器的拷贝操作无异常
* **死锁**: 一个互斥, 无死锁
* **并发程度**: 
  * 读写锁, 并发程度高
  * 多互斥保护不同部分, 并发程度高

```C++
void add_or_update_mapping(Key const& key, Value const& value)
{
    lock_guard<shared_mutex> l(m);

    bucket_iterator const it = find_entity(key);

    if (it == data.end())
    {
        data.push_back(node(key, value));
    }
    else
    {
        it->second = value;
    }
}
```

写操作

* **互斥**: `shared_mutex` 保护查询和修改操作
* **不变量**: 整个修改过程被排他锁保护, 不变量的破坏不会被其他线程看到
* **条件竞争**: 接口自行判断是否找到 `key`, 独立自洽, 无条件竞争
* **异常**:
  * `data.push_back()` 异常: `list` 保证即使异常也不会改变数据结构, 所以异常安全
  * `it->second = value;` 异常: 可能会出问题, 留给使用者处理
* **死锁**: 一个互斥
* **并发程度**:
  * 排他锁, 并发程度低
  * 多互斥, 并发程度高

```C++
void remove_mapping(Key const& key)
{
    lock_guard<shared_mutex> l(m);

    bucket_iterator const it = find_entity(key);

    if (it != data.end())
    {
        data.erase(it);
    }
}
```

写操作

* **互斥**: `shared_mutex` 保护查询和修改操作
* **不变量**: 整个修改过程被排他锁保护, 不变量的破坏不会被其他线程看到
* **条件竞争**: 接口自行判断是否找到 `key`, 独立自洽, 无条件竞争
* **异常**:
  * `data.erase()` 异常: `list` 保证即使异常也不会改变数据结构, 所以异常安全
* **死锁**: 一个互斥
* **并发程度**:
  * 排他锁, 并发程度低
  * 多互斥, 并发程度高

**threadsafe_lookup_table 接口**

```C++
bucket& get_bucket(Key const& key) const
{
    size_t const idx = hasher(key) % buckets.size();
    return *buckets[idx];
}
```

桶的数目固定, 内存地址也不会变化, 所以尽管内容会变化, 但接口只需要返回引用, 所以不需要加锁

```C++
Value get_value(Key const& key, Value const& default_value) const
{
    return get_bucket(key).get_value(key, default_value);
}

void add_or_update_mapping(Key const& key, Value const& value)
{
    get_bucket(key).add_or_update_mapping(key, value);
}

void remove_mapping(Key const& key)
{
    get_bucket(key).remove_mapping(key);
}
```

**get_bucket()** 无需保护, 结果固定

连锁调用后的那个函数会自行加锁保护其过程, 且返回过程不需要锁的保护

```C++
map<Key, Value> get_map() const
{
    map<Key, Value> res;
    {
        vector<shared_lock<shared_mutex>> locks;
        for (int i = 0; i < buckets.size(); i++)
            push_back(shared_lock<shared_mutex>(buckets[i].m));

        for (int i = 0; i < buckets.size(); i++)
            for (bucket_iterator it = buckets[i].data.begin();
                it != buckets[i].data.end(); ++it)
                res.insert(*it);
    }
    return res;
}
```

读操作

* **互斥**: `shared_mutex` 保护每个 `bucket` 的查询操作
* **不变量**: 整个修改过程被所有共享锁保护, 不变量的破坏不会被其他线程看到
* **条件竞争**: 接口自行访问所有数据, 独立自洽, 无条件竞争
* **异常**:
  * `push_back()` 异常: 数据未改动, 且 `shared_lock` 保证正确解锁, 所以异常安全
  * `res.insert(*it);` 异常: 数据未改动, 数据未被破坏, 仅返回值可能不正确, 异常留给使用者处理
* **死锁**: 多个互斥, 按照桶的序号获取, 无死锁
* **并发程度**:
  * 共享, 并发程度高
  * 多互斥, 并发程度高
  * `res` 的初始化和返回都在锁外, 并发程度高

## 支持迭代的线程安全链表

### 分析

迭代器的生命周期不由容器控制, 不应向外提供迭代器, 因为:

* 外部迭代器指向内部, 但内部却会被修改, 导致**迭代器可能在不变量被破坏的时刻访问容器内部**, **除非迭代器持有锁**

考虑让容器以接口形式提供迭代功能, 运行用户提供的函数, 但有可能造成**死锁和条件竞争**

考虑对每个结点分别使用一个互斥来保护, 能够提高并发程度

### 代码

```C++
#include <mutex>
#include <memory>

using namespace std;

template<typename T>
class threadsafe_list
{
	struct node
	{
		mutex m;
		shared_ptr<T> data;
		unique_ptr<node> next;

		node() {}

		node(T const& x) : data(make_shared<T>(x)) {}
	};

	node head;

public:
	threadsafe_list() {}
	~threadsafe_list()
	{
		remove_if([](node const&) {return true; });
	}

	threadsafe_list(threadsafe_list const& other) = delete;
	threadsafe_list& operator=(threadsafe_list const& other) = delete;

	void push_front(T const& x)
	{
		unique_ptr<node> p(new node(x));

		lock_guard<mutex> l(head.m);

		p->next = move(head.next);
		head.next = move(p);
	}

	template<typename Function>
	void for_each(Function func)
	{
		node* pre = &head;		// pre 表示已经处理完的结点, head 不需要处理, 相当于处理完
		unique_lock<mutex> pre_l(head.m);
			
		while (node* const cur = pre->next.get())	// cur 表示当前要处理的结点
		{
			unique_lock<mutex> cur_l(cur->m);
			pre_l.unlock();

			func(*cur->data);
			pre = cur;
			pre_l = move(cur_l);
		}
	}

	template<typename Predicate>
	shared_ptr<T> find_first_if(Predicate predic)
	{
		node* pre = &head;
		unique_lock<mutex> pre_l(head.m);

		while (node* const cur = pre->next.get())
		{
			unique_lock<mutex> cur_l(cur->m);
			pre_l.unlock();

			if (predic(*cur->data))
			{
				return cur->data;
			}

			pre = cur;
			pre_l = move(cur_l);
		}
		return shared_ptr<T>();
	}

	template<typename Predicate>
	void remove_if(Predicate predic)
	{
		node* pre = &head;
		unique_lock<mutex> pre_l(head.m);

		while (node* const cur = pre->next.get())
		{
			unique_lock<mutex> cur_l(cur->m);

			if (predic(*cur->data))
			{
				unique_ptr<node> old_next = move(pre->next);

				pre->next = move(cur->next);

				cur_l.unlock();
			}
			else
			{
				pre_l.unlock();
				pre = cur;
				pre_l = move(cur_l);
			}
		}
	}
};
```

### 接口分析

```C++
void push_front(T const& x)
{
    unique_ptr<node> p(new node(x));

    lock_guard<mutex> l(head.m);

    p->next = move(head.next);
    head.next = move(p);
}
```

* **互斥**: 由于要修改 `head`, 所以只需在 head 的修改期间锁住 `head.m`
* **不变量**: `head` 不变量的破坏在 `head.m` 的保护范围
* **条件竞争**: 
  * `push_front()` 无需别的接口判断, 独立自洽
  * 没有调用用户自定义接口, 不会向外传递内部数据的引用和指针
* **异常**:
  * 内存分配异常: 数据未改动, `unique_ptr` 保证无内存泄漏, 异常安全
  * `unique_ptr` 移动操作可以认为异常安全
* **死锁**:
  * 一个互斥
  * 无调用用户接口, 不会发生嵌套锁
* **并发程度**
  * 内存分配在锁外进行, 并发程度高
  * 每个结点单独一个互斥, 并发程度高

```C++
template<typename Function>
void for_each(Function func)
{
    node* pre = &head;		// pre 表示已经处理完的结点, head 不需要处理, 相当于处理完
    unique_lock<mutex> pre_l(head.m);
        
    while (node* const cur = pre->next.get())	// cur 表示当前要处理的结点
    {
        unique_lock<mutex> cur_l(cur->m);
        pre_l.unlock();

        func(*cur->data);
        pre = cur;
        pre_l = move(cur_l);
    }
}
```

* **互斥**: 从 `head.m` 开始轮流获取紧挨着的两个互斥(`pre->m` 和 `cur->m`)直到下一互斥获取之后且数据处理完后, 再释放当前互斥

* **不变量**

  * `pre` 节点由 `pre->m` 保护, 操作为读取

  * `cur` 节点由 `cur->m` 保护, 操作为访问(可能读也可能写, 取决于 func 函数), 下一循环的条件判断那里需要访问 `get()` 成员函数, 并判断是否执行处理

    > 下一循环 cur 就变为 pre 了, cur_l 就变为 pre_l 了

* **条件竞争**

  * 与其余接口可能存在条件竞争, 但由于从head 开始按顺序加锁, 所以不会出现死锁和条件竞争
  * 接口本身独立自洽, 但却会**访问用户提供的函数**, 导致可能向外传递内部数据的引用, 从而产生数据竞争, 即条件竞争

* **异常**

  * 仅用户提供的接口可能出异常
  * 其余操作都是指针操作, 可以认为无异常

* **死锁**

  * 多互斥, 容易死锁, 但按规定顺序持锁, 所以无问题
  * 用户提供的接口可能获取锁, 导致出现嵌套所, 可能会死锁

* **并发程度**

  * 多互斥, 并发程度高
  * head 地址不变, 放在锁外, 并发程度高
  * 获取到 `cur->m` 且处理完 `pre` 后, `pre->m` 就解锁, 并发程度高

```C++
template<typename Predicate>
shared_ptr<T> find_first_if(Predicate predic)
{
    node* pre = &head;
    unique_lock<mutex> pre_l(head.m);

    while (node* const cur = pre->next.get())
    {
        unique_lock<mutex> cur_l(cur->m);
        pre_l.unlock();

        if (predic(*cur->data))
        {
            return cur->data;
        }

        pre = cur;
        pre_l = move(cur_l);
    }
    return shared_ptr<T>();
}
```

* **互斥**:  `pre->m` 保护 pre 节点的访问,  `cur->m` 保护 cur 节点的访问
* **不变量**: `cur->m` 的获取在 `pre->m` 的保护范围内, 获取后 `pre->m` 再解锁, 故不变量未被破坏
* **条件竞争**: 
  * 按顺序获取互斥, 无条件竞争
  * 用户接口可能向外传递引用, 可能存在数据竞争
* **异常**:
  * 用户接口可能有异常
* **死锁**
  * 按顺序获取互斥, 无死锁
  * 用户接口可能获取锁, 造成嵌套锁, 可能会死锁
* **并发程度**
  * 多互斥
  * 不必要操作在锁外
  * `return cur->data;` 在锁内, 可能优化到锁外比较好

```C++
template<typename Predicate>
void remove_if(Predicate predic)
{
    node* pre = &head;
    unique_lock<mutex> pre_l(head.m);

    while (node* const cur = pre->next.get())
    {
        unique_lock<mutex> cur_l(cur->m);

        if (predic(*cur->data))
        {
            unique_ptr<node> old_next = move(pre->next);

            pre->next = move(cur->next);

            cur_l.unlock();
        }
        else
        {
            pre_l.unlock();
            pre = cur;
            pre_l = move(cur_l);
        }
    }
}
```

* **互斥**:  `pre->m` 保护 pre 节点的访问,  `cur->m` 保护 cur 节点的访问

* **不变量**: `cur->m` 的获取在 `pre->m` 的保护范围内, 然后判断 if 语句的条件:

  * 成立时: 删除 cur 节点, 故要同时修改 `pre` 和 `cur` 节点, 所以要同时持有两个锁, 修改完后, cur 节点认为被删除, `cur->m` 解锁, `pre->m` 依旧保护 pre 节点, `pre` 和 `pre_l` 不需要做修改

    > cur 节点真正销毁是在 `cur->m` 解锁后, 可能会引发条件竞争, 但由于已经持有 `pre->m`, 所以不可能有线程越过 pre 节点访问 cur 节点, 所以不变量未被破坏, 不会引发条件竞争

  * 不成立时:

    认为 cur 节点处理完, 正常将 pre 和 pre_l 置为 cur 节点的值和锁

* **条件竞争**: 

  * 按顺序获取互斥, 无条件竞争
  * 用户接口可能向外传递引用, 可能存在数据竞争
  * cur 节点的销毁在锁外, 可能引发条件竞争(解锁之后, 销毁之前其余线程访问 cur 节点), 但由于当前正持有 `pre->m`, 不会有其余线程越过 pre 节点在 cur 节点上获取锁, 所以不会发生条件竞争

* **异常**:

  * 用户接口可能有异常

* **死锁**

  * 按顺序获取互斥, 无死锁
  * 用户接口可能获取锁, 造成嵌套锁, 可能会死锁

* **并发程度**

  * 多互斥, 按顺序加锁
  * 不必要操作在锁外

