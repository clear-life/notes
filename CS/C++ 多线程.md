### 线程与进程

共享**代码段, 堆段**, 独占**栈段**

```C++
void fun()
{
	cnt++;    
}
```

`cnt++` 翻译为汇编语言有三步:

1. 取数 `mov 0x3f %eax`
2. 计算 `add 1 %eax`
3. 存回 `mov %eax 0x3f `

**有线程 A 和 B**

线程 A 在**第二步**完成后切换到线程 B

>  此时内存中 cnt 的值为 0, 寄存器中的值为 1

线程 B 执行完三步, 存回内存的值为 1

再切换到线程 A 执行存回操作, 存回内存的值也为 1

**问题:** 本应该加两次的 cnt 只加了一次, 原因在于**多线程对临界区访问**造成的问题

### bind

函数适配器, 接受一个可调用对象 `callable`, 生成一个新的可调用对象 `newCallable`

**普通函数**

```C++
void fun(int a, int b, int c);

auto f = bind(fun, _2, 2, _1);	
// _1 表明是新调用对象 f 的第一个参数, _2 表明是新调用对象 f 的第二个参数

f(1, 3);
// 调用 fun(3, 2, 1)
```

**类成员函数**

绑定对象的 `this` 指针

```C++
class A
{
public:
    void fun(int a, int b, int c);
}

A a;
auto f = bind(&A::fun, &a, _2, 2, _1);
```

### std::move

**将左值转化为右值引用**, 转移对象的所有权

### 右值引用

引用方式使用右值，**右值引用变量本身是左值**

过程: 生成一个匿名变量, 然后引用该变量

```assembly
int&& a = 1;
mov dword ptr [rbp+24h],1	; 匿名变量
lea rax,[rbp+24h]  			; 匿名变量的地址
mov qword ptr [a],rax		; a 的本质是指针
```

**判断左值引用和右值引用**

```C++
if(std::is_lvalue_reference<decltype(v)>::value) 
    cout << "左值引用" << endl;
if(std::is_rvalue_reference<decltype(v)>::value)
    cout << "右值引用" << endl;
```

### 汇编

引用本质就是指针

```assembly
int b = 1;
mov dword ptr [b],1  	; dword ptr [b] 双字节指针，内存地址为 b 的数据


; 左值引用
int& a = b;
lea rax,[b]  		
mov qword ptr [a],rax 	; a 的本质是 b 的指针

; 指针
int* a = &b;
lea rax,[b]  
mov qword ptr [a],rax  ; a 是 b 的指针

; 右值引用
int&& a = 1;
mov dword ptr [rbp+24h],1; 匿名变量
lea rax,[rbp+24h]  		; 匿名变量的地址
mov qword ptr [a],rax	; a 的本质还是指针

; 右值
int a = b + 1;
mov eax,dword ptr [b]  
inc eax  				; 右值就是在寄存器里的值
mov dword ptr [a],eax 	
```

结论:

1. **左值引用和右值引用的本质都是指针**
2. **右值本质是寄存器的值**
3. 右值引用**生成一个匿名变量**，然后用指针指向该匿名变量

### 寄存器寻址方式

1. 立即寻址	立即数
2. 寄存器寻址 寄存器
3. 直接寻址     寄存器存 = 地址
4. 段寄存器/基址寄存器 + 变址寄存器 + 偏移量 = 地址

### 完美转发

> [现代C++之万能引用、完美转发、引用折叠](https://zhuanlan.zhihu.com/p/99524127)

函数模板往下传递参数时，**保留被转发参数的左、右值属性**

> 左值依然是左值，右值依然是右值

**1. 左右值接收**

**函数模板**中的**右值引用**是**万能引用**， 既能接收右值，也能接收左值

> 万能引用表示: 既可能是 && 又可能是 & 
>
> 若一个变量或者参数被声明为**T&&**，其中T是**被推导**的类型，该变量或者参数就是一个万能引用。

```C++
template <typename T>
void fun(T&& t)
{
    fun2(t);
}
```

**引用折叠**

`A&& &&` 变为 `A&&`

`A& &&` 变为 `A&`     

**2. 传递左右值属性**

模板函数 `forward<T>()` 连同左右值属性传递给函数

```C++
template <typename T>
void fun(T&& t)
{
    fun2(forward<T>(t));
}
```

```C++
#include <iostream>

using namespace std;

template<typename T>
void print(T& t) {
    cout << "左值" << endl;
}
template<typename T>
void print(T&& t) {
    cout << "右值" << endl;
}

template<typename T>
void TestForward(T&& v) {
    if (std::is_lvalue_reference<decltype(v)>::value)
        cout << "左值引用" << endl;
    if (std::is_rvalue_reference<decltype(v)>::value)
        cout << "右值引用" << endl;
    print(v);                   // 无论如何都传递左值
    print(std::forward<T>(v));  // 传递实参的左右值属性
    print(std::move(v));        // 无论如何都传递右值
    cout << endl;
}

int main() {
    int&& x = 1;
    TestForward(x);             // 右值引用变量本身是左值
    int y = 2;
    TestForward(y);
    return 0;
}
```

