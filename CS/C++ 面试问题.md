# C++ 面试问题

### C++ 内存分区

五大区: 堆, 栈, 静态存储区, 常量存储区, 代码区

**栈(stack)**: 编译器控制

**堆(heap)**: 程序员控制

**静态区(static)**: 全局变量和静态变量, 细分为**初始化区**和**未初始化区**

**文字常量区(const)**: 常量字符串

**代码区**: 程序二进制代码

### new 和 malloc

**new 语法**

`new type `

`:: new (placement_params) type initializer`

**new 用法**

```C++
// 变量
int *p = new int;	// 默认初始化: 内置类型和组合类型的值未定义, 类类型调用默认构造函数
int *p = new int();	// 值初始化
delete p;

// 一维数组
int *p = new int[10];
delete [] p;

// 二维数组
int **p = new int* [n];
for(int i = 0; i < n; i++)
    p[i] = new int [m];

for(int i = 0; i < n; i++)
    delete [] p[i];
delete [] p;
```

> 内置类型默认初始化: 由定义的位置决定, **函数内未定义**, **函数外为 0**

```C++
#include <iostream>

using namespace std;

int main()
{
    int n, m;
    cin >> n >> m;

    // 申请二维数组
    int **f = new int* [n];
    for(int i = 0; i < n; i++)
        f[i] = new int [m];

    // 使用二维数组
    for(int i = 0, k = 0; i < n; i++)
        for(int j = 0; j < m; j++, k ++)
            f[i][j] = k;
    for(int i = 0; i < n; i++)
    {
        for(int j = 0; j < m; j++) 
            cout << f[i][j] << " ";
        cout << endl;
    }

    // 销毁二维数组
    for(int i = 0; i < n; i++)
        delete [] f[i];
    delete [] f;
    
    return 0;
}
```

**new 过程**

**1. 内置类型**

内置类型**没有构造和析构**

new : operator new

new[] : 计算大小后, 调用 operator new

delete : operator delete

delete[] : operator delete

**2. 类类型**

**new** 

1. **operator new** 申请内存
2. **构造函数**

new []

1. operator new[] 申请内存
2. 在前四个字节写入数组大小 n
3. 调用 n 次构造函数

**delete** 

1. **析构函数**
2. **operator delete** 释放内存

delete []

1. 取出前四个字节存储的数组大小 n
2. 调用 n 次析构函数
3. operator delete [] 释放内存

```C++
class A
{
public:
    A()
    { 
        cout << "A 的构造函数" << endl; 
    }
    
    ~A()
    { 
        cout << "A 的析构函数" << endl;
    }

    void * operator new(size_t sz)
    {
        A* res = (A*) malloc(sizeof(A));
        cout << "申请内存" << endl;

        return res;
    }

    void operator delete(void * p)
    {
        free(p);
        cout << "释放内存" << endl;
    }
};

// 申请内存
// A 的构造函数
// A 的析构函数
// 释放内存
```



**malloc**

```C++
void* malloc(size_t size);	// size 以字节为单位

int *p = (int *) malloc(n * sizeof(int));	
free(p);
```

**new 和 malloc 比较**

``` C++
int *p = new int;
void* malloc(size_t size);	// size 以字节为单位
```

new 更高级, malloc 更底层

new 做的更全面, malloc 做的更基本

**1. 输入**

new: **类型**, 自动计算字节数

malloc: **字节数**

**2. 输出**

new: **类型**

malloc: **void ***

| 比较         | new                                          | malloc                 |
| ------------ | -------------------------------------------- | ---------------------- |
| 类型         | **运算符**                                   | **库函数**             |
| 输入         | **类型**, 自动计算字节数                     | **字节数**             |
| 输出         | **类型**, 类型安全                           | **void ***, 类型不安全 |
| 申请内存位置 | **自由存储区** free srore (堆/静态存储区/空) | **堆**                 |
| 分配失败     | 抛出**异常**                                 | 返回 **NULL**          |
| 过程         | 先底层调用 malloc 申请内存, 然后调用构造函数 | 向操作系统申请内存     |

> delete 先调用析构函数, 再底层调用 free 释放内存

**注意事项**

```C++
// new 和 malloc 申请内存后, 可以访问到未申请的内存区域, 但在 delete 和 free 时会出错
int *p = new int[2];
p[2] = 2;

delete [] p;	// delete 时会出错, 因为访问到了未申请的区域
```

### new operator 和 operator new

**new** 运算符

1. 先调用函数 **operator new** **申请内存**
2. 再调用**构造函数**初始化内存

> operator new 一般用 malloc 实现
>
> delete 先调用析构函数, 再调用 operator delete 函数释放内存
>
> operator delete 一般用 free 实现

|        | new 运算符                                           | 函数 operator new                           |
| ------ | ---------------------------------------------------- | ------------------------------------------- |
| 类型   | 内置运算符, 无法改变行为                             | 函数, 可以重载, 类似于 malloc, 但更高级一点 |
| 编译器 | 改写成两个函数: 一个 operator new 函数, 一个构造函数 | 一个函数                                    |

### placement new

new 分两步

1. 调用 operator new **申请内存**
2. 调用**构造函数**

placement new 是传入可选参数 `placement_params` 的 new, 但并不申请内存, **只调用构造函数**

作用:  申请内存后, **可以重复构造和析构该内存**, **节省多次申请和释放内存的开销**

```C++
class A
{
public:
    A()
    { 
        cout << "A 的构造函数" << endl; 
    }
    
    ~A()
    { 
        cout << "A 的析构函数" << endl;
    }
};

char *mem = new char[n * sizeof(A)];	// 申请内存

A *p = new (mem) A;		// placement new, 调用构造函数

p->~A();				// 显示调用析构函数

delete [] mem;			// 释放内存
```

### delete [] 如何知道 delete 多大的数组

delete [] 类类型数组

new [] 类类型数组时, 在**前四个字节记录数组大小 n**, delete [] 时**取出数组大小**, **调用 n 次析构函数**, 然后调用 operator delete [] 释放内存

### 内存泄漏

由于种种原因, **没有释放内存**, 造成内存浪费

**原因**

1. 没有 delete

   ```C++
   {
       int *p = new int;
   	p = nullptr;
    	// p 出局部作用域时销毁了    
   }
   ```

2. new [] 时调用 delete

   此时只会释放了第一个对象的内存空间, 正常应该调用 delete []

3. new 时调用 free

   这种方式会**少一步调用析构函数**, 析构函数会释放成员变量

4. 循环引用

**防止**

1. 尽量使用**系统栈**分配内存

2. **正确释放**

   malloc 对应 free

   new 对应 delete

   new [] 对应 delete []

3. **避免使用裸指针**

4. 尽量使用 **STL**

   string 代替 char *

   vector 代替数组

   make_shared 代替 new

5. 尽量使用**智能指针**

**检测是否泄漏**

1. **打印地址**

   分配和释放内存的时候**打印地址**, 计算**分配的内存大小**是否等于**释放的内存大小**

2. **统计次数**

   统计分配和释放的次数是否相等

3. **工具**



### 堆区和栈区的区别

**堆**: 编译器控制, 存放函数参数, 局部变量, 类似与**数据结构中的栈**

| 区别     | 栈(stack)                    | 堆(heap)                 |
| -------- | ---------------------------- | ------------------------ |
| 控制     | 编译器, **系统自动分配释放** | 程序员, **手动申请释放** |
| 内容     | 函数参数, 局部变量           |                          |
| 数据结构 | 栈                           | 链表                     |
| 方向     | 由高到低增长                 | 由低到高增长             |
| 效率     | 高                           | 低                       |

> 操作系统有一个记录空闲内存地址的链表
>
> 堆会遍历链表, 找到第一个大于申请空间的堆节点, 然后将该节点分配给程序
