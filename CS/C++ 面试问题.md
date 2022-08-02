# C++ 面试问题

### new 和 malloc

#### new 语法

`new type `

`:: new (placement_params) type initializer`

#### new

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

#### malloc

```C++
void* malloc(size_t size);	// size 以字节为单位

int *p = (int *) malloc(n * sizeof(int));	
free(p);
```

#### new 和 malloc 比较

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

#### 注意事项

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

|        | new 运算符                                           | 函数 operator new                           |
| ------ | ---------------------------------------------------- | ------------------------------------------- |
| 类型   | 内置运算符, 无法改变行为                             | 函数, 可以重载, 类似于 malloc, 但更高级一点 |
| 编译器 | 改写成两个函数: 一个 operator new 函数, 一个构造函数 | 一个函数                                    |

### placement new

new 分两步: 调用 operator new **申请内存**, 调用**构造函数**

placement new 是传入可选参数 `placement_params` 的 new, 但并不申请内存, **只调用构造函数**

作用:  申请内存后, **可以重复构造和析构该内存**, **节省多次申请和释放内存的开销**

```C++

```

