# C++ 面试问题

### new 和 malloc 区别

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

| 比较     | new                                          | malloc             |
| -------- | -------------------------------------------- | ------------------ |
| 输入     | 类型, 自动计算字节数                         | 字节数             |
| 输出     | 类型                                         | void *             |
| 类型     | 运算符                                       | 库函数             |
| 分配失败 | 抛出异常                                     | 返回 NULL          |
| 过程     | 先底层调用 malloc 申请内存, 然后调用构造函数 | 向操作系统申请内存 |

> delete 先调用析构函数, 再底层调用 free 释放内存

```C++
#include <iostream>

using namespace std;


int main()
{
    // int *p = (int *) malloc(16);
    int n = 101;
    // for(int i = 0; i < n; i++) p[i] = i;
    // for(int i = 0; i < n; i++) cout << p[i] << " ";
    // cout << endl;

    // while(p)
    //     free(p);

    int *q = new int[100];
    for(int i = 0; i < n; i++) q[i] = i;
    for(int i = 0; i < n; i++) cout << q[i] << " ";
    cout << endl;

    delete [] q;

    cout << "YES" << endl;

    return 0;
}
```

