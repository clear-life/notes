# C++ 面试问题

### new 和 malloc 区别

#### new

```C++
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

