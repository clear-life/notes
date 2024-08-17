# C++ 速成

导入模块 

`import <iostream>;`

> C++20



预处理指令

```C++
#include [file]
#define [id] [value]

#ifndef [id]
#endif

#pragma once
```



main() 函数

```C++
int main(int argc, char* argv[])
```



命名空间

```C++
using namespace ::std;		// 命名空间
using namespace ::std::cin;	// 命名空间成员

namespace A = B;	// 别名
```



统一初始化

```C++
[type] [id] {value};
```

> 零初始化 `[type] [id] {};`

 

指派初始化器

```
[type] [id] {
	.a = [value],
	.b = [value]
}
```





类型转换

```C++
static_cast<type>(var);
```



运算符

* 算术运算符
* 位运算符
* 关系运算符
* 逻辑运算符



枚举类型

```C++
enum class [id] {};
```



```C++
if (<初始化>; <条件表达式>)
switch (<初始化>; <表达式>)
```



三向比较运算符

```C++
strong_ordering res { A <=> B };

strong_ordering :: less greater equal
partial_ordering :: less greater equivalent unordered
weak_ordering :: less greater equivalent
```



 函数重载

参数数量或类型不同



属性

控制编译器的信息



范围for循环

```C++
for (<初始化>; 循环变量 : 容器)
```



初始化列表

```C++
int Sum(initializer_list<int> q)
{
	int res { 0 };
	for (int i : q)
		res += i;
	return res;
}
```



指针

```C++
int *p { new int };

delete p;
p = nullptr;
```



const

```
const int *p;
int const *p;
int * const p;
```

> const 作用于其直接左侧



constexpr

编译期求值

```C++
constexpr int fun() { return 2; }
```



别名

```
using A = B;
```



引用

```
const int &b { a };
int * &p { a };
```

