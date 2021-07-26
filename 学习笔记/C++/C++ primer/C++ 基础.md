# C++ 基础

## 第 2 章	变量和基本类型

### 2.1 基本内置类型

### 2.2 变量

#### 2.2.1 变量定义

```C++
类型说明符	变量名列表;
int		a, b;
```

#### 2.2.2 声明与定义

* 声明: 告诉编译器变量类型与变量名标识符
* 定义: 分配对应的内存空间

```C++
extern	int a	声明 a
int	a; 			声明并定义 a
```



### 2.3 复合类型

基于其他类型定义的类型	例: 引用和指针

声明语句:	
**基本数据类型**	**声明符**列表

> 声明符: 命名一个变量并指定与基本数据类型有关的类型 



#### 2.3.1 引用

引用: 给对象**起别名**

```C++
int a = 1;
int &b = a;		
1. b 一直绑定到 a
2. 引用必须用对象初始化
3. b 本身不是对象, 只是 a 的别名
```



#### 2.3.2 指针

指针: **指向**另一个对象

```C++
int a = 1;
int *p = &a;
1. p 是一个指针对象, 含有 a 的地址与类型信息
```



**空指针**

```C++
int *p = 0;
int *p = NULL;
int *p = nullptr;
```



**void 指针**

```C++
int a;
void *p = &a;
p 仅含有 a 的地址信息
```



#### 2.3.3 理解复合类型的声明

声明: 

基本数据类型 	声明符列表

> 类型修饰符(* 和 &)仅仅是声明符的一部分, 影响范围仅仅是一个声明符

```C++
int a;
int b, *p = &a, &c = a;
基本数据类型  int
声明符列表  b *p &c
类型修饰符  * & 
b 整形对象	p 指针对象	c 引用
```



**嵌套**

```C++
int a = 1;
int *p = &a;
int **pp = &p;
pp 是指向指针对象 p 的指针对象(二级)


int a = 1;
int *p;
int *&b = p;
b 是对指针对象 p 的引用, 即 b 是 p 的别名

b = &a;		p 指向 a
*b = 0;		a 赋值为 0
```

> 理解 int *&b = p; 
>
> **从右往左**读 b 的定义, 先遇到 & 表明 b 是一个引用, 后遇到 * 表明表明引用的对象是一个指针



### 2.4 const 限定符

**将内存地址设为只读**

```C++
const int a = 1;		编译时初始化
const int a = fun();	运行时初始化
```



#### 2.4.1 const 的引用

**const 的目标是引用的对象**

**对 const 对象的引用** : 引用本身也必须是 const

```C++
const int a = 1;
const int &b = a;	const 表明通过 b 对内存空间的操作是只读的
```



**const 的目标是引用本身**

**引用是 const** 

```C++
int a = 1;
const int &b = a;
int &c = a;
b = 0;	错误, b 是一个 const 引用
c = 0;	正确, c 不是一个 const 引用
1. 允许将 const int & 绑定到普通 int 对象上
2. 此时不能通过 b 来修改 a 的值, 但能通过其他途径修改 a 的值
```



#### 2.4.2 指针和 const

**指向 const 对象** : 指针本身也必须是 const 

```C++
int a = 1;
const int *p = &a;
通过 p 对 a 内存空间的权限是只读的
```

**指针是 const**

```C++
int a = 0;
int *const b = &a;
1. const 占主导表明是 const 对象, * 表示对象的类型是指针
2. b 永远指向 a,  b 对象本身的内存空间是只读的

const int a = 0;
const int *const b = &a;
1. b 是一个 const 指针, 指向 const 对象 a
2. a, b 的内存空间都是只读的
```

#### 2.4.3 顶层 const 与 底层const

* 顶层 `const` : 对象本身是 `const`

* 底层 `const` : 指向或引用的对象是 `const`

  ```C++
  int a = 0;
  int *const p = &a;	
  // p 为 const 对象(顶层 const ), 可以被 &a 初始化, 但之后不能被赋值
  
  const int b = a;	
  // b 为 const 对象(顶层 const )
  
  const int *p = &b;
  1. p 为指向 const 对象的指针(底层 const ), 不能通过 p 来修改 b 的值 
  2. 允许修改 p 的值, 但不能通过 p 来修改 b 的值 
  3. 给 p 赋值的类型必须为 const int *, 不能是 int *
  
  const int &r = b;
  // r 引用 const 对象(底层 const), 不能通过 r 来修改 b

* 底层 `const` 赋值必须类型一致

  ```C++
  int *p;
  const int *p1;
  const int *p2;
  p2 = p1;
  // p1 和 p2 都是底层 const, 可以相互赋值
  
  p = p1;
  //错误, p 为 int * 类型, p1 为 const int * 类型
  不能把 p1 赋值给 p

* 顶层 `const` 可以类型不一致

### 2.5 处理类型

#### 2.5.1 类型别名

**`typedef`** 与 **`using`**

```C++
typedef int integer;
using integer = int;

// a 是 int [10] 的类型别名
typedef int a[10];		
using a = int [10];
```

#### 2.5.3 decltype 类型指示符

**decltype**: 选择并返回操作数的数据类型

* 输入左值, 返回其引用

* 输入右值, 返回对应类型

  ```C++
  int *p;
  
  输入左值
  decltype(*p)
  解引用运算符 * 生成左值 int 类型
  decltype(*p) 结果为 int & 类型
  
  输入右值
  decltype(&p)
  取地址符 & 生成右值 int ** 类型
  decltype(&p) 结果为 int ** 类型

```C++
const int a = 0, &b = a;	a 是 const int 类型  b 是 const int & 类型, 引用 a
decltype(a) c = 1; 		c 是 const int 类型
decltype(b) d = c;		d 是 const int & 类型, 引用 c
```

**decltype 的结果与表达式形式相关**

```C++
int a = 1, &b = a;
decltype(b + 0) c; 	b + 0 的结果是 int , 则 c 是 int 类型

decltype(a) d;		d 是 int 类型
decltype((a)) e; 	e 是 int & 类型, 因为 a 加了小括号
```





## 第 3 章	string , vector 和数组

### 3.1 using 声明

```C++
using namespace::name;

使用 name 直接访问 namespace::name
```



### 3.2 string

```C++
标准库类型 string
#include <string>
using std::string;
```

#### 3.2.1 初始化

* `=` : 拷贝初始化
* 其余: 直接初始化

```C++
string s;
string s = s1, 			s(s1);
string s = "string", 	s("string");
string s(10,'c');
```

#### 3.2.2 操作

```C++
getline(is, s)	is 输入流 	s string 类型		返回 is	
s.size()		返回 string::size_type 类型
s1 = s2			s2 复制覆盖 s1 中
s1 == s2		s1 与 s2 字符串是否完全一样
字符串拼接	+ 运算符两侧至少有一个 string 
```



#### 3.2.3 string 中的字符

```C++
范围 for 语句
for(declaration : expression)
	statement
	
declaration 定义循环变量
expression 序列对象

改变 string 中字符, 循环变量定义为引用类型
for(auto &c : s)
	c = 'a';
```

**下标运算符 `[]`**

输入: `string::size_type`类型

输出: 字符的引用

### 3.3 vector

对象的集合, **容器**, 类模板

```C++
#include <vector>
using std::vector;

实例化
vector<int> a;
```

#### 3.3.1 初始化

```C++
vector<T> v;
vector<T> v1 = v,	v1(v);			v 的副本初始化 v1
vector<T> v1(n),	v1(n, value);	n 个 value 初始化
vector<T> v1 = {a, b, c...},		v1{a, b, c...};		列表初始化
vector<T> v(begin, end);			
begin: 首元素迭代器或指针, end: 尾后元素迭代器或指针
```

#### 3.3.2 添加元素

vector 的 push_back() 成员函数

```C++
vector<int> a;
a.push_back(0);		把 0 加入 a 的后面
vector 高效增长模型: 动态表(CLRS 摊还分析)

注: 不能用范围 for 循环向遍历序列中添加元素
```

#### 3.3.3 vector 操作

```C++
v.empty()
v.size()	返回 vector::size_type 类型
v.push_back(t)
v[n]		返回引用
```



### 3.4 迭代器介绍

迭代器: 容器的指针

注:

1. 所有容器都能用迭代器
2. 少数容器能用下标运算符 `[]`
3. `string` 不是容器, 但支持迭代器
4. `vector` 支持下标运算符

#### 3.4.1 使用迭代器

**begin 和 end**

1. `begin` 指向首元素, `end` 指向尾元素的下一位置
2. 若容器为空, 则 `begin` 与 `end` 相等, 都是尾后迭代器

```C++
vector<int> v;
auto b = v.begin(), e = v.end();
```

**迭代器运算符**

```C++
*iterator	返回所指元素引用
iterator->men	等价于 (*iterator).men
++iterator --iterator
==  !=		判断是否相等

迭代器的循环中不用 < 进行判断, 而是用 == 和 != 进行判断
因为所有容器都定义 == 和 != 运算符, 大多数未定义 < 运算符
```

**迭代器类型**

迭代器类型: `iterator` 和 `const_iterator`

```C++
vector<int>::iterator it;		vector<int> 类型迭代器
vector<int>::const_iterator it;	it 只能读元素, 不能写元素

string::iterator it;			string 类型迭代器
string::const_iterator it;		it 只能读字符, 不能写字符
```

* 非常量对象
  begin 和 end 返回 iterator 类型迭代器

* 常量对象
  begin 和 end 返回 const_iterator 类型迭代器

* 限制
  * 不能在范围 `for` 循环中向 vector 添加元素
  * 改变 vector 对象容量的操作会使得迭代器失效
  * 上述限制跟动态表有关

#### 3.4.2 迭代器运算

```C++
vector 和 string 支持的运算

有效位置:容器内元素和尾元素的下一元素
iter + n	iter - n	移动 n 个位置
iter += n	iter -= n
iter1 - iter2	计算迭代器之间的距离, 返回 difference_type 有符号整数类型
> >= < <= 	关系运算符, 比较位置关系
```



### 3.5 数组

#### 3.5.1 定义与初始化

**定义**

```C++
int a[d];	d 必须是常量表达式
```

**初始化**

```C++
int a[3] = {0, 1, 2};		int a[] = {0, 1, 2};
int a[5] = {0, 1, 2};
```

**复杂数组声明**

```C++
int *p[10];		从右往左: 10个元素数组 	数组名为 p	数组元素为 int * 类型

int (*p)[10] = &arr;	
从内往外: 
1. *p 表明 p 是一个指针		
2. 10 表明 p 是一个指向 10 元素数组的指针
3. int 表明数组元素是 int 类型
总: p 是一个指向 10 元素 int 数组的指针

int (*p)[10] = &arr;
从右向左:
1. 10 表明是 10 元素数组
2. (*p)	看作临时数组名
3. int 表明数组元素是 int 类型
总: p 指向数组名为 (*p) 的 10 元素 int 数组
p 是二级指针
```

```C++
int (&r)[10] = arr;
从内往外:
1. &r 表明 r 是一个引用
2. 10 表明引用的对象是一个 10 元素数组
3. int 表明数组元素类型是 int
总: r 是一个 10 元素 int 数组的引用

int (&r)[10] = arr;
从右往左:
1. 10 表明是 10 元素数组
2. (&r) 是临时数组名
3. int 表明数组元素类型为 int
总: r 是 10 元素 int 数组的引用
```

```C++
作业:
int *(&r)[10] = p;
```

#### 3.5.2 访问数组元素

**数组下标**

* `size_t` 类型, `cstddef` 头文件

#### 3.5.3 指针和数组

* 多数情况下, 数组对象(即数组名)是一个指向数组首元素的指针

  ```C++
  int a[10];
  int *p = a;		等价于	int *p = &a[0];

* 有些情况下, 数组对象表示数组整体

  ```C++
  int a[10];
  decltype(a) b = {0, 1, 2};	b 是 10 元素 int 数组对象

* 尾后迭代器 和 尾后指针 不能执行解引用和递增操作



**标准库函数 begin 和 end**

* 输入: 数组对象

* 输出: 首指针 和 尾后指针

  ```C++
  int a[3] = {0, 1, 2};
  int *b = begin(a);	b 指向 0
  int *e = end(a);	e 指向 2 的下一位置

**指针运算**

```C++
指针相减: ptrdiff_t 有符号整数类型		cstddef 头文件
```



**下标和指针**

内置下标为有符号类型, stl 下标为无符号类型

```C++
int a[3] = {0, 1, 2};
int *p = &a[2];
int k = p[-2];	k = 0, p[-2] 返回 a[0] 的引用
```



#### 3.5.4 C 风格字符串

**C 风格字符串函数**

```C++
strlen(s)			s 长度, 不包括空字符
strcmp(s1, s2)		比较 s1 和 s2
strcat(s1, s2)		s2 附加到 s1 后, 返回 s1
strcpy(s1, s2)		s2 拷贝到 s1, 返回 s1
```

#### 3.5.5 C++ 对 C 的接口

```C++
string s;
const char *p = s.c_str();
成员函数 c_str :
输入:
输出: string 对象的 c 风格字符串, const char * 类型
```



**旧与新的初始化**

旧类型可以用来初始化新类型, 新类型不能用来初始化旧类型

```C++
string s("hello");

int a[] = {0, 1, 2};
vector<int> b(begin(a), end(a));
```



### 3.6 多维数组

**遍历多维数组**

```C++
int a[3][4];
for(auto &i : a)
	for(auto &j : i)
	{
		j = 0;
	}
	
```

**指针与多维数组**

```C++
int a[3][4];		a 转换的指针是第一个内存数组的指针, 即 a[0] 这个 4 元素 int 数组
int (*p)[4] = a;	p 指向 4 元素 int 数组, 初始化为 a, 即 p 指向 a 的第一个元素(a[0])
```



## 第 4 章 	表达式

### 4.1 基础

#### 4.1.1 基本概念

* 分为: 一二三元运算符, **函数调用**(运算符)

* 表达式(结果) = 运算符 + 运算对象

  * 运算符: 函数
  * 运算对象: 输入
  * 结果: 输出

* 优先级 结合律 求值顺序

  > 求值顺序: 运算对象的求值顺序
  >
  > 例: 
  >
  > f1() + f2();
  >
  > `+` 运算符并未规定求值顺序
  >
  > 则可能先算 f1(), 后算 f2()
  >
  > 也可能先算 f2(), 后算 f1()

**重载运算符**

* **自定义**运算符的**输入类型**和**输出类型**

  > 输入类型即运算对象的类型
  >
  > 输出类型即返回值的类型

* 优先级 结合律 运算对象个数 无法改变

  > 这三者应该是在编译器的语法规则里定死, 不能在 C++ 语言层面改变

**左值和右值**

* 左值: 地址, 对象或函数的内存空间

  * 左值可以作为右值使用

* 右值: 数据, 对象的内容(地址存放的数据)

  ```C++
  赋值运算符 =  
  输入: 左值	输出: 左值
  
  取地址符 & 
  输入: 左值	输出: 右值
  
  解引用运算符, 下标运算符
  输入: 左值	输出: 左值
  
  递增递减 ++ --
  输入: 左值	输出: 前置为左值, 后置为右值



#### 4.1.3 求值顺序

```C++
a = f1() + f2();
不知道 f1() 与 f2() 的求值顺序
```

**4 种运算符明确规定运算符顺序**

* 逻辑与 && : 先左后右
* 逻辑或 || : 先左后右
* 条件运算符 ?:   先左后右
* 逗号运算符 ,  先左后右



### 4.2 算术运算符

输入: 右值	输出: 右值

**取余运算符 %**

```C++
(-m)/n	m/(-n) 等价于	-(m/n)
m%(-n)	等价于	m%n
(-m)%n	等价于	-(m%n)
```

### 4.3 逻辑和关系运算符

输入: 右值	输出: 右值

### 4.4 赋值运算符

输入: 左侧对象: 可修改左值, 右侧对象: 转化为左侧对象类型

输出: 左值, 左侧对象

* 满足右结合律
* 优先级较低

### 4.5 递增和递减运算符

输入: 左值	输出: 前置为左值, 后置为右值

* 后置运算符高于解引用运算符

  ```C++
  *p++ 很常用
  *p++ 等价于 *(p++)
  ```

### 4.8 位运算符

```C++
某位设为 1
该位为 1, 其余为 0, 位或运算 |

某位设为 0
该位为 0, 其余为 1, 位与运算 &

检测某位值
该位为 1, 其余为 0, 位与运算 & 
```

**移位运算符满足左结合律**



### 4.9 sizeof 运算符

sizeof 不会实际求值, 直接获取对象类型所占空间大小

### 4.10 逗号运算符

输入: 两个输入

求值顺序: 从左向右

输出: 右侧表达式的值, 返回类型: 左值返回左值, 右值返回右值

### 4.11 类型转换

#### 4.11.1 隐式转换

* 算术类型

  * 算数类型的隐式转换原则是: 尽可能避免精度损失
  * 运算对象转换成最宽的类型

* 数组转指针

  ```
  数组作为 decltype & sizeof typeid 的运算对象时不转换为指针

#### 4.11.2 显式转换

`cast-name<type>(expression);`

* `cast-name` 指定转换类型
* `type` 指定转换的目标类型
* `expression` 为要转换的值

**const_cast**

只修改底层 `const` 的 `const` 属性, 不改变类型

`const_cast` 的类型必须是指针或引用

1. 去掉 `const` 性质

   ```C++
   const int * p;
   // p 是底层 const
   int *p1 = const_cast<int *>(p)
   1. 把 p 类型由 const int * 转换为 int * 初始化 p1
   2. 通过 p1 修改 p 所指对象是合法的, 但 *p1 不是可修改的左值

2. 添加 `const` 属性

   ```C++
   int *p;
   // p 为非常量对象
   const int *p1 = const_cast<const int *>(p);

3. 只调节类型限定符, 不改变基础类型

   ```C++
   const char *p;
   string &s = const_cast<string&>(p);
   // 错误, const_cast 转换不改变表达式类型



## 第 5 章 语句

**复合语句**

* `{}` 是一个复合语句, 不需要加 `;` 
* 迭代与条件等语句只跟一条语句, 常需要跟复合语句, 这就是这些语句经常要用 `{}` 的原因

**悬垂 `else`**

`else` 与最近尚未匹配的 `if` 匹配, 消除二义性



### 5.3 条件语句

#### 5.3.2 switch 语句

```C++
switch(整形常量表达式)
{
	case int_value1: [break;]
	case int_value2: [break;]
	case int_value3: [break;]
	default:
}
```

**switch 内部的变量定义**

跳转语句不能跳过**带初始值**的**变量定义**

```C++
switch(整型常量表达式)
{
	case 0:
		int a;
		int b = 0;
	case 1:
		// a, b 均在作用域内
		// 但 a 是正确用法, b 是错误用法, 因为 b 经过了初始化
}

switch(1)
{
    case 0:
    	int a;
    cout << "case 0" << endl;
    	break;
    case 1:
    	a = 1;
    	cout << "a = " << a << endl;
    	cout << "case 1" << endl;
}
输出: 
a = 1
case 1
```



### 5.4 迭代语句

#### 5.4.2 传统 for 语句

```
for(init-statement; condition; expression)
	statement
```

* `init-statement` 只能有一条声明语句, 但可以定义多个对象

* 循环情况

  ```C++
  第一轮
  1. init-statement
  2. condition
  3. statement
  
  第二轮
  1. expression
  2. condition
  3. statement
  
  第三轮
  1. expression
  2. condition
  3. statement
  
  ......
  
  第 n 轮(最后一轮)
  1. expression
  2. condition
  3. statement
  
  第 n + 1 轮
  1. expression
  2. condition
  ```

  ```C++
  1. init-statement
  2. condition
  
  第一轮迭代(#1 statement)
  
  1. expression
  2. condition
  
  第二轮迭代(#2 statement)
  
  1. expression
  2. condition
  
  ......
  
  第 n 轮迭代(#n statement) 最后一轮
  
  1. expression
  2. condition
  
  
  



#### 5.4.3 范围 for 语句

遍历序列元素, 序列能返回迭代器 `begin` 和 `end` 成员

```C++
for(declaration: expression)
	statement
```

```C++
vector<int> a;
for(int i : a)
{
	i = ...
}
```

* 每次迭代后, `i` 被**重新定义**并**初始化为序列的下一个值**
  即 **迭代后** `i` 所占**内存空间被销毁**, **重新分配内存空间**给 `i` , 并**初始化为下一个元素的值**

**等价形式**

```C++
vector<int> a;
for(auto beg = a.begin(), end = a.end(); beg != end; ++beg)
{
	......
}
```

* 在 `for` 循环内增删元素会使得 `end` 的值无效, 判断条件出错



### 5.5 跳转语句

#### 5.5.1 break 语句

`break` 语句作用于最近的循环或 `switch` 语句

#### 5.5.2 continue

对 `for` 循环来说, `continue` 中断 `statement`, 继续 `expression` , `condition` 和下一次迭代



### 5.6 try 语句块和异常处理

**异常处理** 

* `throw` 表达式	

  检测异常

* `try` 语句块	    

  异常处理

  * 关键字 `try` + 多个 `catch` 子句
  * 紧跟 `try` 的代码抛出异常, `catch` 子句处理异常

* 异常类

  在 `throw` 与 `catch` 中传递异常的信息

#### 5.6.1 throw 表达式

`throw` + 表达式 + `;`

表达式类型即为异常类型

```C++
if(a != 1)
	throw runtime_error("Error!");
	
//若程序执行到这里, 说明没有抛出异常
cout << "Normal" << endl;


若 a != 1 则抛出一个异常, 该异常是 runtime_error 类型的对象,
然后中断当前函数, 跳过后面的代码, 将程序控制权交给异常处理代码

runtime_error 是标准库异常类型的一种, 定义在头文件 stdexcept 中, 
用 string 对象初始化
```



#### 5.6.2 try 语句块

```C++
try
{
	正常程序代码
	若失败, 则抛出异常
	throw 表达式 ;
}
catch(异常声明)
{
	异常处理
}
catch(异常声明)
{
	异常处理
}...
```

* `catch` 语句块只执行一个

```C++
try
{
	throw runtime_error("Error!");
}
catch (runtime_error err)
{
	cout << err.what() << endl;
}
```

* `err` 就是抛出的异常的变量名
* `what` 成员函数为初始化时 `string` 对象的副本

**寻找异常处理代码**

* 与函数调用相反, 函数调用是**由外向内**调用的, 直到不能继续调用为止
* 寻找异常处理是**由内向外**的, 由内层 `try` 语句块 到外层 `try` 语句块依次查找
* 最终会找到标准库函数 `terminate`

#### 5.6.3 标准异常

标准库中的异常类

* `exception` 头文件中的异常类 `exception`
  * 只报告异常发生, 不提供额外信息
* `stdexcept` 头文件
* `new` 头文件定义了 `bad_cast` 异常类型
* `type_info` 头文件定义了 `bad_cast` 异常类型

## 第 6 章 函数

函数: 命名了的代码块

* 函数:  返回类型, 函数名, 形参列表, 函数体

* 调用运算符 `()` 调用函数

  ```C++
  int fun(int a, int b)
  {
  	...
  	return a + b;	//返回类型不能是数组和函数
  }

* 函数执行过程

  1. 定义并初始化形参
  2. 执行函数体
  3. 返回值(用于初始化调用的表达式)
  4. 控制权交给主调函数

  ```C++
  调用 fun 函数
  
  c = fun(1, 2);
  
  过程如下:
  fun
  {
  	int a = 1;
  	int b = 2;	// 并未规定实参求值顺序
  	return a + b;	
  }
  
  c = return 3;

#### 6.1.1 局部对象

**局部静态对象**

* **仅第一次定义时被初始化**, 其余时忽略初始化
* 生命周期从**从定义时起到整个程序终止时**

#### 6.1.2 函数声明

* 和变量一样, 函数最好在**头文件中声明**, 在**源文件中定义**
* 含有**函数声明**的**头文件**应该被包含在**定义函数**的**源文件**中



### 6.2 参数传递

* **引用传递**

  形参绑定到实参上, **形参是实参的别名**

* 值传递

  **实参的值初始化形参**, 二者相互独立

#### 6.2.2 传引用参数

```C++
int a = 0;
void fun(const int &i)
{
    i = 1;
}
// 说是 i 绑定到 a 对象上, 但二者没有大的差别, 其实是绑定到 a  对象的内存空间上, 即二者都绑定到同一块内存空间
// i 是 a 的别名, a 和 i 都是对同一块内存空间的命名, 用 a 和 i 都可以访问
//区别在于: 根据定义时限定符的不同, a 和 i 对内存空间的访问权限不同, a 可读可写, i 只读
```

#### 6.2.3 const 形参和实参

和一般初始化一样, 实参初始化形参时, 会**忽略形参的顶层 `const` 属性**

* 非常量可以初始化顶层 `const` 对象	

  ```C++
  int a = 0;	
  void fun(const int i){}
  fun(a);		// 可以向顶层 const 形参传入非常量对象

指针或引用与 `const`

* 非常量可以初始化底层 `const` 对象

  ```C++
  int a = 0;			//非常量
  const int *p = &a;	//底层 const 指针 p
  const int &r = a;	//底层 const 引用 r
  const int &r = 0;	//常量初始化底层 const 引用 
  
  //错误示范
  int *p1 = p;		//错误,类型不匹配
  int &r1 = r;		//错误,类型不匹配
  int &r1 = 0;		//错误,类型不匹配
  ```

  ```C++
  int a = 0;
  //正确示范
  void fun(const int *p);
  fun(&a);
  
  void fun(const int &r);
  fun(a);
  fun(0);
  
  
  //错误示范, 错误原因与上述相同
  const int *p;
  void fun(int *p1);
  fun(p);
  
  const int &r;
  void fun(int &r1);
  fun(r);
  fun(0);
  

#### 6.2.4 数组形参

数组特殊性质：1. 不能拷贝数组对象 2. 通常会把数组转换成指针

```C++
void fun(int *);
void fun(int []);
void fun(int [10]);
//三者等价, 数组第一维的维度会被忽略, 加上无影响
//本质都是 int 指针类型
```

**数组引用形参**

```C++
void fun(int (&r)[10]);
// r 是 10 元素 int 数组对象的引用, 数组维度也是构成数组类型的一部分, 不能省略
```

**传递多维数组**

```C++
void fun(int p[][10]);
void fun(int (*p)[10]);
//两个声明等价, p 实际上是指向 10 元素 int 数组对象的指针
//除了第一维之外, 所有维度大小都是数组类型的一部分, 都不能省略
```

#### 6.2.6 含有可变参数的函数

处理不同数量的实参, 主要方法有两种:

1. 实参类型相同

   传递 `initializer_list` 标准库类型

2. 实参类型不同

   可变参数模板

   > 还有一种特殊的形参类型--省略符, 可用来传递可变数量的实参

**initializer_list 形参**

* 头文件: `<initializer_list>`, 模板类型

* `initializer_list` 对象中的元素永远是常量

  ```C++
  void fun(initializer_list<int> list)
  {
      for(auto beg = list.begin(); beg != list.end(); ++beg)
          cout << *beg << endl;
  }
  
  if(a > b)
      fun({1, 2});
  else
      fun({1, 2, 3});
  ```

* 有 **initializer_list** 形参的函数也可以拥有其他形参

  ```C++
  void fun(string s, initializer_list<int> list);
  fun("Hello", {1, 2, 3});

 ### 6.3 返回类型与 return 语句

```C++
void 函数也能使用 return expression 语句, 此时 expression 必须是另一个返回 void 的函数
```

#### 6.3.2 有返回值函数

**返回值时的赋值操作**与**初始化**变量和形参的方式完全一样

* 返回的值用于**初始化调用点的一个临时量**

  ```C++
  int fun()
  {
  	return 1;
  }
  
  int a = fun();
  /*
  过程细节如下:
  tmp = fun();
  int a = fun();
  */

* 返回左值时将临时变量绑定到所引对象上

  ```C++
  int &fun(int &a)
  {
      return a;
  }
  int a = 0;
  int b = fun(a);		// 用 fun(a) 的值初始化 b, b 为 int 类型
  int &c = fun(a);	// 把 c 绑定到 fun(a) 上, 由于 fun(a) 返回 a 的引用, 则 c 也绑定到 a 上
  ```

**不要返回局部对象的引用或指针**

局部对象和字面量都会在函数结束时被释放掉, 其引用和指针会失效



**引用返回左值**

调用**返回引用**的函数**得到左值**, 其他返回类型得到右值

**列表初始化返回值**

函数可以返回 `{}` 包围的值的列表, 然后对临时量执行列表初始化

> 若返回类型时内置类型, 则列表必须最多包含一个值

```C++
vector<int> fun()
{
	if(expression1)
        return {};
    else if(expression2)
        return {1, 2};
    else
        return {1, 2, 3};
}
```

**主函数 `main` 的返回值**

头文件: `cstdlib`

预处理变量, 由预处理器管理, 不需要加 `std::,` 也不需要 `using` 声明

成功: `EXIT_SUCCESS`

失败: `EXIT_FAILURE`

#### 6.3.3 返回数组指针

数组不能被直接返回, 但可以通过指针和引用间接返回, 也可使用类型别名

**数组指针**

```C++
int a[10];
int (*p)[10] = a;	// 错误, a 是 int [10] 类型, 该式把 a 默认转换成 int * 类型, 但 p 是 int (*)[10] 类型, 二者类型不匹配且不能转换
int (*p)[10] = &a;	// 正确, a 是 int [10] 类型, &a 是 int (*)[10] 类型, p 也是 int (*)[10] 类型
```

**声明一个返回数组指针的函数**

```C++
Type (*fun(形参列表)) [dimension]
元素类型 (* 函数名(形参列表)) [数组维度]

int (*fun(int a)) [10];
输入: int 类型
输出: int (*)[10] 类型, 即指向 10 int 元素数组的指针
```

**使用尾置返回类型**

前面放 auto, 真正的返回类型跟在**形参列表之后**

```
auto fun(int a) -> int (*)[10];
```

**使用 `decltype`**

```C++
int a[10];
decltype(a) *fun(int i);
1. decltype 不会把 a 转换成 int * 类型
2. decltype(a) 的结果是 int [10] 类型
3. decltype(a) * 是 int (*)[10] 类型
```



### 6.4 函数重载

**同一作用域内**, **函数名相同**,  **形参列表不同**, 返回类型可相同可不同

```C++
void fun(int a);
double fun(double a);
void fun(string a);
```

> `main` 函数不能重载

**重载和 `const` 形参**

* 顶层 `const` 不能用于区分重载函数

  ```C++
  void fun(int a);
  void fun(const int a);
  // 二者声明等价, 编译器无法根据形参类型的不同区分重载函数
  //二者都可以接收常量和非常量实参

* 底层 `const` 可以用于区分重载函数

  ```C++
  void fun(int &a);			//常量和非常量实参都可以接收
  void fun(const int &a);		//只能接收常量对象, 且为精确匹配
  
  void fun(int *p);			//常量和非常量实参都可以接收
  void fun(const int *P);		//只能接收常量对象, 且为精确匹配

**`const_cast` 和重载**

`const_cast` 在函数重载中用于改变常量属性

```C++
const int &fun(const int &a, const int &b)
{
    return a < b ? a : b;	//返回 a 和 b 中值较小的引用
}
/*
输入: 非常量或常量 a 和 b的常量引用
输入: a 或 b 的常量引用
缺点: 不能在 a 和 b 是非常量时, 返回 a 或 b 的非常量引用
*/

// 优化: 使用 const_cast 类型转换进行一次中转, 使得能做到:
// a 和 b 是非常量, 返回非常量引用, 但又希望能做到在函数内不能修改 a 和 b 的值, 即函数内有 const 属性

// 方法: 使用 const_cast 类型转换进行中转, 先加上 const 属性, 然后进行操作, 返回时再去掉 const 属性
int &fun(int &a, int &b)
{
    auto &r = fun(const_cast<const int &>(a), const_cast<const int &>(b));
    // 先把 a 和 b 加上 const 属性, 然后调用 fun 的 const 版本, 返回 const int & 类型给 r
    
    return const_cast<int &>(r);
    // 然后去掉 r 的 const 属性, 返回 int & 类型 
}
// 类型变换过程如下:
// int &   ->   const int &   ->   处理   ->   int &   ->   返回
```



#### 6.4.1 重载与作用域

**内层**作用域声明的**名字**会**隐藏****外层**作用域的**同名实体**

```C++
void fun(double a);
{
	void fun(int a);	//隐藏外层的 void fun(double a);
}
```

### 6.5 特殊用途语言特性

默认实参, 内联函数, `constexpr` 函数

#### 6.5.1 默认实参

形参列表默认实参必须**全部紧挨靠右**

**默认实参声明**

* **局部变量不能用于默认实参**

* 多从声明同一函数是合法行为

* 给定作用域, 一个形参只能被赋予一次默认值, 赋值顺序为**从右往左**

* 用于默认实参的变量名在**函数声明作用域解析**, 但**求值过程**发生在**函数调用**时

  ```C++
  //全局变量
  int a = 1;
  char c = ' ';
  string fun(int = a, char = c);	//形参名被省略, 语法合法, 但无法被用户使用
  string s = fun();				//调用 fun(1, ' ')
  
  int main()
  {
      int a = 0;					//尽管局部变量 a 隐藏了全局变量 a, 但默认值依然是全局变量 a
      c = '*';					//默认实参 c 的值被赋为 '*', 函数调用时为 '*'
      string s = fun();			//调用 fun(1, '*')
  }

#### 6.5.2 内联函数和 `constexpr` 函数

**内联函数**

编译器负责在编译时在内联函数**所有的调用点展开内联函数**

```C++
inline int fun(int a, int b)
{
    return a < b ? a : b;
}
```

**`constexpr` 函数**

`constexpr` 函数指能用于**常量表达式**的**函数**

语法规定:

* **输入类型**和**输出类型**必须都是**字面值类型**

* 函数体中必须**有且只有一条** `return` 语句

  ```C++
  constexpr int fun() { return 1; }
  constexpr int a = fun();	// a 是一个常量表达式
  ```

特点:

1. `constexpr` 函数被隐式地指定为 `inline` 函数

2. 允许 `constexpr` 函数返回非常量

   ```C++
   constexpr int fun(int a) { return 2 * a; }	// 若 a 是常量表达式, 则 fun 的返回值也是常量表达式
   fun(1);			//fun(1) 是常量表达式
   int a = 1;
   fun(a);			//fun(a) 不是常量表达式

3. `constexpr` 函数不一定返回常量表达式
4.  `inline` 函数和 `constexpr` 函数通常定义在头文件中

#### 6.5.3 调试帮助

用于调试的代码, 需要在发布时被屏蔽掉

**`assert` 预处理宏**

头文件: `cassert`, 预处理器管理

```C++
assert(expression);
```

* 若表达式为真, 无事发生
* 若表达式为假, `assert` 输出信息并终止程序

**`NDEBUG` 预处理变量**

作用: 若定义 `NDEBUG` , 则 `assert` 被忽略, 失去调试作用

用法:

```C++
#define NDEBUG
```



**定义好的特殊变量**

**`__func__`**

编译器为每个函数都定义了 `__func__` 变量, 用于存放函数名, 类型为: **`const char` 的静态数组**

**预处理器**

```C++
__FILE__	文件名
__LINE__	当前行号
__TIME__	文件编译时间
__DATE__	编译时期
类型: 字符串字面值或整形字面值
```

### 6.6 函数匹配

1. 确定**本次调用**的**重载函数集**

   * 候选函数
     1. 与被调函数同名
     2. 声明在调用点可见

2. 根据**本次调用的实参**, 选出**能被实参调用的函数**

   * 可行函数
     1. 形参与实参数量相同
     2. 实参与形参类型相同(精确匹配), 或能转换成形参的类型(类型转换)

3. 寻找**最佳匹配**

   * 实参与形参类型越接近, 匹配得越好
   * 精确匹配比类型转换匹配好
   * 最佳匹配要求
     * 每个匹配都不劣
     * 至少有一个匹配优

   

   #### 6.6.1 实参类型转换

   实参类型转换划分:

   1. 精确匹配
      * 类型相同
      * 数组或函数转换成指针
      * 修改顶层 `const` 属性
   2. `const` 转换
   3. 类型提升
   4. 算数类型转换和指针转换
   5. 类类型转换


### 6.7 函数指针

* 函数指针**指向的是函数**, **不是对象**
* 函数类型由**返回类型**和**形参类型**决定

**函数指针声明**

```C++
int (*p)(int a, int b);
// p 是指向 int (int , int ) 类型函数的函数指针
```

**使用函数指针**

函数名和数组名一样, 作为值使用时, **自动转换成指针**

```C++
int fun(int a, int b);	// fun 是 int (int , int ) 类型的函数
int (*p)(int , int);	// p 指向 int (int , int ) 类型的函数
// 两种赋值语句等价
p = fun;				
p = &fun;				
```

1. 函数指针可当作函数名使用

   ```C++
   int a = p(1, 2);
   int a = fun(1, 2);
   //二者等价, 因为 p 指向 fun 函数

2. 函数指针也可用解引用符使用

   ```C++
   int a = (*p)(1, 2);	
   int a = fun(1, 2);
   //二者等价, 因为 p 指向 fun 函数

> 指向不同函数类型的指针不能相互转换

**重载函数的指针**

指针类型必须与重载函数的某一个**精确匹配**

```C++
void fun(int );
void fun(char );

void (*p)(int) = fun;	//精确匹配
```

**函数指针形参**

和数组一样, 形参不能是函数类型, 但可以是**指向函数的指针类型**

```C++
void fun(int a, int b, int p(char, char));		// p 是函数类型, 会自动转化为指向该函数类型的指针
void fun(int a, int b, int (*p)(char, char));	// 等价声明, 显式声明 p 为指向函数的指针


int p(char, char);
fun(1, 2, p);		//自动将 p 转换成指针
```

**类型别名与 `decltype`**

```c++
bool fun(int, int);
	
typedef bool fun1(int, int);	// fun1 是 bool (int, int) 函数类型的类型别名
typedef decltype(fun) fun2;		// fun2 是 fun 的函数类型的类型别名, 即 bool (int, int) 函数类型的类型别名

bool fun(int, int);
	
typedef bool (*p1)(int, int);	// p1 是 bool (*)(int, int) 的类型别名
typedef decltype(fun) *p2;		// p2 是 指向 fun 类型的指针, 即指向 bool (int, int) 类型的指针, 即 bool (*)(int, int) 的类型别名
```

> `decltype` 参数为函数时不会将函数转换为指针类型, 而是返回函数类型
>
> ```C++
> bool fun(int, int);
> decltype(fun);		//返回 bool (int, int) 类型

**返回指向函数的指针**

* 和数组一样, 不能返回函数类型, 但可以返回函数指针类型
* 但**必须把返回类型显式写为函数指针类型**, 编译器**不会自动把函数转换为指针类型返回**

**使用 `using` 类型别名**

```C++
using fun1 = bool (int, int);		// fun1 是 bool (int, int) 的类型别名
using fun2 = bool (*)(int, int);	// fun2 是 bool (*)(int, int) 的类型别名
```

必须**显式指定**返回类型为**函数指针**类型

```C++
fun1 fun(int);	// 错误, fun1 为函数类型, 返回类型不能是函数类型
fun2 fun(int);	// 正确, fun2 是函数指针类型, 允许函数返回函数指针
fun1 *fun(int);	// 正确, 显式指定返回类型是指向函数的指针

fun2 fun(int);
/*
输入: int
输出: bool (*)(int, int)
*/

// 与 fun2 fun(int) 等价的声明一
bool (*fun(int))(int, int);
/*
由外到内的顺序读声明语句, 先把 (*fun(int)) 看作函数名
bool (*tmp)(int, int) 表明 (*tmp) 的类型为 bool (int, int), 即 tmp 的类型为 bool (*)(int, int)
则 bool (*fun(int))(int, int) 表明 fun(int) 为 bool(*)(int, int) 类型
则 fun 函数的返回类型为 bool(*)(int, int)
总结:
bool (*fun(int))(int, int);
函数名: fun
输入: int
输出: bool(*)(int, int)
*/

// 与 fun2 fun(int) 等价的声明二
auto fun(int) -> bool (*)(int, int);
//函数名: fun
//输入: int
//输出: bool(*)(int, int)
```

将 `auto` 和 `decltype` 用于函数指针类型

```C++
bool fun1(int, int);
bool fun2(int, int);

// 根据形参的取值, get_fun 返回指向 fun1 或 fun2 的指针
decltype(fun1) *get_fun(int);
```

## 第 7 章 类

思想: **数据抽象**和**封装**

* 数据抽象: **接口**和**实现**分离
  * 接口: **用户能实现的操作**
  * 实现: **数据成员**, 负责接**口实现的函数**(成员和非成员函数), **私有成员函数**

抽象数据类型: 实现数据抽象和封装

### 7.1 定义抽象数据类型

成员函数

* **声明必须在类的内部**
* **定义**在**类内类外都可**, 类内是隐式 `inline` 函数, 类外需显式声明 `inline`
* `inline` 成员函数一般与类定义在同一头文件中

非成员函数

* 接口组成的一部分
* 定义和声明都在类外

```C++
struct Book
{
    std::string isbn() const { return book_id; }
    Book& conbine(const Book&);
    
    std::string book_id;
    int price = 0;
}
```

**成员函数默认隐藏形参 `this`**

* 所有成员函数都有 `this` 形参

* `this` 形参类型为**指向该类对象的常量指针**, 即 `Bool* const` 类型

* 当调用成员函数时, 编译器把调用该成员函数的**对象地址**传递给 `this` 形参

  ```C++
  struct Book
  {
     	std::string isbn(Book* const this) const { return this -> book_id; }	//使用 this 调用对象的成员
      Book& conbine(Book* const this, const Book&);
      
      std::string book_id;
      int price = 0; 
  }

** `const` 成员函数/常量成员函数**

1. 成员函数 `isbn() const` 后面的 `const` 表明这是一个 `const` 成员函数

2. `const` 修饰 `this` 指针的常量属性, 即**指向常量类对象的常量指针**

   `this` 类型由 `Book *const` 类型变为 `const Bool *const` 类型

3. 使用 `const this` 指针的好处

   既可以接受常量对象的调用, 也可以接受非常量对象的调用

   ```C++
   struct Book
   {
       std::string isbn() { return book_id; }
       std::string book_id = "-";
   } 
   Book a;
   a.isbn();
   // 正确, a 是非常量对象, 可以调用普通成员函数
   
   const Book b;
   b.isbn();
   // 错误, b 是产量对象, 类型为 const Book, 传入参数为 const Book * 
   // 而 this 类型为 Book *const 
   // 二者类型不匹配, 不能把 const Book * (底层 const)传递给 Book *const (顶层 const)
   
   // 若 this 指针是 const Book *const 类型, 则二者都可传入
   std::string isbn() const { return book_id; }
   Book a;
   a.isbn();
   // 正确, Book * 传递给 const Book *const
   
   const Book b;
   b.isbn();
   // 正确, const Book * 传递给 const Book *const
   ```

4. 等价声明

   ```C++
   std::string isbn(const Book *const this) { return this -> book_id; }

> **常量对象**, 常量对象的**引用和指针**都只能调用**常量成员函数**

**类作用域和成员函数**

**编译器处理类**的步骤

1. 编译**成员的声明**
2. 编译**成员的函数体**

> 因此, 成员函数体可以**不用在意成员出现次序**地使用成员

**返回 `this`对象的函数**

成员函数类似某个运算符时, 应该**尽量模仿该运算符**

```C++
struct Book
{
   	Book& conbine(Book* const this, const Book&);	// 显式写出 this 形参, 语法上是错误的, 但理解意思就行
    int price = 0;    
}

// 函数外定义成员函数
Book& Book :: conbine(Book* const this, const Book& b)
{
    this -> price = this -> price + b.price;
    return *this;
}

// 调用成员函数
Book a, b;
a.combine(b);	// b 的 price 加到 a 的 price 上
// 1. a 的地址传给 this, b 的地址传给 b
// 2. 结果相加后, 返回 *this 即 a 的引用
```

#### 7.1.3 定义类相关的非成员函数

类的非成员函数

1. 概念上是类的接口的一部分
2. 实际上不属于类, 只是与类有关的普通函数
3. 一般与类的声明在同一头文件

**定义 `read` , `print` 和 `add` 函数**

1.  `IO` 类不能背拷贝, 所以要用引用
2. 输出函数尽可能对格式的控制, 把格式控制交给用户
3. 拷贝类的对象就是拷贝对象的数据成员

```C++
struct Book 
{
    std::string book_id = "-";
    int price = 0;
};

// 类外定义非成员函数
istream &read(istream &is, Book &b)
{
    is >> b.book_id >> b.price;
    return is;
}
ostream &print(ostream &os, const Book &b)
{
    os << b.book_id << " " << b.price;
    return os; 
}
Book add(const Book &a, const Book &b)
{
    Book sum = a;
    sum.combine(b);	//b 加到 sum 上并返回 sum 的引用
    return sum;		//返回值, 拷贝 sum 的数据成员
}
```

#### 7.1.4 构造函数

1. 无返回类型
2. 函数名与类型相同
3. 无视 `const` 对象的 `const` 属性, 相当于初始化过程

**默认构造函数(编译器合成)**

1. 先用类内初始值初始化成员
2. 再用默认初始化

**默认构造函数有不能正确处理的情况**

一般需要自己定义默认构造函数, 原因:

1. 一旦定义构造函数, 则编译器不会生成默认构造函数
2. 内置类型和复合类型**默认初始化值是未定义**的
3. 有些类不能被合成默认构造函数

```C++
struct Book
{
    Book() = default;
    Book(const std::string &s): book_id(s) {}
    Book(const int p): price(p) {}
    Book(const std::string &s, const int p): book_id(s), price(p) {} 
    Book(std::istream &);
    int price = 0;
    std::string book_id = "-";
};

```

程序解释:

1. `Book() = default;` 表示要求编译器合成默认构造函数

2. 构造函数初始值里列表

   ```C++
   Book(const std::string &s, const int p): book_id(s), price(p) {} 
   1. 注意  : 与 ,
   2. 没有在初始值列表里的成员将先类内初始值初始化, 再默认初始化

**类外定义构造函数**

```C++
Book::Book(std::istream &is)
{
	read(is, *this);	//从 is 入读信息到 this 指向的对象中
}
//没有在初始值列表里初始化的成员将先类内初始化再默认初始化
//price 和 book_id 执行类内初始化
```

#### 7.1.5 拷贝, 赋值和析构

* 拷贝: 初始化变量, 值传递
* 赋值: 赋值运算符
* 析构: 销毁对象
* 自己不定义时, 编译器合成默认

**某些类不能依赖合成的版本**

当需要分配类对象之外的资源时, 合成版本往往会失效

1. 管理动态内存的类不能依赖合成版本
2. 需要动态内存的类应该使用 `vector` 和 `string` 管理内存空间

### 7.2 访问控制与封装

封装

1. 访问说明符确定访问权限, 实现封装
2. `public`: 所有都可访问, 完全公开
3. `private`: 不能被外界访问, 私密
4. 访问说明符有效范围: 直到下一个访问说明符或类的尾部

```C++
class Book
{
public:
	Book() = default;
    Book(const std::string &s, const int a) : book_id(s), price(a) {}
private:
    std::string book_id = "-";
    int price = 0;
};
```

**`class` 和 `struct` 关键字**

`struct`: 第一个访问说明符之前的成员为 `public`

`class`: 第一个访问说明符之前的成员为 `private`

#### 7.2.1 友元

类可以给其他类或函数访问非公有成员, 方法是设为**友元**

1. 在类内对函数或类做友元声明
2. 友元声明不是函数声明
3. 友元不是类的成员, 也不受所在区域访问控制级别的约束

```C++
class Book
{
    //友元声明
    friend Book add(const Book&, const Book&);
    friend std::istream &read(std::istream &, Book&);
    friend std::ostream &print(std::ostream &, const Book&);
    
public:
	Book() = default;
   	Book(const std::string &s, const int a) : book_id(s), price(a) {}
private:
    std::string book_id = "-";
    int price = 0;
};

//函数声明
Book add(const Book&, const Book&);
std::istream &read(std::istream &, Book&);
std::ostream &print(std::ostream &, const Book&);
```

**封装优点**

1. 用户不会破坏对象的成员
2. 类的实现细节可以随时改变, 只需保证接口不变

**友元的声明**

1. 友元的声明仅仅是指定了访问权限, 不是函数声明
2. 通常把友元(函数和类)的声明与类本身放在同一头文件中



### 7.3 类的其他特性

#### 7.3.1 类成员再探

**类型成员**

1. 自定义某种类型在类里的别名
2. **成员为某种类型**
3. 类型成员必须**先定义后使用**, 所以一般放在类开头位置

```C++
class Screen
{
public:
    typedef std :: string :: size_type pos;
    // 等价用法
    using pos = std :: string :: size_type;
private:
    pos cursor = 0;
    pos height = 0, width = 0;
    std :: string contents;
};
```

**重载成员函数**

```C++
class Screen
{
public:
    char get() const { return contents[cursor]; }
    char get(pos h, pos w) const;		// 无需在定义和声明的地方同时声明 inline , 只需要在定义的地方声明 inline
};

inline char Sereen :: get(pos h, pos w) const	// 只需在定义的地方声明 inline
{
    return contents[h * width + w];
}
```

**可变数据成员**

* 即使是 `const` 成员函数内, 也可修改可变数据成员的值, `mutable` 关键字
* 即使是 `const` 对象的成员, 可变数据成员也不会是 `const`

```C++
class Screen
{
public:
    void fun() const;
private:
    mutable int count = 0;
};

void Screen :: fun() const
{
    ++count;
}
```

#### 7.3.2 返回 *this 的成员函数

```C++
class Screen
{
public:
    Screen &set(char);			// 当前光标处设为新字符, 返回对象的引用
    Screen &set(pos, pos, char);// 指定光标处设为新字符, 返回对象的引用
};

Screen my_screen;
my_screen.set('#').set(3, 4, '%');	// 若成员函数返回对象的引用, 则一系列操作可以在一个表达式完成
```

**从 `const` 成员函数返回 `this`**

`const` 成员函数中

1. `this` 指向 `const`对象, `this` 类型为 `const Type *`

2. `*this` 类型为 `const Type`
3. `const` 成员函数返回 `*this`引用, 即 `const Type` 类型, 返回的是**不可修改的左值**

```C++
// 错误示范
class Screen
{
    const Screen &display(ostream &) const;
    Screen &set(const char);
}
Screen my_screen;
my_screen.display(cout).set('*');
/*
错误, display 返回常量引用, 不能在修改其内容
1. my_screen.display(cout)
	函数名: display
	输入: Screen *const
	输出: const Screen &
2. my_screen.display(cout).set('*')
	函数名: set
	输入: const Screen &
	输出: Screen &
	语法错误: 不能把 const Screen * 赋给 Screen * 类型的 this 指针 
*/
```

**基于 `const` 的重载**

1. 底层 `const` 可以用来区分重载函数
2. 只能在 `const` 对象上调用 `const` 成员函数

```C++
class Screen
{
public:
    Screen(const std::string &s) : contents(s) {}
    
    // 非常量版本
    Screen &display(std::ostream &os) { do_display(os); return *this; }
    
    // 常量版本
    const Screen &display(std::ostream &os) const { do_display(os); return *this; }
private:
    // 公共成员函数共同使用的私有成员函数
    void do_display(std::ostream &os) const { os << contents; }
    std::string contents;
};

Screen screen1("***");
const Screen screen2("###");
screen1.display(cout);
screen2.display(cout);

// 非常量版本
/*
screen1.display(cout);
函数: Screen &display 
输入: Screen * 传递给 this 指针
过程: 调用 do_display 函数
	函数: void do_display() const
	输入: Screen * 传递给 const this 指针, 即 const Screen *
	输出: 无
输出: Screen * 的解引用返回 Screen & 

总结: Screen * -> this -> const this -> void 
			     *this -> Screen & 
*/

// 常量版本
/*
screen2.display(cout);
函数: Screen &display() const
输入: Screen * 传递给 const Screen * 的 const this 指针
过程: 调用 do_display 函数
	函数: void do_display() const
	输入: const Screen * 传递给 const Screen * 的 const this 指针
	输出: 无
输出: const Screen * 解引用返回 const Screen &

总结: const Screen * -> const Screen * -> const Screen * -> void
					   *(const Screen *) -> const Screen &   
*/
```



#### 7.3.3 类类型

**每个类都是唯一的类型**

1. 对两个类, 类名肯定不同

2. 即使两个类的成员类型和名字完全一样, 二者也是不同的类型

   ```C++
   // 成员完全一致, 类也不相同
   struct First
   {
       int a;
       double b;
   };
   
   struct Second
   {
       int a;
       double b;
   };

**类名可直接作为类型使用**

```C++
// 也可以加上 struct 或 class 关键字
First a;
struct First a;
```

**类的声明**

1. 可以先声明暂时不定义类

   ```C++
   class Screen;

2. 声明类后, 如果不需要直到类对象所需空间大小, 可以被使用

   1. **定义**指向该类类型的**指针或引用**
   2. **声明**作为**形参或返回值**的类型

   ```C++
   class Link_screen
   {
       Link_screen *next;
       Link_screen *prev;
   };

但只有在**类定义后**, 编译器才知道**类对象所需内存空间大小**

```C++
class Screen
{
    int a;
    double b;
};
```

只有在**类定义后**, **才能声明该类类型的对象**

所以**一个类的成员类型不能是该类自己**

```C++
class Screen;
Screen s;		// 错误, Screen 未定义, 是不完全类型

class Screen
{
    int a;
    double b;
};
Screen s;		// 正确, Screen 已定义, 可以声明对象


// 同样地, 必须在类完成定义后, 才能在其他类里声明该类类型的对象 
class Screen
{
    int a;
    double b;
};

class Link_screen
{
    Screen window;
    Link_screen *next;
    Link_screen *prev;
};
```

#### 7.3.4 友元再探

1. 类可以把**非成员函数**, **其他类**, **其他类的成员函数**定义成友元
2. 友元函数可以**定义在类的内部**, 这样该函数是**隐式内联**的
3. 类的**友元类的所有成员函数**都**可以访问该类的所有成员**

```C++
class Screen
{
    friend class Window;
    
private:
    std::string contents;
};

class Window
{
    void clear(Screen &s) { s.contents = string(s.contents.length(), ' '); }
};
```

> 友元关系没有传递性

**令其他类的成员函数成为友元**

目的:

1. 类 `Window` 有成员函数 `clear`
2. 类 `Screen`
3. `Window :: clear` 需要声明形参为 `Screen` 的引用
4. `Screen` 需要声明 `Window :: clear` 为友元函数

分析:

1. 由于 类 `Screen` 需要声明类 `Window` 的 `clear` 成员函数为友元, 所以类 `Window` 和成员 `clear` 需要先声明(`Window` 先定义, `clear` 先声明)
2. 由于 `clear` 需要 `Screen` 作为参数, 所以 `Screen` 要先声明
3. 由于 `clear` 需要使用 `Screen`, 所以 `clear` 的定义要放到 `Screen` 定义之后
4. 最终顺序为:
   1. 声明 `Screen`
   2. 定义 `Window`, 声明 `clear`
   3. 定义 `Screen`, 声明 `clear` 为友元
   4. 定义 `clear`

```C++
class Screen; 

class Window
{
public:
    void clear(Screen &s);
};

class Screen
{
    friend void Window::clear(Screen &s);
private:
    std::string contents;
};

void Window::clear(Screen &s)
{ 
    s.contents = string(s.contents.length(), ' '); 
}


```

> 重载函数需要分别声明为友元函数

**友元声明和作用域**

1. **友元的声明**不必非要在**友元声明**之前

2. 假定: **友元声明中的友元名字在当前作用域可见**

3. **友元的声明**可以声明在**其他作用域**, 只需友元在当前作用域可见

4. 即使是在类的内部定义友元函数, 也**必须在类外声明该友元函数**

   ```C++
   struct X
   {
   	friend void f() { return; } 	//类内定义友元函数
       void a() { f(); }				//错误, 函数 f() 未声明
       void b();
   };
   void f();							//类外声明友元函数
   
   void b() { f(); }					//正确, 函数 f() 已声明

### 7.4 类的作用域

**一个类就是一个作用域**

```C++
void Window::clear(...)
{
    ...
}

// Window 表明当前处于 Window 类的作用域中
// 所以 形参列表和函数体中的变量名都是在 Window 类的作用域中查找
```

1. `Window` 表明当前处于 `Window` 类的作用域中

2. 函数的**返回类型通常在函数名之前**, 此时**返回类型的名字在类的作用域之前(之外)**, 此时要显式声明其作用域

   ```C++
   class Window
   {
   public:
       using Index = std::string::size_type;
       Index add(const Index a, const Index b);
   };
   // 显式声明返回类型的作用域, 因为其在 Window 之前出现
   Window::Index Window::add(const Index a, const Index b) { return a + b; }

#### 7.4.1 名字查找与类的作用域

**名字查找**

寻找**与该名字最匹配的声明**

* 一般的名字查找

  只考虑在该**名字之前出现的声明**

  1. 名字所在的块
  2. 外层作用域
  
* 类成员名字的查找

  1. **编译完类中的所有声明**

     * **声明中的名字**(返回类型或形参列表中的名字)必须**在该声明前可见**

  2. 类全部可见后**编译函数体**

     因为直到整个类可见后才处理成员函数体, 所以**成员函数体能使用类中定义的任何名字**

  ```C++
  typedef double time;
  string t;
  class Class
  {
  public:
      // fun 函数声明的返回类型是一般的名字查找: 先找当前块, 再找外层作用域, 但必须是在该声明前可见
      time fun() { return t;}		// 优先找到成员 t, 而不是外层作用域的 string 对象
  private:
      time t;
  };

**类型名要特殊处理**

若成员使用了外层作用域中的某个类型名, 则不能在之后重新定义该名字

> 类型名的定义应该出现在类的开始处

**成员函数定义中块作用域的名字查找**

1. 先在**函数内**声明该名字**之前**查找
2. 再从**整个类**中查找
3. 最后在**类外**且**成员函数定义之前**查找

> 若出现了隐藏另一个名字的现象, 可以显式声明名字的作用域或用 this 指针显式访问
>
> `:: name` 表示全局变量 `name`
>
> `this -> name` 表示类内的 `name` 成员

```C++
class Class
{
public:
    void set(int i);
    
private:
    int a = 0;
};

int fun(int);
void Class::set(int i)
{
    a = fun(i);			// 正确, fun 先在成员函数内查找, 再从整个类内查找, 最后在函数定义前查找
}
```



### 7.5 构造函数再探

构造函数的初始化顺序

```C++
Book::Book(int a, double b, char c): name(a), isbn(b) { id = c; }
```

1. 初始值**列表初始化**
2. 剩余的成员执行**默认初始化**(类内初始化, 值初始化)
3. **函数体赋值初始化**

