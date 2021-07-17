# C++ 基础

## 第 2 章	变量和基本类型

### 2.1 基本内置类型

### 2.2 变量

#### 2.2.1 变量定义

```
类型说明符	变量名列表;
int		a, b;
```

#### 2.2.2 声明与定义

* 声明: 告诉编译器变量类型与变量名标识符
* 定义: 分配对应的内存空间

```
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

```
int a = 1;
int &b = a;		
1. b 一直绑定到 a
2. 引用必须用对象初始化
3. b 本身不是对象, 只是 a 的别名
```



#### 2.3.2 指针

指针: **指向**另一个对象

```
int a = 1;
int *p = &a;
1. p 是一个指针对象, 含有 a 的地址与类型信息
```



**空指针**

```
int *p = 0;
int *p = NULL;
int *p = nullptr;
```



**void 指针**

```
int a;
void *p = &a;
p 仅含有 a 的地址信息
```



#### 2.3.3 理解复合类型的声明

声明: 

基本数据类型 	声明符列表

> 类型修饰符(* 和 &)仅仅是声明符的一部分, 影响范围仅仅是一个声明符

```
int a;
int b, *p = &a, &c = a;
基本数据类型  int
声明符列表  b *p &c
类型修饰符  * & 
b 整形对象	p 指针对象	c 引用
```



**嵌套**

```
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

```
const int a = 1;		编译时初始化
const int a = fun();	运行时初始化
```



#### 2.4.1 const 的引用

**const 的目标是引用的对象**

**对 const 对象的引用** : 引用本身也必须是 const

```
const int a = 1;
const int &b = a;	const 表明通过 b 对内存空间的操作是只读的
```



**const 的目标是引用本身**

**引用是 const** 

```
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

```
int a = 1;
const int *p = &a;
通过 p 对 a 内存空间的权限是只读的
```

**指针是 const**

```
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

  ```
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

  ```
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

**tydef** 与 **using**

```
tydef int integer;
using integer = int;
```

#### 2.5.3 decltype 类型指示符

**decltype**: 选择并返回操作数的数据类型

* 输入左值, 返回其引用

* 输入右值, 返回对应类型

  ```
  int *p;
  
  输入左值
  decltype(*p)
  解引用运算符 * 生成左值 int 类型
  decltype(*p) 结果为 int & 类型
  
  输入右值
  decltype(&p)
  取地址符 & 生成右值 int ** 类型
  decltype(&p) 结果为 int ** 类型

```
const int a = 0, &b = a;	a 是 const int 类型  b 是 const int & 类型, 引用 a
decltype(a) c = 1; 		c 是 const int 类型
decltype(b) d = c;		d 是 const int & 类型, 引用 c
```

**decltype 的结果与表达式形式相关**

```
int a = 1, &b = a;
decltype(b + 0) c; 	b + 0 的结果是 int , 则 c 是 int 类型

decltype(a) d;		d 是 int 类型
decltype((a)) e; 	e 是 int & 类型, 因为 a 加了小括号
```





## 第 3 章	string , vector 和数组

### 3.1 using 声明

```
using namespace::name;

使用 name 直接访问 namespace::name
```



### 3.2 string

```
标准库类型 string
#include <string>
using std::string;
```

#### 3.2.1 初始化

* `=` : 拷贝初始化
* 其余: 直接初始化

```
string s;
string s = s1, 			s(s1);
string s = "string", 	s("string");
string s(10,'c');
```

#### 3.2.2 操作

```
getline(is, s)	is 输入流 	s string 类型		返回 is	
s.size()		返回 string::size_type 类型
s1 = s2			s2 复制覆盖 s1 中
s1 == s2		s1 与 s2 字符串是否完全一样
字符串拼接	+ 运算符两侧至少有一个 string 
```



#### 3.2.3 string 中的字符

```
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

```
#include <vector>
using std::vector;

实例化
vector<int> a;
```

#### 3.3.1 初始化

```
vector<T> v;
vector<T> v1 = v,	v1(v);			v 的副本初始化 v1
vector<T> v1(n),	v1(n, value);	n 个 value 初始化
vector<T> v1 = {a, b, c...},		v1{a, b, c...};		列表初始化
vector<T> v(begin, end);			
begin: 首元素迭代器或指针, end: 尾后元素迭代器或指针
```

#### 3.3.2 添加元素

vector 的 push_back() 成员函数

```
vector<int> a;
a.push_back(0);		把 0 加入 a 的后面
vector 高效增长模型: 动态表(CLRS 摊还分析)

注: 不能用范围 for 循环向遍历序列中添加元素
```

#### 3.3.3 vector 操作

```
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

```
vector<int> v;
auto b = v.begin(), e = v.end();
```

**迭代器运算符**

```
*iterator	返回所指元素引用
iterator->men	等价于 (*iterator).men
++iterator --iterator
==  !=		判断是否相等

迭代器的循环中不用 < 进行判断, 而是用 == 和 != 进行判断
因为所有容器都定义 == 和 != 运算符, 大多数未定义 < 运算符
```

**迭代器类型**

迭代器类型: `iterator` 和 `const_iterator`

```
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

```
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

```
int a[d];	d 必须是常量表达式
```

**初始化**

```
int a[3] = {0, 1, 2};		int a[] = {0, 1, 2};
int a[5] = {0, 1, 2};
```

**复杂数组声明**

```
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

```
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

```
作业:
int *(&r)[10] = p;
```

#### 3.5.2 访问数组元素

**数组下标**

* `size_t` 类型, `cstddef` 头文件

#### 3.5.3 指针和数组

* 多数情况下, 数组对象(即数组名)是一个指向数组首元素的指针

  ```
  int a[10];
  int *p = a;		等价于	int *p = &a[0];

* 有些情况下, 数组对象表示数组整体

  ```
  int a[10];
  decltype(a) b = {0, 1, 2};	b 是 10 元素 int 数组对象

* 尾后迭代器 和 尾后指针 不能执行解引用和递增操作



**标准库函数 begin 和 end**

* 输入: 数组对象

* 输出: 首指针 和 尾后指针

  ```
  int a[3] = {0, 1, 2};
  int *b = begin(a);	b 指向 0
  int *e = end(a);	e 指向 2 的下一位置

**指针运算**

```
指针相减: ptrdiff_t 有符号整数类型		cstddef 头文件
```



**下标和指针**

内置下标为有符号类型, stl 下标为无符号类型

```
int a[3] = {0, 1, 2};
int *p = &a[2];
int k = p[-2];	k = 0, p[-2] 返回 a[0] 的引用
```



#### 3.5.4 C 风格字符串

**C 风格字符串函数**

```
strlen(s)			s 长度, 不包括空字符
strcmp(s1, s2)		比较 s1 和 s2
strcat(s1, s2)		s2 附加到 s1 后, 返回 s1
strcpy(s1, s2)		s2 拷贝到 s1, 返回 s1
```

#### 3.5.5 C++ 对 C 的接口

```
string s;
const char *p = s.c_str();
成员函数 c_str :
输入:
输出: string 对象的 c 风格字符串, const char * 类型
```



**旧与新的初始化**

旧类型可以用来初始化新类型, 新类型不能用来初始化旧类型

```
string s("hello");

int a[] = {0, 1, 2};
vector<int> b(begin(a), end(a));
```



### 3.6 多维数组

**遍历多维数组**

```
int a[3][4];
for(auto &i : a)
	for(auto &j : i)
	{
		j = 0;
	}
	
```

**指针与多维数组**

```
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

  ```
  赋值运算符 =  
  输入: 左值	输出: 左值
  
  取地址符 & 
  输入: 左值	输出: 右值
  
  解引用运算符, 下标运算符
  输入: 左值	输出: 左值
  
  递增递减 ++ --
  输入: 左值	输出: 前置为左值, 后置为右值



#### 4.1.3 求值顺序

```
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

```
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

  ```
  *p++ 很常用
  *p++ 等价于 *(p++)
  ```

### 4.8 位运算符

```
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

   ```
   const int * p;
   // p 是底层 const
   int *p1 = const_cast<int *>(p)
   1. 把 p 类型由 const int * 转换为 int * 初始化 p1
   2. 通过 p1 修改 p 所指对象是合法的, 但 *p1 不是可修改的左值

2. 添加 `const` 属性

   ```
   int *p;
   // p 为非常量对象
   const int *p1 = const_cast<const int *>(p);

3. 只调节类型限定符, 不改变基础类型

   ```
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

```
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

```
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

  ```
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

  ```
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

```
for(declaration: expression)
	statement
```

```
vector<int> a;
for(int i : a)
{
	i = ...
}
```

* 每次迭代后, `i` 被**重新定义**并**初始化为序列的下一个值**
  即 **迭代后** `i` 所占**内存空间被销毁**, **重新分配内存空间**给 `i` , 并**初始化为下一个元素的值**

**等价形式**

```
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

```
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

```
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

```
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

  ```
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

  ```
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
