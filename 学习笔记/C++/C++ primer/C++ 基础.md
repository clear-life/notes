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



### 2.5 处理类型

#### 2.5.1 类型别名

**tydef** 与 **using**

```
tydef int integer;
using integer = int;
```

#### 2.5.3 decltype 类型指示符

**decltype**: 选择并返回操作数的数据类型

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

