# C++ 标准库

 ## 第 8 章 IO 库

常用 IO 库的功能

* 类
  * `istream` 类, 输入相关
  * `ostream` 类, 输出相关
* 对象
  * `cin`, `istream` 对象
  * `cout`, `ostream` 对象
  * `cerr`, `ostream` 对象
* 运算符
  * `>>`
    * 输入: `istream` 对象, 左值对象
    * 输出: `istream` 对象
  * `<<`
    * 输入: `ostream` 对象, 右值对象
    * 输出: `ostream` 对象
* 函数
  * `getline` 函数
    * 输入: `istream` 对象, `string` 对象
    * 输出: `istream` 对象

### 8.1 IO 类

**IO 类应该具有的功能**

1. 既能操作**单字节字符**, 也能操作**多字节字符**

   > 单字节字符 	`char` 类型
   >
   > 多字节字符/宽字符	`wchar_t` 类型
   >
   > 宽字符的类型和函数以 `w` 开头

2. 基础: 读写**流**

3. 扩展: 读写**文件**, 读写内存 **`string`**

**IO 库头文件和类类型**

| 头文件     | 类类型                                                       |
| ---------- | ------------------------------------------------------------ |
| `iostream` | `istream`, `wistream` 从流读<br />`ostream`, `wostream` 向流写<br />`iostream`, `wiostream` 读写流 |
| `fstream`  | `ifstream`, `wifstream` 从文件读<br />`ofstream`, `wofstream` 向文件写<br />`fstream`, `wfstream` 读写文件 |
| `sstream`  | `istringstream`, `wistringstream` 从 `string` 读<br />`ostringstream`, `wostringstream` 向 `string` 写<br />`stringstream`, `wstringstream` 读写 `string` |

**IO 类型间的关系**

**继承**使得能忽略不同类类型的流之间的差异

1. `ifstream` 和 `istringstream` 继承自 `istream`
   * 可以像使用 istream 对象一样使用 `ifstream` 和 `istringstream` 对象	
   * 像使用 `cin` 一样, 对 `ifstream` 和 `istringstream` 对象调用 `getline` 函数
2. `ofstream` 和 `ostringstream` 继承自 `ostream`

#### 8.1.1 IO 对象无拷贝和赋值

1. 不能拷贝和赋值 IO 对象
2. 形参和返回类型不能直接是 IO 类型
3. 形参和返回类型可以使用 IO 类型的引用
4. 读写 IO 对象会改变其状态, 故引用不能是 `const`

#### 8.1.2 条件状态

条件状态反映当前流的状态

| IO 库条件状态 | 作用(strm 是某个 IO 类的类型)             |
| ------------- | ----------------------------------------- |
| strm::iostate | 整个条件状态                              |
| strm::badbit  | 流已崩溃, 系统级错误, 流不可再使用        |
| strm::failbit | IO 操作失败, 可修复错误, 流可以再使用     |
| strm::eofbit  | 文件结束                                  |
| strm::googbit | 未出错                                    |
| s.rdstate()   | 返回流 s 的当前状态, 类型为 strm::iostate |

* 流若发生错误, 后续的 IO 操作都会失败

* 使用流之前要检查是否良好

  ```C++
  while(cin >> a)
      // 运行到这里说明读取成功

**管理条件状态**

流对象的 `rdstate` 返回 `iostate`

`setstate` 给定位置位

`clear` 重置, 或置为新的流状态

```C++
auto s = cin.rdstate();	// 记录当前状态 s
cin.clear();			// 重置 cin 的状态
cin >> ...				// 使用 cin
cin.setstate(s);		// 置为原来的状态 s
```

#### 8.1.3 管理输出缓冲

每个输出流都管理一个缓冲区

**缓冲机制**可以将**多个输出**操作组合成**一个系统级写操作**(向设备写数据), 减少 IO 操作

缓冲刷新原因:

1. 正常结束, 缓冲刷新是 `return` 操作的一部分
2. 缓冲区满
3. 操纵符
4. 流关联

每个输出流都管理一个**缓冲区**

**刷新缓冲区**

`endl` 	换行并缓冲

`flush`	直接缓冲

`ends` 	插入空字符并缓冲

**`unitbuf` 操纵符**

设置每次输出后都刷新缓冲区

```C++
cout << unitbuf;	// 设置立即刷新
cout << nouitbuf;	// 设置正常缓冲
```

> 如果程序异常终止, 缓冲区不会刷新

**关联输入和输出流**

输入流 A 关联到输出流 B 时, 从 A 读数据会刷新 B 的缓冲区

标准库将 `cout` 和 `cin` 关联在一起

```C++
cin >> a;	// 会刷新 cout 的缓冲区
```

**关联函数**

`tie` 有两个重载的版本

1. 无参数版本

   若 A 关联到 B, 则 A.tie() 返回指向 B 的指针

   若 A 未关联到流, 则 A.tie() 返回空指针

2. 有参数版本

   参数: 指向 ostream 的指针

   作用: A.tie(&B) 将流 A 关联到输出流 B

### 8.2 文件输入输出

文件流对象特有的操作

| 操作             | 功能                                        |
| ---------------- | ------------------------------------------- |
| fstream fstrm(); | 创建文件流对象                              |
| fstrm.open(s)    | 打开文件 s , 并将文件流对象 fstrm 与 s 绑定 |
| fstrm.close()    | 关闭绑定的文件                              |
| fstrm.id_open()  | 返回 bool, 表明文件是否打开                 |

#### 8.2.1 使用文件流对象

**用 fstream 代替 iostream&**

可以用**继承类**代替**基类**

可以用 `fstream` 和 `sstream` 替代 `iostream`

```c++
ifstream in(file);		// 打开并关联到文件 file
in.close();				// 关闭文件
in.open(file+"2")		// 打开另一个文件
```

**自动构造和析构**

局部文件流对象离开作用域时会被销毁, **`close` 成员会自动被调用**

则与该局部文件流对象关联的文件会被关闭

```c++
while(true)
{
    ifstream input(file);
    if(input)
    {
        int a;
        input >> a; 
    }
}	// 离开作用域时, input 对象会被销毁, close 成员自动被调用用来关闭关联的文件
```

#### 8.2.2 文件模式

| 文件模式 | 作用                   |
| -------- | ---------------------- |
| in       | 读方式                 |
| out      | 写方式                 |
| app      | 追加到末尾方式         |
| ate      | 定位到末尾方式         |
| trunc    | 截断文件, 丢弃文件内容 |
| binary   | 二进制方式             |

**out 模式会丢弃已有数据**

默认情况下, ofstream 对象是 out 模式打开文件,

打开文件时, **文件的内容默认会被丢弃**, 需要同时指定 **app 模式**

```C++
ofstream file("file1", ofstream::app);	// 隐含为输出模式
ofstream file("file2", ofstream::out | ofstream::app);	// 显示指定为 out 和 app 模式
// ofstream::app 表明是 ofstream 作用的 app 类型, 本质应该是一个 const 数字, 应该以二进制形式读该数字
// ofstream::out | ofstream::app 中的 | 应该是位或运算
```

每次调用 open 时都会重新确定文件模式, 可以是隐式设置, 也可以是显式设置

### 8.3 string流

  `string` 流对象特有的操作

| 操作             | 作用                                  |
| ---------------- | ------------------------------------- |
| sstream strm;    | 创建一个未绑定的 sstream 对象         |
| sstream strm(s); | 创建并用 s 初始化 ssream 对象         |
| strm.str()       | 返回保存的 string 的拷贝              |
| strm.str(s)      | 将 string s 拷贝到 strm 中, 返回 void |

#### 8.3.1 istringstream

对已有文本进行挨个处理

```C++
string line, word;
while(getline(cin, line))		// 从标准输入读取一行数据到 string 对象 line 中, getline 以换行符为界
{
    istringstream record(line);	// 创建并绑定到 string 对象 line 
    while(record >> word)		// 挨个读到 word 中并处理, istringstream 以空白符为界
        ...
}
```

#### 8.3.2 ostreamstream

挨个构造输出, 最后一起输出刷新缓冲区

```C++
ostringstream out;
for(auto i : datas)
{
    out << i;
}
os << out.str() << endl
```



## 第 9 章 顺序容器

### 9.1 容器总览

容器(**模板类**): **顺序容器**, **关联容器**

容器适配器: `stack`, `queue`, `priority_queue`

| 顺序容器     | 数据结构 |
| ------------ | -------- |
| vector       | 可变数组 |
| deque        | 双端队列 |
| list         | 双向链表 |
| forward_list | 单项链表 |
| array        | 固定数组 |
| string       | 字符容器 |

| 容器适配器     | 数据结构 |
| -------------- | -------- |
| stack          | 栈       |
| queue          | 队列     |
| priority_queue | 优先队列 |

### 9.2 容器库概述

**容器操作**

* 所有容器共有
* 顺序容器, 关联容器, 无序容器独有
* 小部分容器独有

| 容器操作             | 说明                                   |
| -------------------- | -------------------------------------- |
| **类型别名**         |                                        |
| iterator             | 该容器的迭代器类型                     |
| const_iterator       | const 版本                             |
| size_type            | 容器 size, 无符号整数                  |
| difference_type      | 迭代器间的举例, 有符号整数             |
| value_type           | 元素类型                               |
| reference            | 元素的引用类型, 左值                   |
| const_reference      |                                        |
| **构造函数**         |                                        |
| Class c;             | 默认构造函数                           |
| Class c1(c2);        | c2 的拷贝                              |
| Class c(b, e);       | 迭代器 b 和 e 范围的拷贝(array 不支持) |
| Class c{a, b, c...}; | 列表初始化                             |
| **赋值与 swap**      |                                        |
| c1 = c2;             |                                        |
| c = {a, b, c...};    |                                        |
| a.swap(b)            | 交换 a 和 b 的元素                     |
| swap(a, b)           |                                        |

| size                                       |                          |
| ------------------------------------------ | ------------------------ |
| c.size()                                   | 元素数目                 |
| c.max_size()                               | 容器 size                |
| c.empty()                                  | 是否为空                 |
| **增/删元素**                              |                          |
| c.insert(args)                             | 拷贝增加                 |
| c.emplace(inits)                           | 构造增加                 |
| c.erase(args)                              | 删除元素                 |
| c.clear()                                  | 清空元素                 |
| **关系运算符**                             |                          |
| == , !=                                    | 所有容器都支持           |
| <, <=, >, >=                               | 部分容器支持             |
| **迭代器**                                 |                          |
| c.begin(), c.end()                         | 首元素迭代器, 尾后迭代器 |
| c.cbegin(), c.cend()                       | const 版本               |
| **反向容器**(不支持 forward_list 单向列表) |                          |
| reverse_iterator                           | 逆序迭代器               |
| const_reverse_iterator                     | const 版本               |
| c.rbegin(), c.rend()                       | 尾元素迭代器, 首前迭代器 |
| c.crbegin(), c.crend()                     | const 版本               |

#### 9.2.1 迭代器范围

一对迭代器 `begin` 和 `end` 的左闭右开区间

$[begin,\,end)$​​​​

#### 9.2.2 容器的类型别名成员

```c++
vector<int>::difference_type count;	// count 是 vector<int> 类定义的 difference_type 类型
list<string>::iterator iter;	// iter 是 list<string>类定义的 iterator 类型
```

#### 9.2.4 容器定义与初始化

| 容器定义和初始化                         | 作用                                                         |
| ---------------------------------------- | ------------------------------------------------------------ |
| C c;                                     | 默认构造函数                                                 |
| C c1(c2)                                 | c2 的拷贝                                                    |
| C c1 = c2                                | 且 c1 与 c2 容器和元素类型都相同                             |
| C c{a, b, c...}                          | 列表初始化                                                   |
| C c = {a, b, c...}                       |                                                              |
| C c(b, e)                                | 迭代器指定范围初始化                                         |
| **顺序容器(不包括 array)接受 size 参数** |                                                              |
| C seq(n)                                 | n 个元素值初始化(元素为**类类型**时用**默认构造函数初始化**) |
| C seq(n, t)                              | n 个元素, 初始化为 t                                         |

**标准库 array**

* 固定大小
* array 的大小也是类型的一部分
* 如果**元素是类**, 则必须有**默认构造函数**
* array 允许**拷贝和赋值**操作

#### 9.2.6 赋值和 swap

| 容器赋值运算         | 作用                                |
| -------------------- | ----------------------------------- |
| c1 = c2              | c2 的拷贝赋值                       |
| c = {a, b, c...}     | 列表赋值(array 不适用)              |
| swap(c1, c2)         | 交换 c1 和 c2 元素                  |
| c1.swap(c2)          | 交换 c1 和 c2 元素                  |
| seq.assign(b, e)     | 迭代器范围赋值, b 和 e 不能指向 seq |
| seq.assign(initlist) | 初始化列表赋值                      |
| seq.assign(n, t)     | n 个 t 元素的赋值                   |

**使用 assign (仅顺序容器)**

允许从一个**不同但相容**的容器赋值

```C++
list<string> s;
vector<const char * > p;
s.assign(p.cbegin(), p.cend());
```

**使用 swap**

除 array 的容器: 元素并未移动, swap 相当于仅交换了容器名

* 除 string 外, 指针不失效, 但指向的容器改变了
* string 的 swap 操作会使指针失效

array: swap 会真正交换二者的元素

* 绑定的地址不变, 但内容已经被交换了

#### 9.2.6 容器大小操作

* **size** : 元素数目
* **empty** : 是否为空
* **max_size** : 最大 **size**

#### 9.2.7 关系运算符

所有容器都支持 $==$ 和 $!=$​

除无序关联容器外都支持 $>, >=, <, <=$

关系运算符的运算对象必须**容器类型**和**元素类型**相同

**容器的关系运算符使用元素的关系运算符完成比较**

### 9.3 顺序容器操作

#### 9.3.1 向顺序容器添加元素

| 向顺序容器(不包括 array )添加元素                            | 作用                               |
| ------------------------------------------------------------ | ---------------------------------- |
| forward_list 有自己的 insert 和 emplace                      |                                    |
| forward_list 不支持 push_back 和 emplace_back                |                                    |
| vector 和 string 不支持 push_front 和 emplace_front          |                                    |
| c.push_back(t) 返回 void                                     | 尾部新增元素 t , 拷贝方式          |
| c.emplace_back(args) 返回 void                               | 尾部新增元素 , 构造方式            |
| c.push_front(t) 返回 void                                    | 头部新增元素 t , 拷贝方式          |
| c.emplace_front(args) 返回 void                              | 头部新增元素 , 构造方式            |
| c.insert(p, t) 返回新添加元素的迭代器                        | 迭代器 p 前新增元素 t , 拷贝方式   |
| c.emplace(p, args) 返回新添加元素的迭代器                    | 迭代器 p 前新增元素 , 构造方式     |
| c.insert(p, n, t) 返回新添加的第一个元素的迭代器, n 为 0 就返回 p | 迭代器 p 前新增 n 个元素 t         |
| c.insert(p, b, e) 返回新添加的第一个元素的迭代器, 范围为空就返回 p | 迭代器 p 前新增 b 到 e 范围的元素  |
| c.insert(p, initlist) 返回新添加的第一个元素的迭代器, 列表为空就返回 p | 迭代器 p 前新增元素值列表 initlist |

**push_back**

* 除 `array` 和 `forward_list` 外, 每个顺序容器都支持 `push_back`

* 拷贝方式, 新增到容器中的元素是一个拷贝

```C++
string word;
word.push_back('s'); 	// 等价于 word += 's'
// 新增到 string 中的元素是 's' 的拷贝
```

**push_front**

* `list`, `forward_list` 和 `deque` 支持

```C++
list<int> l;
l.push_front(1);
```

**insert**

* 指定位置添加元素

* `vector`, `deque`, `list` 和 `string` 支持 `insert`
* `forward_list` 支持特殊版本的 `insert`

```C++
vector<string> s;
vector<string> v;
s.insert(s.begin(), "Hello!");				// 单个插入
s.insert(s.begin(), 10, "Hello!");			// 多个插入, 指定数量和值
s.insert(s.begin(), v.end()-2, v.end());	// 多个插入, 指定迭代器范围
s.insert(s.begin(), {"Hello ", "world!"});	// 多个插入, 指定初始化列表
s.insert(s.begin(), s.begin(), s.end());	// 错误, 不能使用自身的迭代器来插入
```

* 返回插入的第一个元素的迭代器, 若插入的数量为 0 就返回迭代器 p 

**emplace**

新标准引入 `emplace_front`,  `emplace` 和 `emplace_back`, **构造方式新增元素**

`emplace` 方式会在**容器管理的内存空间中直接创建对象**, **参数必须与元素的构造函数匹配**

`push` 和 `insert` 方式则创建一个**局部临时对象**, 然后压入容器中

```C++
c.emplace_back("abc", 1, 2.0);	// 使用元素的构造函数直接在容器的内存空间创建元素的对象
c.push_back("abc", 1, 2.0);		// 错误, 没有该版本的 push_back 
c.push_back(Book("abc", 1, 2.0));	// 创建一个临时 Book 对象传递给 push_back

c.emplace_back();				// 使用元素的默认构造函数
c.emplace(iter, "abc");	// 使用类的 Book(string) 构造函数
```

#### 9.3.2 访问元素

访问元素的函数返回该元素的引用

如果是 const 对象, 则返回 const 引用

如果不是 const 对象, 则返回普通引用

| 顺序容器访问操作                                       | 作用                                                         |
| ------------------------------------------------------ | ------------------------------------------------------------ |
| at 和 下标 操作只适用于 vector, array, string 和 deque |                                                              |
| back 不适用于 forward_list                             |                                                              |
| c.back()                                               | 尾元素的引用, c 不能为空                                     |
| c.front()                                              | 首元素的引用, c 不能为空                                     |
| c[n]                                                   | 下标为 n 的元素的引用, n 为无符号整数<br />若 n >= c.size() 则函数行为未定义 |
| c.at(n)                                                | 下标为 n 的元素的引用<br />若下标越界欸, 则抛出 out_of_range 异常 |

```C++
if(!c.empty())
{
    auto a = c.back();	// 用引用给 a 拷贝初始化
    a = 10;				// c.back() 元素并未改变
    auto &a = c.back();	// a 是 c.back() 的引用
    a = 10;				// c.back() 元素改变
}
```

#### 9.3.3 删除元素

| 顺序容器删除操作                                             | 作用                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| 删除操作不适用于 array                                       |                                                              |
| forward_list 有特殊版本的 erase                              |                                                              |
| forward_list 不支持 pop_back; vector 和 string 不支持 pop_front |                                                              |
| c.pop_back()                                                 | 删除尾元素, 返回 void                                        |
| c.pop_front()                                                | 删除首元素, 返回 void                                        |
| c.erase(p)                                                   | 删除迭代器 p 指向的元素, 返回 p 后的迭代器                   |
| c.erase(b, e)                                                | 删除 b 和 e 指定范围的元素, 返回最后一个删除元素之后的迭代器 |
| c.clear()                                                    | 删除 c 中所有元素, 返回 void                                 |

#### 9.3.4 特殊的 forward_list 操作

`forward_list` 定义了 `before_begin`, 返回**首前迭代器**

| forward_list 插入/删除操作      | 作用                                                         |
| ------------------------------- | ------------------------------------------------------------ |
| flist.before_begin()            | 返回首前迭代器, 此迭代器不能解引用                           |
| flist.cbefore_begin()           | 首前迭代器 const 版本                                        |
| flist.insert_after(p, t)        | 迭代器 p 之后插入元素 t                                      |
| flist.insert_after(p, n, t)     | p 之后插入 n 个 t                                            |
| flist.insert_after(p, b, e)     | p 之后插入 b 到 e 指定的范围元素                             |
| flist.insert_after(p, initlist) | p 之后插入初始化列表                                         |
| flist.emplace_after(p, args)    | p 之后新增元素, 构造方式                                     |
| flist.erase_after(p)            | 删除 p 之后的元素, 返回删除元素之后的迭代器                  |
| flist.erase_after(b, e)         | 删除 b 到 e 指定范围的元素, 返回最后一个删除元素之后的迭代器 |

#### 9.3.5 改变容器大小

| 顺序容器改变大小操作 | 作用                                                         |
| -------------------- | ------------------------------------------------------------ |
| 不适用于 array       |                                                              |
| c.resize(n)          | 调整 size 为 n <br />若 n < c.size(), 则多的元素被丢弃<br />若 n >= c.size(), 则新增少的元素, 并值初始化 |
| c.resize(n, t)       | 调整 size 为 n ,若有新增, 则新增的元素初始化为 t             |

#### 9.3.6 容器操作可能会使迭代器失效

**添加元素操作**

* **vector 和 string**
  * 若存储空间**重新分配**, 则**迭代器, 指针和引用**都失效
  * 若存储空间不重新分配, 则**插入位置之前**的迭代器, 指针和引用**有效**, **插入位置之后**的迭代器, 指针和引用**失效**

* **deque**
  * 首尾位置添加元素, **迭代器失效, 引用和指针不会失效**
  * 首尾之外添加元素, **迭代器, 指针和引用都失效**

* **list 和 forward_list**
  * **迭代器, 指针和引用都有效**

**删除元素操作**

指向**被删除元素**的迭代器, 指针和引用**都失效**

* **vector 和 string**
  * 删除元素之前的**迭代器, 指针和引用仍有效**, 删除元素之后的**迭代器, 指针和引用都失效**

* **deque**
  * 首尾之外删除元素, **迭代器, 指针和引用仍有效**
  * 删除首尾元素, 仅尾后迭代器失效, 其余仍有效
* **list 和 forward_list**
  * **迭代器, 指针和引用仍有效**

### 9.4 vector 对象如何增长

**容量相关的成员函数**

| 容器 size 操作                                 | 作用                                              |
| ---------------------------------------------- | ------------------------------------------------- |
| shrink_to_fit 只适用于 vector, string 和 deque |                                                   |
| capacity 和 reserve 只适用于 vector 和 string  |                                                   |
| c.shrink_to_fit()                              | 将 capacity() 减少为 size() 大小                  |
| c.capacity()                                   | 当前 c 最多能保存多少元素                         |
| c.reserve(n)                                   | 空间至少能容纳 n 个元素, 如果不能, 就重新分配空间 |

### 9.5 额外的 string 操作
