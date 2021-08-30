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

#### 9.5.1 构造 string 的其他方法

| 构造 string 的其他方法   | 作用                                                         |
| ------------------------ | ------------------------------------------------------------ |
| n, pos, len 都是无符号数 |                                                              |
| string s(p, n)           | p 指向的字符数组的前 n 个字符的拷贝<br />该字符数组必须以空字符结尾 |
| string s(s2, pos)        | string s2 从下标 pos 开始的拷贝                              |
| string s(s2, pos, len)   | string s2 从下标 pos 开始 len 个字符的拷贝                   |
| s.substr(pos, n)         | 返回从 s 的 pos 开始的 n 个字符的 string 子串<br />pos 默认值为 0, n 默认值为 s.size() - pos |

#### 9.5.2 改变 string 的其他方法

| 修改 string 的其他方法         | 作用                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| s.insert(pos, args)            | 在 pos 前插入 args 指定的字符<br />pos 可以是下标和迭代器<br />下标则返回引用, 迭代器则返回迭代器 |
| s.erase(pos, len)              | 删除从 pos 开始的 len 个字符<br />len 省略时删除到末尾<br />返回 s 的引用 |
| s.assign(args)                 | 将 s 中的字符替换为 args 指定的字符<br />返回 s 的引用       |
| s.append(args)                 | 将 args 指定的字符追加到 s 末尾<br />返回 s 的引用           |
| s.replace(range, args)         | 删除 s 中 range 中的字符, 替换为 args 指定的字符<br />range 可以是 下标, 长度, 一对迭代器<br />返回 s 的引用 |
| args 可能是下列形式之一        | append 和 assign 可以使用所有形式                            |
| str 不能是 s, 迭代器不能指向 s |                                                              |
| str                            | string str                                                   |
| str, pos, len                  | str 中 pos 开始的最多 len 个字符                             |
| p, len                         | p 指向的字符数组的前(最多) len 个字符                        |
| p                              | p 指向的以空字符结尾的字符数组                               |
| n, c                           | n 个 c 字符                                                  |
| b, e                           | 迭代器 b 和 e 指定范围的字符                                 |
| initlist                       | 字符初始化列表                                               |

#### 9.5.3 string 搜索操作

* 返回类型: string::size_type, 表示下标
* 失败返回 string::npos 的 static 成员
* npos 的类型: const string::size_type, 初始化为 -1
  * npos 为无符号类型, 则 -1 表示 string 的 size 最大值

| string 搜索操作                       | 作用                                                         |
| ------------------------------------- | ------------------------------------------------------------ |
| 返回指定字符的下标, 未找到则返回 npos |                                                              |
| s.find(args)                          | args 第一次出现的位置                                        |
| s.rfind(args)                         | args 逆序第一次出现的位置(最后一次出现的位置)                |
| s.find_first_of(args)                 | args 中任一字符第一次出现的位置                              |
| s.find_last_of(args)                  | args 中任一字符最后一次出现的位置                            |
| s.find_first_not_of(args)             | 第一个不在 args 中的字符的位置                               |
| s.find_last_not_of(args)              | 最后一个不在 args 中的字符的位置                             |
| args 必须是下列形式之一               |                                                              |
| c, pos                                | 从 pos 开始查找字符 c, pos 默认为 0                          |
| s2, pos                               | 从 pos 开始查找字符串 s2, pos 默认为 0                       |
| p, pos                                | 从 pos 开始查找 p 指向的以空字符结尾的字符数组, pos 默认为 0 |
| p, pos, n                             | 从 pos 开始查找 p 指向的以空字符结尾的字符数组的前 n 个字符<br />pos 和 n 无默认值 |

#### 9.5.4 compare 函数

比较两个字符串/字符数组的大小

| s.compare() 的参数形式 | 作用                                                         |
| ---------------------- | ------------------------------------------------------------ |
| s2                     | 比较 s 和 s2                                                 |
| pos, n, s2             | s 中 pos 开始的 n 个字符与 s2 比较                           |
| pos1, n1, s2, pos2, n2 | s 中 pos1 开始的 n1 个字符 与 s2 中 pos2 开始的 n2 个字符比较 |
| p                      | 比较 s 和 p 指向的以空字符结尾的字符数组                     |
| pos, n, p              | s 中 pos 开始的 n 个字符与 p 指向的以空字符结尾的字符数组比较 |
| pos, n1, p, n2         | s 中 pos 开始的 n1 个字符与 p 指向的地址的 n2 个字符比较     |

#### 9.5.5 数值转换

| string 和数值之间的转换 | 作用                                                         |
| ----------------------- | ------------------------------------------------------------ |
| to_string(val)          | 返回 val 的 string 表示                                      |
| stoi(s, p, b)           | 返回 s 的数值表示(整数), b 是转换的基数, 默认为 10<br />返回 int |
| stol(s, p, b)           | p 是 size_t 指针, 默认为 0, 保存 s 中第一个非数值字符的下标<br />返回 long |
| stoul(s, p, b)          | 返回 unsigned long                                           |
| stoll(s, p, b)          | 返回 long long                                               |
| stoull(s, p, b)         | 返回 unsigned long long                                      |
| stof(s, p)              | 返回 s 的数值表示(浮点数)<br />返回 float                    |
| stod(s, p)              | 返回 double                                                  |
| stold(s, p)             | 返回 long double                                             |

### 9.6 容器适配器

**顺序容器适配器**

* stack 栈
* queue 单向队列
* priority_queue 优先队列

**容器适配器**

* 把一个容器**装饰为一种数据结构**

| 所有的容器适配器都支持的操作和类型                      | 作用                                  |
| ------------------------------------------------------- | ------------------------------------- |
| size_type                                               | size 的类型                           |
| value_type                                              | value 的类型                          |
| container_type                                          | 被装饰的容器的类型                    |
| A a;                                                    | 创建空适配器 a                        |
| A a(c);                                                 | 创建空适配器 a, 初始化为容器 c 的拷贝 |
| 所有适配器都支持所有的关系运算符<br />==, !=, <=, >, >= | 关系运算符                            |
| a.empty()                                               | a 是否为空                            |
| a.size()                                                | 返回 a 的元素数目                     |
| swap(a, b)                                              | 交换 a 和 b 的内容                    |
| a.swap(b)                                               | a 和 b 必须适配器类型和容器类型都相同 |

* 所有适配器都要求容器有**添加, 删除和访问尾元素**的能力

  * 所以 array 和 forward_list 不能用来构造容器适配器

* 默认情况下, stack 和 queue 基于 deque 实现, priority_queue 基于 vector 实现

* **只能使用适配器的操作**, 不能使用底层容器的操作

  ```C++
  deque<int> deq;
  stack<int> stk(deq);	// 从 deq 拷贝元素来初始化 stk
  
  // 基于 vector 实现 stack 数据结构
  stack<int, vector<int>> stk;
  ```

  

**栈适配器**

| 栈独有操作                                                | 作用                               |
| --------------------------------------------------------- | ---------------------------------- |
| stack 默认基于 deque 实现, 也可以在 list 或 vector 上实现 | stack 只要求能在末尾增删和访问元素 |
| s.pop()                                                   | 弹出栈顶元素, 但不返回元素值       |
| s.push(item)                                              | 压入元素, 拷贝方式                 |
| s.emplace(args)                                           | 压入元素, 构造方式                 |
| s.top()                                                   | 返回栈顶元素                       |

**队列适配器**

| queue 和 priority_queue 独有操作                             | 作用                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| queue 默认基于 deque 实现, priority_queue 默认基于 vector 实现 |                                                              |
| queue 也可以用 list 和 vector 实现, priority_queue 也可以用 deque 实现 |                                                              |
| q.pop()                                                      | 删除 queue 首元素 / 删除 priority_queue  最高优先级元素<br />但不返回该元素 |
| q.push(item)                                                 | queue 末尾入队元素 item / priority_queue 合适位置入队 item, 拷贝方式 |
| q.emplace(args)                                              | 入队操作, 构造方式                                           |
| q.front()                                                    | 返回首元素                                                   |
| q.back()                                                     | 返回尾元素, 仅 queue                                         |
| q.top()                                                      | 返回最高优先级元素, 仅 priority_queue                        |

```C++
priority_queue<Type, Container, Functional>
// Type: 元素类型
// Container: 容器类型
// Functional: 比较"运算符"
priority_queue <int, vector<int>，greater<int>> q;	// 升序
priority_queue <int, vector<int>，less<int>>q;		// 降序
```



## 第 10 章 泛型算法

### 10.1 概述

头文件:

* <algorithm> 
* <numeric>

算法工作过程

* 算法运行于迭代器之上, 不执行容器的操作, 只执行迭代器的操作
* 算法不会直接添加或删除容器元素

### 10.2 初识泛型算法

**输入范围**

泛型算法操作的元素范围, 使用两个迭代器(首元素和尾后元素迭代器)

#### 10.2.1 只读算法

```C++
accumulate(begin, end, sum)		// 输入范围和求和的初值
```

**操作两个序列**

```C++
equal(a1.begin, a1.end, a2.begin)
// 从 a2 某个迭代器开始比较与 a1 的输入元素是否相等
```

> 单一迭代器表示一个序列的时, 假定该序列与两个迭代器表示的范围一样长

* **算法不检查空间是否足够操作**, 由程序员检查

#### 10.2.2 写容器算法

**back_inserter**

* 插入迭代器
* 相当于调用 push_back 函数
* 输入: 容器的引用
* 输出: 容器的插入迭代器

```C++
auto it = back_inserter(容器);	// it 是与容器绑定的插入迭代器
*it = 1;			// 向容器插入元素 1
fill_n(it, 5, 0);	// 向容器插入 5 个 0
```

#### 10.2.3 重排容器算法

```C++
sort(a.begin, a.end)		// 对 a 排序
auto it = unique(a.begin, a.end)		// 重复元素放最后, 函数返回第一个重复元素的迭代器
a.erase(it, a.end)			// 删除从 it 到 a 末尾的所有元素
```



### 10.3 定制操作

自定义**比较运算符**, 即自定义**谓词**

* **谓词**是一个**可调用的对象/表达式**, **返回 bool 类型**的值
  * 可调用: **能用调用运算符调用**
  * **可调用对象**
    * 函数和函数指针
    * lambda 表达式
    * 重载了函数调用运算符的类
* 分为一元谓词和二元谓词

#### 10.3.1 传递函数

向算法传递函数

```C++
bool fun(int a, int b) {return a < b;}
sort(a.begin, a.end, fun);
```

#### 10.3.2 lambda 表达式

lambda 表达式形式

```C++
[capture list] (parameter list) -> return type {function body}
capture list: 捕获列表		不可忽略
    捕获 lambda 所在函数中的局部非 static 变量
parameter list: 参数列表	可忽略
return type: 返回类型		可忽略
function body: 函数体		 不可忽略
    
// 忽略参数列表和返回类型
auto f = [] {return 0;};
cout << f() << endl;		// 调用运算符调用
```

* lambda 表达式类似**未命名的内联函数**
* 忽略返回类型会自动推断返回类型, 若**包含 return 语句之外的语句**, 则**返回 void 类型**

* lambda 不能由默认参数
* lambda 可以直接使用当前函数之外的名字

#### 10.3.3 lambda 捕获和返回

定义 lambda 时, 编译器**生成一个新的类类型**

lambda **捕获的变量作为类的数据成员**在 lambda 对象初始化时初始化

**值捕获**

被捕获的变量在 lambda 对象**创建时拷贝初始化**

```C++
void fun()
{
    int a = 0;
    auto f = [a] {return a;};
    a = 1;
    auto b = f();	// b 的值为 0 而不是 1
}
```

**引用捕获**

引用捕获必须**保证**调用 lambda 时**被引用的对象存在**

```C++
void fun()
{
    int a = 0;
    auto f = [&a] {return a;};
    a = 1;
    auto b = f();	// b 的值是 1 
}
```

**隐式捕获**

告诉编译器自动推断捕获

= 表示值捕获, & 表示引用捕获

```C++
//值捕获
void fun()
{
    int a = 0;
    auto f = [=] {return a;};	// 值隐式捕获
    a = 1;
    auto b = f();	// b 的值是 0 
}

// 引用捕获
void fun()
{
    int a = 0;
    auto f = [&] {return a;};	// 引用隐式捕获
    a = 1;
    auto b = f();	// b 的值是 1 
}
```

**混合捕获**

隐式捕获与显式捕获混合使用, 但**二者必须捕获类型不同**

```C++
//值捕获
void fun()
{
    int a = 0, b = 0;
    auto f = [=, &b] {return a + b;};	// a 隐式值捕获, b 显式引用捕获
    a = 1;
    auto c = f();	// c 的值是 0 
}

// 引用捕获
void fun()
{
    int a = 0, b = 0;
    auto f = [&, b] {return a + b;};	// a 引用隐式捕获, b 显式值捕获
    a = 1;
    auto c = f();	// c 的值是 1 
}
```

**可变 lambda**

mutable 关键字使得**值传递的变量是可修改的左值**

> lambda 值捕获的变量依然没有修改, 修改的只是拷贝给 lambda 表达式的参数变量

```C++
void fun()
{
    int a = 0;
    auto f = [a] () mutable {return ++a;};	// 正常情况下传进去的 a 是 const 变量, 不能对其修改
    auto b = f();	// b 的值是 1 
}
```

**指定 lambda 返回类型**

```C++
[] (int i) {return i < 0 ? -i : i;};	// 返回类型推断为 int 

// 替换为 if 语句后, 看似与上式等价, 实际返回类型是 void
[] (int i) {if (i < 0) return -1; else return i;};	// 由于有 return 之外的语句, 所以推断为 void 返回类型

// 使用 return 之外的语句需要显式指定返回类型
[] (int i) -> int {if (i < 0) return -1; else return i;};
```

#### 10.3.4 参数绑定

**标准库 bind 函数**

函数适配器

输入: 可调用对象

输出: 新的可调用对象, 参数列表不同

```C++
auto newCallble = bind(callable, arg_list);
// callable: 可调用对象
// arg_list: 参数列表, 包含占位符 _n, 表示 newCallble 的第 n 个参数
// newCallble: 新的可调用对象
```

```C++
check_size(string s, int sz);

auto check6 = bind(check_size, _1, 6);

// _1 和 6 对应 check_size 的两个参数 string s 和 int sz
// _1 表示是 check6 的第一个参数, 同时也是唯一一个参数
// 调用 check6 只需要 string 类型的 _1 参数, 然后调用 _1 和 6 为参数的 check_size 函数 
```

**使用 placeholders 名字**

命名空间:  

std :: placeholders :: _n

**bind 的参数**

bind 可以重新安排参数顺序

```C++
auto g = bind(f, a, b, _2, c, _1);
// f 的参数为 a, b, _2, c, _1
// g 的参数为 _1, _2
g(_1, _2)	映射为 f(a, b, _2, c, _1) 
```

**bind 绑定引用参数**

默认情况下, bind 的非占位符参数被拷贝到旧的可调用对象里

但我们希望能用**引用传递**方式传递非占位符参数

* 标准库 `ref` 函数返回对象的引用, 该引用是可以拷贝的
* 标准库 `cref` 函数返回对象的 `const` 引用

```C++
ostream os;		// ostream 对象不能拷贝, 只能引用
bind(print, ref(os), _1, ' '); 		// ref 返回 os 的引用, 且该引用可以被拷贝, 符合 bind 的要求
```

### 10.4 再探迭代器

除了普通迭代器之外, 还有四种特殊迭代器

头文件: **<iterator>**

* 插入迭代器( insert iterator )	

  插入元素

* 流迭代器( stream iterator )

  遍历 IO 流

* 反向迭代器( reverse iterator )

  反向移动

* 移动迭代器( move iterator )

  移动元素

#### 10.4.1 插入迭代器

* 迭代器适配器
  * 输入: 容器
  * 输出: 插入迭代器

| 插入迭代器操作     | 作用                                                         |
| ------------------ | ------------------------------------------------------------ |
| it = value         | 插入迭代器 it 指向的位置插入值 value<br />共 push_front push_back insert 三种插入方式 |
| *it, ++it, it++    | 操作依然存在, 但 it 无任何变化, 都返回 it                    |
| **插入迭代器类型** | **作用**                                                     |
| front_inserter     | 调用 push_front                                              |
| back_inserter      | 调用 push_back                                               |
| inserter           | 调用 insert                                                  |

```C++
deque<int> a = ...;
auto it = insert(a, iter);
// 向 it 赋值会永远在 iter 迭代器之前插入元素

auto it = front_insert(a);
// 向 it 赋值会永远在容器 a 的开头插入元素

auto it = back_insert(a);
// 向 it 赋值会永远在容器 a 的末尾插入元素
```

#### 10.4.2 iostream 迭代器

IO 类型对象的迭代器, **将流当作特定类型的元素序列来处理**

* istream_iterator

  读取输入流

* ostream_iterator

  向输出流写数据

**istream_iterator**

* 绑定流, 不绑定就初始化为**尾后迭代器 end**
* 读取的类型必须定义 `>>` 输入运算符

``` C++
istream_iterator<int> it(cin);	// 绑定 cin 流对象, 从 cin 流读取 int 数据
istream_iterator<int> eof;		// 尾后迭代器 end, 判断是否结束使用
vector<int> a;

while(it != eof)
    a.push_back(*it++);			// 与普通迭代器一样, *it++ 作用是返回当前迭代器指向元素的值并递增迭代器

// 等价于
vector<int> a(it, eof);			// 用迭代器范围构造容器初始值
```

| istream_iterator 操作               | 作用                                                         |
| ----------------------------------- | ------------------------------------------------------------ |
| istream_iterator<T> it ( istream ); | it 从输入流 istream 读取类型为 T 的值                        |
| istream_iterator<T> end;            | 尾后迭代器                                                   |
| it1 == it2                          | 读取类型相同, 若都是尾后迭代器或绑定到相同的输入流, 则二者相等 |
| it1 != it2                          | 否则, 二者不等                                               |
| *it                                 | 解引用, 返回从流中读取的值                                   |
| it -> mem                           | 等价于 (*it).mem                                             |
| ++it, it++                          | 与普通迭代器相同                                             |

**ostream_iterator**

| ostream_iterator 操作              | 作用                                                         |
| ---------------------------------- | ------------------------------------------------------------ |
| ostream_iterator<T> it(ostream);   | it 将类型 T 的值写入输出流 ostream 中                        |
| ostream_iterator<T> it(ostream, d) | it 将类型 T 的值写入输出流 ostream 中, 每个值后面都添加一个 d<br />d 是一个 C 风格字符串指针 |
| it = value                         | 用 << 运算符将 value 写入 it 绑定的输出流中                  |
| *it, ++it, it++                    | 运算符存在, 但 it 无任何变化, 结果都为 it                    |

```C++
vector<int> a = ...;
ostream_iterator<int> it(cout, " ");
for(auto i; a)
    *it++ = i;		// *it++ = i 与 it = i 等价
```

#### 10.4.3 反向迭代器

反向移动的迭代器

> forward_list 不支持

* 反向迭代器需要 `++` 和 `--` 运算符

* base 成员函数得到反向迭代器对应的普通迭代器

  ```C++
  a.rbegin().base();
  ```

* it 和 it.base() **位置相邻** 

### 10.5 泛型算法结构

算法要求的迭代器操作可分为 5 个类别

| 迭代器类别     | 作用                       |
| -------------- | -------------------------- |
| 输入迭代器     | 只读, 单遍, 递增           |
| 输出迭代器     | 只写, 单遍, 递增           |
| 前向迭代器     | 读写, 多遍, 递增           |
| 双向迭代器     | 读写, 多遍, 递增递减       |
| 随机访问迭代器 | 读写, 多遍, 全部迭代器运算 |

#### 10.5.1 5 类迭代器

**输入迭代器**

* `==`, `!=`
* `++`
* `*`
* `->`

**输出迭代器**

* `++`
* `*`

**前向迭代器**

* 可读写

* 只能沿一个方向移动

**双向迭代器**

* 可正向/反向读写
* `++`, `--`

**随机访问迭代器**

支持所有操作

#### 10.5.2 算法形参模式

| 算法参数形式                           | 作用                        |
| -------------------------------------- | --------------------------- |
| alg(beg, end, other args);             | beg, end 表示算法的输入范围 |
| alg(beg, end, dest, other args);       | dest 目的位置               |
| alg(beg, end, beg2, other args);       | beg2 第二个范围             |
| alg(beg, end, beg2, end2, other args); | beg2, end2 第二个范围       |

> 算法假定空间足够大

#### 10.5.3 算法命名规范

**重载形式传递谓词**

```C++
unique(beg, end);
unique(beg, end, compare);	// 谓词 compare 比较元素
```

**_if 版本**

```C++
find(beg, end, value);		// 查找 value 第一次出现的位置
find(beg, end, 谓词);		   // 查找第一个使 谓词 为真的元素值
```

**_copy 版本**

```C++
reverse(beg, end);				// 反转输入范围的元素
reverse_copy(beg, end, dest);	// 将输入范围的元素逆序拷贝到 dest 中
```

### 10.6 特定容器算法

| list 和 forward_list 成员函数版本的算法 | 作用                                           |
| --------------------------------------- | ---------------------------------------------- |
| **都返回 void**                         |                                                |
| list.merge(list2)                       | list2 的元素合并到 list 中, list 和 list2 有序 |
| list.merge(list2, compare)              | list2 的元素会被删除                           |
| list.remove(value)                      | 调用 erase 删除元素 value                      |
| list.remove_if(谓词)                    | 调用 erese 删除使 谓词 为真的元素              |
| list.reverse()                          | 反转 list 的元素                               |
| list.sort()                             | 排序 list 元素                                 |
| list.sort(谓词)                         | 使用谓词排序 list 元素                         |
| list.unique()                           | 调用 erase 删除重复的元素                      |
| list.unique(谓词)                       | 调用 erase 删除使谓词相等的元素                |

**splice**

此算法是链表特有, 不需要通用版本

| list 和 forward_list 的 splice 成员函数的参数        |
| ---------------------------------------------------- |
| list.splice(args) 和 forward_list.splice_after(args) |
| (p, list2)                                           |
| (p, list2, p2)                                       |
| (p, list2, b, e)                                     |

**链表特有的操作会改变容器**

## 第 11 章

**关联容器**: `map`, `set`

**头文件**

* `<map>`: `map`, `multimap`
* `<set>`: `set`, `multiset`
* `<unordered_map>`: `unordered_map`, `unordered_multimap`
* `<unordered_set>`: `unordered_set`, `unordered_multiset`

| 关联容器类型       | 说明                |
| ------------------ | ------------------- |
| **按关键字有序**   |                     |
| map                | 关联数组: key-value |
| set                | 集合                |
| multimap           | key 可重复          |
| multiset           | key 可重复          |
| **无序(哈希函数)** |                     |
| unordered_map      | 哈希 map            |
| unordered_set      | 哈希 set            |
| unordered_multimap | key 可重复          |
| unordered_multiset | key 可重复          |

### 11.1 使用关联容器

**map**

```C++
map<string, int> a;
string s;
while(cin >> s)
    a[s]++;
```

**set**

```C++
set<string> a = {"One", "Two", "Three"};
a.find("Two");
```

### 11.2 关联容器概述

关联容器的迭代器都是双向的

#### 11.2.1 定义关联容器

```C++
map<string, int> m = {{"One", 1},
                      {"Two", 2},
                      {"Three", 3}};		// {key, value}
set<string> s = {"One", "Two", "Three"};
```

**初始化 `multimap` 和 `mutliset`**

```C++
vector<int> v = {0,0,1,1,2,2};
set<int> s(v.cbegin(), v.cend());		// 用 v 初始化 s
multiset<int> ms(v.cbegin(), v.cend());	// 用 v 初始化 ms, ms 允许 key 重复
cout << s.size() << " " << ms.size() << endl;	// s 的 size 为 3, ms 的 size 为 6
```

#### 11.2.2 关键字类型的要求

**有序容器的关键字类型**

关键字类型必须定义**严格弱序**($\leq$)

> * 组织元素的操作的类型也是容器类型的一部分
>
> * 必须在定义关联容器时提供该操作的类型
> * 尖括号中的类型, 仅仅只是一个类型, 只有在创建容器对象的时候, 才会以构造函数的形式提供真正的比较操作

**使用关键字类型的比较函数**

```C++
// 比较函数, 定义 "<"
bool compare(const pair<int,int> &a, const pair<int,int> &b)
{
    return a.second < b.second;
}
set<pair<int,int>,decltype(compare)*> s(compare);	// 传入 compare 函数作为构造函数的参数
s.insert({{1,2}, {2,4},{3,6}});
```

#### 11.2.3 pair 类型

头文件: `<utility>`

```C++
pair<int,int> p = {1,2};
```

| pair 的操作                | 作用                                      |
| -------------------------- | ----------------------------------------- |
| pair<T1, T2> p;            |                                           |
| pair<T1, T2> p(v1, v2);    |                                           |
| pair<T1, T2> p = {v1, v2}; |                                           |
| make_pair(v1, v2);         |                                           |
| p.first                    |                                           |
| p.second                   |                                           |
| p1 relation_operator p2    | 关系运算符                                |
| p1 == p2                   | first 和 second 成员都相等时, pair 才相等 |
| p1 != p2                   |                                           |

**返回 pair 对象的函数**

```C++
pair<string, int> fun(vector<string> &v)
{
    if(!v.empty())
        return {v.back(), v.back().size()};	// 列表初始化
    	return make_pair(v.back(), v.back().size());	// 函数构造
    else
        return pair<string, int>();			// 默认初始化	
}

```

### 11.3 关联容器

| 关联容器的类型别名 | 说明       |
| ------------------ | ---------- |
| key_type           | 关键字类型 |
| mapped_type        | 关联的类型 |
| value_type         | 元素的类型 |

* `set`
  * key_type 和 value_type 一样
* `map`
  * value_type 是 pair
  * key_type 和 mapped_type 是 pair 的 first 和 second 的类型
  * 由于 关键字 不能修改, 所以 pair 的 first 类型是 const 版本的

``` C++
set<string>::key_type s;		// string
set<string>::value_type s;		// string
map<string, int>::key_type s;	// string
map<string, int>::mapped_type s;// int
map<string, int>::value_type s;	// pair<const string, int>
```

#### 11.3.1 关联容器迭代器

解引用关联容器的迭代器时, 得到容器的 `value_type` 的引用类型

```C++
map<string, int> m;
auto it = m.begin();
cout << it -> first << " " << it -> second << endl;
it -> second ++;
```

**set 的迭代器是 const**

虽然 set 同时定义了 iterator 和 const_iterator 类型, 但二者都只能读取 set 中的元素, 因为 set 的元素是关键字, 不能修改

> 有序容器的迭代器按关键字升序遍历元素

#### 11.3.2 添加元素

向 `set` 和 `map` 插入已存在的元素对容器没有任何影响

```C++
vector<int> v = {2,4,6,2,4,6};
set<int> s;
s.insert(v.begin(), v.end());	// s 现有 3 个元素 {2,4,6}
s.insert({1,3,5,1,3,5});		// s 现有 6 个元素 {1,2,3,4,5,6}
```

**向 map 添加元素**

`map` 的元素类型是 `pair`

```C++
map<string, int> m;
string s;
m.insert({s,1});
m.insert(make_pair(s,1);
m.insert(pair<string,int>(s,1));
m.insert(map<string,int>::value_type(s,1));
```

| 关联容器 insert 操作 | 作用                                                         |
| -------------------- | ------------------------------------------------------------ |
| c.insert(v)          | v 是 value_type 类型, args 用来构造 value_type 对象          |
| c.emplace(args)      | 返回 pair, first 是迭代器, 指向插入元素, second 是 bool, 表示是否插入成功 |
| c.insert(b, e)       | 插入范围元素                                                 |
| c.insert(initlist)   | 插入初始化列表元素                                           |
| c.insert(p, v)       | p 作为提示, 指出应从哪里开始搜索插入元素的位置, 返回迭代器, 指向插入元素 |
| c.emplace(p, args)   |                                                              |

**检查 insert 的返回值**

```C++
map<string,int> m;
string s;
auto it = m.insert({s,1});
if(!it.second)
    it.first -> second ++;	// it.first 是指向插入元素的迭代器, it.first -> second 是插入元素的关联值 
```

**向 multiset 或 multimap 添加元素**

```C++
multimap<string,int> m;
m.insert({"a",1});
m.insert({"a",2});
```

> 允许重复的关联容器, 接收单个元素的 insert 返回指向插入元素的迭代器

#### 11.3.3 删除元素

| 关联容器删除操作 | 作用                                                         |
| ---------------- | ------------------------------------------------------------ |
| c.erase(k)       | 删除所有关键字为 k 的元素, 返回 size_type 类型, 表示删除的元素数量 |
| c.erase(p)       | 删除迭代器 p 指向的元素, 返回删除的后一个元素的迭代器        |
| c.erase(b, e)    | 删除迭代器 b 到 e 范围内的元素, 返回 e                       |

```C++
multimap<string, int> m = {{"a",1},{"a",2}};
auto count = m.erase("a");		// 删除所有关键字为 "a" 的元素, count 的值为 2
```

#### 11.3.4 map 的下标操作

| 不重复 map (map 和 unordered_map)的下标操作 | 作用                                                         |
| ------------------------------------------- | ------------------------------------------------------------ |
| c[k]                                        | 返回关键字为 k 的关联值, 如果 k 不存在, 就添加进去, 并进行初始化 |
| c.at(k)                                     | 返回关键字为 k 的关联值, 如果 k 不存在, 就抛出 out_of_range 异常 |

```C++
map<string, int> m;
m["a"] = 1;		// 由于 m 的关键字没有 "a" , 所以添加 pair<"a", 1> 到 m 中
```

> * map 的下标操作返回 mapped_type 对象的引用
> * 解引用 map 的迭代器会得到 value_type 对象的引用

#### 11.3.5 访问元素

| 关联容器查找元素操作                                         | 说明                                                         |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| lower_bound 和 upper_bound 不适用于无序容器                  |                                                              |
| 下标操作只适用于非 const 的 map 与 unordered_map, 即 const 版本不能进行下标操作 |                                                              |
| c.find(k)                                                    | 返回一个迭代器, 指向第一个关键字为 k 的元素<br />若 k 不存在, 则返回尾后迭代器 |
| c.count(k)                                                   | 返回关键字为 k 的元素数量                                    |
| c.lower_bound(k)                                             | 返回一个迭代器, 指向第一个关键字 $\geq$ k 的元素             |
| c.upper_bound(k)                                             | 返回一个迭代器, 指向第一个关键字 $\textgreater$  k 的元素    |
| c.equal_range(k)                                             | 返回一个 pair, 表示关键字等于 k 的元素范围, 若 k 不存在, pair 的两个成员都是 c.end() |

```C++
map<string, int> m = {{"a",1},{"b",2},{"c",3}};
if(m.find("a") == m.end())		// 判断关键字为 "a" 的元素是否在 m 中
    ...
    
multimap<string, int> m = {{"a",1},{"a",2},{"c",3}};
auto count = m.count("a")
if (count)			// 根据查找的数量判断是否有关键字为 "a" 的元素		
    ...
```

**面向迭代器的解法**

```C++
multimap<string, int> m = {{"a",1},{"a",2},{"c",3}};
// 访问所有关键字为 "a" 的元素, 如果没有该关键字, 则 b 和 e 直接相等, for 循环直接结束
for(auto b = m.lower_bound("a"), e = m.upper_bound("a"); b != e; b++)
    ...
或
// 直接调用 equal_range 函数获取关键字为 "a" 的元素的迭代器范围
for(auto p = m.equal_range("a"); p.first != p.second; ++p.first)
    ...
```

### 11.4 无序容器

用哈希函数和关键字的 `==` 运算符组织元素

**使用无序容器**

元素并未按关键字的顺序存储

```C++
unordered_map<string,int> um;
string s;
um[s] = 1;
```

**管理桶**

一个桶是一个哈希值的链表

| 无序容器管理操作       | 说明                            |
| ---------------------- | ------------------------------- |
| **桶接口**             |                                 |
| c.bucket_count()       | 当前桶的数量                    |
| c.max_bucket_count()   | 最大的桶数量                    |
| c.bucket_size(n)       | 第 n 个桶的 size                |
| c.bucket(k)            | 查找关键字为 k 的元素所在的桶   |
| **桶迭代**             |                                 |
| local_iterator         | 桶中元素的迭代器                |
| const_local_iterator   | const 版本                      |
| c.begin(n), c.end(n)   | 桶 n 的首元素迭代器和尾后迭代器 |
| c.cbegin(n), c.cend(n) | const 版本                      |
| **哈希策略**           |                                 |
| c.load_factor()        | 装载因子, 桶的平均元素数量      |
| c.max_laod_factor()    | 最大装载因子                    |
| c.rehash(n)            | 重新 hash                       |
| c.reserve(n)           | 重新存储, 但不必重新 hash       |

**无序容器对关键字类型的要求**

无序容器使用 hash<key_type> 类型的对象来生成哈希值

* STL 为 内置类型, string, 智能指针 和一些标准库类型提供了 hash 模板
* 但关键字为自定义类型的话, 需要专门提供 hash 模板版本

```C++
Class c;
int hasher(const Class &c)
{return hash<int>()(c.int);}

bool equal(const Class &c1, const Class &c2)
{return c1.string == c2.string;}

unordered_set<Class, decltype(hasher)*, decltype(equal)*> us = (1, hasher, equal);
```



