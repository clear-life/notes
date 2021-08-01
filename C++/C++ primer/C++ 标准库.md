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

缓冲机制使得可以将多个输出组合为一个输出, 减少 IO 操作

缓冲刷新原因:

1. 正常结束
2. 缓冲区满
3. 操纵符
4. 流关联

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

