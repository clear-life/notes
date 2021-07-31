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

