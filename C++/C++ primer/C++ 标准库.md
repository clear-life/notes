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

3. 扩展: 读写**文件**, 读写内存 `string**`**

**IO 库头文件和类类型**

| 头文件     | 类型                                                         |
| ---------- | ------------------------------------------------------------ |
| `iostream` | `istream`, `wistream` 从流读<br />`ostream`, `wostream` 向流写<br />`iostream`, `wiostream` 读写流 |
| `fstream`  | `ifstream`, `wifstream` 从文件读<br />`ofstream`, `wofstream` 向文件写<br />`fstream`, `wfstream` 读写文件 |
| `sstream`  | `istringstream`, `wistringstream` 从 `string` 读<br />`ostringstream`, `wostringstream` 向 `string` 写<br />`stringstream`, `wstringstream` 读写 `string` |

