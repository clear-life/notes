# C++

### 输入

#### 输入流

1. **来源**: 标准, 文件, 字符串
2. **数据类型**: 基本, 字符串, 自定义
3. **读取**: 格式化, 非格式化, 二进制
4. **状态**: 正常, 错误, 结束

**输入内容 = 空白符(\n\t\r' ') + 非空白符 + EOF**

**输入缓冲区 = 固定长度数组**

```
#include <sstream>

stringstream line(s);
while(line >> s)
```



#### 输入函数

**std::cin** 跳过前空白符, 后空白符留缓冲区

**std::getline** 读整行, 丢弃 `\n`

**cin.get** 读任一字符

```
getline(cin, str, delim)

std::cin.get(c)
c = std::cin.get()
```

