# C++ 算法题语法总结

## 通用

### IO

```C++
cin.peek()		// 返回输入流的下一字符, 且并不会跳过该字符
    
c = getchar()	// cstdio 获取一个字符, 可以是空白符, 常用来吸收掉 '\n'
    
<cstring> memset(a, -1/0/0x3f, sizeof(a));	// 按字节赋值
   
// 加速输入输出
ios::sync_with_stdio(false);	// 设置不兼容 stdio
cin.tie(0);						// 解除 cin 和 cout 的绑定, 加快效率
cout.tie(0);
```



#### 字符串

```C++
strlen(C 风格字符数组的某个起始位置)		// 返回从起始位置开始的长度, 头文件 cstring
    
// 判断字符 c 是否是 0 ~ 9 之间的数字
if(isdigit(char c)
   cout << "是数字" << endl;
else
   cout << "不是数字" << endl;
```



#### `<algorithm>`

```C++
// 最值
*max_element(v.begin(), v.end());	
*min_element(v.begin(), v.end());	
```



**范围 for 循环**

```C++
map<int, int> mp;
for(auto [a, b] : mp)
    cout << a << " " << b << endl;

for(auto &[a, b] : mp)
```



## STL

### 常用容器

**vector**: 变长数组, 倍增思想

**pair<int, int>**: 二元组

**string**: 字符串, substr(), c_str()

**queue**: 队列, push(), pop(), front(), back()

**priority_queue**: 优先队列(堆), push(), pop(), top()

**stack**: 栈, push(), pop(), top()

**deque**: 双端队列

**set, map, multiset, multimap**: 平衡二叉树(红黑树), 动态维护有序序列

**unordered_set, unordered_map, unordered_multiset, unordered_multimap**: 哈希表, O(1)

**bitset**: 压位



### 通用用法

所有(或大多数)容器都有的用法

**size()**		元素个数

**empty()**	是否为空

**clear()**		清空容器

> queue, priority_queue, stack 没有 clear 函数

**front()**/**back()**	首尾元素的引用

**begin()**/**end()**	首及尾后迭代器

操作系统分配空间的速度, 与空间大小无关, 与申请次数有关

返回迭代器时, 如果不存在, 一般返回 endl 迭代器

### 具体用法

#### vector

```C++
#include <vector>

vector<int> a(10, 1);	// 初始化长度为 10, 值为 1 的数组

push_back()/pop_back()	// 插入或删除末尾元素

[] 		// 随机访问
vector<int> a(4, 3), b(3, 4)	// 支持比较运算
a < b 为 true
```

#### pair

```C++
first		// 第一个元素
second		// 第二个元素
			// 支持比较运算, 默认以第一个元素为主, 第二个元素为次
// 初始化
a = make_pair(1, 2)
a = {1, 2}
```



#### string

```C++
string a = "ab"
a += "b"
a += 'c'
    
a.substr(1, 2)		// 返回从下标为 1 开始, 长度为 2 的子串
a.substr(1)
a.length()			// 返回字符串长度
    
a.c_str()			// 返回对应 C 字符串的首地址
```



#### queue

```C++
// queue, priority_queue, stack 没有 clear 函数

q = queue<int>();	// 清空队列 q
```



#### deque

```C++
deque<int> d([size], [value])
deque<int> d(begin, end) 
    
// 成员函数
front() back()
push_back() pop_front()
pop_back() pop_front()
```





#### priority_queue

```C++
// 默认大顶堆
push()	// 插入元素
pop		// 弹出堆顶元素
top		// 返回堆顶元素
    
priority_queue<int, vector<int>, greater<int>> heap;	// 定义小顶堆
```





#### stack

```C++
push()	// 向栈顶插入元素
pop		// 弹出栈顶元素
top		// 返回栈顶元素
```



#### set/multiset

```C++
#include <set>
insert()	// 插入
find()		// 查找
count()		// 返回某个数的个数
erase()		// 删除元素
    输入数 x, 删除所有 x
    输入迭代器, 删除迭代器对应的元素
lower_bound(x)	// 返回 >= x 的最小数的迭代器, 不存在就返回 endl 迭代器
upper_bound(x)	// 返回 > x 的最小数的迭代器, 不存在就返回 endl 迭代器
```



#### map/multimap

```C++
#include <map>
insert()	// 插入, 插入元素是 pair
find()		// 查找
erase()		// 删除元素, pair 或迭代器
    输入数 x, 删除所有 x
    输入迭代器, 删除迭代器对应的元素
lower_bound()/upper_bound()
    
map<string, int> m;
m["yjq"] = 1;	// [] 随机访问
```



#### bitset

```C++
bitset<1000> b;		// 长度为 1000 的数组, 每个元素大小是 1 位, b 可看作以恶整数
&, |, ~, ^
>>, <<
==, != 
[]
    
count()		// 返回 1 的个数
any()		// 是否有 1
none()		// 是否全为 0

set()		// 所有位置为 1
set(k, v)	// 第 k 位置为 v
reset()		// 所有位置为 0

flip()		// 所有位取反
flip(k)		// 第 k 位取反
```

