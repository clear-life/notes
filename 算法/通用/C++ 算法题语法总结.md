# C++ 算法题语法总结

## 通用

### IO

```C++
cin.peek()		// 返回输入流的下一字符, 且并不会跳过该字符
    
c = getchar()	// cstdio 获取一个字符, 可以是空白符, 常用来吸收掉 '\n'
    
<cstring> memset(a, -1/0/0x3f, sizeof(a));	// 按字节赋值
   
// 加速输入输出
ios::sync_with_stdio(0);	// 设置不兼容 stdio
cin.tie(0);						// 解除 cin 和 cout 的绑定, 加快效率
cout.tie(0);


// 不固定输入
#include <sstream>

string line;			// line 用于保存一行输入
getline(cin, line);		// 接收一行字符, 包含空白符
stringstream ssin(line);// 用 line 初始化字符流 ssin
while(ssin >> a[n]) n++;// 用字符流按 a[n] 的类型输入给 a[n], 不包括空白符
```

**cin**

过程: 忽略并清除空白符, 读取成功后, 分隔符留在缓冲区

```C++
int n;
cin >> n;

输入: 
  3		// 两个空格 + 数字 3
结果:
n = 3, 并且缓冲区剩下一个换行符
```

**getline**

过程: 

1. string s 记录遇到分隔符前的字符串, 初始化为空
2. 遇到分隔符后, 将 s 记录的字符出赋值给目标字符串
3. 然后跳过分隔符, 返回流对象

```C++
int n;
cin >> n; cin.get();

string t;
for(int i = 0; i < n; i++)
    getline(cin, t);
```







#### 字符串

```C++
strlen(C 风格字符数组的某个起始位置)		// 返回从起始位置开始的长度, 头文件 cstring
    
// 判断是否是数字和小写字母
isdigit(c)	// 数字
islower(c)	// 小写字母
isupper(c)	// 大写字母
isalpha(c)	// 字母
isalnum(c)	// 字母和数字

// 获取一行字符, 包含空格
getline(cin, s);
    
string.substr(pos, len)	// 从 pos 开始的 len 个长度的子字符串

// 将字符 c 转化为小写/大写字母, 如果 c 是其余字符, 则不变
tolower(c);
toupper(c);
   
stoi(str, st, k);	// 将从 str 字符串 st 位置及以后的部分看作 k 进制并转化为十进制的数字
stoi(str)	// 将 str 转化为数字, 默认从 0 开始, 转化为十进制数
   
// 将十六进制补码字符串转化为十进制整数, 32 位
long long a = stoll(s, 0, 16);
long long res = a;
if(a >= 1ll << 31) res = a - (1ll << 32);
```

**stringstream**

字符串流

```C++
#include <sstream>

string line, word;		
getline(cin, line);		// geline 可以输入空白符
stringstream ssin(line);// 字符串流绑定到 line 上
while(ssin >> word)		// ss 会把 s 中每个单词输入到 word 上
{
    ...
}
```





**范围 for 循环**

```C++
map<int, int> mp;
for(auto [a, b] : mp)
    cout << a << " " << b << endl;

for(auto &[a, b] : mp)
```

### 位运算

**异或**

相同为 0, 不同为 1

```C++
// 将某位取反    
int reverse(int x, int k)
{
	return x ^ (1 << k);
}
```

### 运算符

#### 运算符优先级

| 运算符 | 说明     |
| ------ | -------- |
| +  -   | 加减     |
| <<  >> | 左移右移 |
| ==     | 等于     |
| &      | 按位与   |
| &&     | 逻辑与   |

**三目运算符**

三目运算符优先级较低, 如果要在表达式中嵌套使用三目运算符, 那么就需要加括号

```C++
int val = ( bool ? a : b) + ...
```



## 注意事项

不能在使用容器 size的同时, 改变容器 size

```C++
for(int i = 0; i < arr.size(); i++)
{
    ...
    arr.push_back(..);
}
```



## STL

### <algorithm\>

```C++
// 最值
*max_element(v.begin(), v.end());	
*min_element(v.begin(), v.end());

// min max
min(a, b) max(a, b)	// a 和 b 的类型必须要相同
// a 是 int, b 是 double/ size_type 都不行, 必须进行强制类型转换

// 累加
accumulate(v.begin(), v.end(), 初值);

// 去除重复元素
arr[N] = {...}

sort(arr, arr + n);		// 去重前必须排序, 因为去重是把相邻重复元素去掉
auto end = unique(arr, arr + n);	// 去除相邻的重复元素, 其实是重复元素的位置被后面的元素覆盖了, 返回最后一个不重复元素的下一位置, 也就是重复元素区域的第一个位置, 类型是迭代器
int len = end - arr;	// end 迭代器减去数组首地址就是去重后数组的大小
int len = unique(arr, arr + n) - arr;	// 一条语句实现去重 + 计算数组大小的功能

// lower_bound >=
lower_bound(first, last, val);		// >= val 的第一个元素
lower_bound(first, last, val, cmp);	// >= val 的第一个元素, cmp 指定大小关系

// upper_bound >
upper_bound(first, last, val);		// > val 的第一个元素
upper_bound(first, last, val, cmp);	// > val 的第一个元素, cmp 指定大小关系
```



### 容器

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

头文件 `<utility>`

但不用特意引入该头文件

因为 `<iostream>` 会引入

```C++
first		// 第一个元素
second		// 第二个元素
			// 支持比较运算, 默认以第一个元素为主, 第二个元素为次
// 初始化
a = make_pair(1, 2)
a = {1, 2}

// pair 数组 memset
pair<int, int> a[10];
memset(a, -1, sizeof a);	// a 数组的值都被初始化为 {-1, -1}
```



#### string

```C++
// 初始化
string s = "ab"
string s(10, 'a')	// s 是十个 'a'
s += "b"
s += 'c'
    
s.substr(1, 2)		// 返回从下标为 1 开始, 长度为 2 的子串
s.substr(1)
    
s.find(c)		// 返回字符串 s 中字符 c 的第一个位置, 失败返回 s.npos
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
// 初始化
priority_queue<int> q(v.begin(), v.end());	// v 是容器

// 默认大顶堆
push()	// 插入元素
pop		// 弹出堆顶元素
top		// 返回堆顶元素
    
priority_queue<int, vector<int>, greater<int>> heap;	// 定义小顶堆

// 自定义比较函数
struct cmp
{
    bool operator()(const int a, const int b) 
    {
        return a < b;
	}
}
priority_queue<int, vector<int>, cmp> heap;		// cmp 作为一个类型

sort(v.begin(), v.end(), cmp());				// cmp 作为一个函数
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
find()		// 查找某个数, 返回一个迭代器
count()		// 返回某个数的个数
erase()		// 删除元素
    输入数 x, 删除所有 x
    输入迭代器, 删除迭代器对应的元素
S.lower_bound(x)	// 返回 >= x 的最小数的迭代器, 不存在就返回 end 迭代器
S.upper_bound(x)	// 返回 > x 的最小数的迭代器, 不存在就返回 end 迭代器
    

// 初始化
unordered_set<int> set;

// 插入
set.insert(key);

// 删除
set.erase(key) 		// 删除所有值为 key 的元素
set.erase(iterator)	// 删除迭代器对应的值
set.erase(set.find(key))	// 删除一个值为 key 的元素
   
// 查询
set.find(key)		// 返回值为 key 的一个元素的迭代器

// 查询元素的个数
set.count(key)

// 集合大小
set.size()

// 遍历集合
for(auto it = set.begin(); it != set.end(); it++)
    cout << *it << " ";

for(auto x : set)
    cout << x << " ";

// 清空
set.clear()
    
// 查看集合是否为空
set.empty()
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

// 遍历map
map<int, int> mp;
for(auto [a, b] : mp)
    cout << a << " " << b << endl;

for(auto &[a, b] : mp)
```

#### unordered_map

```C++
#include <unordered_map>

// 初始化 
unordered_map<int, int> hash

// 插入
hash.insert(make_pair(k, v))
hash.insert({k, v})
    
// 更新(如果没有就插入)
hash[k] = v
    
// 查询
hash[k]
    
// 根据 key 删除元素
hash.erase(key)
 
// 计数
hash.count(key)
    
// size
hash.size()
    
// 遍历
for(auto &[k, v] : hash)
    cout << v << " "
for(auto it = hash.begin(); it != hash.end(); i++)
    k = it->first, v = it->second

// 清空
hash.clear()

// 是否为空
hash.empty()
```



#### bitset

```C++
bitset<1000> b;		// 长度为 1000 的数组, 每个元素大小是 1 位, b 可看作一个整数
==, != 
    
// 初始化
// 初始化的值: 如果数据比表示的范围大, 则整数取后面, 字符串取前面
bitset<4> b;	// 默认全 0
bitset<4> b(5);	// 用十进制数 5 初始化 b, b = 0101
bitset<4> b(s);	// 用字符串 s 初始化 b, s 只能含有 0 和 1 字符

// 运算符
& | ~ ^
    
// 访问
b[index] 	// 随机访问, 注意 b[0] 代表 b 的最低位, 也就是字符串的最后一位
    
// 成员函数
// 成员函数都会对下标越界进行检查, [] 不会
b.count()	// 1 的个数
b.size()	// 位数大小

b.any()		// 是否有 1
b.none()	// 是否无 1
b.all()		// 是否全 1
    
b.test(index)	// 第 index 位是否为 1, index 要从右边开始数
    
// 位操作
b.flip()	// 全取反
b.flip(k)	// 只对第 k 位取反

b.set()		// 全置为 1
b.set(k)	// 第 k 位置为 1
b.set(k, v)	// 第 k 位置为 v, v 为 0 则为 0, v 非 0 置 1, 不影响其他位

b.reset()	// 全置为 0
b.reset(k)	// 第 k 位置为 0
 
// 转化为其他类型
b.to_string()	// 转化为字符串
b.to_ulong()	// 转化为 unsigned long
b.to_ullong()	// 转化为 unsigned long long
```

