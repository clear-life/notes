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
   
// 将十六进制补码字符串转化为十进制整数, 32 位
long long a = stoll(s, 0, 16);
long long res = a;
if(a >= 1ll << 31) res = a - (1ll << 32);
```

**stringstream**

字符串流

```C++
#include <sstream>

string s, word;			// s 中可以含空白符
stringstream ssin(s);	// 字符串流绑定到 s 上
while(ss >> word)		// ss 会把 s 中每个单词输入到 word 上
{
    ...
}
```



#### `<algorithm>`

```C++
// 最值
*max_element(v.begin(), v.end());	
*min_element(v.begin(), v.end());

// 累加
accumulate(v.begin(), v.end(), 初值);
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
// 初始化
priority_queue<int> q(v.begin(), v.end());	// v 是容器

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

