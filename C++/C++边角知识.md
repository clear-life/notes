# C++ 边角知识

头文件 <cstring\>

**memset**

按字节赋值

```C++
int a[10];
memset(a, -1, sizeof(a));	
// sizeof(a) 为 40
// 单字节的 -1 是全 1 , 即 11111111, 四字节的 -1 也是全 1
// 即 int 类型的 -1 是 11111111 11111111 11111111 11111111
// memset(a, -1, sizeof(a)) 会把从 a 开始的 40 个字节赋为 11111111, 于是 a 数组的 10 个元素都是 -1

memset(a, -1, sizeof(a))	// 从 a 开始的 sizeof(a) 个字节为 11111111 , a 数组为全 -1
memset(a, 0, sizeof(a));	// 从 a 开始的 sizeof(a) 个字节为 00000000 , a 数组为全 0
memset(a, 127, sizeof(a));	// 从 a 开始的 sizeof(a) 个字节为 01111111 , a 数组为很大的数
memset(a, 128, sizeof(a));	// 从 a 开始的 sizeof(a) 个字节为 10000000 , a 数组为很小的数
```



**isdigit**

判断字符 c 是否是 0 ~ 9 之间的数字

```cpp
if(isdigit(char c)
   cout << "是数字" << endl;
else
   cout << "不是数字" << endl;
```

 

**加速输入输出**

```cpp
ios::sync_with_stdio(false);	// 设置不兼容 stdio
cin.tie(0);						// 解除 cin 和 cout 的绑定, 加快效率
cout.tie(0);
```

