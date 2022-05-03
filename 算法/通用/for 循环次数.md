# for 循环次数

想要让 for 循环 n 次,

```C++
for(int i = 0; i < n; i++)
    ...
```

$~$

假定下标从 0 开始, 想要让 0 递增到下标为 idx (每次 + 1)

idx 到 0 的偏移量为 idx, 所以需要循环 idx 次

```C++
for(int i = 0; i < idx; i++)
```

$~$

更一般的情况:

假定下标从 a 开始, 想要让 a 递增到下标为 b (每次 + 1)

a 到 b 的偏移量为 (b - a), 所以需要循环 (b - a) 次

```C++
// 做法 1
int len = b - a;
for(int i = 0; i < len; i++)
    a++;

// 做法 2
for(int i = a; i < b; i++) 		// 也是循环 b - a 次
```

