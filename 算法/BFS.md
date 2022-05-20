# BFS

## 思路(伪代码)

**一次循环访问一个结点**

> 出队时访问结点, 好处是边界情况不用特意处理
>
> 坏处是慢和在访问时不能看到上一层结点的情况

```C++
queue q
q.push(起点)

while(q.size())
{
    auto u = q.front(); q.pop();
    visit(u)	// 结点出队时访问, 好处是不用考虑边界情况(即不用在加入起点时访问起点), 坏处是慢一点点和多遍历一层
    for v  in  u 的邻接点
        if v 未遍历过	// 有些时候不用加上是否遍历过的判断, 即使遍历过了我依然要再遍历一次
            q.push(v)
}
```

**一次循环访问一层结点**

> 出队时访问结点

```C++
queue q
q.push(起点)
    
while(q.size())
{
    int len = q.size();
    
    while(len--)
    {
        auto u = q.front(); q.pop();
        visit(u)
        for v  in  u 的邻接点
            if v 未遍历过
                q.push(v)
	}
}
```

**入队时访问结点**

> 好处是快和能看到上一层结点的情况
>
> 坏处是边界情况处理麻烦

```C++
queue q
visit(起点)
q.push(起点)
    
while(q.size())
{
    auto u = q.front(); q.pop();
    for v  in  u 的邻接点
        if v 未遍历过
            visit(v)
            q.push(v)
}
```





## 数组模拟

```C++
int h[N], e[N], ne[N], idx;
bool st[N];		// 保证不会重复访问
memset(h, -1, sizeof h);

void bfs()
{
    queue<int> q;
    q.push(start);
    st[start] = true;

    while(q.size())
    {
        auto u = q.top(); q.pop();
        
        for(int i = h[u]; i != -1; i = ne[i])
        {
            int v = e[i];
            if(!st[v])
            {
                q.push(v);
                st[v] = true;
            }
		}
	}
}
```

## 链表实现

```C++
```

## 题库

[二叉树的层序遍历](https://leetcode-cn.com/problems/binary-tree-level-order-traversal/)	一次遍历访问一层结点

[走迷宫](https://www.acwing.com/problem/content/846/)	一个起点的 bfs

[墙与门](https://leetcode-cn.com/problems/walls-and-gates/)	多个起点的 bfs

[岛屿数量](https://leetcode-cn.com/problems/number-of-islands/)	连通块数量计算: dfs, bfs, 并查集

[打开转盘锁](https://leetcode-cn.com/problems/open-the-lock/)	入队时访问元素效率高

[路径总和](https://leetcode-cn.com/problems/path-sum/)	入队元素为结构体

[二叉树的序列化与反序列化](https://leetcode.cn/problems/serialize-and-deserialize-binary-tree/)	层序遍历序列化与层序遍历反序列化

# 提高课知识

**BFS**: 1. 求**最小**  2. 基于**迭代**

## 应用

**flood fill 算法**

尽量用 bfs 实现, 因为 dfs 有可能爆栈

**最短路模型**

每个结点记录由哪个结点转移过来的

**最小步数模型**

常用哈希 map 记录是否访问过, key 是结点, value 记录状态

> 哈希 map 的 value 可以是任意类型, 有时候需要存储两个信息的话可以用 pair 类型
>
> 根据 map 中是否有 key 可以判定对应的结点是否访问过

 **两种边权的广搜**

边权为 0 和 1 两种情况下的广搜需要用双端队列 `deque`

本质是 dijkstra 算法, 所以每个点距起点的距离可能更新多次后才是最短的, 但根据 dijkstra 的贪心证明, 每次从队头拿出的结点的距离一定是最短的.

**证明 BFS 正确性**

1. **两段性**: 任一时刻, 队列里最多分为两段 [x, ... , x, x + 1, ... , x + 1]

2. **单调性**: 队列里的值是单调递增的

3. **证明**

   数学归纳法

   初始: 成立

   假定某一时刻成立, 证明将队头出队, 将其所有邻接点入队, 两个性质依然成立

**双向广搜**

常用于最小步模型, 适用于搜索空间很大的情况(时间复杂度常为**指数级**)

**思想: 同时从起点和终点开始 bfs 搜索, 相遇的时候就是解**

**时间复杂度**

设最大步数为 l, 每次的方向为 d

**单向广搜**的时间复杂度为 $O(d^l)$

**双向广搜**的时间复杂度为 $O(2d^{\frac{l}{2}})$

