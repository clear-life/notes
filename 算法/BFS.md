# BFS

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

[走迷宫](https://www.acwing.com/problem/content/846/)	一个起点的 bfs

[墙与门](https://leetcode-cn.com/problems/walls-and-gates/)	多个起点的 bfs

[岛屿数量](https://leetcode-cn.com/problems/number-of-islands/)	连通块数量计算: dfs, bfs, 并查集

[打开转盘锁](https://leetcode-cn.com/problems/open-the-lock/)	入队时访问元素效率高
