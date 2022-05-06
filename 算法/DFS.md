# DFS

## 思路(伪代码)

```C++
void dfs(int u)
{
    visit(u)
        
    for v  in  u 的邻接点
        if v 未遍历过
            dfs(v)
}
```

