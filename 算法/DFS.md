# DFS

用 dp 的思想来考虑递归

**树形 dp**

自底向上: 用子树信息组合出父节点信息

自顶向下: 子树信息由父结点信息得出, 父结点向子节点传递信息

## 思路(伪代码)

和一般的 bfs 一样, dfs 也是出队时访问

```C++
void dfs(int u)
{
    visit(u)	// 相当于出队
        
    for v  in  u 的邻接点
        if v 未遍历过
            dfs(v)	// 相当于入队
        else
            做出相应操作
}
```

希望遍历每个点的所有邻接点时用, 无论邻接点是否遍历过

```C++
type dfs(int u)
{
    if u 遍历过 
        return 相应的值
        
    visit(u)	// 相当于出队
        
    for v  in  u 的邻接点
        dfs(v)	// 相当于入队
}
```

显示用栈实现 DFS

```C++
stk.push(root)
while(stk.size())
{
    auto u = stk.top(); stk.pop();
    visit(u);
    st[u] = true;
    
    for v  in  u 的邻接点	// 跟递归实现的方式要反着来(如果考虑顺序的话, 如果不考虑就无所谓)
        if v 未遍历过
            stk.push(v)
        else
            ...
}
```

### 回溯

出队时访问

```C++
dfs(int u)
{
    st[u] = true;
    visit(u)
        
   	for v  in  u 的邻接点	// 跟递归实现的方式要反着来(如果考虑顺序的话, 如果不考虑就无所谓)
        if v 未遍历过
            dfs(v)
        else
            ...
            
    st[u] = false;
}
```

**递归终止条件**: **遍历完所有结点**, 有时候**递归到空结点**等价于该终止条件

[二叉树的最大深度](https://leetcode-cn.com/problems/maximum-depth-of-binary-tree/)	

**自底向上递归**

先递归子节点, 根据子节点的结果组合出要返回的值, 然后返回结果

类似于后序遍历

```C++
int dfs(TreeNode* node)
{
    if(!node) return 0;		// 递归的终止情况

    int l = dfs(node->left), r = dfs(node->right);	// 类比后序遍历的左右子树
    return max(l, r) + 1;	// 类比访问当前结点
}

dfs(root);
```

**自顶向下递归**

该结点的信息已知, 递归子结点, 并**向子节点传递信息**

类似于先序遍历

```C++
void dfs(TreeNode* node, int d)
{
    if(!node->left && !node->right)	// 递归的终止情况: 该结点是叶节点
        res = max(res, d);

    if(node->left) dfs(node->left, d + 1);
    if(node->right) dfs(node->right, d + 1);
}
```

**后序遍历与 dp**

自顶向下的 dp 思想: 假定子问题的解得到了, 然后由子问题的解组合出原问题的解

后序遍历视角下的 dp : 先递归得到所有子树的解, 然后遍历根节点得到整棵树的解

## 题库

[对称二叉树](https://leetcode-cn.com/problems/symmetric-tree/)	逻辑判断递归

[路径总和](https://leetcode-cn.com/problems/path-sum/)	dfs bfs, 自定向下

[克隆图](https://leetcode-cn.com/problems/clone-graph/)	dfs 重复判断可以在访问时判断, 模板增加重复点的处理逻辑

[二叉树的最近公共祖先](https://leetcode.cn/problems/lowest-common-ancestor-of-a-binary-tree/)	dfs 可以返回子树的状态, 返回值用状态码表示

[二叉树的序列化与反序列化](https://leetcode.cn/problems/serialize-and-deserialize-binary-tree/)	重要, dfs 序列化与 dfs 反序列化

## 算法提高课

**点的模型**

1. **具体的点**

   如迷宫中的点, 二维点, 三维点....

2. **抽象的点**

   抽象的一个状态
