# DFS

用 dp 的思想来考虑递归

**树形 dp**

自底向上: 用子树信息组合出父节点信息

自顶向下: 子树信息由父结点信息得出, 父结点向子节点传递信息

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

该结点的信息已知, 递归子结点, 并向子节点传递信息

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

## 题库

[对称二叉树](https://leetcode-cn.com/problems/symmetric-tree/)	逻辑判断递归

[路径总和](https://leetcode-cn.com/problems/path-sum/)	dfs bfs, 自定向下
