# std::memory_order

## 术语

### memory access

**内存访问**: 从 memory **read** 的过程和向 memory **write** 的过程

> memory 通常指内存, 即**内存条**

> **存储器分类**:
>
> * 只读存储器 ROM
>   * BIOS
> * 随机存储器 RAM
>   * 内存条
> * 缓冲存储器 cache
>   * 三级缓存

## memory_order

### memory_order

```C++
#include <atomic>

// C++11
enum memory_order 
{
    memory_order_relaxed,
    memory_order_consume,
    memory_order_acquire,
    memory_order_release,
    memory_order_acq_rel,
    memory_order_seq_cst
};

// C++20
enum class memory_order
{
    relaxed,
    consume,
    acquire,
    release,
    acq_rel,
    seq_cst
};
```

`std::memory_order` 指定**内存访问**(包括**非原子内存访问**)**在原子操作上的次序**

**定义**:

线程间**同步**和**内存次序**决定了表达式的

