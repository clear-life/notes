# C++ demo

### 命名空间

```C++
namespace Grade1
{
    namespace Class2
    {
        int wang = 1;
    }
}

namespace Grade2
{
    namespace Class2
    {
        int wang = 2;
    }
}

Grade1::Class2::wang;
Grade2::Class2::wang;
```

### 模板

```C++
template<class TYPE> 
class A
{
public:
    TYPE x;
};

A<int> a;	// a.x 是 int 类型
A<float> b;	// b.x 是 float 类型
```

