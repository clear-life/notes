#include <iostream>
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include <array>
#include <vector>
#include <optional>

// import archive;
// Archive a;
// a.a = 1;
// a.b = 1.0f;
// a.c = '1';

using namespace std;

// 命名空间
namespace yjq::son
{
    void foo()
    {
        cout << fmt::format("func:{}", __func__) << endl;
    }
}

/*
namespace yjq
{
    namespace son
    {
        void foo()
        {
        }
    }
}
 */

// 命名空间别名
namespace myson = yjq::son;

using yjq::son::foo;

// 函数
auto foo(int a)
{
    cout << fmt::format("func:{}", __func__) << endl;
    return a;
}

// 属性
[[nodiscard]] int func([[maybe_unused]] int a)
{
    return 0;
}

// 表明不会将控制权返回给调用点
[[noreturn]] void terminal()
{
    std::exit(1);
}

bool test_noreturn()
{
    if (true)
        terminal();
    return false;
}

[[deprecated("deprecated method, please use xyz")]] void deprecated_func()
{
}

optional<int> getData(bool flag)
{
    if (flag)
        return 1;
    return nullopt; // return {};
}

int main(int argc, char *argv[])
{
    // 初始化
    int uninitial;
    int assign = 1;     // 赋值
    int initial{ 1 };  // 统一初始化
    int initial0{ };    // 零初始化
    cout << fmt::format("{} random", uninitial) << endl;
    cout << fmt::format("{} initial", initial) << endl;

    {
        int arr1[5] { 0, 1, 2 };
        int arr2[5] {};

        size_t n { std::size(arr1) };
        cout << fmt::format("size:{}", n) << endl;
    }


    // 类型极限
    cout << "int:" << endl;
    cout << fmt::format("max: {}", numeric_limits<int>::max()) << endl;
    cout << fmt::format("min: {}", numeric_limits<int>::min()) << endl;         // 最小数
    cout << fmt::format("lowest: {}", numeric_limits<int>::lowest()) << endl;   // 最小数
    cout << "double:" << endl;
    cout << fmt::format("max: {}", numeric_limits<double>::max()) << endl;         
    cout << fmt::format("min: {}", numeric_limits<double>::min()) << endl;         // 最小正数
    cout << fmt::format("lowest: {}", numeric_limits<double>::lowest()) << endl;   // 最小负数

    // 类型转换
    short s{ 1 };
    int i0{ s };
    int i1{ (int)s };
    int i2{ int(s) };
    int i3{ static_cast<int>(s) };


    // 运算符优先级
    // ++ -- 后置
    // ! ++ -- 前置
    // * / %
    // + -
    // << >>
    // & 
    // ^
    // |
    // = += -= *= /= %= &= |= ^= <<= >>=


    // 枚举类型
    enum class Type : unsigned
    {
        King = 1,
        Queen,
        Rook = 4,
        Pawn
    };

    {
        using enum Type;
        Type a{ King };
        Type b{ Queen };
    }

    {
        using Type::King;
        Type a{ King };
        Type b{ Type::Queen };
    }


    // 条件语句
    // if (<initializer>; <expression>) { <body> } 
    if (int a{1}; a)
    {
        cout << a << endl;
    }

    // switch (<initializer>; <expression>) { <body> } 
    switch (int a{1}; a)
    {
        case 1:
            [[fallthrough]];
        case 2:
            cout << a << endl;
        default:
            break;
    }


    // 三向比较运算符 <compare>
    {
        int a{ 1 };
        std::strong_ordering res{ a <=> 0 };
        if (res == std::strong_ordering::less)
            cout << "less" << endl;
        if (res == std::strong_ordering::greater)
            cout << "greater" << endl;
        if (res == std::strong_ordering::equal)
            cout << "equal" << endl;

        if (std::is_lt(res))
            cout << "less" << endl;
        if (std::is_gt(res))
            cout << "greater" << endl;
        if (std::is_eq(res))
            cout << "equal" << endl;
    }

    {
        double b{ 1 };
        std::partial_ordering res{ b <=> 0 };
        if (res == std::partial_ordering::less)
            cout << "less" << endl;
        if (res == std::partial_ordering::greater)
            cout << "greater" << endl;
        if (res == std::partial_ordering::equivalent)
            cout << "equivalent" << endl;
        if (res == std::partial_ordering::unordered)
            cout << "unordered" << endl;

        if (std::is_lt(res))
            cout << "less" << endl;
        if (std::is_gt(res))
            cout << "greater" << endl;
        if (std::is_eq(res))
            cout << "equal" << endl;
    }


    // 函数
    foo();

    cout << fmt::format("return type = {} value= {}", typeid(foo(1)).name(), foo(1)) << endl;
    {
        int a = func(1);
        cout << fmt::format("{}", a) << endl;
    }


    // 属性
    //  test_noreturn();

    switch (1)
    {
        [[likely]] case 1:
            break;
        case 2:
            break;
        [[unlikely]] default:
            break;
    }


    // <array> 和 <vector>
    {
        array<int, 5> arr { 0, 1, 2 };
        cout << fmt::format("size:{} arr[1]:{}", arr.size(), arr[1]) << endl;

        // 假定 申请内存空间 cost = 1 赋值 cost = 1
        // 数学原理 1 + 2 + 4 + 8 + 16 + ... + n = 2n - 1
        vector<int> vec { 0, 1, 2 };
        vec.push_back(3);
        cout << fmt::format("size:{} capacity:{} vec[1]:{}", vec.size(), vec.capacity(), vec[1]) << endl;
    }


    // <optional>
    {
        bool flag = false;
        optional<int> d { getData(flag) };

        if (d.has_value()) // if (d)
            cout << d.value() << endl;
        cout << d.value_or(0) << endl;
    }


    // 结构化绑定
    // array struct pair tuple
    {
        array<int, 3> arr { 0, 1, 2 };
        auto [x, y, z] { arr };
        cout << fmt::format("x:{} y:{} z:{}", x, y, z) << endl;
    }

    {
        struct St
        {
            int a;
            double b;
            char c;
        };
        St st{ 0, 1, '2' };
        auto [x, y, z] { st };
        cout << fmt::format("x:{} y:{} z:{}", x, y, z) << endl;
    }


    // 循环
    //for (<initializer>; <declaration> : <initializer>) { <body> }
    for (array arr { 0, 1, 2 }; int i : arr) { cout << i << endl; }
}

