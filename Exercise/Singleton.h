#include <mutex>

class Singleton
{
private:
    static Singleton *instance;
    static std::mutex mutex;    // 线程安全

    // 类内且只有一份
private:
    // 类内
    Singleton() = default;   
    // 不会被销毁   
    ~Singleton() = default;    

    // 只有一份
    Singleton(const Singleton&) = delete;
    Singleton(Singleton&&) = delete;            // 移动操作修改源对象, 不能是 const
    Singleton& operator=(const Singleton&) = delete;
    Singleton& operator=(Singleton&&) = delete; // 移动操作修改源对象, 不能是 const

public:
    static Singleton* getInstance()
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (instance == nullptr)
            instance = new Singleton();
        return instance;
    }

    static void destoryInstance()
    {
        std::lock_guard<std::mutex> lock(mutex);
        if (instance != nullptr)
        {
            delete instance;
            instance = nullptr;
        }
    }
};

Singleton* Singleton::instance = nullptr;
std::mutex Singleton::mutex;