// 模块接口文件
ecport module archive;
export struct Archive
{
    int a;
    double b;
    char c;
};

export class Archive2
{
    public:
        Archive2();
        ~Archive2();

        void foo();
    private:
        int m_a;
        double m_b;
        char m_c;
};
