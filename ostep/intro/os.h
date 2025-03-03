#ifndef OS_H
#define OS_H

#include <string>

using namespace std;

const int N = 100000;

class Memory
{
    public:
    string memory_[N];    
};

class CPU
{
    public:
    Memory mem_;

    int pc_;
    string inst_[3];

    string op;
    int a;
    int b;
    int c;

    public:
    void fetch()
    {
        inst_[0] = mem_.memory_[pc_++];
        inst_[1] = mem_.memory_[pc_++];
        inst_[2] = mem_.memory_[pc_++];
    }

    void decode()
    {
        op = inst_[0];
        a = stoi(inst_[0]);
        b = stoi(inst_[0]);
    }

    void execute()
    {
        if(op == "add")
            c = a + b;
        else if(op == "sub")
            c = a - b;
    }
};

#endif