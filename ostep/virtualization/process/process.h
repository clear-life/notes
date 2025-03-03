#ifndef PROCESS_H
#define PROCESS_H

#include <string>

using namespace std;

struct Context
{
    int PC;
    int SP;
    int FP;
    ...
};

enum State
{
    RUNNING,
    READY,
    BLOCKED
};

struct Process
{
    public:
    char* mem;
    int sz;

    State state;
    int pid;

    Context context;
};

#endif