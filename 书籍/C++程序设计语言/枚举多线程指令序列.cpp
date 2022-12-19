#include <iostream>
#include <vector>

using namespace std;

int cnt;

vector<string> a{"a1", "a2", "a3"};		// 过程 A 的操作序列
vector<string> b{"b1", "b2", "b3"};		// 过程 B 的操作序列
vector<string> path;					// 并发后的操作序列

void dfs(int i, int j)
{
    if(i + j == a.size() + b.size())
    {
        cnt++;
        for(auto t : path)
            cout << t << "  ";
        cout << endl;
        return;
    }
    if(i < a.size())
    {
        path.push_back(a[i]);
        dfs(i + 1, j);
        path.pop_back();
    }
    if(j < b.size())
    {
        path.push_back(b[j]);
        dfs(i, j + 1);
        path.pop_back();
    }
}

int main()
{
    dfs(0, 0);
    cout << cnt << endl;
}