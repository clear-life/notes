#include <iostream>
#include <unordered_map>
#include <vector>
#include <climits>

using namespace std;

using uint = unsigned int;

class SceneUser;
class Pass;



int main()
{
    // cout << UINT_MAX << endl;
    return 0;
}

class SceneUser
{
    Pass* pass_;
};

class Pass
{
public:
    Pass(){};
    ~Pass(){};

    void GetExpByObj(unordered_map<uint, uint> &objects);
    void LevelUp(uint exp);

private:
    // 配置
    uint max_level1_;    // 最大级数
    uint max_level2_ = UINT_MAX;    // 溢出后的最大级数
    unordered_map<uint, uint> exp_config_ = {{1, 10}, {2, 20}, {3, 30}, {4, 40}, {5, 50}, {6, 60}};  // 每一级的经验, 即每一级升级所需经验
    unordered_map<uint, uint> obj_to_exp_ = {{1, 10}, {2, 20}, {3, 40}, {4, 80}};  // 道具 ip -> 经验

    // 经验获取
    bool has_straight_up_ = false;      // 是否已直升
    bool has_unlock_ = false;           // 是否已解锁

    // 等级
    uint cur_level_;
    uint cur_exp_;

    // 宝箱
    uint ojb_num_;  // 未领取宝箱数
};

void Pass::GetExpByObj(unordered_map<uint, uint> &objects)
{
    uint exp = 0;
    for(auto &obj : objects)
        exp += obj.second * obj_to_exp_[obj.first];
    
    LevelUp(exp);
}

void Pass::LevelUp(uint exp)
{
    
}