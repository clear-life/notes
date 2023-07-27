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

	void Init();

    void GetExpByObj(unordered_map<uint, uint> &objects);
	void GetExpByStraight();
	void GetExpByMoney(uint num);
    void LevelUp(uint exp);
	void MailToUser();
private:
    // 配置
    uint max_level1_;    // 最大级数
    uint max_level2_ = UINT_MAX;    // 溢出后的最大级数
    unordered_map<uint, uint> level_to_exp_ = {{1, 10}, {2, 20}, {3, 30}, {4, 40}, {5, 50}, {6, 60}};  // 每一级的经验, 即每一级升级所需经验
    unordered_map<uint, uint> obj_to_exp_ = {{1, 10}, {2, 20}, {3, 40}, {4, 80}};  // 道具 ip -> 经验

    // 经验获取
    bool has_straight_up_ = false;      // 是否已直升
    bool has_unlock_ = false;           // 是否已解锁

    // 等级
    uint cur_level_ = 1;
    uint cur_exp_ = 0;

    // 宝箱
    uint ojb_num_ = 0;  // 未领取宝箱数
};
 
void Pass::Init()
{
	// 数据填充
}

void Pass::GetExpByObj(unordered_map<uint, uint> &objects)
{
    uint exp = 0;
    for(auto &obj : objects)
        exp += obj.second * obj_to_exp_[obj.first];
    
    LevelUp(exp);
}

void Pass::GetExpByStraight()
{
	if(has_straight_up_) return;

	for(uint i = 0; i < 20; i++)
	{
		if(
			level_to_exp_.count(cur_level_) > 0 &&		
			level_to_exp_[cur_level_] > 0 
		)
			LevelUp(level_to_exp_[cur_level_]);
	}
	has_straight_up_ = true;
}

void Pass::GetExpByMoney(uint num)
{
	if(!has_unlock_) return;

	for(uint i = 0; i < num; i++)
	{
		if(
			level_to_exp_.count(cur_level_) > 0 &&		
			level_to_exp_[cur_level_] > 0 
		)
			LevelUp(level_to_exp_[cur_level_]);
	}
}

void Pass::LevelUp(uint exp)
{
    cur_exp_ += exp;

	while(	cur_level_ < max_level2_ && 				// 等级检查
			level_to_exp_.count(cur_level_) > 0 &&		// key 检查
			level_to_exp_[cur_level_] > 0 &&			// value 检查
			cur_exp_ >= level_to_exp_[cur_level_])		// 升级检查
	{
		cur_exp_ -= level_to_exp_[cur_level_];
		cur_level_++;
		
		// 执行升级相关的逻辑
		//
		if(cur_level_ <= max_level1_)
		{

		}
		else if(cur_level_ <= max_level2_)
		{
	
			ojb_num_++;
		}
	}
}

void Pass::MailToUser()
{

}