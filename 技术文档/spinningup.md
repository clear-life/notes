# spinningup



## Introduction



### 是什么

* `openai` 深度强化学习教程
* 学习资源(重要的关键论文,算法相关文档)



### 为什么

* 论文忽略实现细节
* 代码难以阅读



### 怎么做

* 算法尽可能简单
* 算法实现结构相似



## Installation

环境需要:`Python3，OpenAI Gym 和 OpenMPI`,`MuJoCo(可选)`



### python

anaconda 创建环境

```
conda create -n spinningup python=3.6
conda activate spinningup
```



### OpenMPI

```
sudo apt-get update && sudo apt-get install libopenmpi-dev
```



### Spinning Up

```
git clone https://github.com/openai/spinningup.git
cd spinningup
pip install -e .
```



### 检测是否安装成功

#### 运行训练算法

```
python -m spinup.run ppo --hid "[32,32]" --env LunarLander-v2 --exp_name installtest --gamma 0.999
```



#### 查看训练结果视频

```
python -m spinup.run test_policy data/installtest/installtest_s0
```



#### 图形化展示

```
python -m spinup.run plot data/installtest/installtest_s0
```



## Algorithm



### 是什么

* `VPG,TRPO,PPO,DDPG,TD3,SAC`
* `pytorch 和 tensorflow`版本



### 为什么

* 这些算法能很好的体现算法效率的演进和设计与使用时的权衡
* `On-Policy Algorithms`
  * VPG 是 DRL 领域最基础,最入门的算法,然后演进出了 TRPO 和 PPO
  * On-Policy 算法不使用旧数据,样本效率弱,改进目标就是样本效率
* `Off-Policy Algorithms`
  * DDPG 是类似 VPG 的基础算法, DDPG 与 Q-learning 算法相关, 其同时学习一个Q函数和一个策略, Q函数与策略互相更新改进
  * Off-Policy 算法能有效利用旧样本,但稳定性不高,稳定性就是改进目标



### 怎么做(代码格式)



算法实现都遵循同一个模板:包含核心逻辑的算法文件,包含共同需求的核心文件

#### Algorithm File

1. 类定义
   算法文件总是以一个用于经验缓冲区对象的类定义为开始, 该类用于存储智能体与环境间的交互信息. 

2. 算法函数

   然后是一个运行算法的简单函数. 该算法函数遵循一个模板, 包含`pytorch`和 `tensorflow `两个版本, 但二者大体相似. 

3. 命令行支持

   最后是一些能让算法可以直接再 GYM 环境中通过命令行方式运行的支持

* 算法函数: `pytorch`版本

   `pytorch`版本的算法函数大体遵循下列实现顺序

  1. 日志设置

  2. 随机种子设置

  3. 环境实例化

  4. 通过 `actor_critic` 函数来构建 actor_critic 的 pytorch 模块,  `actor_critic` 函数会作为一个参数传递给算法函数

  5. 经验缓冲区实例化

  6. 可调用损失函数设置, 损失函数会提供具体算法的评价

  7. 创建 pytorch 优化器
  8. 通过日志设置模型保存
  9. 更新函数设置, 该函数运行一个周期的优化或一步的下降
  10. 运行算法的主要循环
      1. 运行环境中的智能体
      2. 根据算法的主要公式定期更行智能体的参数
      3. 日志记录关键性能指标并保存智能体

* 算法函数: `tensorflow `版本

  1. 日志
  2. 随机种子
  3. 环境
  4. 创建计算图的占位符
  5. 通过`actor_critic` 函数构建`actor_critic` 计算图, `actor_critic` 函数会作为参数传递给算法函数
  6. 经验缓冲区
  7. 构建损失函数和算法评判的计算图
  8. 创建训练 ops
  9. 创建 TF 会话并初始化参数
  10. 通过日志设置模型保存
  11. 定义运行算法主循环所需的函数(如核心更新函数,获取动作函数,测试智能体函数)
  12. 运行算法主循环
      1. 运行智能体
      2. 根据公式定期更新智能体参数
      3. 日志记录关键性能指标并保存智能体

  

#### 核心文件

核心文件没有严格的模板,但有类似的结构

1. 仅 tensorflow : 与创建和管理占位符有关的函数
2. 用于构建计算图会话的函数, 该计算图与特定算法的`actor_critic`方法相关
3. 任何其他的函数
4. 与算法兼容的 `MLP actor-critic`实现, 其中策略和价值函数都由简单的 `MLP` 来表示

