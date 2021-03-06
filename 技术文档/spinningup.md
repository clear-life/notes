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

