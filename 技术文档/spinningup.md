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



## Running Experiments



学习 DRL 的最好方法是通过运行算法并观察再不同任务下的表现. spinningup 代码库使小规模(本地)实验很容易实现, 有两种方式启动运行算法: 命令行 和 脚本. 



### 命令行启动

spinning up 附带 `spinup/run.py` , 可以让算法从命令行以任何形式的超参数启动. 还可以作为一个封装器来观察训练过程的策略和绘制图谱.



#### 启动命令

```
python -m spinup.run [algo name] [experiment flags]

例:
python -m spinup.run ppo --env Walker2d-v2 --exp_name walker

python -m spinup.run ppo --exp_name ppo_ant --env Ant-v2 --clip_ratio 0.1 0.2
    --hid[h] [32,32] [64,32] --act torch.nn.Tanh --seed 0 10 20 --dt
    --data_dir path/to/data
默认情况下是 pytorch 版本的算法, 用 ppo_tf1 代替 ppo 就会用 tensorflow 版本的算法

clip_ratio, hid, act 是设置超参数的标志, 可以为超参数设置多个值来运行多个实验
hid 设置神经网络的隐藏层大小 
act 设置神经网络的激活函数
seed 设置随机数生成器的种子,对 RL 算法的性能影响很大
dt 确保保存目录名中有时间戳
data_dir 设置保存的目录
```



#### 选择 pytorch 或 tensorflow

```
python -m spinup.run [algo]_pytorch / [algo]_tf1
如果没有后缀,则会从 spinup/user_config.py 中查找默认版本
```



#### 设置超参数

```
--kwarg
python -m spinup.run [algo name] --help
值会通过 eval() 函数传递
字典参数:
--key dict(v1=value_1, v2=value_2)
--key:v1 value_1 --key:v2 value_2
```



#### 同时启动多个实验

```
给参数提供多个值来启动多个实验,每一个可能的组合都会启动实验,非并行启动多个实验
python -m spinup.run ppo --env Walker2d-v2 --exp_name walker --seed 0 10 20
```



#### 特殊标志

```
--env , --env_name 环境
--hid, --ac_kwargs:hidden_sizes 隐藏层大小
--act, --ac_kwargs:activation 激活函数

实验配置标志,非超参数
--cpu, --num_cpu 设置 cpu 数目, --num_cpu auto 自动设置 cpu 数目
--exp_name 实验名称
--data_dir 设置保存目录
--datestamp 设置保存目录中的时间和日期
```



#### 结果保存

```
data_dir/[outer_prefix]exp_name[suffix]/[inner_prefix]exp_name[suffix]_s[seed]

文件名后缀确定
参数
python -m spinup.run ddpg_tf1 --env Hopper-v2 --hid[h] [300] [128,128] --act tf.nn.tanh tf.nn.relu

文件名后缀
_h128-128_ac-actrelu
_h128-128_ac-acttanh
_h300_ac-actrelu
_h300_ac-acttanh
```



#### 算法路径

```
spinup/algos/BACKEND/ALGO_NAME/ALGO_NAME.py
```



### 脚本启动



每一个算法都是 python 函数, 可以直接从 spinup 包中导入

```
>>> from spinup import ppo_pytorch as ppo

查阅算法的文档页面可以找到参数的详细说明
```



#### 使用 ExperimentGrid

spinningup 提供了一个工具叫 experimentgrid 用多个可能的超参数来运行相同的算法



```
 from spinup.utils.run_utils import ExperimentGrid
 from spinup import ppo_pytorch
 import torch

 if __name__ == '__main__':
     import argparse
     parser = argparse.ArgumentParser()
     parser.add_argument('--cpu', type=int, default=4)
     parser.add_argument('--num_runs', type=int, default=3)
     args = parser.parse_args()

     eg = ExperimentGrid(name='ppo-pyt-bench')
     eg.add('env_name', 'CartPole-v0', '', True)
     eg.add('seed', [10*i for i in range(args.num_runs)])
     eg.add('epochs', 10)
     eg.add('steps_per_epoch', 4000)
     eg.add('ac_kwargs:hidden_sizes', [(32,), (64,64)], 'hid')
     eg.add('ac_kwargs:activation', [torch.nn.Tanh, torch.nn.ReLU], '')
     eg.run(ppo_pytorch, num_cpu=args.cpu)
     
     
     
添加参数
eg.add(param_name, values, shorthand, in_name)

添加参数之后
eg.run(thunk, **run_kwargs)

配置作为 python 的 kwargs 传递给函数 thunk 来运行实验
```



## 实验输出



本节内容

* 算法实现后的输出
* 存储格式 和 组织格式
* 存储位置
* 加载并运行策略

```
目前没法从已经训练了一部分后的智能体中恢复训练
```



### 算法输出

算法都会保存 超参数设置, 学习进度, 已训练的智能体, 值函数 和 一个环境的副本. 输出目录包括

```
pyt_save/ 仅 pytorch 实现. 一个用于恢复已训练智能体和值函数的目录
tf1_save/ 仅 tensorflow 实现. 一个用于恢复已训练智能体和值函数的目录
config.json 仅用来保存配置记录, 不能从该文件启动算法. 一个保存尽可能多参数信息的字典, 如果配置信息不能转化为 JSON 格式, 就会被日志记录
progress.txt 制表符分开的值文件
vars.pkl 一个包含算法状态的 pickle 文件, 包含环境副本
```



#### pytorch 保存目录信息

```
model.pt 训练好的模型文件
```



#### tensorflow 保存目录信息

```
variables/	包含 Tensorflow Saver 输出的目录
model_info.pkl 	包含解压模型信息的目录
saved_model.pb	协议缓冲区, 保存模型
```



### 保存目录路径

```
spinningup/data/
```



### 加载运行已训练的策略



#### 如果环境保存成功

```
python -m spinup.run test_policy path/to/output_directory

flags:
-l L, --len=L, default=0 	回合长度
-n N, --episodes=N, default=100		回合数
-nr, --norender	不图形化展示
-i I, --itr=I, default=-1	保存快照的时机
-d, --deterministic	仅用于 SAC
```



#### 环境未找到错误

```
Traceback (most recent call last):
  File "spinup/utils/test_policy.py", line 153, in <module>
    run_policy(env, get_action, args.len, args.episodes, not(args.norender))
  File "spinup/utils/test_policy.py", line 114, in run_policy
    "and we can't run the agent in it. :( nn Check out the readthedocs " +
AssertionError: Environment not found!

 It looks like the environment wasn't saved, and we can't run the agent in it. :(

 Check out the readthedocs page on Experiment Outputs for how to handle this situation.
```



#### 使用已训练的值函数

* PyTorch

  ```
  用 torch.load 加载模型文件, 查看算法文档, 看 ActorCritic 对象有哪些模块
  ```

* Tensorflow

  ```
  用 restore_tf_graph 加载计算图, 查看算法文档, 看保存了哪些函数
  ```