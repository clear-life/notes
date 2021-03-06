# spinningup



## Introduction



### 是什么

* `openai` 深度强化学习教程
* 学习资源



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