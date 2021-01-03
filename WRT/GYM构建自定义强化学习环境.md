# GYM构建自定义强化学习环境

GYM

* 开源库:按照提供的接口自定义环境
* 已有环境

核心方法

* reset(self):重置环境,返回观察
* step(self,action):给环境一个动作,环境进入下一个状态,返回观测,奖励,是否种植,info
* render:环境可视化,绘制环境的一帧

`logging`库:记录日志

`random`库:随机数库

`gym`库:RL环境库



自定义环境的路径

```
anaconda3/envs/wrt(虚拟环境)/lib/python3.6/site-packages(当前环境下的各种第三方库)/gym(GYM库)/envs(包含所有的自定义RL环境项目)/nwpu/(自定义RL环境项目,包含所有自定义的环境,每一个环境都是一个.py文件,或者说是一个类)

```

`nwpu/(项目文件夹)`下的`__init__.py`

* 作用是导入项目里各个环境里的类名到一块

  * ```
    from gym.envs.nwpu.simu_wrt_v1 import SimuWRTEnv
    
    gym.envs.nwpu是项目的相对路径,nwpu就是包含wrt项目所有环境的项目文件夹
    simu_wrt_v1是文件名,SimuWRTEnv是文件里的类名
    ```

`gym/envs/(所有自定义环境项目)`下的`__init__.py`

* 注意这个路径是上一个路径的父目录

* 作用是链接算法里`make`函数的参数(即`id`)与环境项目里的类名(`entry`条目)

* ```
  register(
      id='SimuWRTEnv-v1',
      entry_point='gym.envs.nwpu:SimuWRTEnv',
      max_episode_steps=200,
      reward_threshold=100.0,
  )
  id对应make函数里的参数
  entry_point对应自定义环境的类名
  !!!二者与环境的文件名(simu_wrt_v1)无关!!!
  max_episode_steps是每回合最大步数
  
  ```

环境文件

`metedata`与`render`相关

* `render.modes`模式
* `video.frames_per_second`每秒的帧数

`__init__`初始化

* 状态空间
* 终止状态
* 奖励设置:`s + a -> r`
* 状态转移表`s + a -> s`
* 超参数设置

`step`

1. 判断是否终止
2. 查表计算下一状态s
3. 计算奖励r
4. 判断是否下一状态是否为终止状态
5. 返回s,r,done,info元组