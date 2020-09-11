# GYM文档

#### 基础概念

* 环境env   env 传递 actions 给env

* 智能体agent   env 返回 observation , rewards

* ```
  reset(self)	重置环境,返回observation
  step(self,action) 步入下一环境状态,返回observation,rewards,done,info
  render(self, mode='human') 渲染环境的一帧
  ```

#### 需要python3.5~3.8

## agent

agent描述了运行RL算法的方法

agent包含算法本身,也可以简单提供**算法**与**环境**之间的**集成**