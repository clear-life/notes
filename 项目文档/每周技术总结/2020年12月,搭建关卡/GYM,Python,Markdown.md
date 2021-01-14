# GYM,Python,Markdown





## GYM



### GYM

* 开源库:按照提供的接口自定义环境
* 已有环境



### 核心方法

* reset(self):重置环境,返回观察
* step(self,action):给环境一个动作,环境进入下一个状态,返回观测,奖励,是否种植,info
* render:环境可视化,绘制环境的一帧



### 基础库

`logging`库:记录日志

`random`库:随机数库

`gym`库:RL环境库



### 环境路径

自定义环境的路径

```
anaconda3/envs/wrt(虚拟环境)/lib/python3.6/site-packages(当前环境下的各种第三方库)/gym(GYM库)/envs(包含所有的自定义RL环境项目)/nwpu/(自定义RL环境项目,包含所有自定义的环境,每一个环境都是一个.py文件,或者说是一个类)

```



### 两个 `__init__.py`



#### `nwpu/(项目文件夹)`下的`__init__.py`

* 作用是导入项目里各个环境里的类名到一块

  * ```
    from gym.envs.nwpu.simu_wrt_v1 import SimuWRTEnv
    
    gym.envs.nwpu是项目的相对路径,nwpu就是包含wrt项目所有环境的项目文件夹
    simu_wrt_v1是文件名,SimuWRTEnv是文件里的类名
    ```



#### `gym/envs/(所有自定义环境项目)`下的`__init__.py`

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



#### `env_id`环境文件



##### `metedata`与`render`相关

* `render.modes`模式
* `video.frames_per_second`每秒的帧数



##### `__init__`初始化

* 状态空间
* 终止状态
* 奖励设置:`s + a -> r`
* 状态转移表`s + a -> s`
* 超参数设置



##### `step`

1. 判断是否终止
2. 查表计算下一状态s
3. 计算奖励r
4. 判断是否下一状态是否为终止状态
5. 返回s,r,done,info元组





## Python基础

#### 缩进

**缩进**表示**程序框架**和**层次关系**,一般用4个空格或1个TAB,但需要在程序内一致



#### 分号

多行代码写在一行时,用分号分隔



#### 换行

一行代码写在多行,用`\`进行连接

#### 注释

`#` 单行注释

`'''`

多行注释

`'''`



#### 命名

关联标识符的过程叫命名



#### 标识符

大小写字母,数字,下划线,**汉字**等组合,首字符不能是数字

`True`真	`False`假



#### 保留字

```
关系
and	or	not	True	False
库引用
import	as	from
循环
for	in	while	continue	break
条件
if	elif	else
函数
def	return
类
class	try
```



#### 赋值语句

右值的数值和**类型**赋给左值的变量



#### 输入

```
<变量> = input(<提示信息>)	input得到的是字符串类型,需要用eval函数去掉两侧引号
string = input("请输入")
```

#### 输出

```
print(<字符串或变量>)

print("转换后的温度是{:.2f}C".format(C))
{}表示槽,变量会填充到槽中	:是固定格式
.2f表示是取两位小数的浮点数

print("Hello:",0)	用","连接各种输出信息	print会自动在输出信息之间添加一个空格
结果:Hello: 0

print("Hello world!",end="") end表示结尾

print("执行开始".center(scale//2,"-"))用'-'来填充
```



#### 评估函数`eval()`

去掉参数最外侧的引号并执行对应的语句

```
eval(<字符串或变量>)
eval('1+2')				输出:3
eval('"1+2"')			输出:'1+2'
eval('print("Hello")')	输出:Hello
```

 

#### 库引用

```
import <库名>
<库名>.<函数名>(<函数参数>)
```

```
from <库名> import <函数名>
或
from <库名> import * 

就可以直接使用库中的函数名直接访问库中函数了,不需要使用"."运算符
```

```
import <库名> as <库别名>
<库别名>.<函数名>(<函数参数>)
```



#### range()函数

产生循环计数序列

```
range(N)		产生0..N-1的整数序列	共N个
range(M[, N])		产生M..N-1的整数序列	共N-M个
range(M[, N, K]) 从M到N-1每次递增K
```



#### 内嵌函数

* `sorted(x)`对x排序
* `len(x)`计算x的长度
* `type(x)`返回x的类型
* `map(fun,<组合数据类型>)`将函数`fun`作用于组合数据的每一个元素



## Markdown



* 标题前后空一行
* 标题结尾不要有标点符号



### 推荐的结构

前后都有空行



| # 标题      |
| :---------- |
| 作者        |
| 摘要        |
| 目录        |
| ## 标题1    |
| ### 标题1.1 |
| ## 标题2    |
| ### 标题2.1 |
| ### 标题2.2 |



### 段落

* 空白行是分隔段落的标志
* 段内换行:结尾插入两个及以上的空格后回车
* 每行不可太长



### 列表

* 嵌套列表:table 键
* 列表项之间没有换行:1个空格
* 列表项之间有换行:无序列表3个空格,有序列表2个空格
* 列表项没有换行,则列表项之间不换行
* 列表项有换行,则列表项之间换1行
* 列表项前后分别空1行





