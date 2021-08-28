# python 语法

## 常见语法

**列表推导式**

```python
[表达式 for 迭代变量 in 可迭代对象 [if 条件表达式] ]
a = [x**2 for x in range(-3, 3) if x >= -1]
# a = [1, 0, 1, 4]
```



## sys

python 解释器相关的库

**sys.modules** 

字典, 记录当前已导入的模块

```python
import numpy as np
is_numpy = "numpy" in sys.modules	# true
```

## os

操作系统相关的库

**os.path.join**

函数, 拼接成一个路径

输入: 多个字符串

输出: 一个字符串

```python
a = os.path.join(".","dir1","dir2")	# .\dir1\dir2
```

**os.makedirs**

函数, 递归创建目录

输入: 字符串路径

输出: 创建目录

```python
os.makedirs("./dir1/dir2")	# 创建两个目录, dir1 目录里面包含了空目录 dir2
os.makedirs("./dir1/dir2", exist_ok=True)
# exist_ok 表示目录已存在时是否是正常的, 默认为 false(目录已存在是不正常的) 发出异常, True(目录已存在是正常的) 为不发出异常
```

## matplotlib

数学绘图库

```python
import matplotlib as mpl
```

### matplotlib.pyplot

子库, 包含一系列绘图相关的函数

```python
import matplotlib.pyplot as plt
```

**plt.savefig**

保存图片

```python
plt.savefig(file, format=fig_extension, dpi=resolution)
# file 		保存的文件名, 就是保存路径
# format 	保存的格式
# dpi 		每英寸的像素数量, 图像质量
```

**plt.imshow**

绘制图像

```python
plt.imshow(X)	# X 为图像数据
# 参数
# interpolation: 插值方法
```

**plt.show**

展示图像

```
plt.show()
```

**plt.axis**

设置坐标轴

```python
plt.axis("square")	# 正方形
plt.axis("equal")	# x,y 轴刻度等长
plt.axis("off")		# 关闭坐标轴
```





## sklearn

全称 scikit-learn, 机器学习库

> Machine learning module for Python

### datasets

获取数据集的 module

**datasets.fetch_openml**

从 http://openml.org/ 获取数据集的函数

```
datasets.fetch_openml('mnist_784', version=1, as_frame=False)
```

## numpy

向量运算库

**np.zeros**

返回给定 shape 的全 0 数组

```python
np.zeros(shape, dtype=float)
# shape 是一个元组
```



### ndarray 类

**reshape**

成员函数, 重组对象 shape

```python
np.reshape(28, 28)
```

**astype**

成员函数, 返回对应数据类型的副本

```python
np.astype("float64")
np.astype(np.uint8)
```

