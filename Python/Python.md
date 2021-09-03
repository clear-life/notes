# python

## 常见语法

**列表推导式**

```python
[表达式 for 迭代变量 in 可迭代对象 [if 条件表达式] ]
a = [x**2 for x in range(-3, 3) if x >= -1]
# a = [1, 0, 1, 4]
```

### 字符串

**string.strip()**

删除头尾的指定字符

```python
string.strip(',')	# 删除字符串头尾的 ','
```





### 装饰器

装饰其他函数的函数

#### 1. 函数也是对象

```python
def fun1(a):		# 创建函数对象 fun1
    print("fun"+a)
fun1("1")			# 调用函数对象 fun1, 输出: fun1

fun2 = fun1			# 创建函数对象 fun2, 用 fun1 初始化

del fun1			# 销毁函数对象 fun1

fun2("2")			# 调用函数对象 fun2, 输出: fun2
```

#### 2. 在函数中定义函数

```python
def fun():
    print("fun")
    
	def fun1():			# 函数内部定义的函数只能在函数内访问
		print("fun1") 	# 不能在函数外访问
    
    def fun2():
        print("fun2")
    
    fun1()
    fun2()

fun()	# 可以访问, 输出: fun  fun1  fun2

fun1()	# 不可以访问
```

#### 3. 从函数中返回函数对象

```python
def fun(num = 1):
    print("fun")
    
	def fun1():			# 函数内部定义的函数只能在函数内访问
		print("fun1") 	# 不能在函数外访问
    
    def fun2():
        print("fun2")
    
    if(num == 1):
        return fun1()
    else:
    	return fun2()

a = fun()	# 访问 fun, 输出 "fun", 返回函数对象 fun1 赋值给 a
a()			# 调用函数对象 a, a 的内容和 fun1 一样

# a = fun()
# a()
# 相同输出的代码

fun()()		
# 先访问 fun, 输出 "fun", 返回 fun1
# 然后访问 fun1, 输出 "fun1", 返回空
```

#### 4. 函数对象作为参数

```python
def fun():
    print("fun")
def decorate(fun):
    print("before")
    fun()
    print("after")

decorate(fun)	
# 先调用 decorate 函数对象, 传入参数为函数对象 fun
# 然后执行 decorate 函数
# 再调用函数对象 fun, 执行函数对象 fun
```

#### 5. 第一个装饰器

```python
# 装饰器函数
# 输入: 待装饰的函数对象
# 输出: 装饰后的函数对象, 在被装饰对象执行前和执行后做了一些操作, 但不修改被装饰对象本身

def decorate(fun):
    def decorated():
        print("before fun")
        fun()
        print("after fun")
      
    return warp

# 普通的函数对象
def fun():
	print("fun")

# 装饰前
fun()		# 执行 fun, 输出 "fun"

# 装饰
fun = decorate(fun)	# 装饰好函数对象 fun 后赋值给自己

# 装饰后
fun()		
# 调用装饰后的函数对象 fun
# 输出 before fun    fun    after fun    

# 用 @ 来装饰函数
#函数定义时
```

**在函数定义时用 @ 直接装饰**

```python
def decorate(fun):
    def decorated():
        print("before fun")
        fun()
        print("after fun")
      
    return warp

@decorate		# 等价于 fun = decorate(fun)
def fun():
	print("fun")
    
fun() 	# 调用装饰后的 fun 函数
```

**输出被装饰函数的名字**

```python
print(fun.__name__)		# 装饰器会重写被装饰函数的函数名和注释文档, 结果输出 warp

# 使用 functools 模块的 warps 函数来装饰我们自己写的装饰器, 使得被装饰函数不会被修改函数名(fun.__name__)和注释文档(fun.__doc__)

def decorate(fun)
	@wraps(fun)	# 在我们自己写的装饰器上使用 functools 模块的 warps 函数
	def decorated():
        ...
    ...
    
@decorate	
def fun():
    ...
```

#### 6. 使用场景

**授权**

检查是否被授权

```python
def requires_auth(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        auth = request.authorization
        if not auth or not check_auth(auth.username, auth.password):
            authenticate()
        return f(*args, **kwargs)
    return decorated
```

**日志**

```python
def logit(func):
    @wraps(func)
    def decorated(*args, **kwargs):
        print(func.__name__ + " was called")
        return func(*args, **kwargs)
    return decorated
```

#### 7. 带参数的装饰器

```python
def logit(logfile='out.log'):
    def logging_decorator(fun):
        @wraps(fun)
        def decorated(*args, **kwargs):
            log_string = fun.__name__ + " was called"
            print(log_string)	# 打印日志
            with open(logfile, 'a') as opened_file:	# 保存日志
                opened_file.write(log_string + '\n')
            return fun(*args, **kwargs)	# 被装饰函数原封不动返回
        return decorated
    return logging_decorator
```

#### 8. 装饰器类

用类构建装饰器

```python
class logit(object):
    def __init__(self, logfile='out.log'):
        self.logfile = logfile
 
    def __call__(self, fun):
        @wraps(fun)
        def decorated(*args, **kwargs):
            log_string = func.__name__ + " was called"
            print(log_string)	# 打印日志
            with open(self.logfile, 'a') as opened_file:	# 保存日志	
                opened_file.write(log_string + '\n')
            self.notify()		# 提醒用户
            return func(*args, **kwargs)	# 返回函数本身, 不做任何修改
        return decorated
 
    def notify(self):
        # 提醒用户
        
# 用法和以前一样
@logit()
def fun():
    pass
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

### path

路径和文件属性相关模块

**path.join**

函数, 拼接成一个路径

输入: 多个字符串

输出: 一个字符串

```python
a = os.path.join(".","dir1","dir2")	# .\dir1\dir2
```

**os.getcwd()**

返回当前路径

```python
current_directory = os.getcwd()
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

**plt.figure**

创建画板

```python
plt.figure(figsize=(5,5))
# 宽和高, 单位为英寸
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

### linear_model

线性模型 module

**linear_model.SGDClassifier**

类, 使用 SGD 优化方法的线性分类器

```python
sgd = linear_model.SGDClassifier(max_iter=1000, tol=1e-3, random_state=42)
# max_iter: 训练数据的迭代次数, 就是 epochs
# tol: 误差
# random_state: 数据混洗

sgd.fit(X_train, y_train)	# 训练

sgd.predict(X_test)			# 预测
```



## numpy

向量运算库

**np.zeros**

返回给定 shape 的全 0 数组

```python
np.zeros(shape, dtype=float)
# shape 是一个元组
```

**np.concatenate**

连接

```
a = np.array([[1,2],[3,4]])
b = np.array([[1,3],[5,7]])
c = np.array([[2,4],[6,8]])
res0 = np.concatenate((a,b,c),axis=0)	# 按第 0 维度连接, 只有维度 0 改变, 其他维度大小不变
res1 = np.concatenate((a,b,c),axis=1)	# 按第 1 维度连接, 只有维度 1 改变, 其他维度大小不变

res0.shape		# (6,2), axis = 0 改变, 其余不变
res1.shape		# (2,6), axis = 1 改变, 其余不变
```

**np.transpose**

轴变换

```python
np.transpose(1,0,2) 	# 将 0 轴和 1 轴变换
np.transpose()			# 无参数就转置
```

### linalg

线性代数模块

**linalg.eig()**

计算方针的特征值和特征向量

```python
linalg.eig(a)
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

## heapq

优先队列



## psycopg2

操作 postgreSQL 数据库的模块

## configparser

读取配置文件

### configparser.ConfigParser()

生成 ConfigParser 对象

```python
config = configparser.ConfigParser()

# read 方法读取配置文件
config.read(filename, encoding='utf-8')
```

