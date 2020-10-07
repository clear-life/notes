## turtle标准库

绘制基本图形

#### 绘图窗体

```
turtle.setup(width,height,startx,starty)设置船体大小及位置,后两个参数可选
```



### 空间坐标体系

#### 绝对坐标

```
turtle.goto(x,y)让任何位置的海龟turtle到达绝对坐标(x,y)
```

#### 海归坐标(相对坐标)

```
turtle.fd(d)海龟的正前方向
turtle.bk(d)海龟的反方向
turtle.circle(r,angle)以海龟左侧的某个点为圆心曲线前进
```



### 角度坐标体系

#### 绝对角度

```
turtle.seth(angle)改变行进方向,但不行进
```

#### 海龟角度

```
turtle.left(angle)向海龟的左侧
turtle.right(angle)向海龟的右侧
```



### RGB色彩体系

* RGB指红绿蓝三个通道的颜色组合
* 三个通道每个取值范围为0~255整数或0~1的小数

#### 色彩模式

```
turtle.colormode(mode)	1.0小数值模式	255整数值模式
```



#### 画笔控制函数

```
turtle.penup()		别名	turtle.pu()		抬起画笔
turtle.pendown() 	别名	turtle.pd()		落下画笔
```

```
turtle.pensize(width)	别名	turtle.width(width)	画笔宽度设置
turtle.pencolor(color)	color为颜色的字符串或r,g,b值	画笔颜色设置
color有三种形式:	
"purple"	字符串
0.63, 0.13, 0.94	小数值
(0.63, 0.13, 0.94)	元组值
```



#### 运动控制函数

```
turtle.forward(d)	别名	turtle.fd(d)	直线向前行进,d可以为负数,单位为像素
turtle.circle(r, extent = None)		r是半径,extent是角度,默认是360度
根据半径r绘制extent角度的弧形,默认圆心在海龟左侧r距离的位置
```



#### 方向控制函数

```
turtle.setheading(angle)	别名		turtle.seth(angle)	改变行进方向 
turtle.left(angle)	向左转
turtle.right(angle)	向右转
```



`turtle.done()`设置窗体不会自动退出,需要手动退出



## time标准库

处理时间



### 时间获取



* `time()`获取当前时间戳,浮点数,计算机内部时间值,从1970年1月1日0点(UNIX诞生的"纪元")至今为止的秒数
* `ctime()`返回字符串,表示当前的时间,格式为易读的方式
* `gmtime()`返回计算机可以处理的时间格式



### 时间格式化

* `strftime(tpl,ts)` 将ts按tpl规定的格式转化为字符串,tpl是格式化模板字符串,定义输出效果,ts是计算机可处理的时间类型

* `strptime(str,tpl)`将str按照tpl规定的格式转化为计算机可以操作的时间类型

* 格式化字符串

  * `%Y`年份 0000~9999

  * `%m`月份 01~12   `%B`月份名称January~December   `%b`月份名称缩写 Jan~Dec

  * `%d`日期 01~31

  * `%A`星期 Monday~Sunday   `%a`星期缩写Mon~Sun

  * `%H`小时(24h) 00~23   `%I`小时(12h) 01~12

  * `%p`上/下午 AM/PM

  * `%M` 分钟 00~59

  * `%S` 秒 00~59

    

### 程序计时应用

* `per_counter()`返回一个CPU级别的精确时间计数值,单位为秒,浮点数类型,两次调用计算差来代表经过的时间
* `sleep(s)`程序停顿s秒,s可以是浮点数



## random标准库

使用随机数

* 伪随机数:使用梅森旋转算法生成的伪随机序列
* 使用:`import random`



### 基本随机数函数

* 随机数种子  	->		梅森旋转算法		-> 		随机序列

* 只要种子相同,随机序列就唯一确定了 

* ````seed
  seed(a=None)	初始化随机种子,默认为当前系统时间
  >>>random.seed(10)		设置种子为10 
  ````

* ```
  random()		生成一个[0..1]之间的随机小数
  >>>random.random()
  ```



### 扩展随机数函数

* ```
  randint(a,b)	生成一个[a,b]之间的整数
  >>>random.randint(10,100)
  ```

* ```randrange
  randrange(m,n[,k])	在[m,n)之间生成步长为k的随机整数
  >>>random.randrange(10,100,10)
  ```

* ```
  getrandbits(k)	生成一个k比特长的随机整数
  ```

* ```
  uniform(a,b)	生成一个[a,b]之间的随机小数
  ```

* ```
  choice(seq)		从序列seq中随机选择一个元素
  >>>random.choice([1,2,3])
  ```

* ```
  shuffle(seq)	将序列seq的元素随机排序并返回
  ```



## PyInstaller第三方库

* 打包源代码,生成可执行文件

#### 安装

```
pip install pyinstaller
```

#### 使用

```
cmd命令行下,在源代码的目录下,执行命令
pyinstaller -F <filename.py>

会在当前目录下生成三个文件夹:
__pycache__
build
dist
与源文件同名的可执行文件在dist文件夹下
```

#### 常用参数

````
pyinstaller	<参数>
-h	查看帮助
--clean	清理打包过程中的临时文件
-D,--onedir	默认值,生成dist文件夹
-F,--onefile	在dist文件夹中只生成独立的打包文件,即可以独立运行
-i <图标文件名.ico>	指定打包程序的图标文件
````



## jieba第三方库

* 中文分词第三方库,通过分词获得单个单词

#### 安装

```
pip install jieba
```



#### 分词原理

* 利用中文词库,确定汉字间的关联概率
* 汉字间概率大的组成词组,也可以添加自定义词组



#### 三种模式

* 精确模式
  * 精确切分文本,没有冗余单词
* 全模式
  * 得出所有可能的单词,有冗余
* 搜索引擎模式
  * 在精确模式的基础上,再次切分长词



#### 常用函数

```
jieba.lcut(s)		精确模式,返回分词的列表类型
jieba.lcut(s,cut_all=True)	全模式,返回分词的列表,有冗余
jieba.lcut_for_search(s)	搜索引擎模式,返回分词的列表,有冗余
jiaba.add_word(w)	向词典增加新词w
```



## wordcloud第三方库

词云展示,将一段文本转化成一个词云

#### 安装

```
pip install wordcloud
```

#### 基本使用

词云作为一个WordCloud对象

```
w = wordcloud.WordCloud(<参数>)	生成一个词云对象,变量名为w
参数
width		生成图片的宽度,默认为400像素
height		生成图片的高度,默认为200像素
min_font_size	指定字体的最小字号,默认为4号
max_font_size	指定字体的最大字号,根据高度自动调节
font_step		字体字号的增加步长,默认为1
font_path		指定字体文件的路径,默认None
max_words		指定词云显示的最大单词数量,默认为200
stop_words		指定词云的排除词列表
background_color指定背景颜色,默认为黑色

mask			指定词云形状
from scipy.misc	import imread
mk = imread("pic.png")
w = wordcloud.WordCloud(mask = mk)
```

##### 常规方法

```
w.generate(txt)		向WordCloud对象w中加载文本txt
w.to_file(filename)	将词云输出为图像文件	.png或.jpg格式
```

##### 步骤

```
1.	配置对象参数		c = wordcloud.WordCloud()
2.	加载词云文件		c.generate("wordcloud by Python")	
3.	输出词云文件		c.to_file("pywordcloud.png")
```



## os标准库

提供通用的,基本的操作系统交互功能

#### 路径操作

os.path 字库以 path 为入口,操作和处理文件, path 包括目录或包含文件名的路径

```
import os.path
import os.path as op
```

##### 方法

```
os.path.abspath(path) 返回 path 在当前系统中的绝对路径
os.path.normpath(path) 归一化 path 的表示形式,统一用 \\ 分割路径
os.path.relpath(path) 返回当前 python 程序与调用的文件之间的相对路径

os.path.dirname(path) 返回 path 中的目录名称
os.path.basename(path) 返回 path 中的文件名称
os.path.join(path, *paths) 组合 path 与 paths,返回路径字符串

os.path.exists(path) 判断 path 对应的目录或文件是否存在
os.path.isfile(path) 判断 path 的文件是否存在
os.path.isdir(path) 判断 path 的目录是否存在

os.path.getatime(path) 返回上一次的访问时间
os.path.getmtime(path) 返回上一次的修改时间
os.path.getctime(path) 返回创建时间
```



#### 进程管理

执行程序或命令,返回调用信息

```
os.system(path)
```



#### 环境参数

```
os.chdir(path) 修改当前程序操作的路径
os.getcwd() 返回程序的当前路径

os.getligin() 登录用户的名称
os.cpu_count() CPU数量

os.urandon(n) 返回 n 字节长度的随机字符串,一般用于加解密运算
```

