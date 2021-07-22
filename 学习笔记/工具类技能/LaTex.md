# LaTex

## 文章内容

1. 最简洁的 LaTex 编辑器, TeXworks 的使用
2. 中英混排
3. 文章组织结构
4. 数学公式
5. 插入图片/表格
6. 版面设置
7. 常见 TeX 单词含义
8. 出现问题如何解决



## 编辑器简单介绍

1. TeX 源代码后缀为 `.tex `纯文本文件
2. 常用 **TeXworks** 编辑器

### 启动 TeXworks



### 排版工具(引擎, 相当于编译器)

TeXworks 预设了若干排版工具: pdfTeX, pdfLaTeX, XeTeX, XeLaTeX 等

本文主要用 **XeLaTeX**



## 第一篇文档

### Hello, world!

```latex
\documentclass{article}
% 这里是导言区
\begin{document}
Hello, world!
\end{document}
```

1. `\documentclass{article}`

   1. **控制序列**, 以 `\` 开头, 以**第一个空格或非字母**结束

   2. 控制序列不输出, 控制输出效果

   3. 解释:

      1. 控制序列 `documentclass`
      2. 控制序列参数 `{article}`, 参数值为 `article`
      3. 作用: 调用名为 `article` 的**文档类** ( `.cls`  文件)

      > 1. `TeX` 对控制序列的大小写敏感
      > 2. `[]` 为可选参数
      > 3. 文档类: 已定义好的功能库

2. `% 这里是导言区`

   行注释 `%`, 可用 `\` 转义

3. `\documentclass{article}` 到 `\begin{document}` 之间的部分是**导言区**, **控制整篇文档的格式控制**

4. `\begin{document}` 与 `\end{document}`

   1. 控制序列 `begin` 和 `end` , 成对出现
   2. 二者第一个参数必须一致, 称为环境名(这里环境名为 `document` )
   3. 二者之间的内容是环境, 环境的内容才会被输出

### 实现中英混排

1. 宏包( `.sty`  文件)

   * 控制序列的集合, 把**常用的控制序列放在同一文件里, 即宏包**里

   * 用 `\usepackage{}` 调用宏包 

2. `CTeX` 宏集

   1. **面向中文的通用 LaTeX 排版框架**

   2. 提供四个**文档类**和三个**宏包**

      * 文档类

        * **ctexart.cls**

          标准文档类 `article` 的汉化版本，一般适用于短篇幅的文章

        * **ctexrep.cls**

          标准文档类 `report` 的汉化版本，一般适用于中篇幅的报告

        * **ctexbook.cls**

          标准文档类 `book` 的汉化版本，一般适用于长篇幅的书籍

        * **ctexbeamer.cls**

          文档类 `beamer` 的汉化版本，适用于幻灯片演示

      * 宏包

        * **ctex.sty**

          提供全部功能，但默认不开启章节标题设置功能，需要使用 `heading` 选项来开启

        * **ctexsize.sty**

          定义和调整中文字号，可以在ctex宏包或CTEX中文文档类之外单独调用

        *  **ctexheading.sty**

          提供章节标题设置功能，可以在ctex宏包或CTEX中文文档类之外单独调用

   3. 使用 `CTeX` 宏集

      ```latex
      \documentclass[UTF8]{ctexart}
      % 这里是导言区
      \begin{document}
      你好, world!
      \end{document}

## 组织文章

### 标题, 作者和日期

```latex
\documentclass[UTF8]{ctexart}
\title{你好, world!}	   % 标题
\author{Yangjq}			% 作者
\date{\today}			% 日期
% 这里是导言区
\begin{document}
\maketitle				% 控制序列 maketitle : 将导言区定义的内容按预定格式排版
你好, world!
\end{document}
```

### 章节和段落

**关键字解释**

|    关键字     |   含义   |  类比  |
| :-----------: | :------: | :----: |
|    section    |   部分   | 标题一 |
|  subsection   |  子部份  | 标题二 |
| subsubsection | 子子部份 | 标题三 |
|   paragraph   |   段落   | 缩进一 |
| subparagraph  |  子段落  | 缩进二 |

**代码**

```latex
\documentclass[UTF8]{ctexart}


\begin{document}


\section{标题一}
标题一的相关文字

\subsection{标题二}
标题二的相关文字

\subsubsection{标题三}
标题三的相关文字

\paragraph{缩进一}
缩进一的相关文字

\subparagraph{缩进二}
缩进二的相关文字

\subsection{标题二}
\paragraph{缩进一} 
缩进一的相关文字


\end{document}
```

> 代码中的空白符会被忽略

**说明**

文档类 `article/ctexart` 中, 定义了五个控制序列来组织文章结构

```latex
\section{·}
\subsection{·}
\subsubsection{·}
\paragraph{·}
\subparagraph{·}
```

> 文档类 `report/ctexrep` 有 `\chapter{·}`
>
> 文档类 `book/ctexbook` 有 `\part{·}`

### 插入目录

```latex
\documentclass[UTF8]{ctexart}

\title{你好，world!}	% 需要有 title 才能生成目录

\begin{document}

\maketitle			  % 生成文章开头
\tableofcontents	  % 目录一般在文章开头后面

\section{标题一}
标题一的相关文字

\subsection{标题二}
标题二的相关文字

\subsubsection{标题三}
标题三的相关文字

\paragraph{缩进一}
缩进一的相关文字

\subparagraph{缩进二}
缩进二的相关文字

\subsection{标题二}
\paragraph{缩进一} 
缩进一的相关文字


\end{document}
```

> LaTeX 将一个换行当作一个空格处理

## 数学公式

导言区加载 `amsmath` 宏包使用 `AMS-LaTeX` 宏集(排版框架)提供的数学功能

```latex
\usepackage{amsmath}
```

### 数学模式

#### 行内模式

行中插入公式

```latex
$...$	
或
\(...\)
```

#### 行间模式

公式单独成行, 自动居中

```latex
\[
...
\]

% 编号行间公式
\begin{equation}	% equation 方程
...
\end{equation}
```

### 上下标

1. 上标 `^`, 下标 `_`, 默认只作用于后一个字符, 可以用 `{}` 括住连续字符作为上下标
2. 行内公式的标点应放在公式外, 行间公式的标点应放在公式内

```latex
\documentclass{article}

\usepackage{amsmath}	% 数学公式宏包


\begin{document}

% 行内公式
$E=mc^2$.

% 行间无编号公式
\[ E=mc^2. \]

% 行间有编号公式
\begin{equation}
E=mc^2.
\end{equation}

\end{document}
```

```latex
\[ e^{2x}. \]
```

### 根式与分式

根式 `\sqrt{·}`

分式 `\frac{·}{·}`	第一个参数为分子, 第二个参数为分母

```latex
\documentclass{article}

\usepackage{amsmath}


\begin{document}

$\frac{1}{2}$.			% fraction 分数,分式

\[ \sqrt{x}, \]

\[ \frac{1}{2}. \]

\end{document}
```

> 行内行间的分式输出效果不同
>
> 若要行内分式显示为 行间分式的大小, 可以使用 `\dfrac`

### 运算符

小的运算符可以在数学模式下直接输入, 另一些则需要控制序列生成

```latex
\[ 
\pm\; 		% ±
\times\; 	% ×
\div\; 		% ÷
\cdot\; 	% • 点乘
\cap\; 		% ∩
\cup\;		% ∪
\geq\; 		% ≥
\leq\; 		% ≤
\neq\; 		% ≠
\approx\; 	% ≈
\equiv\; 	% ≡
\]
```

连加, 连乘, 极限, 积分

```latex
连加 \sum		\sum _{i=1}^n i
连乘 \prod	\prod _{i=1}^n
极限 \lim		\lim _{x\to0} x^2	
积分 \int		\int _a^b x^2 dx
空格 \quad
不压缩上下标	\limits		\sum\limits _{i=1}^n i
压缩上下标	 \nolimits	 \int\nolimits _a^b x^2 dx
→	\to
```

多重积分

```latex
双重	\iint
三重	\iiint
四重	\iiiint
多重	\idotsint
```

### 定界符

括号

```latex
小  中 大   尖 (括号)
() [] \{\} \langle\rangle
```

竖线

```latex
竖线	 \lvert \rvert 
双竖线	\lVert \rVert
```

调整定界符大小

```latex
从小到大	\big \Big \bigg \Bigg
```

### 省略号

```latex
\dots	普通下标省略号	   
\cdots	横向省略号		
\vdots	纵向省略号
\ddots	斜向省略号
```

### 矩阵

```latex
环境而不是控制序列, 所以需要加上 \begin 和 \end 
小		中	   大	  竖线	 双竖线
pmatrix bmatrix Bmatrix vmatrix Vmatrix

\[ 
\begin{pmatrix} a&b\\c&d \end{pmatrix} 
\begin{bmatrix} a&b\\c&d \end{bmatrix} 
\begin{Bmatrix} a&b\\c&d \end{Bmatrix} 
\begin{vmatrix} a&b\\c&d \end{vmatrix} 
\begin{Vmatrix} a&b\\c&d \end{Vmatrix} 
\]

\\ 换行

行内公式小矩阵: 环境 smallmatrix
$ ( \begin {smallmatrix} a&b\\c&d \end {smallmatrix} ) $
```

### 多行公式

**不对齐**

`multline` 环境

```latex
\begin{multline}
x = a+b+c+{} \\
d+e+f+g
\end{multline}
```

不要编号, `multline*` 环境

**对齐**

用 `aligned` **次环境**实现公式对齐, **次环境必须包含在数学环境内**

```latex
\[
\begin{aligned}
x ={}& a+b+c+{} \\
&d+e+f+g
\end{aligned}
\]
```

**公式组**

无需对齐公式组使用 `gather` 环境

需要对齐公式组使用 `align` 环境

不需要编号就用带 `*` 的版本

```latex
% 不对齐
\begin{gather}
a = b+c+d \\
x = y+z
\end{gather}

%  对齐
\begin{align}
a &= b+c+d \\
x &= y+z
\end{align}
```

**分段函数**

用次环境 `cases` 实现分段函数, 次环境必须包含在数学环境内

```latex
\[ 
y= 
\begin{cases}
-x, x\leq0 \\
x, x>0
\end{cases} 
\]

```



### 辅助工具

```
https://mathpix.com/
截屏识别为 latex 公式

http://detexify.kirelabs.org/classify.html
鼠标绘制单个数学符号, 返回 latex 代码和所需宏包
```

## 插入图片和表格

### 图片

一般用 `graphicx` 宏包提供的 `\includegraphics` 控制序列

可选参数: `[width = .8\textwidth]` 图片宽度设为页面宽度的 80% 

```latex
\documentclass{article}

\usepackage{graphicx}

\begin{document}

\includegraphics{a.jpg}		% 插入同目录下的 a.jpg 图片

\end{document}
```

### 表格

`tabular` 环境中的表格功能

`\hline` 	横线

 `|` 			列格式中表示竖线

 `&` 			分列

`\\`			 换行

 `l`、`c`、`r` 每列居左、居中、居右的横向对齐方式

```latex
\documentclass{ctexart}

\usepackage{amsmath}

\begin{document}

\begin{tabular}{|l|c|r|}		
 \hline
操作系统 & 发行版 & 编辑器\\
 \hline
Windows & MikTeX & TexMakerX \\
 \hline
Unix/Linux & teTeX & Kile \\
 \hline
Mac OS & MacTeX & TeXShop \\
 \hline
通用 & TeX Live & TeXworks \\
 \hline
\end{tabular}

\end{document}
```

### 浮动体

自动调整位置

用 `figure` 和 `table` 环境来实现

```latex
\begin{figure}[htbp]		
% 指定插图的理想位置
% 字母代表   here   top   bottom   float page
% 即        这里    页顶   页尾     浮动页
\centering					% 设置插图居中
\includegraphics{a.jpg}
\caption{插图标题}			% 设置插图标题
\label{fig:myphoto}

\end{figure}
```
