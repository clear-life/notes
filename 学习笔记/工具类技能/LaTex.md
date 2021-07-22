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
