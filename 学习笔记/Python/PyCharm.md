# PyCharm



## README

* 学习 IDE 工作流 和 用例, 快捷键
* 快捷键冲突
* 关闭项目



## IDE 基础知识



### 上下文操作

`Alt + Enter` 				显示上下文: 显示建议, 采用建议



### 搜索操作

`Ctrl + Shift + A` 	查找操作: 搜索操作

`Shift + Shift`			随处搜索: 搜索所有

工作流: 键入 `key`,  `Enter`, `Esc` 



### 选中

`Ctrl + W`					逐级扩大

`Ctrl + Shift + W`	逐级缩小



### 注释行

`Ctrl + /`					注释



### 复制删除行

`Ctrl + D`					复制选中行

`Ctrl + Y`					删除选中行



### 移动代码段

`Alt + Shift + 上下箭头`			移动当前行

`Ctrl + Shift + 上下箭头`			光标位于函数名所在行, 移动整个函数



### 收起与展开代码

`Ctrl + 减号`				收起代码

`Ctrl + 等号`				展开代码

`Ctrl + Shift + 减号`			收起所有代码

`Ctrl + Shift + 等号`			展开所有代码



### 添加和解除模板代码

`Ctrl + Alt + T`					添加模板代码

`Ctrl + Shift + Delete`	解除模板代码



### 多选与替换

`Alt + J`						逐个选择光标处的标识符	

`Alt + Shift + J`		逐个取消选中的匹配项

`Ctrl + Alt + Shift + J`		选择所有匹配项

选中后可以直接输入 key 替换



## 代码补全



### 基本补全

`Enter`					一般补全

`Ctrl + 空格`		显式补全



### 替换补全

`Ctrl + 空格 + Tab`		替换补全



### 后缀补全

`.if`



### 类型匹配补全

`Ctrl + Shift + 空格`			智能补全



### F-string 补全

`{`





## 重构



### 重构菜单

`Ctrl + Alt + Shift + T`		重构菜单



### 重命名

`Shift + F6`			重命名

展开动态, 选中排除项, 按 `Delete`		排除项



### 提取变量

`Ctrl + Alt + V`		提取变量



###  提取方法

`Ctrl + Alt + M`		提取方法



### 快速修复重构

改变结构	`Alt + Enter`	更改签名快速修复	设置默认值



### 就地重构

在函数定义处修改, `Alt + Enter`

​	

## 编程辅助



### 代码格式

`Ctrl + Alt + L`		优化代码格式



### 参数信息

`Ctrl + P`					参数信息



### 方法信息

`Ctrl + Q`					函数名

`Ctrl + Shift + I`	函数完整定义



### 编程辅助

`F2`								跳转下一个错误

`Alt + Enter`			修复错误

`Ctrl + F1`				查看错误信息

`Ctrl + Shift + F`显示标识符的所有调用处



## 导航



### 随处搜素

`Shift + Shift`		搜索所有

`Ctrl + N`					查找类



### 查找和替換

`Ctrl + Shift + F`	查找	 	`Alt + W`	完整词

`Ctrl + Shift + R`	替换



### 声明和用法

`Ctrl + B`		声明

`Alt + F`		  用法



### 文件结构

`Ctrl + F12`		文件结构

`Alt + 7`			 文件结构窗口



### 最近文件和位置

`Ctrl + B`		声明

`Ctrl + E`		最近打开的文件

`Ctrl + Shift + E`		最近访问文件中的代码



## 运行和调试



## 运行

`Ctrl + Shift + F10`		运行

每次运行都是临时配置

永久配置: 下拉菜单  保存配置/ 编辑配置



### 调试

`Ctrl + F8` 或 行号		设置断点

`Shift + F9`	按钮		调试

`+`	按钮添加表达式监控	
