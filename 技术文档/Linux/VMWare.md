# VMWare

## VMWare安装

### 1.下载

去官网下载

### 2.安装

不加入体验计划

### 3.激活

在网上找到批量激活码永久激活

```
CG392-4PX5J-H816Z-HYZNG-PQRG2
```



## Ubuntu18.04安装

### 1.下载

在官网的以往版本中下载18.04版本的Ubuntu镜像,名称为

```
ubuntu-18.04.5-desktop-amd64.iso
```

### 2.新建虚拟机

说明:没有写出来就用默认的配置

1. 新建虚拟机
2. 自定义配置
3. 一定要选择**稍后安装操作系统**
4. 根据情况选择版本
5. 名称与**虚拟机存放位置**
6. 根据情况选择网络类型,一般选**网络地址转换**
7. 创建完毕

### 3.配置虚拟机

1. 编辑虚拟机设置
2. CD/DVD-使用ISO映像文件-选中下载好的Ubuntu映像文件

### 4.安装Ubuntu系统

1. 开启此虚拟机
2. 正常安装-下载更新-安装第三方软件
3. 安装类型:Something else
4. 设置分区
   1. 启动分区`boot`
      * 大小256MB
      * 挂载点`/boot`
   2. 交换分区`swap`
      * 大小两G
      * Use as:`swap area`
   3. 根分区`/`
      * 剩下所有空间
5. 设置密码:`123`,设置自动登录



### 5.删除虚拟机

1. 右键选中相应虚拟机
2. 管理-从磁盘中删除

### 6.遇到的问题及解决办法

#### 6.1启动虚拟机时`模块“CPUIDEarly”启动失败`

​	进入BIOS界面,将`Virtualization Technology`设置为`Enable`

#### 6.2鼠标切换

​	`ctrl + alt`切换到本机



## Ubuntu18.04熟悉

### 1.设置方面

#### 1.1设置语言

1. 右上角小三角-扳手(设置),进入设置界面
2. `Region&Language`-`Manage Installed Languages`,进入语言安装管理界面(安装语言支持)
3. 语言支持-安装/移除语言-安装简体中文
4. 在菜单和窗口语言里把汉语拖到第一个位置
5. 重启Ubuntu,保留英文的文件名

#### 1.2安装VMware Tools

1. VMware菜单栏-虚拟机-安装VMware Tools,Ubuntu桌面出现一个光盘文件

2. 打开光盘文件,把`.tar.gz`压缩包打开,把文件夹提取到home(主目录)下

3. 打开终端

   ```
   ls
   cd vmware-tools-distrib
   sudo ./vmware-install.pl
   ```

4. 输入`root`密码,一路`'y'`,成功后重启Ubuntu
5. 安装成功后就可以设置共享文件夹了
6. 在虚拟机设置中启用共享文件夹,并添加文件夹目录,Ubuntu中的`/mnt/hgfs`就是另一个共享文件夹

#### 1.3终端熟悉

1. 命令行`:`左边的组成是`用户名`+`主机名`,`:`右边的组成是`路径`+`$/#`,`$`表示普通用户,`#`表示超级用户

#### 1.4设置服务器镜像源

1. `show applications`
2. 软件和更新
3. `Ubuntu Software`-`Download from`-`Other`-`Select Best Server`

#### 1.5`apt`终端命令

`apt`全称为`Advanced Packaging Tool`

帮助命令:

`apt`

常用命令

`sudo apt install ...`安装

`sudo apt remove ...`卸载

`sudo apt update ...`更新

`sudo apt upgrade ...`升级

删除不使用的软件

`libreoffice`办公软件

```
sudo apt remove libreoffice-common
```

`Amazon`

```
sudo apt remove unity-webapps-common
```

`apt`和`apt-get`

二者都是安装软件的命令,早期使用`apt-get`,从`Ubuntu16`开始,官方建议用`apt`

#### 1.6`deb`安装格式

`deb`是`Debian Linux`的安装格式,需要使用`dpkg`命令进行安装

```
sudo dpkg -i <package.deb>		-i/--install
```





### 2.安装常用软件



#### 谷歌浏览器

```
sudo apt install libappindicator1 libindicator7
sudo dpkg -i google-chrome-stable_current_amd64.deb
sudo apt -f install
```



### 输入法

`fcitx`是一个以`GPL`方式发布的输入法平台,支持多种输入法

```
sudo apt install fcitx
```

系统设置-语言支持-键盘输入法系统设置为`fcitx`

搜狗输入法

去官网下载最新安装包到`Download`文件夹

```
sudo dpkg -i sogoupinyin_*_amd64
sudo apt -f install
```

重启后点击右上角键盘标志

`configure Current Input Method`-添加搜狗输入法

`Trigger Input Method`触发输入法

`Scroll between Input Method`设置输入法切换



#### vim

```
sudo apt install vim
```



#### git

```
sudo apt install git
```

