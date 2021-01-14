# Anaconda

#### 下载

去官网下载对应`python`版本的`64bit`安装包

#### 安装

`python 3.x`

```
bash ~/Downloads/Anaconda3-5.2.0-Linux-x86_64.sh
```

#### 添加环境变量

在`~/.bashrc`文件后面加上

```
export PATH="/<path to anaconda>/bin:$PATH" 
注意一定要用pwd命令来查找文件真正的路径,文件系统中的home包含了/home/用户名/两层路径
```

#### 目录

安装目录

```
/home/用户名/anaconda3
```

#### 配置环境基础

```
sudo apt-get update && sudo apt-get install cmake libopenmpi-dev python3-dev zlib1g-dev
```



### 常用命令

####  管理环境

1. ##### 创建新环境

   ```
   conda create --name <env_name> <package_names>
   例：
   conda create -n python3 python=3.5 numpy pandas	
   //-n是--name的缩写	python3是环境名称	python=3.5指定包的版本号 
   //numpy和pandas也是一个包		conda把所有东西都看成一个包，包括python和自身conda
   //环境默认保存在 /Users/<user_name>/anaconda3/env 目录下
   ```

2. ##### 切换环境

   ```
   activate <env_name>
   //创建环境时默认为Anaconda的python版本
   //切换环境后，行首以(env_name)/[env_name]开头
   ```

3. ##### 退出环境

   ```
   conda deactivate
   ```

4. ##### 显示环境信息

   ```
   conda info --envs
   或
   conda info -e
   或
   conda env list
   //"*"所在行即为当前所在环境
   ```

5. ##### 复制环境

   ```
   conda create --name <new_env_name> --clone <copied_env_name>
   //copied_env_name是被复制的环境
   conda create --name py3 --clone python3 
   //复制python3，新环境名为py3
   ```

6. ##### 删除环境

   ```
   conda remove --name <env_name> --all
   ```

7. 取消/设置自动激活`base`环境

   ```
   conda config --set auto_activate_base false
   conda config --set auto_activate_base true
   ```

#### 管理包

1. ##### 查找可以安装的包版本

   1. 精确查找

      ```
      conda search --full-name <package_full_name>
      conda search --full-name python
      //查找全名为python的包有哪些可以安装的版本
      ```

   2. 模糊查找

      ```
      conda search <text>
      conda search py 
      //查找含有py字段的包
      ```

2. ##### 列出已安装的包

   ```
   conda list
   ```

3. ##### 安装包

   ```
   conda install --name <env_name> <package_name>
   //在指定环境中安装包，没有指定环境的话就在当前环境安装
   
   conda install 无法安装时，可以用pip安装
   pip install <package_name>
   //1、pip不能管理环境，需要先切换到目标环境中，再用pip命令安装
   //2、pip不能更新python
   //3、pip和conda都能安装一些对方不能安装的包
   
   //从Anaconda.org安装包
   //从http://anaconda.org中拿到目标包的路径开始下载
   //复制“To install this package with conda run:”下方的命令，粘贴在终端中执行
   ```

4. ##### 卸载包

   ```
   conda remove --name <env_name> <package_name>
   conda remove --name python3 pandas
   //卸载环境名为python3中的pandas包，默认情况下为当前所在环境
   ```

5. ##### 更新包

   ```
   //更新所有包
   conda update --all
   或
   conda upgrade --all
   
   //更新指定包
   conda update <package_name>
   或
   conda upgrade <package_name>
   conda update pandas numpy matplotlib
   //更新多个指定包
   ```

### pip

`pip install/uninstall <package>` 安装与卸载

`pip list` 列出库



#### apt

`/var/cache/apt/archives`安装包缓存目录

`sudo apt autoclean`自动清理软件/包

`sudo apt autoremove`自动删除软件/包



#### 取消每次命令行前的base

```
conda config --set auto_activate_base false
```



#### conda提速

##### 配置源位置/镜像

```
conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/free/
```



#### pip提速

##### 配置源位置/镜像

```
升级pip
pip install pip -U

配置全局镜像
pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
```