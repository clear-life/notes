# VSCode(Windows)配置Haskell开发环境

[TOC]

## 简介

### Haskell

* 理论基础: λ 演算
* 强静态类型
* 类型推断
* 惰性求值
* 纯函数式编程语言: 无 side-effect (副作用)
* 并发编程

### Haskell 工具链

* **GHCup**

​		版本管理器, 管理 Haskell 工具链 (GHC, Cabal, Stack, HLS)

* **GHC**

  Haskell 编译器

* **Cabal Stack**

  包管理器和构建系统

* **HLS**

  Haskell Language Server 提供代码编辑相关服务(代码补全, 跳转等)
  

$~$

## 安装

### 准备工作

关闭杀毒软件直至安装完成

### GHCup

**1. 安装 GHCup 本体**

以非管理员身份在 PowerShell 中运行:

```powershell
$env:BOOTSTRAP_HASKELL_YAML = 'https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-latest.yaml'
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://mirrors.ustc.edu.cn/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
```

一路 enter 过去, 能把 GHCup 安装就算成功, 其余的都能用 GHCup安装

包下载过程中遇到有关墙方面的网络问题尝试通过后面的 clash 全局代理方法

**2. 换源**

**GHCup 配置中科大源**

修改 GHCup 配置文件

其余教程(Linux) ghcup 配置文件位于 `~/.ghcup/config.yaml` 

对应在 Windows 中在 `C:/Users/用户名/.ghcup/config.yaml`

实际情况却是 `C:/ghcup/config.yaml`

```yaml
url-source:
  OwnSource:
    - https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-latest.yaml
```

**Cabal 配置中科大源**

powershell 执行

```powershell
cabal user-config init
```

修改 Cabal 配置文件

其余教程(Linux) Cabal 配置文件位于 `~/.cabal/config` 

对应在 Windows 中在 `C:/Users/用户名/.cabal/config`

实际情况却是 `C:/cabal/config`

**Cabal >= 1.24 (GHC 8.0)**

找到官方仓库：

```yaml
repository hackage.haskell.org
  url: http://hackage.haskell.org/
  -- secure: True
  -- root-keys:
  -- keys-threshold: 3
```

替换为:

```yaml
repository mirrors.ustc.edu.cn
  url: https://mirrors.ustc.edu.cn/hackage/
  secure: True
```

powershell 执行

 ```powershell
 cabal update
 ```



**Stack 配置中科大源**

修改 Stack 配置文件

其余教程(Linux) Stack 配置文件位于 `~/.stack/config.yaml` 

对应在 Windows 中在 `C:/Users/用户名/.stack/config.yaml`

实际情况也在 `C:/Users/用户名/.stack/config.yaml`

**>= v2.9.3**

```yaml
package-index:
  download-prefix: https://mirrors.ustc.edu.cn/hackage/
  hackage-security:
    keyids:
      - 0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d
      - 1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42
      - 280b10153a522681163658cb49f632cde3f38d768b736ddbc901d99a1a772833
      - 2a96b1889dc221c17296fcc2bb34b908ca9734376f0f361660200935916ef201
      - 2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3
      - 51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921
      - 772e9f4c7db33d251d5c6e357199c819e569d130857dc225549b40845ff0890d
      - aa315286e6ad281ad61182235533c41e806e5a787e0b6d1e7eef3f09d137d2e9
      - fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0
    key-threshold: 3 # number of keys required

    # ignore expiration date, see https://github.com/commercialhaskell/stack/pull/4614
    ignore-expiry: true
```

### 安装

包下载过程中遇到有关墙方面的网络问题尝试通过后面的 clash 全局代理方法

ghc

```bash
ghcup install ghc latest
```

cabal

```bash
ghcup install cabal latest
```

stack

```bash
ghcup install stack latest
```

hls

```bash
ghcup install hls latest
```

### Clash

如果通过换源的方式下载不了某些包, 可以通过 Clash for Windows 的 TUM mode 创建的虚拟网卡代理所有流量来下载

#### TUN Mode

Clash For Windows → 常规 → 系统代理打开

TUN Mode 需要先安装 Service Mode(服务模式)	

Clash For Windows → 常规 → Service Mode 后的管理 → 安装 → 绿色地球

Clash For Windows → 常规 → TUN Mode打开

#### 代理

非管理员权限的 PowerShell 执行

```powershell
# 假设 Clash For Windows 使用默认端口号 7890
$env:HTTP_PROXY="http://127.0.0.1:7890"
$env:HTTPS_PROXY="http://127.0.0.1:7890"
```

bash 执行

```bash
export HTTP_PROXY="http://127.0.0.1:7890"
export HTTPS_PROXY="http://127.0.0.1:7890"
```

随便找个目录执行 curl

在 powershell 中执行会报错, 所以我在 git bash 中执行 curl

```bash
curl -LO https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1
```

编辑脚本

```bash
# Exec "$Bash" '-lc' 'exit' # 注释掉这行
& "$Bash" '-lc' 'exit' 		# 加上这行
```

运行脚本

命令行或直接运行

```bash
./bootstrap-haskell.ps1 -Interactive -DisableCurl
```

### VSCode

安装插件 haskell.haskell

## 检验

查看模块安装情况

```bash
ghcup tui
```

构建项目

```bash
mkdir test
cd test
cabal init --interactive
cabal build
cabal run
```

## 参考

[国内如何安装 Haskell 运行环境 (Windows)](https://niallzhou.cfd/post/tricks/install_haskell_env/)

[中科大镜像站: GHCup](https://mirrors.ustc.edu.cn/help/ghcup.html)

[清华镜像站: Hackage](https://mirrors.tuna.tsinghua.edu.cn/help/hackage/)

[如何安装 Haskell 工具链（2022 年版）](https://zhuanlan.zhihu.com/p/455688955)
