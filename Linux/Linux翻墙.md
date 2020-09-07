# Linux翻墙

[教程链接](https://github.com/qingshuisiyuan/electron-ssr-backup)

## Debian系列--Ubuntu18.04为例

#### 安装依赖

```
sudo apt install libcanberra-gtk-module libcanberra-gtk3-module gconf2 gconf-service libappindicator1
如果报错,安装可选依赖
sudo apt-get install libssl-dev
sudo apt-get install libsodium-dev
```

#### 安装`SSR`软件

```
sudo dpkg -i *.deb
```

#### 安装`python`

```
sudo apt install python
```

#### 运行软件

```
electron-ssr
```

