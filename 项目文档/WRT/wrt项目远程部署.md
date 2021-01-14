# wrt项目远程部署

* 远程部署 @胡书明 @杨金泉
  * Termius安装，10.69.14.44，账号lx，密码abc@123
  * 沙盒的位置：/home/lx/multi_wrtsb，分别是1和2
  * 你们构建自己的场景算法环境
  * docker ps：查看当前容器进程
    * docker start/stop/restart wrtsb_1/2，启动/停止/重启 沙盒容器
  * scp远程拷贝，rewards.db（/home/lx/multi_wrtsb/1/UDS/sqliteConf）
    * scp ./mcp.db lx@10.69.14.44:/home/lx/multi_wrtsb/1，将本地指定文件拷贝至远程服务器指定目录下
    * scp lx@10.69.14.44:/home/lx/multi_wrtsb/1/mcp.db .，将远程服务器指定路径下的文件拷贝到当下目录
  * tar -czf  wrt.tar.gz UDS MCP *.py ，tar -zxvf wrt.tar.gz，压缩和解压文件/文件夹
  * 浏览器访问：
    * wrtsb_1：http://10.69.14.44:8081/mcp，对应第一个python算法
    * wrtsb_2：http://10.69.14.44:8082/mcp，对应第二个python算法
  * ps aux|grep python 查看进程python
  * top -d 1 查看机器资源和进程