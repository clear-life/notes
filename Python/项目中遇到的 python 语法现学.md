# 项目中遇到的 python 语法现学



#### *args 与 **kwargs

* *args 指代 非字典类型的组合数据类型

  ```
  def myFun(*argv): 
      for arg in argv: 
          print (arg)
     
  myFun('Hello', 'Welcome', 'to', 'GeeksforGeeks') 
  输出:
  Hello
  Welcome
  to
  GeeksforGeeks
  ```

* **kwargs指代字典类型

  ```
  def myFun(**kwargs): 
      for key, value in kwargs.items():
          print ("%s == %s" %(key, value))
   
  myFun(first ='Geeks', mid ='for', last='Geeks')  
  输出:
  last == Geeks
  mid == for
  first == Geeks
  ```