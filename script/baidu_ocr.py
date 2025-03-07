import re
import os
import sys
import threading
from concurrent.futures import ThreadPoolExecutor
import time
from paddleocr import PaddleOCR

# ------------工具函数--------
def process_input():
    print(f"输入工作目录, 并确保该目录下有文件夹 files, files 中含有要处理的文件\n"
          "工作目录示例: D:/目录1/目录2/workspace\n"
          "建议使用 / 作为路径分隔符\n")

    global work_path
    work_path = input("请输入工作目录: ")
    while not os.path.exists(work_path) or not os.path.exists(f"{work_path}/files"):
        print(f"{work_path} 路径或 files 子目录不存在")
        work_path = input("请输入工作目录: ")
    print(f"工作目录: {work_path}")

    global files_path
    files_path = f"{work_path}/files"

    print()
    print("输入要匹配的关键词列表, 脚本在文本中最先匹配到的某个关键词作为判定结果\n"
          "匹配规则举例: 关键词 \"甲方\" 在文本中能匹配 \"甲\"+零到多个空白符+\"方\"\n"
          "注: 空白符指 空格 制表符 换行符\n"
          "关键词示例: 关键词1 关键词2 关键词3\n")

    global keywords
    keywords = input("请输入要匹配的关键词(1~n个): ").split()
    print(f"关键词列表: {keywords}")

    with open(f"{work_path}/{skip_log}", "w") as f:
        pass


def get_new_name(file_name, match_keyword):
    file_name = file_name[:-4]
    if file_name[-len(match_keyword):] == match_keyword:
        print(f"{file_name}.pdf 已有后缀 -{match_keyword}")
        return f"{file_name}.pdf"
    return f"{file_name}-{match_keyword}.pdf"


def process_exit():
    print()
    print("程序执行完毕。")
    print(f"pdf 文件中被跳过的部分记录在 {work_path}/{skip_log} 中")
    print(f"错误信息记录在 {work_path}/{err_log} 中")
    input("按 Enter 键退出...")


def process_exist(path):
    if not os.path.exists(path):
        des = f"{path} 不存在"
        print(des)
        with open(f"{work_path}/{skip_log}", "a") as f:
            f.write(f"{des}\n")
        return False
    return True


# ------------功能函数--------
def process_text_return_keyword(file_path):
    text = ""

    # ocr = PaddleOCR(use_angle_cls=True, lang="ch", page_num=1)
    result = ocr.ocr(file_path, cls=True)

    for i in range(len(result)):
        res = result[i]

        if res is None:
            continue

        for line in res:
            if len(line) > 1 and len(line[1]) > 0:
                text += line[1][0]

    print("---------------文本识别结果-------------")
    print(text)
    print("-------------文本识别结果分隔符------------")

    res = regex.search(text)

    return ''.join(res.group().split()) if res else None


def process_directory(dir_name):
    print("dir_name:", dir_name)
    dir_path = f"{files_path}/{dir_name}"

    if not process_exist(dir_path):
        return

    root, dirs, files = next(os.walk(dir_path))
    for file_name in files:
        unique_number = str(int(time.time() * 1000))
        os.rename(f"{dir_path}/{file_name}", f"{dir_path}/{unique_number}")
        time.sleep(0.001)

    root, dirs, files = next(os.walk(dir_path))
    for i, file_name in enumerate(files):
        os.rename(f"{dir_path}/{file_name}", f"{dir_path}/{dir_name}({i+1}).pdf")

    time.sleep(0.001)
    root, dirs, files = next(os.walk(dir_path))
    for file_name in files:
        process_single_file(dir_path, file_name)
        print()

    # with ThreadPoolExecutor(max_workers=5) as executor:
    #     for file_name in files:
    #         executor.submit(process_single_file, dir_path, file_name)
    #         print()


def process_single_file(dir_path, file_name):
    print("file_name:", file_name)
    file_path = f"{dir_path}/{file_name}"

    if not file_path.endswith('.pdf'):
        print(f"{file_path} 不是 pdf 文件, 跳过")
        return

    if not process_exist(file_path):
        return

    # try:
    match_keyword = process_text_return_keyword(file_path)
    print(f"匹配结果关键词: {match_keyword}")
    # except Exception as e:
    #     des = f"由于发生异常 {e}, 跳过 {file_name}"
    #     print(des)
    #     with open(f"{work_path}/{skip_log}", "a") as f:
    #         f.write(f"{des}\n")
    #     return

    if match_keyword:
        new_name = get_new_name(file_name, match_keyword)
        print(f"{file_name} 将被重命名为", new_name)

        # if input("请确认是否重命名([y]/n):") == "n":
        #     des=f"{file_name} 手动跳过"
        #     print(des)
        #     with open(f"{work_path}/{skip_log}", "a") as f:
        #         f.write(f"{des}\n")
        #     return

        rename = f"{dir_path}/{new_name}"
        os.rename(file_path, rename)
        print(f"{file_name} 已被重命名为", new_name)
    else:
        des = f"{file_name} 并未匹配到任何一个关键词, 跳过"
        print(des)
        with open(f"{work_path}/{skip_log}", "a") as f:
            f.write(f"{des}\n")


# C:\Users\clearlife\Downloads\fsd
# 劳动合同 巨人网络集团 张子轩
if __name__ == "__main__":
    work_path = r'C:\Users\clearlife\Downloads\fsd'
    files_path = ''
    keywords = []

    err_log = 'error.log'
    skip_log = 'skip.log'

    process_input()

    sys.stderr = open(f"{work_path}/{err_log}", 'w')

    key_word_pattern = r'(?:' + '|'.join([r"\s*".join(keyword) for keyword in keywords]) + r')'
    print(f"key_word_pattern: {key_word_pattern}")

    regex = re.compile(key_word_pattern)
    print()

    ocr = PaddleOCR(use_angle_cls=True, lang="ch", page_num=1)

    root, dirs, files = next(os.walk(files_path))

    # 单线程版本
    for dir_name in dirs:
        process_directory(dir_name)

    for file_name in files:
        process_single_file(files_path, file_name)
        print()


    # 多线程版本
    # with ThreadPoolExecutor(max_workers=5) as executor:
    #     executor.map(process_directory, dirs)
    #
    # threads = []
    # for file_name in files:
    #     thread = threading.Thread(target=process_single_file, args=(files_path, file_name))
    #     thread.start()
    #     threads.append(thread)
    #     print()
    #
    # for thread in threads:
    #     thread.join()

    process_exit()
