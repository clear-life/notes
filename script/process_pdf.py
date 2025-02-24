import re
import os
import sys
import cv2
import numpy as np
import threading
from concurrent.futures import ThreadPoolExecutor
from pdf2image import convert_from_path
import pytesseract
import uuid
import time


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


def correct_image_orientation(image):
    # 使用Tesseract的OSD功能获取图像方向信息
    try:
        osd = pytesseract.image_to_osd(image)
    except Exception as e:
        print(f"correct_image_orientation 异常 {e}", file=sys.stderr)
        return image
    rot = [int(s) for s in osd.split() if s.isdigit()][-1]

    # 根据检测到的方向旋转图像
    if rot == 90:
        image = cv2.rotate(image, cv2.ROTATE_90_CLOCKWISE)
    elif rot == 180:
        image = cv2.rotate(image, cv2.ROTATE_180)
    elif rot == 270:
        image = cv2.rotate(image, cv2.ROTATE_90_COUNTERCLOCKWISE)

    return image

def process_text_return_keyword(file_path, page_num):
    # 提取整个 pdf 文本信息
    images = convert_from_path(f"{file_path}", first_page=1, last_page=1)
    if len(images) <= 0:
        print("出错啦, 从 pdf 识别出来的图片数为0")
        return

    opencv_image = cv2.cvtColor(np.array(images[0]), cv2.COLOR_RGB2BGR)

    # 旋转图片到正向
    try:
        rotated_image = correct_image_orientation(opencv_image)
    except Exception as e:
        rotated_image = opencv_image
        des = f"{file_path} 文件出现异常: {e}"
        print(f"{des}")
        print(f"{des}", file=sys.stderr)

    # 灰度转换
    # gray = cv2.cvtColor(rotated_image, cv2.COLOR_BGR2GRAY)
    # plt.imshow(gray)
    # plt.axis('off')
    # plt.show()
    # 噪音去除
    #blur = cv2.GaussianBlur(gray, (3, 3), 0)

    # 二值化
    #thresh = cv2.adaptiveThreshold(blur, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 11, 2)

    # 形态学操作
    #kernel = np.ones((5, 5), np.uint8)
    #dilate = cv2.dilate(thresh, kernel, iterations=1)
    #erode = cv2.erode(dilate, kernel, iterations=1)

    # 图像缩放
    #resized = cv2.resize(erode, None, fx=1.5, fy=1.5, interpolation=cv2.INTER_CUBIC)

    # OCR
    text = pytesseract.image_to_string(rotated_image, config=f'--psm 6 -l {languages}')

    print("---------------文本识别结果-------------")
    print(text)
    print("-------------文本识别结果分隔符------------")

    res = regex.search(text)

    return ''.join(res.group().split()) if res else None


def process_exist(path):
    if not os.path.exists(path):
        des = f"{path} 不存在"
        print(des)
        with open(f"{work_path}/{skip_log}", "a") as f:
            f.write(f"{des}\n")
        return False
    return True


def process_directory(dir_name):
    print("dir_name:", dir_name)
    dir_path = f"{files_path}/{dir_name}"

    if not process_exist(dir_path):
        return

    root, dirs, files = next(os.walk(dir_path))
    for file_name in files:
        unique_number = str(int(time.time() * 1000))
        print(unique_number)
        os.rename(f"{dir_path}/{file_name}", f"{dir_path}/{unique_number}")
        time.sleep(0.001)

    root, dirs, files = next(os.walk(dir_path))
    for i, file_name in enumerate(files):
        os.rename(f"{dir_path}/{file_name}", f"{dir_path}/{dir_name}({i+1}).pdf")

    root, dirs, files = next(os.walk(dir_path))
    for file_name in files:
        process_single_file(dir_path, file_name)
        print()


def process_single_file(dir_path, file_name):
    print("file_name:", file_name)
    file_path = f"{dir_path}/{file_name}"

    if not file_path.endswith('.pdf'):
        print(f"{file_path} 不是 pdf 文件, 跳过")
        return

    if not process_exist(file_path):
        return

    match_keyword = process_text_return_keyword(file_path, 1)
    print(f"匹配结果关键词: {match_keyword}")

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


# D:/workspace/Python
if __name__ == "__main__":
    work_path = ''
    files_path = ''
    keywords = []
    languages = ['eng', 'chi_sim', 'chi_sim_vert', 'chi_tra', 'chi_tra_vert']
    languages = '+'.join(languages)

    err_log = 'error.log'
    skip_log = 'skip.log'

    process_input()

    sys.stderr = open(f"{work_path}/{err_log}", 'w')

    key_word_pattern = r'(?:' + '|'.join([r"\s*".join(keyword) for keyword in keywords]) + r')'
    print(f"key_word_pattern: {key_word_pattern}")

    regex = re.compile(key_word_pattern)
    print()

    root, dirs, files = next(os.walk(files_path))


    # 单线程版本
    # for dir_name in dirs:
    #     process_directory(dir_name)
    #
    # for file_name in files:
    #     process_single_file(files_path, file_name)
    #     print()


    # 多线程版本
    with ThreadPoolExecutor(max_workers=50) as executor:
        executor.map(process_directory, dirs)

    threads = []
    for file_name in files:
        thread = threading.Thread(target=process_single_file, args=(files_path, file_name))
        thread.start()
        threads.append(thread)
        print()

    for thread in threads:
        thread.join()

    print()
    print("程序执行完毕。")
    print(f"pdf 文件中被跳过的部分记录在 {work_path}/{skip_log} 中")
    print(f"错误信息记录在 {work_path}/{err_log} 中")
    input("按 Enter 键退出...")
