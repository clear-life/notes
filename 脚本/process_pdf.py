import re
import os
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import TextConverter
from pdfminer.layout import LAParams
from io import StringIO


def get_new_name(file_name, match_keyword):
    file_name = file_name[:-4]
    return f"{file_name}-{match_keyword}.pdf"


def process_text_return_keyword(file_path, page_num):
    # 提取整个 pdf 文本信息
    with StringIO() as output:
        manager = PDFResourceManager()
        converter = TextConverter(manager, output, laparams=LAParams())
        interpreter = PDFPageInterpreter(manager, converter)

        with open(file_path, 'rb') as f:
            for page in PDFPage.get_pages(f, pagenos=[page_num - 1]):  # 处理第 page_num 页
                interpreter.process_page(page)

        converter.close()
        text = output.getvalue()    # 第 page_number 页的文本内容在 text 中存储

    res = regex.search(text)

    if res:
        return ''.join(res.group().split())
    else:
        return None


def process_single_file(file_name):
    file_path = f"{files_path}/{file_name}"
    if not file_path.endswith('.pdf'):
        print(f"{file_path} 不是 pdf 文件, 跳过")
        return

    match_keyword = process_text_return_keyword(file_path, 1)
    print(f"匹配结果关键词: {match_keyword}")

    if match_keyword:
        new_name = get_new_name(file_name, match_keyword)
        print(f"{file_name} 将被重命名为", new_name)
        if input("请确认是否重命名([y]/n):") == "n":
            print(f"跳过 {file_name}")
            return

        rename = f"{files_path}/{new_name}"
        os.rename(file_path, rename)
        print(f"{file_name} 已被重命名为", new_name)
    else:
        print(f"{file_name} 并未匹配到任何一个关键词")


def process_input():
    print(f"输入工作目录, 并确保该目录下有文件夹 files, files 中含有要处理的文件\n"
          "工作目录示例: D:/目录1/目录2/workspace\n")

    global work_path
    work_path = input("请输入工作目录: ")
    while not os.path.exists(work_path) or not os.path.exists(f"{work_path}/files"):
        print(f"{work_path} 路径或 files 子目录不存在")
        work_path = input("请输入工作目录: ")

    print()
    print("输入要匹配的关键词列表, 脚本在文本中最先匹配到的某个关键词作为判定结果\n"
          "匹配规则举例: 关键词 \"甲方\" 在文本中能匹配 \"甲\"+零到多个空白符+\"方\"\n"
          "注: 空白符指 空格 制表符 换行符\n"
          "关键词示例: 关键词1 关键词2 关键词3\n")

    global keywords
    keywords = input("请输入要匹配的关键词(1~n个): ").split()
    print(f"关键词列表: {keywords}")


# D:/workspace/Python
if __name__ == "__main__":
    work_path = ''
    keywords = []

    process_input()

    files_path = f"{work_path}/files"
    key_word_pattern = r'(?:' + '|'.join([r"\s*".join(keyword) for keyword in keywords]) + r')'
    print(f"key_word_pattern: {key_word_pattern}")
    regex = re.compile(key_word_pattern)
    print()

    for root, dirs, files in os.walk(files_path):
        for file_name in files:
            print("file_name:", file_name)
            process_single_file(file_name)
            print()

    print()
    print("程序执行完毕。")
    input("按 Enter 键退出...")
