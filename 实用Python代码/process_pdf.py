from pdfminer import high_level
import re
import os


def process_text(file):
    # 提取整个 pdf 文本信息
    text = high_level.extract_text(file)
    lines = text.split("\n")

    for num, line in enumerate(lines):
        print(f"{num}:", line)
        if "乙  方" in line:
            chinese_name = lines[num + 2]
            print(chinese_name)

    regex = r'企业基本情况-(.*?)\n'        # 正则表达式
    qy_base = re.findall(regex, text)   # 匹配结果
    print(f'输出信息：{qy_base}')

    return "111" + ".pdf"


def process_single_file(file_name):
    file = f"{file_path}/{file_name}"
    if not file.endswith('.pdf'):
        des = f"{file} 不是 pdf 文件"

        with open(f"{work_path}/pdf.log", "a") as f:
            f.write(f"{des}\n")

        return


    new_name = process_text(file)
    print(f"new_name:", new_name)
    rename = f"{file_path}/{new_name}"

    # os.rename(file, rename)


if __name__ == "__main__":
    work_path = 'D:/workspace/Python'
    file_path = f"{work_path}/data"

    with open(f"{work_path}/pdf.log", "w") as f:
        pass

    for root, dirs, files in os.walk(file_path):
        print("root:", root)
        for file_name in files:
            print("file_name:", file_name)
            process_single_file(file_name)