import base64
import os
import re
import threading
import time
import sys
from io import BytesIO
import json


from concurrent.futures import ThreadPoolExecutor
from pdf2image import convert_from_path
from tencentcloud.common import credential
from tencentcloud.common.profile.client_profile import ClientProfile
from tencentcloud.common.profile.http_profile import HttpProfile
from tencentcloud.ocr.v20181119 import ocr_client, models
from tencentcloud.common.exception.tencent_cloud_sdk_exception import TencentCloudSDKException


# ------------工具函数--------
def process_input():
    print("注意事项:\n"
          "1.不要在开通系统代理/vpn/翻墙软件情况下使用\n"
          "2.不要在打开pd f文件情况下使用\n"
          "3.需要在联网状态下使用")
    print()

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

    print()
    global SecretId
    global SecretKey
    print("请输入腾讯云开通过ocr识别服务账号的密钥id及key")
    SecretId = input("SecretId:")
    SecretKey = input("SecretKey:")

    while input("确认该密钥是否正确输入([y]/n):") == "n":
        SecretId = input("SecretId:")
        SecretKey = input("SecretKey:")


def process_exist(path):
    if not os.path.exists(path):
        des = f"{path} 不存在"
        print(des)
        with open(f"{work_path}/{skip_log}", "a") as f:
            f.write(f"{des}\n")
        return False
    return True


def get_new_name(file_name, match_keyword):
    file_name = file_name[:-4]
    if file_name[-len(match_keyword):] == match_keyword:
        print(f"{file_name}.pdf 已有后缀 -{match_keyword}")
        return f"{file_name}.pdf"
    return f"{file_name}-{match_keyword}.pdf"


def tencent_ocr(Base64_image):
    # 实例化一个认证对象，入参需要传入腾讯云账户 SecretId 和 SecretKey，此处还需注意密钥对的保密
    # 代码泄露可能会导致 SecretId 和 SecretKey 泄露，并威胁账号下所有资源的安全性。以下代码示例仅供参考，建议采用更安全的方式来使用密钥，请参见：https://cloud.tencent.com/document/product/1278/85305
    # 密钥可前往官网控制台 https://console.cloud.tencent.com/cam/capi 进行获取
    cred = credential.Credential(f"{SecretId}", f"{SecretKey}")

    # 实例化一个http选项，可选的，没有特殊需求可以跳过
    httpProfile = HttpProfile()
    httpProfile.endpoint = "ocr.tencentcloudapi.com"

    # 实例化一个client选项，可选的，没有特殊需求可以跳过
    clientProfile = ClientProfile()
    clientProfile.httpProfile = httpProfile

    # 实例化要请求产品的client对象,clientProfile是可选的
    client = ocr_client.OcrClient(cred, "ap-shanghai", clientProfile)

    # 实例化一个请求对象,每个接口都会对应一个request对象
    req = models.GeneralAccurateOCRRequest()
    params = {
        "ImageBase64": f"{Base64_image}",
        "ImageUrl": None,
        "IsWords": None,
        "EnableDetectSplit": True,
        "IsPdf": True,
        "PdfPageNumber": 1,
        "EnableDetectText": None
    }
    req.from_json_string(json.dumps(params))

    # 返回的resp是一个GeneralAccurateOCRResponse的实例，与请求对象对应
    while True:
        try:
            resp = client.GeneralAccurateOCR(req)
            # 输出json格式的字符串回包
            return json.loads(resp.to_json_string())

        except TencentCloudSDKException as e:
            # print("-------------------异常-------------------")
            # print(f"{e}")
            if e.code == "RequestLimitExceeded":
                # print("请求频率超限")
                time.sleep(1)
                continue  # 继续下一次循环
            else:
                print(f"{e}")
                print(f"{e}", file=sys.stderr)
                return None


# -----------功能函数----------------------
def process_tencent_ocr_res(resp):
    if resp is None:
        return ""

    print(f"腾讯 ocr api 接口 访问 id {resp['RequestId']}")

    text = ""

    for item in resp["TextDetections"]:
        text = text + item['DetectedText']

    return text


def process_text_return_keyword(file_path, page_num):
    # 提取整个 pdf 文本信息
    images = convert_from_path(f"{file_path}", first_page=1, last_page=1)
    if len(images) <= 0:
        print("出错啦, 从 pdf 识别出来的图片数为0")
        return
    image = images[0]

    # 将 PIL Image 对象转换为 Bytes
    buffered = BytesIO()
    image.save(buffered, format="PNG")  # 或者 "JPEG"

    # 获取 Base64 编码
    img_str = base64.b64encode(buffered.getvalue()).decode()

    text = process_tencent_ocr_res(tencent_ocr(img_str))

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


# 实习合同  劳动合同  巨新星专业课课表
# AKIDT0Rer7OrCRadcaVIwU5iManwzzrbFB93
# riVIzd8UwIYhBar49OvUDwjOwkS4a9eC
if __name__ == "__main__":
    work_path = ''
    files_path = ''
    keywords = []

    err_log = 'error.log'
    skip_log = 'skip.log'
    SecretId = ""
    SecretKey = ""

    process_input()

    sys.stderr = open(f"{work_path}/{err_log}", 'w')

    key_word_pattern = r'(?:' + '|'.join([r"\s*".join(keyword) for keyword in keywords]) + r')'
    print(f"key_word_pattern: {key_word_pattern}")

    regex = re.compile(key_word_pattern)
    print()

    root, dirs, files = next(os.walk(files_path))

    # for dir_name in dirs:
    #     process_directory(dir_name)

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

