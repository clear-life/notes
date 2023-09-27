import base64
import os
import re
import threading
import time
import sys
from io import BytesIO


from concurrent.futures import ThreadPoolExecutor
from pdf2image import convert_from_path

import json
from tencentcloud.common import credential
from tencentcloud.common.profile.client_profile import ClientProfile
from tencentcloud.common.profile.http_profile import HttpProfile
from tencentcloud.common.exception.tencent_cloud_sdk_exception import TencentCloudSDKException
from tencentcloud.ocr.v20181119 import ocr_client, models

# ------------工具函数--------
def process_input():
    global work_path
    work_path = input("请输入工作目录: ")
    while not os.path.exists(work_path):
        print(f"{work_path} 路径不存在")
        work_path = input("请输入工作目录: ")
    print(f"工作目录: {work_path}")

    global files_path
    files_path = f"{work_path}"

    # print()
    # global SecretId
    # global SecretKey
    # print("请输入腾讯云开通过ocr识别服务账号的密钥id及key")
    # SecretId = input("SecretId:")
    # SecretKey = input("SecretKey:")
    #
    # while input("确认该密钥是否正确输入([y]/n):") == "n":
    #     SecretId = input("SecretId:")
    #     SecretKey = input("SecretKey:")


def process_exist(path):
    if not os.path.exists(path):
        des = f"{path} 不存在"
        print(des)
        return False
    return True


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
    # req = models.GeneralAccurateOCRRequest()
    req = models.GeneralBasicOCRRequest()
    params = {
        "ImageBase64": f"{Base64_image}",
        "ImageUrl": None,
        "IsWords": None,
        "EnableDetectSplit": True,
        "IsPdf": True,
        "PdfPageNumber": None,
        "EnableDetectText": None
    }
    req.from_json_string(json.dumps(params))

    # 返回的resp是一个GeneralAccurateOCRResponse的实例，与请求对象对应
    while True:
        try:
            resp = client.GeneralBasicOCR(req)
            return json.loads(resp.to_json_string())

        except TencentCloudSDKException as e:
            if e.code == "RequestLimitExceeded":
                time.sleep(1)
                continue
            else:
                print(f"{e}")
                print(f"{e}", file=sys.stderr)

                for i in range(5):
                    try:
                        resp = client.GeneralBasicOCR(req)
                        return json.loads(resp.to_json_string())
                    except TencentCloudSDKException as e:
                        time.sleep(0.1)
                        continue
                return None


# -----------功能函数----------------------
def process_tencent_ocr_res(resp):
    if resp is None:
        return ""

    text = ""

    for item in resp["TextDetections"]:
        text = text + item['DetectedText'] + "\n"

    return text


def get_text(file_path):
    # 提取整个 pdf 文本信息
    images = convert_from_path(f"{file_path}")
    if len(images) <= 0:
        return ""

    text = ""

    for i, image in enumerate(images):
        if 1 < i < len(images) - 2:
            continue
        # 将 PIL Image 对象转换为 Bytes
        buffered = BytesIO()
        image.save(buffered, format="PNG")  # 或者 "JPEG"
        # 获取 Base64 编码列表
        base64_obj = base64.b64encode(buffered.getvalue()).decode()
        text = text + process_tencent_ocr_res(tencent_ocr(base64_obj)) + "\n\n\n"

    # print("---------------文本识别结果-------------")
    # print(text)
    # print("-------------文本识别结果分隔符------------")

    return text


def process_single_file(file_path):
    root = os.path.dirname(file_path)
    file_name = os.path.basename(file_path)
    print("file_name:", file_name)

    if not file_path.endswith('.pdf'):
        print(f"{file_path} 不是 pdf 文件, 跳过")
        return

    if not process_exist(file_path):
        return

    text = get_text(file_path)

    print(f"text type {type(text)}")
    print(f"text len {len(text)}")

    name, extension = os.path.splitext(file_name)

    new_name = name + ".txt"
    with open(os.path.join(root, new_name), "w", encoding="utf-8") as f:
        f.write(text)


def walk_directory(path):
    for root, dirs, files in os.walk(path):
        files_path = []
        for i, file in enumerate(files):
            files_path.append(os.path.join(root, file))
        with ThreadPoolExecutor(max_workers=10) as executor:
            executor.map(process_single_file, files_path)
        # for file_path in files_path:
        #     process_single_file(file_path)


if __name__ == "__main__":
    work_path = ''
    files_path = ''

    err_log = 'error.log'
    SecretId = "AKID9B4kGOXYUQXbYL74tXBj8LVZedRCC8xf"
    SecretKey = "f1IVi0DKMJ9rQnEdt4Ii2Id8icTBmIh0"

    process_input()

    sys.stderr = open(f"{work_path}/{err_log}", 'w')

    walk_directory(files_path)

    print()
    print("程序执行完毕。")
    input("按 Enter 键退出...")
