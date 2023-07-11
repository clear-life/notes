import PIL.Image as Image

img = Image.open('z.png').convert('RGBA')

n, m = img.size

for i in range(n):
    for j in range(m):
        t = img.getpixel((i, j))
        if t[0] > 180 and t[1] > 180 and t[2] > 180 and t[3] > 180:
            img.putpixel((i, j), (0, 0, 0, 0))   # 设置透明

img.save('test.png')