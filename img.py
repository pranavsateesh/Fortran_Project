from PIL import Image
import numpy as np

img = Image.open('test.jpg').convert('L')  # convert image to 8-bit grayscale
print(img.size)
data = np.asarray(img)

np.savetxt("file.txt", data)

new = Image.fromarray(data, 'L')

new.save("file.png")
