from PIL import Image
import numpy as np

img = Image.open('image.jpg').convert('L')  # convert image to 8-bit grayscale

data = np.asarray(img)

np.savetxt("file.txt", data)



new = Image.fromarray(data, 'L')
new.save("file.png")
