import cv2 
import numpy as np
image = cv2.imread("test.jpg") 
image = cv2.resize(image,(400,400))
image = cv2.cvtColor(image,cv2.COLOR_BGR2GRAY)
print(image.shape)

import numpy as np
from PIL import Image

x=Image.open('test.jpg','r')
x=x.convert('L') #makes it greyscale
y=np.asarray(x.getdata(),dtype=np.float64).reshape((x.size[1],x.size[0]))

y=np.asarray(y,dtype=np.uint8) #if values still in range 0-255! 

np.savetxt("data2.txt",y)

cv2.imshow("looks",image)
cv2.waitKey(0)

print(image)

np.savetxt("data.txt",image)

np.savetxt('output.txt',image,fmt='%.2f')
img_array = np.loadtxt("data.txt")

cv2.imshow("hole",img_array)
cv2.waitKey(0)
