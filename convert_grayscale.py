import cv2 
import numpy as np
image = cv2.imread("index.jpg") 
image = cv2.resize(image,(100,100))
image = cv2.cvtColor(image,cv2.COLOR_BGR2GRAY)
print(image.shape)




