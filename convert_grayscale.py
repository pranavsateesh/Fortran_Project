import cv2 
import numpy as np
image = cv2.imread("/run/media/prnvstsh/DATA/python codes/test.jpg") 
image = cv2.resize(image,(400,400))
image = cv2.cvtColor(image,cv2.COLOR_BGR2GRAY)
cv2.imshow("looks",image)
cv2.waitKey(0)
np.savetxt("data.txt",image)


