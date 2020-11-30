from PIL import Image
import numpy
im = Image.open('test.jpg').convert('LA') # img size (480,910,3)
size = (400,400)
im = im.resize(size)
print(im.size)

np_im = numpy.array(im)
numpy.savetxt("test.txt", im)

new_data = numpy.loadtxt('test.txt')

new_data=new_data.reshape((400,400,1))

img = Image.fromarray(new_data,'RGB')
img.save('my.bmp')
img.show()