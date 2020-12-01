import matplotlib.pyplot as plt
with open('animals.txt', 'r') as f:
    lines = f.readlines()
    x = [float(line.split()[0]) for line in lines]
    y = [float(line.split()[1]) for line in lines]
    z = [float(line.split()[2]) for line in lines]
plt.plot(x ,y)
plt.plot(x,z)
plt.show()
plt.plot(y,z)
plt.show()