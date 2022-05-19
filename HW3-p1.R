#install.packages('openxlsx')
#install.packages('rgl')

require(openxlsx) #library(openxlsx)
require(rgl)

data_path1='C:/Users/pinar/Desktop/HW3/X_TRAIN.xlsx'
x_coor=read.xlsx(data_path1,sheet='Sayfa2')

data_path2='C:/Users/pinar/Desktop/HW3/Y_TRAIN.xlsx'
y_coor=read.xlsx(data_path2,sheet='Sayfa2')

data_path3='C:/Users/pinar/Desktop/HW3/Z_TRAIN.xlsx'
z_coor=read.xlsx(data_path3,sheet='Sayfa2')


#1st row
name <- x_coor[1,1]
print(name)
x <- x_coor[1,2:ncol(x_coor)]
y <- y_coor[1,2:ncol(x_coor)]
z <- z_coor[1,2:ncol(x_coor)]

g1 <- print(plot3d(x, y, z, xlab=name, col="blue", type='l', lwd= "5"))

#2nd row
name <- x_coor[2,1]
print(name)
x <- x_coor[2,2:ncol(x_coor)]
y <- y_coor[2,2:ncol(x_coor)]
z <- z_coor[2,2:ncol(x_coor)]

g2 <- print(plot3d(x, y, z, xlab=name, col="pink", type='l', lwd= "5"))

#3rd row
name <- x_coor[3,1]
print(name)
x <- x_coor[3,2:ncol(x_coor)]
y <- y_coor[3,2:ncol(x_coor)]
z <- z_coor[3,2:ncol(x_coor)]

g3 <- print(plot3d(x, y, z, xlab=name, col="orange", type='l', lwd= "5"))

#4th row
name <- x_coor[4,1]
print(name)
x <- x_coor[4,2:ncol(x_coor)]
y <- y_coor[4,2:ncol(x_coor)]
z <- z_coor[4,2:ncol(x_coor)]

print(plot3d(x, y, z, xlab=name, col="yellow", type='l', lwd= "5"))

#5th row
name <- x_coor[5,1]
print(name)
x <- x_coor[5,2:ncol(x_coor)]
y <- y_coor[5,2:ncol(x_coor)]
z <- z_coor[5,2:ncol(x_coor)]

print(plot3d(x, y, z, xlab=name, col="red", type='l', lwd= "5"))

#6th row
name <- x_coor[6,1]
print(name)
x <- x_coor[6,2:ncol(x_coor)]
y <- y_coor[6,2:ncol(x_coor)]
z <- z_coor[6,2:ncol(x_coor)]

print(plot3d(x, y, z, xlab=name, col="green", type='l', lwd= "5"))

#7th row
name <- x_coor[7,1]
print(name)
x <- x_coor[7,2:ncol(x_coor)]
y <- y_coor[7,2:ncol(x_coor)]
z <- z_coor[7,2:ncol(x_coor)]

print(plot3d(x, y, z, xlab=name, col="grey", type='l', lwd= "5"))

#8th row
name <- x_coor[8,1]
print(name)
x <- x_coor[8,2:ncol(x_coor)]
y <- y_coor[8,2:ncol(x_coor)]
z <- z_coor[8,2:ncol(x_coor)]

print(plot3d(x, y, z, xlab=name, col="navy", type='l', lwd= "5"))
