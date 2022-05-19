require(openxlsx) #library(openxlsx)

ilmesafe='C:/Users/pinar/Desktop/ilmesafe.xlsx'
datapath=read.xlsx(ilmesafe,sheet='Sayfa1')


fit <- cmdscale(datapath,eig=TRUE, k=2)

x <- fit$points[,1]
y <- fit$points[,2]

plot(x, y,
     main="Harita", type="n")
text(x, y, labels = colnames(datapath), cex=.7)
