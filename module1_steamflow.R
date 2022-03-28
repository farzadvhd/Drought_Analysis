##################################################


av_3<-read.csv(file = "average_3_months.csv")
Target<-"DJF"
x<-av_3[,which(colnames(av_3)==Target)] #extract all cumulative data in the target 3-month

#fit gamma dist. with MLE method:(fitdistrplus package)
library(fitdistrplus) ## loading package fitdistplus

#as we have not any zero in our dataset we skip the procedure of finding zeros

fg<-fitdist(x,"gamma",method ="mle" )$estimate #finding gamma parameters (PDF)

par(mfrow = c(1, 2))

plot(ecdf(x),col="blue",
     main="Empirical and Gamama CDF",
     xlab=c("3-month average flow (m^3/s)",Target) ,
     ylab="Cumulative probability")

shape<-fg["shape"]
rate<-fg["rate"]

G = pgamma(x, shape, rate)
points(x,G,pch=18)

x.gamma <- seq(min(x),max(x),0.001)
G.gamma <- pgamma(x.gamma,shape,rate)
lines(x.gamma,G.gamma,lwd=2)

legend("bottomright",legend = c("Empirical CDF","Gamma CDF"),
       col=c("blue", "black"),bty = "n",lty=0:1, pch =c(19,18), pt.cex = 1, 
       inset = c(-0.3, 0.02), cex=0.56)

#standardizing the data to reach SPI
spi<-qnorm(G)
plot(spi,G,pch=18,
     main="Standard Normal CDF",
     ylim=c(0,1), # y axis limits
     xlab=c("SSI",Target) ,
     ylab=""
)
G.gamma.nor <- qnorm(G.gamma)
lines(G.gamma.nor,G.gamma,lwd=2)


shape;rate #gamma parameters

#library(xlsx)
#write.xlsx(spi,"DJF.xlsx")

