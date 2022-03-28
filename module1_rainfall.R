#mean(x)
#median(x)
#sd(x) #standard deviation
#hist(x,main="Histogram of observed data")
#z<-(x-mean(x))/sd(x) ##standardized data
#hist(z,main="Histogram of observed data")
#plot(density(x),main="Density estimate of data")
#plot(ecdf(x),main="Empirical cumulative distribution function")
#alfa<-(median(x)^2)/(sd(x)^2)
#beta<-(sd(x)^2)/(median(x)^2)
################################################

#par(mfrow = c(2, 2))
#denscomp(fg)
#qqcomp(fg)
#cdfcomp(fg)
#ppcomp(fg)



#xlab="Return period (years)", 
#ylab=expression(paste("Discharge [",m^3,"/s]")),
#log="x")
##################################################


cum_3<-read.csv(file = "cumulative_3_months.csv")
Target<-"DJF"
x<-cum_3[,which(colnames(cum_3)==Target)] #extract all cumulative data in the target 3-month

#fit gamma dist. with MLE method:(fitdistrplus package)
library(fitdistrplus) ## loading package fitdistplus

#find the zeros
x.1<-data.frame(x)
num.zer<-colSums(x.1==0.0)
m <- unname(num.zer)
n <- length(x)

if (num.zer==0) {fg<-fitdist(x,"gamma",method ="mle" )$estimate #finding gamma parameters (PDF)
    
    par(mfrow = c(1, 2))

     plot(ecdf(x),col="blue",
         main="Empirical and Gamama CDF",
         xlab=c("3-month precipitation (mm)",Target) ,
         ylab="Cumulative probability")
     
     shape<-fg["shape"]
     rate<-fg["rate"]
     
     G = pgamma(x, shape, rate)
    points(x,G,pch=18)
    
    x.gamma <- seq(min(x),max(x),1)
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
         xlab=c("SPI",Target) ,
         ylab=""
    )
    G.gamma.nor <- qnorm(G.gamma)
    lines(G.gamma.nor,G.gamma,lwd=2)
    
    
} else { 
     x_nonzero <- x
     x_nonzero[x_nonzero == 0] <- NA #substituting zeros with NA
     x_nonzero<-x_nonzero[!is.na(x_nonzero)] #removing Na from a vector
  
     fg<-fitdist(x_nonzero,"gamma",method ="mle")$estimate #finding gamma parameters (PDF)

    par(mfrow = c(1, 2))
    
    plot(ecdf(x),col="blue",
         main="Empirical and Gamama CDF",
         xlab=c("3-month precipitation (mm)",Target) ,
         ylab="Cumulative probability")
    
    shape<-fg["shape"]
    rate<-fg["rate"]
    G = pgamma(x, shape, rate)
    q <- m/n
    H <- q+(1-q)*G
    points(x,H,pch=18)
    x.gamma <- seq(min(x),max(x),0.5)
    G.gamma <- pgamma(x.gamma,shape,rate)
    H.gamma <- q+(1-q)*G.gamma
    lines(x.gamma,H.gamma,lwd=2)
    legend("bottomright",legend = c("Empirical CDF","Gamma CDF"),
           col=c("blue", "black"),bty = "n",lty=0:1, pch =c(19,18), pt.cex = 1, 
           inset = c(-0.3, 0.02), cex=0.56)
    
    #standardizing the data to reach SPI
    spi<-qnorm(H)
    plot(spi,H,pch=18,
         main="Standard Normal CDF",
         ylim=c(0,1), # y axis limits
         xlab=c("SPI",Target) ,
         ylab=""
    )
    H.gamma.nor <- qnorm(H.gamma)
    lines(H.gamma.nor,H.gamma,lwd=2)
    } 

shape;rate #gamma parameters


#library(xlsx)
#write.xlsx(spi,"DJF.xlsx")
