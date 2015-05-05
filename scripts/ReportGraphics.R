#report graphs

library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)

#catch and stock weights

dfSW <- read.table(file = paste(FPRESS.Home(),".\\indata\\WHM_StockWeights.dat",sep=""),
                   header = FALSE,
                   sep = ",",
                   col.names = c("Year",seq(0,11,by=1)))

dfCW <- read.table(file = paste(FPRESS.Home(),".\\indata\\WHM_CatchWeights.dat",sep=""),
                   header = FALSE,
                   sep = ",",
                   col.names = c("Year",seq(0,11,by=1)))

dfMat <- read.table(file = paste(FPRESS.Home(),".\\indata\\WHM_Maturity.dat",sep=""),
                   header = FALSE,
                   sep = ",",
                   col.names = c("Year",seq(0,11,by=1)))

#melted <- melt(dfSW,id.vars="Year")

#ggplot(data=melted, aes(x=Year, y=value, group=variable)) + geom_line()

#labels every 5 years
lbl <- dfSW$Year
lbl[-which(lbl%%5==0)]<-NA

ygrid <- function(lbl){lbl[!is.na(lbl)]}
xgrid <- function(){c(0.1,0.2,0.3,0.4)}
ptsym <- 16
ptsymcex <- 1.0
mtextcex <- 1.0
lbltextcex <- 1.2

jpeg(filename=".//plots//StockWeights.jpg",
     width=757, height=635, quality=100)

layout(matrix(seq(1,12),nrow=3,ncol=4,byrow=TRUE))
par(omi=c(1.0,1.0,0,0))
#bottom,left,top,right
par(mar=c(0,0,0,0)+0.2)

#top row

plot(dfSW$Year,dfSW$X0,type="n",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X0,lty=1)
points(dfSW$Year,dfSW$X0,pch=ptsym,cex=ptsymcex)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 0",pos=4, cex=lbltextcex)
#axis(1,at=dfSW$Year,labels=FALSE)
axis(2,at=seq(0,ceiling(10*max(dfSW[,-1]))/10,by=0.1),cex.axis=lbltextcex,las=2)
mtext("Weight (kg)",side=2,line=3,cex=mtextcex)

plot(dfSW$Year,dfSW$X1,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X1,lty=1)
points(dfSW$Year,dfSW$X1,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfSW$Year,labels=FALSE)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 1",pos=4, cex=lbltextcex)

plot(dfSW$Year,dfSW$X2,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X2,lty=1)
points(dfSW$Year,dfSW$X2,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfSW$Year,labels=FALSE)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 2",pos=4, cex=lbltextcex)

plot(dfSW$Year,dfSW$X3,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X3,lty=1)
points(dfSW$Year,dfSW$X3,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfSW$Year,labels=FALSE)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 3",pos=4, cex=lbltextcex)

#middle row

plot(dfSW$Year,dfSW$X4,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X4,lty=1)
points(dfSW$Year,dfSW$X4,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfSW$Year,labels=FALSE)
axis(2,at=seq(0,ceiling(10*max(dfSW[,-1]))/10,by=0.1),cex.axis=lbltextcex,las=2)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 4",pos=4, cex=lbltextcex)
mtext("Weight (kg)",side=2,line=3,cex=mtextcex)

plot(dfSW$Year,dfSW$X5,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X5,lty=1)
points(dfSW$Year,dfSW$X5,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfSW$Year,labels=FALSE)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 5",pos=4, cex=lbltextcex)

plot(dfSW$Year,dfSW$X6,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X6,lty=1)
points(dfSW$Year,dfSW$X6,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfSW$Year,labels=FALSE)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 6",pos=4, cex=lbltextcex)

plot(dfSW$Year,dfSW$X7,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X7,lty=1)
points(dfSW$Year,dfSW$X7,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfSW$Year,labels=FALSE)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 7",pos=4, cex=lbltextcex)

#bottom row

plot(dfSW$Year,dfSW$X8,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X8,lty=1)
points(dfSW$Year,dfSW$X8,pch=ptsym,cex=ptsymcex)
axis(1,at=dfSW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
axis(2,at=seq(0,ceiling(10*max(dfSW[,-1]))/10,by=0.1),cex.axis=lbltextcex,las=2)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 8",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)
mtext("Weight (kg)",side=2,line=3,cex=mtextcex)

plot(dfSW$Year,dfSW$X9,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X9,lty=1)
points(dfSW$Year,dfSW$X9,pch=ptsym,cex=ptsymcex)
axis(1,at=dfSW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 9",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

plot(dfSW$Year,dfSW$X10,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X10,lty=1)
points(dfSW$Year,dfSW$X10,pch=ptsym,cex=ptsymcex)
axis(1,at=dfSW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 10",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

plot(dfSW$Year,dfSW$X11,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfSW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfSW$Year,dfSW$X11,lty=1)
points(dfSW$Year,dfSW$X11,pch=ptsym,cex=ptsymcex)
axis(1,at=dfSW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfSW$Year[1],rev(xgrid())[1],labels = "Age 11+",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

dev.off()








#Catch Weights
#labels every 5 years
lbl <- dfCW$Year
lbl[-which(lbl%%5==0)]<-NA

ygrid <- function(lbl){lbl[!is.na(lbl)]}
xgrid <- function(){c(0.1,0.2,0.3,0.4,0.5)}
ptsym=16
ptsymcex <- 1.0
mtextcex <- 1.0
lbltextcex <- 1.2

jpeg(filename=".//plots//CatchWeights.jpg",
     width=757, height=635, quality=100)

layout(matrix(seq(1,12),nrow=3,ncol=4,byrow=TRUE))
par(omi=c(1.0,1.0,0,0))
#bottom,left,top,right
par(mar=c(0,0,0,0)+0.2)

#top row

plot(dfCW$Year,dfCW$X0,type="n",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X0,lty=1)
points(dfCW$Year,dfCW$X0,pch=ptsym,cex=ptsymcex)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 0",pos=4, cex=lbltextcex)
#axis(1,at=dfCW$Year,labels=FALSE)
axis(2,at=seq(0,ceiling(10*max(dfCW[,-1]))/10,by=0.1),las=2,cex.axis=lbltextcex)
mtext("Weight (kg)",side=2,line=3,cex=mtextcex)

plot(dfCW$Year,dfCW$X1,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X1,lty=1)
points(dfCW$Year,dfCW$X1,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfCW$Year,labels=FALSE)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 1",pos=4, cex=lbltextcex)

plot(dfCW$Year,dfCW$X2,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X2,lty=1)
points(dfCW$Year,dfCW$X2,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfCW$Year,labels=FALSE)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 2",pos=4, cex=lbltextcex)

plot(dfCW$Year,dfCW$X3,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X3,lty=1)
points(dfCW$Year,dfCW$X3,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfCW$Year,labels=FALSE)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 3",pos=4, cex=lbltextcex)

#middle row

plot(dfCW$Year,dfCW$X4,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X4,lty=1)
points(dfCW$Year,dfCW$X4,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfCW$Year,labels=FALSE)
axis(2,at=seq(0,ceiling(10*max(dfCW[,-1]))/10,by=0.1),las=2,cex.axis=lbltextcex)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 4",pos=4, cex=lbltextcex)
mtext("Weight (kg)",side=2,line=3,cex=mtextcex)

plot(dfCW$Year,dfCW$X5,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X5,lty=1)
points(dfCW$Year,dfCW$X5,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfCW$Year,labels=FALSE)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 5",pos=4, cex=lbltextcex)

plot(dfCW$Year,dfCW$X6,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X6,lty=1)
points(dfCW$Year,dfCW$X6,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfCW$Year,labels=FALSE)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 6",pos=4, cex=lbltextcex)

plot(dfCW$Year,dfCW$X7,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X7,lty=1)
points(dfCW$Year,dfCW$X7,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfCW$Year,labels=FALSE)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 7",pos=4, cex=lbltextcex)

#bottom row

plot(dfCW$Year,dfCW$X8,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X8,lty=1)
points(dfCW$Year,dfCW$X8,pch=ptsym,cex=ptsymcex)
axis(1,at=dfCW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
axis(2,at=seq(0,ceiling(10*max(dfCW[,-1]))/10,by=0.1),las=2,cex.axis=lbltextcex)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 8",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)
mtext("Weight (kg)",side=2,line=3,cex=mtextcex)

plot(dfCW$Year,dfCW$X9,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X9,lty=1)
points(dfCW$Year,dfCW$X9,pch=ptsym,cex=ptsymcex)
axis(1,at=dfCW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 9",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

plot(dfCW$Year,dfCW$X10,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X10,lty=1)
points(dfCW$Year,dfCW$X10,pch=ptsym,cex=ptsymcex)
axis(1,at=dfCW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 10",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

plot(dfCW$Year,dfCW$X11,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfCW[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfCW$Year,dfCW$X11,lty=1)
points(dfCW$Year,dfCW$X11,pch=ptsym,cex=ptsymcex)
axis(1,at=dfCW$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfCW$Year[1],rev(xgrid())[1],labels = "Age 11+",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

dev.off()

#Maturity
#labels every 5 years
lbl <- dfMat$Year
lbl[-which(lbl%%5==0)]<-NA

ygrid <- function(lbl){lbl[!is.na(lbl)]}
xgrid <- function(){c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)}
ptsym=16
ptsymcex <- 1.0
mtextcex <- 1.0
lbltextcex <- 1.2

jpeg(filename=".//plots//Maturities.jpg",
     width=757, height=635, quality=100)

layout(matrix(seq(1,12),nrow=3,ncol=4,byrow=TRUE))
par(omi=c(1.0,1.0,0,0))
#bottom,left,top,right
par(mar=c(0,0,0,0)+0.2)

#top row

plot(dfMat$Year,dfMat$X0,type="n",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X0,lty=1)
points(dfMat$Year,dfMat$X0,pch=ptsym,cex=ptsymcex)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 0",pos=4, cex=lbltextcex)
#axis(1,at=dfMat$Year,labels=FALSE)
axis(2,at=seq(0,ceiling(10*max(dfMat[,-1]))/10,by=0.2),las=2,cex.axis=lbltextcex)
mtext("Maturity",side=2,line=3,cex=mtextcex)

plot(dfMat$Year,dfMat$X1,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X1,lty=1)
points(dfMat$Year,dfMat$X1,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfMat$Year,labels=FALSE)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 1",pos=4, cex=lbltextcex)

plot(dfMat$Year,dfMat$X2,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X2,lty=1)
points(dfMat$Year,dfMat$X2,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfMat$Year,labels=FALSE)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 2",pos=4, cex=lbltextcex)

plot(dfMat$Year,dfMat$X3,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X3,lty=1)
points(dfMat$Year,dfMat$X3,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfMat$Year,labels=FALSE)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 3",pos=4, cex=lbltextcex)

#middle row

plot(dfMat$Year,dfMat$X4,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X4,lty=1)
points(dfMat$Year,dfMat$X4,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfMat$Year,labels=FALSE)
axis(2,at=seq(0,ceiling(10*max(dfMat[,-1]))/10,by=0.2),las=2,cex.axis=lbltextcex)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 4",pos=4, cex=lbltextcex)
mtext("Maturity",side=2,line=3,cex=mtextcex)

plot(dfMat$Year,dfMat$X5,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X5,lty=1)
points(dfMat$Year,dfMat$X5,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfMat$Year,labels=FALSE)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 5",pos=4, cex=lbltextcex)

plot(dfMat$Year,dfMat$X6,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X6,lty=1)
points(dfMat$Year,dfMat$X6,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfMat$Year,labels=FALSE)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 6",pos=4, cex=lbltextcex)

plot(dfMat$Year,dfMat$X7,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X7,lty=1)
points(dfMat$Year,dfMat$X7,pch=ptsym,cex=ptsymcex)
#axis(1,at=dfMat$Year,labels=FALSE)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 7",pos=4, cex=lbltextcex)

#bottom row

plot(dfMat$Year,dfMat$X8,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X8,lty=1)
points(dfMat$Year,dfMat$X8,pch=ptsym,cex=ptsymcex)
axis(1,at=dfMat$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
axis(2,at=seq(0,ceiling(10*max(dfMat[,-1]))/10,by=0.2),las=2,cex.axis=lbltextcex)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 8",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)
mtext("Maturity",side=2,line=3,cex=mtextcex)

plot(dfMat$Year,dfMat$X9,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X9,lty=1)
points(dfMat$Year,dfMat$X9,pch=ptsym,cex=ptsymcex)
axis(1,at=dfMat$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 9",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

plot(dfMat$Year,dfMat$X10,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X10,lty=1)
points(dfMat$Year,dfMat$X10,pch=ptsym,cex=ptsymcex)
axis(1,at=dfMat$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 10",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

plot(dfMat$Year,dfMat$X11,type="l",xlab="",ylab="",ylim=c(0,ceiling(10*max(dfMat[,-1]))/10),axes=F)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray93",border=NA)
abline(h=xgrid(),lty=1,col="white")
abline(v=ygrid(lbl),lty=1,col="white")
lines(dfMat$Year,dfMat$X11,lty=1)
points(dfMat$Year,dfMat$X11,pch=ptsym,cex=ptsymcex)
axis(1,at=dfMat$Year,labels=substr(lbl,3,4),las=2,cex.axis=lbltextcex)
text(dfMat$Year[24],rev(xgrid())[3],labels = "Age 11+",pos=4, cex=lbltextcex)
mtext("Year",side=1,line=3,cex=mtextcex)

dev.off()


#statistical outputs
#comparison of long term simulations

spikes = TRUE

#no recruitment spikes
Jose <- read.csv(".\\..\\Jose\\24_09_2014\\YvsF_0_1_1_0_2.csv",header=TRUE)
Andy <- read.csv(".\\stats\\MSE2014_SRSAD99_1scor_0spikes_1sigR.dat",sep="\t",header=TRUE)
title <- ""
maxR <- 3500
maxY <- 80
maxSSB <- 3.0

if (spikes){
  #including spikes
  Jose <- read.csv(".\\..\\Jose\\24_09_2014\\YvsF_1_1_1_0_2.csv",header=TRUE)
  Andy <- read.csv(".\\stats\\MSE2014_SRSAD99_1scor_1spikes_1sigR.dat",sep="\t",header=TRUE)
  title <- ""
  maxR <- 8000
  maxY <- 220
  maxSSB <- 6.0  
}


layout(matrix(seq(1,6),nrow=2,ncol=2,byrow=TRUE))
par(omi=c(1.0,1.0,0,0))
#bottom,left,top,right
par(mar=c(2,0,0,2)+0.2)

#recruitment comparison
plot(Jose$F,Jose$Rmn/1e3,xlab="F",ylab="Recr",main=title,ylim=c(0,maxR),
     lty=1,type="l",axes=FALSE)
lines(Andy$FBar,Andy$Rec,col="purple")
axis(1,labels=FALSE)
axis(2)
text(0,maxR,"MeanR",pos=4)

#risk comparison
plot(Jose$F,Jose$risk1,xlab="F",ylab="Risk",main=title,ylim=c(0,100),
     lty=1,type="l",axes=FALSE)

lines(Jose$F,Jose$risk2,lty=2)
lines(Jose$F,Jose$risk3,lty=3)

lines(Andy$FBar,Andy$Rsk1_ST,lty=1,col="purple")
lines(Andy$FBar,Andy$Rsk2_ST,lty=2,col="purple")
lines(Andy$FBar,Andy$Rsk3_ST,lty=3,col="purple")
legend(0.125,40,legend=c("Type 1","Type 2","Type 3"),lty=c(1,2,3),bty="n")
text(0,100,"Risk",pos=4)
axis(1,labels=FALSE)
axis(2)

#yield comparison
plot(Jose$F,Jose$Ymn/1e3,xlab="F",ylab="Yield",main=title,
     ylim=c(0,maxY),lty=1,type="l",axes=FALSE)

#lines(J.nospike$F,J.nospike$Y10/1e3,lty=2)
lines(Jose$F,Jose$Y50/1e3,lwd=2)
#lines(J.nospike$F,J.nospike$Y90/1e3,lty=2)

lines(Andy$FBar,Andy$Mean.Yld,col="purple")
#lines(A.nospike$FBar,A.nospike$Yld0.1,col="purple",lty=2)
lines(Andy$FBar,Andy$Yld0.5,col="purple",lwd=2)
#lines(A.nospike$FBar,A.nospike$Yld0.9,col="purple",lty=2)
text(0,maxY,"Yield",pos=4)
axis(1)
axis(2)

#SSB at spawning time, no spikes
plot(Jose$F,Jose$SSBmn/1e6,xlab="F",ylab="SSB",main=title,ylim=c(0,maxSSB),
     lty=1,type="l",axes=FALSE)
lines(Jose$F,Jose$SSB10/1e6,col="black",lty=2)
lines(Jose$F,Jose$SSB50/1e6,col="black",lty=1,lwd=2)
lines(Jose$F,Jose$SSB90/1e6,col="black",lty=2)

lines(Andy$FBar,Andy$Mean.SSBST,col="purple")
lines(Andy$FBar,Andy$SSBST_0.1,col="purple",lty=2)
lines(Andy$FBar,Andy$SSBST_0.5,col="purple",lty=1,lwd=2)
lines(Andy$FBar,Andy$SSBST_0.9,col="purple",lty=2)
text(0,maxSSB,"SSB (ST)",pos=4)
axis(1)
axis(2)





#plots of risk vs time for long term simulations
#selected target fishing mortality values (0.0,0.025,0.05,0.075)
df100 <- read.csv(".\\stats\\MSE2014_SRSAD99_1scor_0spikes_1sigR_100_ann.dat",sep="\t",header=TRUE)
df100$iter <- 100
df250 <- read.csv(".\\stats\\MSE2014_SRSAD99_1scor_0spikes_1sigR_250_ann.dat",sep="\t",header=TRUE)
df250$iter <- 250
df500 <- read.csv(".\\stats\\MSE2014_SRSAD99_1scor_0spikes_1sigR_500_ann.dat",sep="\t",header=TRUE)
df500$iter <- 500
df1000 <- read.csv(".\\stats\\MSE2014_SRSAD99_1scor_0spikes_1sigR_1000_ann.dat",sep="\t",header=TRUE)
df1000$iter <- 1000

dfAll <- rbind_all(list(df100,df250,df500,df1000))
df05 <- filter(filter(dfAll,FBar==0.05),From==To)

layout(matrix(seq(1,6),nrow=2,ncol=2,byrow=TRUE))

par(omi=c(1.0,1.0,0,0))
#bottom,left,top,right
par(mar=c(0,0,0,0)+0.2)

plot(df05[df05$iter==100,]$From,df05[df05$iter==100,]$Rsk3_ST,ylim=c(0,100),type="l")
points(df05[df05$iter==100,]$From,df05[df05$iter==100,]$Rsk3_ST,pch=20)
lines(df05[df05$iter==100,]$From,df05[df05$iter==100,]$Rsk1_ST)
points(df05[df05$iter==100,]$From,df05[df05$iter==100,]$Rsk1_ST,pch=20)
lines(df05[df05$iter==100,]$From,df05[df05$iter==100,]$Rsk2_ST)
points(df05[df05$iter==100,]$From,df05[df05$iter==100,]$Rsk2_ST,pch=20)

plot(df05[df05$iter==250,]$From,df05[df05$iter==250,]$Rsk3_ST,ylim=c(0,100),type="l")
points(df05[df05$iter==250,]$From,df05[df05$iter==250,]$Rsk3_ST,pch=20)
lines(df05[df05$iter==250,]$From,df05[df05$iter==250,]$Rsk1_ST)
points(df05[df05$iter==250,]$From,df05[df05$iter==250,]$Rsk1_ST,pch=20)
lines(df05[df05$iter==250,]$From,df05[df05$iter==250,]$Rsk2_ST)
points(df05[df05$iter==250,]$From,df05[df05$iter==250,]$Rsk2_ST,pch=20)

plot(df05[df05$iter==500,]$From,df05[df05$iter==500,]$Rsk3_ST,ylim=c(0,100),type="l")
points(df05[df05$iter==500,]$From,df05[df05$iter==500,]$Rsk3_ST,pch=20)
lines(df05[df05$iter==500,]$From,df05[df05$iter==500,]$Rsk1_ST)
points(df05[df05$iter==500,]$From,df05[df05$iter==500,]$Rsk1_ST,pch=20)
lines(df05[df05$iter==500,]$From,df05[df05$iter==500,]$Rsk2_ST)
points(df05[df05$iter==500,]$From,df05[df05$iter==500,]$Rsk2_ST,pch=20)

plot(df05[df05$iter==1000,]$From,df05[df05$iter==1000,]$Rsk3_ST,ylim=c(0,100),type="l")
points(df05[df05$iter==1000,]$From,df05[df05$iter==1000,]$Rsk3_ST,pch=20)
lines(df05[df05$iter==1000,]$From,df05[df05$iter==1000,]$Rsk1_ST)
points(df05[df05$iter==1000,]$From,df05[df05$iter==1000,]$Rsk1_ST,pch=20)
lines(df05[df05$iter==1000,]$From,df05[df05$iter==1000,]$Rsk2_ST)
points(df05[df05$iter==1000,]$From,df05[df05$iter==1000,]$Rsk2_ST,pch=20)


#
#egg slope multiplier schematic
jpeg(filename = ".//plots//SlopeMultiplier.jpg", width = 788, height = 549, quality = 100)
plot(x = c(-1.5,0,0.5,1.5), y = c(0,1,1.4,1.4), type = "l", axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-1.5,-1.0,-0.5,0,0.5,1.0,1.5), cex.axis = 2)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4), pos = 0, las = 2, cex.axis = 2)
mtext("slope", side = 1, line = 3, cex = 2)
mtext("sl", side = 2, line = 1, cex = 2)
dev.off()

#protection rule
E <- seq(1:100)
Elim <- 80
prot<-function(E,Elim,gamma){
  
  ret<-rep(0,length(E))
  ret<-(E/Elim)^gamma
  ret[E>=Elim]<-1
  ret
}

old.par <- par("mar")
jpeg(filename = ".//plots//ProtectionMultiplier.jpg", width = 788, height = 549, quality=100)
par(mar=c(5.1,6.1,4.1,2.1))
plot(E, prot(E,Elim,1), type = "l", axes = FALSE, xlab = "", ylab = "", lwd = 2)
lines(E,prot(E,Elim,2.0),lty=2,lwd=2)
lines(E,prot(E,Elim,3.0),lty=3,lwd=2)
legend(0,1,legend=c(expression(paste(gamma," = 1")),expression(paste(gamma," = 2")),expression(paste(gamma," = 3"))),lty=c(1,2,3),bty="n",cex=2)
axis(1,at=c(0,20,40,60,80,100),labels=c("0","","","",expression("Egg"[lim]),""),cex.axis=2)
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.0),las=2,cex.axis=2)
mtext("Egg Count",side=1,line=3,cex=2)
mtext("TAC Multiplier",side=2,line=4,cex=2)
par(mar=old.par)
dev.off()


#catch at age - circle area proportional to number 
dfcaa <- read.table(file = paste(fFPRESS_home(),".\\indata\\WHM_CatchAtAge.dat",sep=""),
                    header = FALSE,sep = ",",col.names = c("Year",paste("Age",seq(0,11,by=1),sep="")))
jpeg(filename=".//plots//CAA.jpg",
     width=788, height=549, quality=100)
plot(NA, xlim = c(1982,2013), ylim = c(0,12), type = "n", xlab = "Year", ylab = "Age", axes=FALSE, cex.lab=1.5)
symbols(rep(dfcaa$Year,ncol(dfcaa)-1)[select(dfcaa,contains("Age"))>0],
        rep(seq(0,11),each=nrow(dfcaa))[select(dfcaa,contains("Age"))>0],
        circles=sqrt(unlist(select(dfcaa,contains("Age")))[select(dfcaa,contains("Age"))>0])/pi,inches=0.3,fg="black",bg="grey",add=TRUE)
axis(1, at = dfcaa$Year, cex.axis = 1.5)
axis(2, at = seq(0,11), labels=c("0","1","2","3","4","5","6","7","8","9","10","11+"),las=2, cex.axis = 1.5)
dev.off()


#egg production curves
dfEggProd <- read.table(file = paste(fFPRESS_home(),".\\indata\\WHM_EggProd.dat",sep=""),
                    header = TRUE,sep = ",")

jpeg(filename=".//plots//EggProd.jpg",width=788, height=549, quality=100)

plot(NA, axes=F, xlab="Julian Day", ylab="Egg Production x10^12", xlim=c(40,220), ylim=c(0,25), cex.lab=1.5)
axis(1, at = seq(40,220,by=20), cex.axis = 1.5)
axis(2, at = seq(0,25,by=5), cex.axis = 1.5)

lapply(unique(dfEggProd$Year),
       function(x){
         with(filter(dfEggProd,Year==x),{
           points(MidJulian,DailyEst,pch=which(unique(dfEggProd$Year)==x),cex=1.5)
           lines(MidJulian,DailyEst,lty=which(unique(dfEggProd$Year)==x),lwd=2) 
         })
       })
legend("topleft",legend=unique(dfEggProd$Year),bty="n",pch=seq(1,length(unique(dfEggProd$Year))),
       lty=seq(1,length(unique(dfEggProd$Year))),lwd=2, cex=1.5)

dev.off()




tblinit <- tbl_df(dfinit)
#take a quick look
glimpse(tblinit)
#full dataset in a grid..
View(tblinit)




plot(dfinit$aFec,dfinit$bFec,type="p",xlab="aFec parameter",ylab="bFec parameter")
lmFec <- lm(bFec~aFec,data=dfinit)
summary(lmFec)
#correlation between aFec and bFec
with(dfinit,cor(bFec,aFec))

#risk vs yield

#read in all stat outputs
opt_files <- c("MSE2014_310","MSE2014_311","MSE2014_312","MSE2014_313","MSE2014_314",
               "MSE2014_330","MSE2014_331","MSE2014_332","MSE2014_333","MSE2014_334",
               "MSE2014_340","MSE2014_341","MSE2014_342","MSE2014_343","MSE2014_344",
               "MSE2014_350","MSE2014_351","MSE2014_352","MSE2014_353","MSE2014_354",
               "MSE2014_360","MSE2014_361","MSE2014_362","MSE2014_363","MSE2014_364",
               "MSE2014_370","MSE2014_371","MSE2014_372","MSE2014_373","MSE2014_374",
               "MSE2014_380","MSE2014_381","MSE2014_382","MSE2014_383","MSE2014_384",
               "MSE2014_390","MSE2014_391","MSE2014_392","MSE2014_393","MSE2014_394",
               "MSE2014_400","MSE2014_401","MSE2014_402","MSE2014_403","MSE2014_404")

opt_files <- paste0(opt_files,"_Final")

#read in the statistics for each run
lop<-lapply(opt_files,
            function(x){
              read.table(file = paste0(fFPRESS_home(),".\\stats\\",x,".dat"),header=TRUE,sep="\t")
              })

#merge the results into a single data frame (may take a few seconds)
df = Reduce(function(...) merge(..., all=T), lop)

#only interested in stat periods (i.e. ignore annual stats)
df <- filter(df,!From==To)

#LT (from=2034)
df <- filter(df,From==2034)

jpeg(filename=".//plots//RskVsYld.jpg",width=549, height=788, quality=100)

ptsymcex <- 2.0
lbltextcex <- 2.0

layout(matrix(seq(1,3),nrow=3,ncol=1,byrow=TRUE))
par(omi=c(2.0,2.0,0,0))
#bottom,left,top,right
par(mar=c(0,0,0,0)+0.2)

lapply(seq(1,3),function(x){
  
  plot(NA,xlim=c(0,175),ylim=c(0,100),axes=FALSE)
  
  if (x==3){
    axis(1,at=c(0,25,50,75,100,125,150),cex.axis=lbltextcex)
    mtext("Yield (kt)",side=1,line=4,cex=1.5)
  }
  axis(2,at=c(0,20,40,60,80,100),cex.axis=lbltextcex,las=2)
  mtext("Risk",side=2,line=4,cex=1.5)
  
  with(filter(df,TACRef==200000 & EggLim==0 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=15,col="red",cex=ptsymcex)) #square
  with(filter(df,TACRef==200000 & EggLim==500 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=16,col="red",cex=ptsymcex)) #circle
  with(filter(df,TACRef==200000 & EggLim==1000 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=17,col="red",cex=ptsymcex)) #triangle
  with(filter(df,TACRef==200000 & EggLim==1500 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=6,col="red",cex=ptsymcex)) #inverted triangle
  with(filter(df,TACRef==200000 & EggLim==2000 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=13,col="red",cex=ptsymcex)) #inverted triangle
  
  with(filter(df,TACRef==150000 & EggLim==0 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=15,col="blue",cex=ptsymcex)) #square
  with(filter(df,TACRef==150000 & EggLim==500 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=16,col="blue",cex=ptsymcex)) #circle
  with(filter(df,TACRef==150000 & EggLim==1000 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=17,col="blue",cex=ptsymcex)) #triangle
  with(filter(df,TACRef==150000 & EggLim==1500 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=6,col="blue",cex=ptsymcex)) #inverted triangle
  with(filter(df,TACRef==150000 & EggLim==2000 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=13,col="blue",cex=ptsymcex)) #inverted triangle
  
  with(filter(df,TACRef==100000 & EggLim==0 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=15,col="green",cex=ptsymcex)) #square
  with(filter(df,TACRef==100000 & EggLim==500 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=16,col="green",cex=ptsymcex)) #circle
  with(filter(df,TACRef==100000 & EggLim==1000 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=17,col="green",cex=ptsymcex)) #triangle
  with(filter(df,TACRef==100000 & EggLim==1500 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=6,col="green",cex=ptsymcex)) #inverted triangle
  with(filter(df,TACRef==100000 & EggLim==2000 & EggGamma==x),points(Yld0.5,Rsk1_J1,pch=13,col="green",cex=ptsymcex)) #inverted triangle
  
  if (x==1) {
    legend("topright",legend=c("200kt","150kt","100kt"),title=expression('TAC'[ref]),lty=1,lwd=5,col=c("red","blue","green"),bty="n",cex=2,seg.len=0.5)
    text(0,100,expression(paste(gamma,"=1")),cex=2)
  }

  if (x==2) {
    legend("topright",legend=c("0","500","1000","1500","2000"),title=expression('Egg'[lim]),pch=c(15,16,17,6,13),cex=2,bty="n")
    text(0,100,expression(paste(gamma,"=2")),cex=2)
  }
  
  if (x==3) {
    text(0,100,expression(paste(gamma,"=3")),cex=2)
  }
  
  #grid lines
  lapply(c(20,40,60,80),function(y){
    lines(c(0,150),c(y,y),col="grey")  
  })
  
  #grid lines
  lapply(c(25,50,75,100,125),function(y){
    lines(c(y,y),c(0,80),col="grey")  
  })
  
  
})

dev.off()


#20/04/2015
#varying number of iterations. Box plots of selected statistical outputs
#for long term runs i.e. 50 year stats

#100, 250, 500, 750 iterations (use new readr read_tsv function to read in data)
df100 <- read_tsv(file = ".//stats//MSE2014_SRSAD11_1scor_1spikes_1sigR_lowres_100.dat")
df100$iters <- 100
df250 <- read_tsv(file = ".//stats//MSE2014_SRSAD11_1scor_1spikes_1sigR_lowres_250.dat")
df250$iters <- 250
df500 <- read_tsv(file = ".//stats//MSE2014_SRSAD11_1scor_1spikes_1sigR_lowres_500.dat")
df500$iters <- 500
df750 <- read_tsv(file = ".//stats//MSE2014_SRSAD11_1scor_1spikes_1sigR_lowres_750.dat")
df750$iters <- 750
df1000 <- read_tsv(file = ".//stats//MSE2014_SRSAD11_1scor_1spikes_1sigR_lowres_1000.dat")
df1000$iters <- 1000

df <- bind_rows(df100, df250, df500, df750, df1000)


jpeg(filename=".//plots//IterationsComp.jpg", width=800, height=1200, quality=100)

layout(matrix(seq(1,6),nrow=3,ncol=2,byrow=TRUE))

#outer margins
par(omi=c(0.0,0.0,1.0,0))
#inner
par(mai = c(1.0,1.0,0.5,0))

boxplot(SSBST_0.5~iters, data = filter(df,FTAC==0.05), xlab = "iterations", ylab = "Median SSB (Mt)", main = "F = 0.05", pars = list(cex.axis = 2, cex.lab = 2, cex.main = 2))
boxplot(SSBST_0.5~iters, data = filter(df,FTAC==0.1), xlab = "iterations", ylab = "Median SSB (Mt)", main = "F = 0.1", pars = list(cex.axis = 2, cex.lab = 2, cex.main = 2))

boxplot(Yld0.5~iters, data = filter(df,FTAC==0.05), xlab = "iterations", ylab = "Median Yield (kt)", pars = list(cex.axis = 2, cex.lab = 2))
boxplot(Yld0.5~iters, data = filter(df,FTAC==0.1), xlab = "iterations", ylab = "Median Yield (kt)", pars = list(cex.axis = 2, cex.lab = 2))

boxplot(Rsk1_ST~iters, data = filter(df,FTAC==0.05), xlab = "iterations", ylab = "Risk Type 1", pars = list(cex.axis = 2, cex.lab = 2))
boxplot(Rsk1_ST~iters, data = filter(df,FTAC==0.1), xlab = "iterations", ylab = "Risk Type 1", pars = list(cex.axis = 2, cex.lab = 2))

dev.off()


#recruitment spikes
#generate some spikes (assuming 1000 iterations and start year of 2014. Will generate spikes for 60 years)
lrec_spikes <- lapply(seq(1:1000),function (x) fgenerate_spikes(mean_interval = 19, spike_var = 0.5, projyr = 61, 
                                                                     offset = 2014 - 2001, spike_years = c(1982,2001), 
                                                                     spike_probs = c(0.5,0.5)))
years<-seq(2014,len=61)

jpeg(filename=".//plots//Spikes05.jpg", width=800, height=600, quality=100)

#gather the timing of the spikes and plot
hist(unlist(lapply(lrec_spikes, function(x){years[x>0]})),
     breaks = c(seq(2014,len=61)),
     xlab="", ylab="", main="",
     axes=FALSE)
axis(1,at=c(seq(2014,2074,by=10)),cex.axis=1.5)
axis(2,at=c(0,25,50,75,100,125),cex.axis=1.5)
mtext("Frequency",side=2,line=3,cex=1.5)
mtext("Year",side=1,line=3,cex=1.5)
mtext("Recruitment Spikes (s=0.5)",side=3,line=1,cex=1.5)

dev.off()



#multiplot
opt <- c("MSE2014_320_Final","MSE2014_321_Final",
         "MSE2014_300_Final","MSE2014_301_Final","MSE2014_302_Final","MSE2014_303_Final","MSE2014_304_Final",
         "MSE2014_310_Final","MSE2014_311_Final","MSE2014_312_Final","MSE2014_313_Final","MSE2014_314_Final",
         "MSE2014_330_Final","MSE2014_331_Final","MSE2014_332_Final","MSE2014_333_Final","MSE2014_334_Final",
         "MSE2014_340_Final","MSE2014_341_Final","MSE2014_342_Final","MSE2014_343_Final","MSE2014_344_Final",
         "MSE2014_350_Final","MSE2014_351_Final","MSE2014_352_Final","MSE2014_353_Final","MSE2014_354_Final",
         "MSE2014_360_Final","MSE2014_361_Final","MSE2014_362_Final","MSE2014_363_Final","MSE2014_364_Final",
         "MSE2014_370_Final","MSE2014_371_Final","MSE2014_372_Final","MSE2014_373_Final","MSE2014_374_Final",
         "MSE2014_380_Final","MSE2014_381_Final","MSE2014_382_Final","MSE2014_383_Final","MSE2014_384_Final",
         "MSE2014_390_Final","MSE2014_391_Final","MSE2014_392_Final","MSE2014_393_Final","MSE2014_394_Final",
         "MSE2014_400_Final","MSE2014_401_Final","MSE2014_402_Final","MSE2014_403_Final","MSE2014_404_Final")


lRes <- lapply(opt, function(ref) {
  
  log.dat <- fread_log(ref)
  niter <- log.dat$nits
  opfiles <- paste(log.dat$outdata.path,"FLRObjects_",ref,"_",seq(1,log.dat$resolution+1,by=1),".dat",sep="")
  
  for (f in opfiles){    
    cat(f,"\n")
    load(f)
    #RP1
    op.SSB.J1.true.RP1 <- quantSums(window(op.SSB.J1.true,2014,2023))/1e6
    op.CatchWeight.RP1 <- quantSums(window(op.CatchWeight,2014,2023))/1e3
    #RP2
    op.SSB.J1.true.RP2 <- quantSums(window(op.SSB.J1.true,2014,2033))/1e6
    op.CatchWeight.RP2 <- quantSums(window(op.CatchWeight,2014,2033))/1e3
    #RP3
    op.SSB.J1.true.RP3 <- quantSums(window(op.SSB.J1.true,2034,2053))/1e6
    op.CatchWeight.RP3 <- quantSums(window(op.CatchWeight,2034,2053))/1e3
    
    SSBJ1.RP1 <- as.vector(apply(quantSums(op.SSB.J1.true.RP1),c("iter"),FUN="mean"))
    SSBJ1.RP2 <- as.vector(apply(quantSums(op.SSB.J1.true.RP2),c("iter"),FUN="mean"))
    SSBJ1.RP3 <- as.vector(apply(quantSums(op.SSB.J1.true.RP3),c("iter"),FUN="mean"))
    CW.RP1 <- as.vector(apply(op.CatchWeight.RP1,c("iter"),FUN="mean"))
    CW.RP2 <- as.vector(apply(op.CatchWeight.RP2,c("iter"),FUN="mean"))
    CW.RP3 <- as.vector(apply(op.CatchWeight.RP3,c("iter"),FUN="mean"))
    Rsk1J1.RP1 <- 100*(apply(quantSums(op.SSB.J1.true.RP1)<0.453269,c("year"),FUN="sum")/dim(op.SSB.J1.true.RP1)[6])
    Rsk1J1.RP2 <- 100*(apply(quantSums(op.SSB.J1.true.RP2)<0.453269,c("year"),FUN="sum")/dim(op.SSB.J1.true.RP2)[6])
    Rsk1J1.RP3 <- 100*(apply(quantSums(op.SSB.J1.true.RP3)<0.453269,c("year"),FUN="sum")/dim(op.SSB.J1.true.RP3)[6])
    #Rsk1J1.RP1 <- 100*(apply(quantSums(op.SSB.J1.true.RP1)<0.453269,c("iter"),FUN="sum")/dim(op.SSB.J1.true.RP1)[2])
    #Rsk1J1.RP2 <- 100*(apply(quantSums(op.SSB.J1.true.RP2)<0.453269,c("iter"),FUN="sum")/dim(op.SSB.J1.true.RP2)[2])
    #Rsk1J1.RP3 <- 100*(apply(quantSums(op.SSB.J1.true.RP3)<0.453269,c("iter"),FUN="sum")/dim(op.SSB.J1.true.RP3)[2])
    
    
  }
  
#  SSBJ1.RP1
#  SSBJ1.RP2
#  SSBJ1.RP3
#  CW.RP1
#  CW.RP2
#  CW.RP3
  Rsk1J1.RP1
#  Rsk1J1.RP2
#   Rsk1J1.RP3
  
})

names(lRes) <- opt

jpeg(filename=".//plots//Risk1_RP1.jpg", width=1200, height=1200, quality=100)

layout(matrix(seq(1,9),nrow=3,ncol=3,byrow=TRUE))

#outer margins
par(omi=c(0.0,0.0,1.0,0))
#inner
par(mai = c(1.0,1.0,0.5,0))

for (i in 1:9){
  sub <- lRes[(3+5*(i)):(3+5*(i)+4)]
  l <- fread_log(names(sub)[1])
  gamma <- l$egggamma
  tref <- as.numeric(l$TACref)/1e3
  cat(names(sub)[1],"\n")
  boxplot(sub,
          names=c("NA","500","1000","1500","2000"),
          ylim=c(0,100),
          cex.axis = 2,
          xlab = "Egg Limit (x10^15)",
          cex.lab = 2,
          ylab = "RP1 Risk (%)",
          main = "")
  mtext(bquote(gamma == .(gamma)), side = 3,line =3, cex = 1.5)
  mtext(paste("TACref = ",tref,"kt"), side = 3, line = 1, cex = 1.5)
  #Blim
  #abline(h=0.634577/1.4,col="red")  
  #5% risk
  abline(h=5, col="red")  
}

dev.off()
