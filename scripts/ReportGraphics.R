#report graphs

library(reshape2)
library(ggplot2)
library(dplyr)

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
jpeg(filename=".//plots//SlopeMultiplier.jpg",
     width=788, height=549, quality=100)
plot(x=c(-1.5,0,0.5,1.5),y=c(0,1,1.4,1.4),type="l",axes=FALSE,
     xlab="",ylab="")
axis(1,at=c(-1.5,-1.0,-0.5,0,0.5,1.0,1.5),cex.axis=1.25)
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4),pos=0,las=2,cex.axis=1.25)
mtext("slope",side=1,line=3,cex=1.25)
mtext("sl",side=2,line=1,cex=1.25)
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
jpeg(filename=".//plots//ProtectionMultiplier.jpg",
     width=788, height=549, quality=100)
plot(E,prot(E,Elim,1),type="l",axes=FALSE,xlab="",ylab="",lwd=2)
lines(E,prot(E,Elim,2.0),lty=2,lwd=2)
lines(E,prot(E,Elim,3.0),lty=3,lwd=2)
legend(0,1,legend=c(expression(paste(gamma," = 1")),expression(paste(gamma," = 2")),expression(paste(gamma," = 3"))),lty=c(1,2,3),bty="n",cex=1.25)
axis(1,at=c(0,20,40,60,80,100),labels=c("0","","","",expression("Egg"[lim]),""),cex.axis=1.25)
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.0),las=2,cex.axis=1.25)
mtext("Egg Count",side=1,line=3,cex=1.25)
mtext("TAC Multiplier",side=2,line=3,cex=1.25)
dev.off()