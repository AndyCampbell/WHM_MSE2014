rm(list=ls())
gc()

source(".Rprofile")
sourceDir()

#Baseline runs (with and without spikes)

opt.file <- "MSE2014_SRSAD11_1scor_0spikes_1sigR"

f.FPRESS.Run(runref = opt.file,
             iterations = 1000,
             varmin = 0.0,
             varmax = 0.2,
             resolution = 40)

f.MSE2014Stats(ref = opt.file,
               fname = paste(".\\stats\\",opt.file,".dat",sep=""),
               period = c(2151,2200),
               ssbref = 634557)

f.FLRCompPlot(ref = opt.file,
              fname = paste(".\\plots\\FLR_",opt.file,".pdf",sep=""))

f.PlotFYld(ref = opt.file,
           sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
           pfile = paste(".\\plots\\FYld_",opt.file,".pdf",sep=""),
           title = "MSE2014 - Yield & Risk vs FBar",
           subtitle = "Serial Correlation, No Spike, 100% SigR",
           ylim = c(0,130),
           xlim = c(0,0.2))

f.PlotFSSB(ref = opt.file,
           sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
           pfile = paste(".\\plots\\FSSB_",opt.file,".pdf",sep=""),
           title = "MSE2014 - SSB & Risk vs FBar",
           subtitle = "Serial Correlation, No Spike, 100% SigR",
           ylim = c(0,3.4),
           xlim = c(0,0.2))


opt.file <- "MSE2014_SRSAD11_1scor_1spikes_1sigR"

f.FPRESS.Run(runref = opt.file,
             iterations = 1000,
             varmin = 0.0,
             varmax = 0.2,
             resolution = 40)

f.MSE2014Stats(ref = opt.file,
               fname = paste(".\\stats\\",opt.file,".dat",sep=""),
               period = c(2151,2200),
               ssbref = 634557)

f.FLRCompPlot(ref = opt.file,
              fname = paste(".\\plots\\FLR_",opt.file,".pdf",sep=""))

f.PlotFYld(ref = opt.file,
           sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
           pfile = paste(".\\plots\\FYld_",opt.file,".pdf",sep=""),
           title = "MSE2014 - Yield & Risk vs FBar",
           subtitle = "Serial Correlation, Including Spike, 100% SigR",
           ylim = c(0,270),
           xlim = c(0,0.2))

f.PlotFSSB(ref = opt.file,
           sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
           pfile = paste(".\\plots\\FSSB_",opt.file,".pdf",sep=""),
           title = "MSE2014 - SSB & Risk vs FBar",
           subtitle = "Serial Correlation, Including Spike, 100% SigR",
           ylim = c(0,6.8),
           xlim = c(0,0.2))



#Dec 1 2014
#Comparison of ssb/recruitment with and without spike for Jose's results and FPRESS results

#Jose's results
J.spike <- read.csv(".\\..\\Jose\\24_09_2014\\YvsF_1_1_1_0_2.csv",header=TRUE)
J.nospike <- read.csv(".\\..\\Jose\\24_09_2014\\YvsF_0_1_1_0_2.csv",header=TRUE)

#FPRESS
A.spike <- read.csv(".\\stats\\MSE2014_SRSAD11_1scor_1spikes_1sigR.dat",sep="\t",header=TRUE)
A.nospike <- read.csv(".\\stats\\MSE2014_SRSAD11_1scor_0spikes_1sigR.dat",sep="\t",header=TRUE)

#mean recruitment comparison
png(filename=".\\plots\\RecruitComparison.png",width=800,height=600)
layout(matrix(c(1,2),nrow=1,ncol=2,byrow=TRUE))
plot(J.spike$F,J.spike$Rmn/1000,pch=20,xlab="F",ylab="Rec",main="Inc Spike",ylim=c(0,max(A.spike$Rec,A.spike$Rec0.1,A.spike$Rec0.5,A.spike$Rec0.9)))
lines(A.spike$FBar,A.spike$Rec)
lines(A.spike$FBar,A.spike$Rec0.5,col="red")
lines(A.spike$FBar,A.spike$Rec0.1,col="red")
lines(A.spike$FBar,A.spike$Rec0.9,col="red")
legend("topright",c("JD'O - mean","AC - mean","AC-10th,50th,90th"),lty=c(0,1,1),pch=c(20,NA,NA),col=c("black","black","red"))
plot(J.nospike$F,J.nospike$Rmn/1000,pch=20,xlab="F",ylab="Rec",main="Exc Spike",ylim=c(0,max(A.nospike$Rec,A.nospike$Rec0.1,A.nospike$Rec0.5,A.nospike$Rec0.9)))
lines(A.nospike$FBar,A.nospike$Rec)
lines(A.nospike$FBar,A.nospike$Rec0.5,col="red")
lines(A.nospike$FBar,A.nospike$Rec0.1,col="red")
lines(A.nospike$FBar,A.nospike$Rec0.9,col="red")
dev.off()


png(filename=".\\plots\\SSBComparison.png",width=800,height=600)
layout(matrix(c(1,2),nrow=1,ncol=2,byrow=TRUE))

plot(J.spike$F,J.spike$SSBmn/1e6,xlab="F",ylab="SSB",
     main="Inc Spike",
     ylim=c(0,max(A.spike$SSB0.9,J.spike$SSB90/1e6)),
     lty=1,
     type="l")
lines(J.spike$F,J.spike$SSB10/1e6,lty=2)
lines(J.spike$F,J.spike$SSB50/1e6,lwd=2)
lines(J.spike$F,J.spike$SSB90/1e6,lty=2)

lines(A.spike$FBar,A.spike$Mean.SSB,col="red")
lines(A.spike$FBar,A.spike$SSB0.1,col="red",lty=2)
lines(A.spike$FBar,A.spike$SSB0.5,col="red",lwd=2)
lines(A.spike$FBar,A.spike$SSB0.9,col="red",lty=2)

legend("topright",c("JD'O","AC","10th/90th","50th","mean"),lty=c(1,1,2,1,1),lwd=c(1,1,1,1,2),col=c("black","red","black","black","black"))

plot(J.nospike$F,J.nospike$SSBmn/1e6,xlab="F",ylab="SSB",
     main="Exc Spike",
     ylim=c(0,max(A.nospike$SSB0.9,J.nospike$SSB90/1e6)),
     lty=1,
     type="l")
lines(J.nospike$F,J.nospike$SSB10/1e6,lty=2)
lines(J.nospike$F,J.nospike$SSB50/1e6,lwd=2)
lines(J.nospike$F,J.nospike$SSB90/1e6,lty=2)

lines(A.nospike$FBar,A.nospike$Mean.SSB,col="red")
lines(A.nospike$FBar,A.nospike$SSB0.1,col="red",lty=2)
lines(A.nospike$FBar,A.nospike$SSB0.5,col="red",lwd=2)
lines(A.nospike$FBar,A.nospike$SSB0.9,col="red",lty=2)

dev.off()

