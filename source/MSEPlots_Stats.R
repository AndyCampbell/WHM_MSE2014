#MSE2014 plots and stats

#MSE2014Plots

# fFourInOne <- function(run){
#   
#   layout<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
#   
#   opt.file <- paste("MSE2014_",run,sep="")
#   
#   MSE2014ssbplot(opt.file,ymaxval=10,file=paste("MSE2014_",run,".png",sep=""))
#   MSE2014yieldplot(opt.file)
#   MSE2014fbarplot(opt.file)
#   MSE2014TACplot(opt.file)
#   
#   
# }


fMSE2014ssbplot<-function(ref,box.whisker = 1,conf.int = 0,yminval = "missing",
                         ymaxval = "missing",J1 = TRUE,Bref = "missing",
                         file = "missing",ext = "missing",debug = 0){
  
  #ref - simulation reference
  #conf.int - plot confidence intervals?
  #box.whisker - box whisker plots?
  #yminval - minimum y value
  #ymaxval = maximum y value
  #J1 - SSB at Jan 1, otherwise at spawning time
  #Brefs - draws reference lines
  
  #save graphics settings
  def.par <- par(no.readonly=TRUE)
  
  #read the log file
  log <- fread_log(ref)
  
  #read the output file
  if (file.exists(log$outfile)){
    dat <- load(log$outfile)
  } else {
    stop(paste("Cannot find",log$outfile,sep=" "))
  }
  
  #calculate the SSB by summing over ages & convert to Mt
  if (J1) {ssb <- quantSums(op.SSB.J1.true)} else {ssb <- quantSums(op.SSB.st.true)}
  #convert to Mt
  ssb <- ssb/1e6
  
  #calculate stats
  #median
  med.ssb <- apply(X=ssb,MARGIN=c("year"),FUN=median)
  #mean
  mean.ssb <- apply(X=ssb,MARGIN=c("year"),FUN=mean)
  #SD
  SD.ssb <- sqrt(apply(X=ssb,MARGIN=c("year"),FUN="var"))
  
  #percentiles
  pct.ssb <- apply(X=ssb,MARGIN=c("year"),FUN=quantile,probs=seq(0.05,0.95,by=0.05))
  
  if (!missing(file)) {
    if (!missing(ext)) {
      if (ext=="pdf") {
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(fFPRESS_home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.3,0.1,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,10,len=length(yr))
  
  if (missing(ymaxval)) {
    if (missing(yminval)) {
      #both limits missing
      plot(yr,ydum,type="n",xlab="Year",ylab="SSB (Mt)",axes=FALSE)    
    } else {
      #only max limit missing
      plot(yr,ydum,type="n",xlab="Year",ylab="SSB (Mt)",axes=FALSE,ylim=c(yminval,max(ydum)))    
    }
  } else {
    if (missing(yminval)){
      #min limit missing
      plot(yr,ydum,type="n",xlab="Year",ylab="SSB (Mt)",axes=FALSE,ylim=c(0,ymaxval))    
    } else {
      plot(yr,ydum,type="n",xlab="Year",ylab="SSB (Mt)",axes=FALSE,ylim=c(yminval,ymaxval))    
    }
  }
  
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5),cex.axis=1.5)
  axis(2,cex.axis=1.5,las=2)
  
  #median
  lines(yr,med.ssb)
  
  #box/whisker
  if(box.whisker==1){
    for(i in 1:(length(yr))){
      boxplot(ssb[,i,,,],range=0.1,add=TRUE,at=yr[i],axes=FALSE,pch=".")
    }
  }
  
  #percentiles
  points(yr,pct.ssb["10%",],pch=20,col="red")
  points(yr,pct.ssb["90%",],pch=20,col="red")  
  lines(yr,pct.ssb["10%",],lty=2,col="red")
  lines(yr,pct.ssb["90%",],lty=2,col="red")
  
  #annotations
  mtext("(a) SSB (Mt)",side=3,adj=0,cex=1.5)
  
  #confidence intervals (assumes normal distribution)
  #plot 95%/99% confidence lines?
  if(conf.int==1){
    lines(yr,mean.ssb-1.96*SD.ssb,lty=2)
    lines(yr,mean.ssb+1.96*SD.ssb,lty=2)
    lines(yr,mean.ssb-2.58*SD.ssb,lty=2)
    lines(yr,mean.ssb+2.58*SD.ssb,lty=2)
  }
  
  abline(h=0.634577,lty=2,col="red")
  
  #box(which = 'outer', lty = '1373', col = 'red')
  
  if (!missing(file)) {dev.off()}
  
  #restore plotting defaults
  par(omi=par.omi)
  par(mar=par.mar)
  
}

fMSE2014yieldplot<-function(ref,box.whisker=1,conf.int=0,
                           yminval="missing",ymaxval="missing",
                           file="missing",ext="missing"){
  
  #save graphics settings
  def.par <- par(no.readonly=TRUE)
  
  #read the log file
  log <- fread_log(ref)
  
  #read the output file
  if (file.exists(log$outfile)){
    #remove any existing CatchWeight object
    #if (exists("op.CatchWeight")) {rm("op.CatchWeight")}
    dat <- load(log$outfile)
  } else {
    stop(paste("Cannot find",log$outfile,sep=" "))
  }
  
  #check object CatchWeight was saved by this simulation
  if (!exists("op.CatchWeight")){stop(paste("No CatchWeight object for simulation",ref,sep=" "))}
  
  #calculate the yield by summing over ages & convert to kt
  Yld<-quantSums(op.CatchWeight)/1e3
  
  #calculate stats
  #median
  med.yld <- apply(X=Yld,MARGIN=c("year"),FUN=median)
  #mean
  mean.yld <- apply(X=Yld,MARGIN=c("year"),FUN=mean)
  #SD
  SD.yld <- sqrt(apply(X=Yld,MARGIN=c("year"),FUN="var"))
  
  #percentiles
  pct.yld <- apply(X=Yld,MARGIN=c("year"),FUN=quantile,probs=seq(0.05,0.95,by=0.05))
  
  if (!missing(file)) {
    if (!missing(ext)) {
      if (ext=="pdf") {
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(fFPRESS_home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.3,0.1,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,max(Yld),len=length(yr))
  plot(yr,ydum,type="n",xlab="Year",ylab="Yield (kt)",axes=FALSE,ylim=c(0,250))
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5),cex.axis=1.5)
  axis(2,cex.axis=1.5,las=2)
  
  #median
  lines(yr,med.yld)
  
  #mean
  #lines(yr,mean.yld)
  
  #box/whisker
  if(box.whisker==1){
    for(i in 1:(length(yr))){
      boxplot(Yld[,i,,,],range=0.1,add=TRUE,at=yr[i],axes=FALSE,pch=".")
    }
  }
  
  #percentiles
  points(yr,pct.yld["10%",],pch=20,col="red")
  points(yr,pct.yld["90%",],pch=20,col="red")  
  lines(yr,pct.yld["10%",],lty=2,col="red")
  lines(yr,pct.yld["90%",],lty=2,col="red")
  
  #annotations
  mtext("(b) Yield (kt)",side=3,adj=0,cex=1.5)
  
  #confidence intervals (assumes normal distribution)
  #plot 95%/99% confidence lines?
  if(conf.int==1){
    lines(yr,mean.yld-1.96*SD.yld,lty=2)
    lines(yr,mean.yld+1.96*SD.yld,lty=2)
    lines(yr,mean.yld-2.58*SD.yld,lty=2)
    lines(yr,mean.yld+2.58*SD.yld,lty=2)
  }
  
  #box(which = 'outer', lty = '1373', col = 'red')
  
  if (!missing(file)) {dev.off()}
  
  #restore plotting defaults
  par(omi=par.omi)
  par(mar=par.mar)
  
}

fMSE2014fbarplot<-function(ref,box.whisker=1,conf.int=0,
                          yminval="missing",ymaxval="missing",
                          file="missing",ext="missing"){
  
  #save graphics settings
  def.par <- par(no.readonly=TRUE)
  
  #read the log file
  log <- fread_log(ref)
  
  #read the output file
  if (file.exists(log$outfile)){
    dat <- load(log$outfile)
  } else {
    stop(paste("Cannot find",log$outfile,sep=" "))
  }
  
  #check object FBar was saved by this simulation
  if (!exists("op.fbar")){stop(paste("No FBar object for simulation",ref,sep=" "))}
  
  #calculate stats
  #median
  med.fbar <- apply(X=op.fbar,MARGIN=c("year"),FUN=median)
  #mean
  mean.fbar <- apply(X=op.fbar,MARGIN=c("year"),FUN=mean)
  #SD
  SD.fbar <- sqrt(apply(X=op.fbar,MARGIN=c("year"),FUN="var"))
  
  #percentiles
  pct.fbar <- apply(X=op.fbar,MARGIN=c("year"),FUN=quantile,probs=seq(0.05,0.95,by=0.05))
  

  if (!missing(file)) {
    if (!missing(ext)) {
      if (ext=="pdf") {
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(fFPRESS_home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
 
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.3,0.1,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,0.2,len=length(yr))
  plot(yr,ydum,type="n",xlab="Year",ylab="FBar",axes=FALSE)
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5),cex.axis=1.5)
  axis(2,cex.axis=1.5,las=2)
  
  #median
  lines(yr,med.fbar)
  
  #mean
  #lines(yr,mean.yld)
  
  #box/whisker
  if(box.whisker==1){
    for(i in 1:(length(yr))){
      boxplot(op.fbar[,i,,,,],range=0.1,add=TRUE,at=yr[i],axes=FALSE,pch=".")
    }
  }
  
  #percentiles
  points(yr,pct.fbar["10%",],pch=20,col="red")
  points(yr,pct.fbar["90%",],pch=20,col="red")  
  lines(yr,pct.fbar["10%",],lty=2,col="red")
  lines(yr,pct.fbar["90%",],lty=2,col="red")
  
  #annotations
  mtext("(c) FBar",side=3,adj=0,cex=1.5)
  
  #confidence intervals (assumes normal distribution)
  #plot 95%/99% confidence lines?
  if(conf.int==1){
    lines(yr,mean.fbar-1.96*SD.fbar,lty=2)
    lines(yr,mean.fbar+1.96*SD.fbar,lty=2)
    lines(yr,mean.fbar-2.58*SD.fbar,lty=2)
    lines(yr,mean.fbar+2.58*SD.fbar,lty=2)
  }
  
  #box(which = 'outer', lty = '1373', col = 'red')

  if (!missing(file)) {dev.off()}
  
  #restore plotting defaults
  par(omi=par.omi)
  par(mar=par.mar)
  
}

fMSE2014TACplot<-function(ref,box.whisker=1,conf.int=0,
                         yminval="missing",ymaxval="missing",
                         file="missing",ext="missing"){
  
  #read the log file
  log <- fread_log(ref)
  
  #read the output file
  if (file.exists(log$outfile)){
    dat <- load(log$outfile)
  } else {
    stop(paste("Cannot find",log$outfile,sep=" "))
  }
  
  #check object CatchWeight was saved by this simulation
  if (!exists("op.FTac")){stop(paste("No FTac object for simulation",ref,sep=" "))}
  
  #TAC in kt
  TAC<-op.FTac/1000
  
  #calculate stats
  #median
  med.tac <- apply(X=TAC,MARGIN=c("year"),FUN=median)
  #mean
  mean.tac <- apply(X=TAC,MARGIN=c("year"),FUN=mean)
  #SD
  SD.tac <- sqrt(apply(X=TAC,MARGIN=c("year"),FUN="var"))
  
  #percentiles
  pct.tac <- apply(X=TAC,MARGIN=c("year"),FUN=quantile,probs=seq(0.05,0.95,by=0.05))
  
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.2,0,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  if (!missing(file)) {
    if (!missing(ext)) {
      if (ext=="pdf") {
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(fFPRESS_home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,max(TAC),len=length(yr))
  plot(yr,ydum,type="n",xlab="Year",ylab="TAC (kt)",axes=FALSE)
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5))
  axis(2)
  
  #median
  lines(yr,med.tac)
  
  #mean
  #lines(yr,mean.yld)
  
  #box/whisker
  if(box.whisker==1){
    for(i in 1:(length(yr))){
      boxplot(TAC[,i,,,],range=0.1,add=TRUE,at=yr[i],axes=FALSE,pch=".")
    }
  }
  
  #percentiles
  points(yr,pct.tac["10%",],pch=20,col="red")
  points(yr,pct.tac["90%",],pch=20,col="red")  
  lines(yr,pct.tac["10%",],lty=2,col="red")
  lines(yr,pct.tac["90%",],lty=2,col="red")
  
  #annotations
  mtext("TAC vs Year",side=3,adj=0)
  
  #confidence intervals (assumes normal distribution)
  #plot 95%/99% confidence lines?
  if(conf.int==1){
    lines(yr,mean.tac-1.96*SD.tac,lty=2)
    lines(yr,mean.tac+1.96*SD.tac,lty=2)
    lines(yr,mean.tac-2.58*SD.tac,lty=2)
    lines(yr,mean.tac+2.58*SD.tac,lty=2)
  }
  
  if (!missing(file)) {dev.off()}
  
  #restore plotting defaults
  par(omi=par.omi)
  par(mar=par.mar)
  
}


fMSE2014riskplot<-function(ref,yminval="missing",ymaxval="missing",file="missing",ext="missing"){
  
  #save graphics settings
  def.par <- par(no.readonly=TRUE)
  
  #read the log file
  log <- fread_log(ref)
  
  #read the stat file
  stats.file <- paste(".\\stats\\",ref,".dat",sep="")
  if (file.exists(stats.file)){
    dat <- read.table(stats.file,header=TRUE,sep="\t")
  } else {
    stop(paste("Cannot find",stats.file,sep=" "))
  }
  
  if (!missing(file)) {
    if (!missing(ext)) {
      if (ext=="pdf") {
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(fFPRESS_home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(fFPRESS_home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.3,0.1,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,100,len=length(yr))
  plot(yr,ydum,type="n",xlab="Year",ylab="Risk",axes=FALSE)
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5),cex.axis=1.5)
  axis(2,cex.axis=1.5,las=2)
  
  #annual risks
  lines(dat$From[dat$From==dat$To],dat$Rsk3_ST[dat$From==dat$To])
  
  #failure rate
  lines(dat$From[dat$From==dat$To],dat$FailRate[dat$From==dat$To],col="red")

  #multiannual risks
  if (length(dat$From[!dat$From==dat$To])>0) {
    for (l in seq(1,length(dat$From[!dat$From==dat$To]))){
      lines(seq(dat$From[!dat$From==dat$To][l],dat$To[!dat$From==dat$To][l]),
            rep(dat$Rsk3_ST[!dat$From==dat$To][l],length=length(seq(dat$From[!dat$From==dat$To][l],dat$To[!dat$From==dat$To][l]))))
    }
  }
  
  #50% risk line
  abline(h=50,lty=2)
  #0% risk line
  abline(h=0,lty=2)
  
  #annotations
  mtext("(d) Risk",side=3,adj=0,cex=1.5)
    
  #box(which = 'outer', lty = '1373', col = 'red')

  if (!missing(file)) {dev.off()}
  
  #restore plotting defaults
  par(omi=par.omi)
  par(mar=par.mar)
  
}


fPlotFSSB <- function(ref,sfile,pfile,title,subtitle,ylim="missing",xlim="missing"){
  
  #plot the SSB and Risk 3
  Model.res<-read.table(file=sfile,header=TRUE,sep="\t")
  
  plot.title <- title
  plot.subtitle <- subtitle
  
  names(Model.res) <- c("Ref","TACRef","EggLim","EggGamma","From","To","Blim","FTAC","SSBJ1_0.1","SSBJ1_0.5",
                        "SSBJ1_0.9","Mean_SSBJ1","Rsk1_J1","Rsk2_J1","Rsk3_J1","SSBST_0.1","SSBST_0.5","SSBST_0.9",
                        "Mean_SSBST","Rsk1_ST","Rsk2_ST","Rsk3_ST","Yld0.1","Yld0.5","Yld0.9","Mean_Yld",
                        "Rec0.1","Rec0.5","Rec0.9","Rec","Y/R","TACinc","TACdec","NoChg","AvgInc","AvgDec",
                        "FailRate")
  
  pdf(file=pfile)
  
  par("oma"=c(1,0,0,2))
  
  plot(Model.res$FTAC,Model.res$MeanSSB,type="n",col="green",
       xlab="FBar",ylab="SSB (Mt)",axes=FALSE,
       xlim=c(0,0.2),ylim=if (missing(ylim)) {c(0,5)} else {ylim},main=plot.title)
  
  mtext(plot.subtitle,side=3,line=0)
  mtext(ref,side=1,line=5)
  
  lines(Model.res$FTAC,Model.res$SSBST_0.1,col="green",lty=2)
  lines(Model.res$FTAC,Model.res$SSBST_0.5,col="green",lwd=2)
  lines(Model.res$FTAC,Model.res$Mean_SSBST,col="green",lwd=1)
  lines(Model.res$FTAC,Model.res$SSBST_0.9,col="green",lty=2)
  
#   legend(0,if (missing(ylim)) {5} else {ylim[2]},
#          c("Med SSB","10th,90th Pct SSB","Type 3 Risk (MSY Btrigger)"),
#          lty=c(1,2,1),
#          col=c("green","green","red"),
#          lwd=c(2,1,1),
#          bty="n")
  
  tck<-seq(0,0.2,by=0.02)
  lbl<-c("0.0","","","","","0.05","","","","","0.1","","","","","0.15","","","","","0.2")
  
  axis(side=1,
       at=tck)
  axis(side=2)
  
  par(new=TRUE)
  
  #plot(Model.res$FBar,Model.res$Rsk3,type="l",col="red",axes=FALSE,
  #     xlim=c(0,0.2),ylim=c(0,100),ylab="",xlab="")
  #plot(Model.res$FTAC,Model.res$Rsk3_ST,type="l",col="red",axes=FALSE,
  #     xlim=c(0,0.2),ylim=c(0,100),ylab="",xlab="")
  plot(Model.res$FTAC,Model.res$Rsk1_ST,type="l",col="red",axes=FALSE,
       xlim=c(0,0.2),ylim=c(0,100),ylab="",xlab="")

  #50% risk line
  #abline(h=50,col="red",lty=2)
  
  axis(side=4)
  #mtext("Risk 3",side=4,line=2)
  mtext("Risk 1",side=4,line=2)

  dev.off()
  
}


fPlotMSYJose <- function(ref){
  
  #plot of long term (MSY) simulations based on code from Jose.
  #ref is the MSY run reference
  
  #read in the simulation settings
  ofile <- paste0(fFPRESS_home(),"\\options\\options",ref,".xml")
  lopt <- fread_options(ofile,match.call())
  
  #read the statistics
  sfile <- paste0(fFPRESS_home(),"\\stats\\",ref,".dat")
  Model.res <- read.table(file = sfile, header = TRUE, sep = "\t")
  
  #output (plot) file
  pfile <- paste0(fFPRESS_home(),"\\plots\\MSY",ref,".pdf")

  #updated assessment weights for conditioning
  #bhprop <- 0.46
  #rkprop <- 0.32
  #Blim <- 600700  
  
  #SALY assessment for conditioning
  bhprop <- 0.46
  rkprop <- 0.32
  Blim <- 453269  
  
  #rename some cols so that Jose's code works without modification
  names(Model.res)[names(Model.res)=="FTAC"] <- "F"
  names(Model.res)[names(Model.res)=="SSBJ1_0.1"] <- "SSB10"
  names(Model.res)[names(Model.res)=="SSBJ1_0.5"] <- "SSB50"
  names(Model.res)[names(Model.res)=="SSBJ1_0.9"] <- "SSB90"
  names(Model.res)[names(Model.res)=="Mean.SSBJ1"] <- "SSBmn"
  names(Model.res)[names(Model.res)=="Rsk1_J1"] <- "risk1"
  names(Model.res)[names(Model.res)=="Rsk2_J1"] <- "risk2"
  names(Model.res)[names(Model.res)=="Rsk3_J1"] <- "risk3"
  names(Model.res)[names(Model.res)=="Yld0.1"] <- "Y10"
  names(Model.res)[names(Model.res)=="Yld0.5"] <- "Y50"
  names(Model.res)[names(Model.res)=="Yld0.9"] <- "Y90"
  names(Model.res)[names(Model.res)=="Mean.Yld"] <- "Ymn"
  names(Model.res)[names(Model.res)=="Rec"] <- "Rmn"
  names(Model.res)[names(Model.res)=="Y.R"] <- "Ymn.Rmn"
  
  #some more updates to harmonize units
  #convert SSB from Mt to t
  Model.res["SSB10"] <- 1e6*Model.res["SSB10"]
  Model.res["SSB50"] <- 1e6*Model.res["SSB50"]
  Model.res["SSB90"] <- 1e6*Model.res["SSB90"]
  Model.res["SSBmn"] <- 1e6*Model.res["SSBmn"]
  #yield from kt to t
  Model.res["Y10"] <- 1e3*Model.res["Y10"]
  Model.res["Y50"] <- 1e3*Model.res["Y50"]
  Model.res["Y90"] <- 1e3*Model.res["Y90"]
  Model.res["Ymn"] <- 1e3*Model.res["Ymn"]
  
  #MSY parameters - F, Median SSB and Median Yield corresponding to fishing mortality that results in max median yield
  Fmsy <- Model.res[Model.res[,"Y50"]==max(Model.res[,"Y50"]),"F"]
  Bmsy <- Model.res[Model.res[,"Y50"]==max(Model.res[,"Y50"]),"SSB50"]
  Cmsy <- Model.res[Model.res[,"Y50"]==max(Model.res[,"Y50"]),"Y50"]

  #MSY parameter ranges (see WKMSYREF3 report for details)
  Y95 <- 0.95*max(Model.res[,"Y50"])
  Fmsylo2 <- min(Model.res[Model.res[,"Y50"] >= Y95,"F"])
  Ylo2 <- Model.res[Fmsylo2/0.005+1,"Y50"]
  Bmsylo2 <- Model.res[Fmsylo2/0.005+1,"SSB50"]
  
  if (Ylo2 > Y95) {
    Fmsylo1 <- Fmsylo2-0.005
    Ylo1 <- Model.res[Fmsylo1/0.005+1,"Y50"]
    Bmsylo1 <- Model.res[Fmsylo1/0.005+1,"SSB50"]
    Fmsylo <- Fmsylo1+0.005*(Y95-Ylo1)/(Ylo2-Ylo1)
    Bmsylo <- Bmsylo1+(Bmsylo2-Bmsylo1)*(Y95-Ylo1)/(Ylo2-Ylo1)
  } else {
    Fmsylo <- Fmsylo2
    Bmsylo <- Bmsylo2
  }
  
  Fmsyhi1 <- max(Model.res[Model.res[,"Y50"]>=Y95,"F"])
  Yhi1 <- Model.res[Fmsyhi1/0.005+1,"Y50"]
  Bmsyhi1 <- Model.res[Fmsyhi1/0.005+1,"SSB50"]
  if (Yhi1 > Y95) {
    Fmsyhi2 <- Fmsyhi1+0.005
    Yhi2 <- Model.res[Fmsyhi2/0.005+1,"Y50"]
    Bmsyhi2 <- Model.res[Fmsyhi2/0.005+1,"SSB50"]
    Fmsyhi <- Fmsyhi1+0.005*(Y95-Yhi1)/(Yhi2-Yhi1)
    Bmsyhi <- Bmsyhi1+(Bmsyhi2-Bmsyhi1)*(Y95-Yhi1)/(Yhi2-Yhi1)
  } else {
    Fmsyhi <- Fmsyhi1
    Bmsyhi <- Bmsyhi1
  }
  
  F052<-min(Model.res[Model.res[,"risk1"]>=5,"F"])
  R2<-Model.res[F052/0.005+1,"risk1"]
  Y2<-Model.res[F052/0.005+1,"Y50"]
  if (R2>5) {
    F051<-F052-0.005
    R1<-Model.res[F051/0.005+1,"risk1"]
    Y1<-Model.res[F051/0.005+1,"Y50"]
    F05<-F051+0.005*(5-R1)/(R2-R1)
    Y05<-Y1+(Y2-Y1)*(5-R1)/(R2-R1)
  } else {F05<-F052;Y05<-Y2}
  if (F05<Fmsy) {
    Ymsybot<-0.95*Y05
    Fmsybot2<-min(Model.res[Model.res[,"Y50"]>=Ymsybot,"F"])
    Y2<-YvsF[Fmsybot2/0.005+1,"Y50"]
    if (Y2>Ymsybot) {
      Fmsybot1<-Fmsybot2-0.005
      Y1<-Model.res[Fmsybot1/0.005+1,"Y50"]
      Fmsybot<-Fmsybot1+0.005*(Ymsybot-Y1)/(Y2-Y1)
    } else Fmsybot<-Fmsybot2
  } else Fmsybot <-Fmsylo

  #initiate the graphics device, 12 inches by 5 inches
  pdf(file = pfile, width = 12, height = 5)

  #plot layout and margins
  par(mfrow=c(1,2))
  par(mar=c(5, 5, 4, 4) + 0.1)

  #Yield vs F
  if (lopt$onceoff) {
    plot(Model.res$F, Model.res$Y90/1e3, ylim = c(0,.3e6/1e3), xlab = "F", ylab = "Yield (kt)",
         type = "l", lty = "dotted", lwd = 1, col = "black", main = "Yield and Risk 1 vs. F")
  } else {
    plot(Model.res$F, Model.res$Y90/1e3, ylim = c(0,.15e6/1e3), xlab = "F", ylab = "Yield (kt)",
         type = "l", lty = "dotted", lwd = 1, col = "black", main = "Yield and Risk 1 vs. F")
  }
  
  lines(Model.res$F,Model.res$Y50/1e3,type="l",lty="solid",lwd=2)
  lines(Model.res$F,Model.res$Y10/1e3,type="l",lty="dotted",lwd=1)
  lines(Model.res$F,Model.res$Ymn/1e3,type="l",lty="solid",lwd=1)
  abline(v=Fmsy,col="red",lwd=2)
  abline(v=Fmsylo,col="red",lty=2)
  abline(v=Fmsyhi,col="red",lty=2)
  #abline(v=F05,col="pink",lwd=2)
  
  par(new=T)
  plot(Model.res$F,Model.res$risk1,ylim=c(0,100),axes=F,xlab="",ylab="",type="l",lty="dashed",lwd=2,col="black")
  axis(4,ylim=c(0,max(Model.res$risk1)),lwd=1,line=0)
  mtext(4,text="Risk 1",line=2.5)
  mtext(3,text=paste("bh=",100*bhprop,"%, rk=",100*rkprop,"%, hs=",100*(1-bhprop-rkprop),"%, Blim=",round(Blim),sep=""),line=0.5)
  mtext(1,text=paste("Fmsy=",round(Fmsy,digits=3),sep=""),line=2,cex=0.6,adj=1)
  mtext(1,text=paste("Fmsylo=",round(Fmsylo,digits=3),", Fmsyhi=",round(Fmsyhi,digits=3),sep=""),line=2.5,cex=0.6,adj=1)
  mtext(1,text=paste("Cmsy=",round(Cmsy),", F05=",round(F05,digits=3),sep=""),line=3,cex=0.6,adj=1)
  
  #SSB vs F
  if (lopt$onceoff) {
    plot(Model.res$F, Model.res$SSB90/1e6, ylim = c(0,7e6/1e6), xlab = "F", ylab = "SSB (Mt)", 
         type = "l", lty = "dotted", lwd = 1, col = "black", main = "SSB and Risk 1 vs. F")
  } else {
    plot(Model.res$F, Model.res$SSB90/1e6, ylim = c(0,4e6/1e6), xlab = "F", ylab = "SSB (Mt)",
         type = "l", lty = "dotted", lwd = 1, col = "black", main = "SSB and Risk 1 vs. F")  
  }
  lines(Model.res$F,Model.res$SSB50/1e6,type="l",lty="solid",lwd=2)
  lines(Model.res$F,Model.res$SSB10/1e6,type="l",lty="dotted",lwd=1)
  lines(Model.res$F,Model.res$SSBmn/1e6,type="l",lty="solid",lwd=1)
  
  par(new=T)
  #plot(YvsF$F,YvsF$risk1,ylim=c(0,max(YvsF$risk1)),axes=F,xlab="",ylab="",type="l",lty="dashed",lwd=2,col="black")
  plot(Model.res$F,Model.res$risk1,ylim=c(0,100),axes=F,xlab="",ylab="",type="l",lty="dashed",lwd=2,col="black")
  axis(4,ylim=c(0,max(Model.res$risk1)),lwd=1,line=0)
  mtext(4,text="Risk 1",line=2.5)
  mtext(3,text=paste("fspike=",fspike,", fscor=",fscor,", fsigR=",fsigR,", fbcor=",fbcor,", fsrdev=",fsrdev,sep=""),line=0.5)
  mtext(1,text=paste("Bmsy=",round(Bmsy),sep=""),line=2,cex=0.6,adj=1)
  mtext(1,text=paste("Bmsylo=",round(Bmsylo),", Bmsyhi=",round(Bmsyhi),sep=""),line=2.5,cex=0.6,adj=1)
  abline(v=Fmsy,col="red",lwd=2)
  abline(v=Fmsylo,col="red",lty=2)
  abline(v=Fmsyhi,col="red",lty=2)
  #abline(v=F05,col="pink",lwd=2)
  
  
  dev.off()
  
}


fPlotFYld <- function(ref,sfile,pfile,title,subtitle,ylim="missing",xlim="missing"){
  
  #read statistics
  Model.res<-read.table(file=sfile,header=TRUE,sep="\t")
  
  plot.title <- title
  plot.subtitle <- subtitle
  
  #names(Model.res) <- c("From","To","FBar","SSB0.1","SSB0.5","SSB0.9","MeanSSB","Rsk1","Rsk2","Rsk3","Yld0.1","Yld0.5","Yld0.9","MeanYld","Rec","YPR")
  names(Model.res) <- c("Ref","TACRef","EggLim","EggGamma","From","To","Blim","FTAC","SSBJ1_0.1","SSBJ1_0.5",
                        "SSBJ1_0.9","Mean_SSBJ1","Rsk1_J1","Rsk2_J1","Rsk3_J1","SSBST_0.1","SSBST_0.5","SSBST_0.9",
                        "Mean_SSBST","Rsk1_ST","Rsk2_ST","Rsk3_ST","Yld0.1","Yld0.5","Yld0.9","Mean_Yld",
                        "Rec0.1","Rec0.5","Rec0.9","Rec","Y/R","TACinc","TACdec","NoChg","AvgInc","AvgDec",
                        "FailRate")
  
  
  pdf(file=pfile)
  
  par("oma"=c(1,0,0,2))
  
  plot(Model.res$FTAC,Model.res$Mean_Yld,type="n",col="green",
       xlab="FBar",ylab="Yield (kt)",axes=FALSE,
       xlim=if (missing(xlim)) {c(0,0.2)} else {xlim},ylim=if (missing(ylim)) {c(0,400)} else {ylim},main=plot.title)
  
  mtext(plot.subtitle,side=3,line=0)
  mtext(ref,side=1,line=5)
  
  lines(Model.res$FTAC,Model.res$Yld0.1,col="green",lty=2)
  lines(Model.res$FTAC,Model.res$Yld0.5,col="green",lwd=2)
  lines(Model.res$FTAC,Model.res$Yld0.9,col="green",lty=2)
  
#   legend(0,if (missing(ylim)) {400} else {ylim[2]},
#          c("Med Yield","10th,90th Pct Yield","Type 3 Risk (MSY Btrigger)"),
#          lty=c(1,2,1),
#          col=c("green","green","red"),
#          lwd=c(2,1,1),
#          bty="n")
  
  tck<-seq(0,0.2,by=0.02)
  lbl<-c("0.0","","","","","0.05","","","","","0.1","","","","","0.15","","","","","0.2")
  
  axis(side=1,
       at=tck)
  axis(side=2)
  
  par(new=TRUE)
  
  #plot(Model.res$FBar,Model.res$Rsk3,type="l",col="red",axes=FALSE,
  #     xlim=if (missing(xlim)) {c(0,0.2)} else {xlim},ylim=c(0,100),ylab="",xlab="")
  #plot(Model.res$FTAC,Model.res$Rsk3_ST,type="l",col="red",axes=FALSE,
  #     xlim=if (missing(xlim)) {c(0,0.2)} else {xlim},ylim=c(0,100),ylab="",xlab="")
  plot(Model.res$FTAC,Model.res$Rsk1_ST,type="l",col="red",axes=FALSE,
       xlim=if (missing(xlim)) {c(0,0.2)} else {xlim},ylim=c(0,100),ylab="",xlab="")

  axis(side=4)
  #mtext("Risk 3",side=4,line=2)
  mtext("Risk 1",side=4,line=2)
  #abline(h=50,col="red",lty=2,lwd=1)
  #lines(Model.res$FBar,Model.res$Rsk3,col="red")  
  #lines(Model.res$FTAC,Model.res$Rsk3_ST,col="red")  
  lines(Model.res$FTAC,Model.res$Rsk1_ST,col="red")  
  
  dev.off()
  
}


#FLR comparison
# fFLRCompPlot <- function(ref,fname,f=c(0,0.05,0.1),fcol=c("black","red","blue")) {
#   
#   #makes a plot for comparison with FLR output.
#   #Recruitment, Harvest Rate, Catch, SSB
#   
#   #ref - the run reference
#   #f - vector of f vals to plot
#   #fcol - vector of colours for f vals
#   
#   if (missing(ref)) {ref<-winDialogString("Run Reference?","")}
#   if (is.null(ref)) {stop("Cancelled\n")}
#   if (nchar(ref)==0) stop("Exit")
# 
#   #determine which data files need to be read in
#   #they are post fixed with a number as the simulation progresses through the f or TAC values
# 
#   #read log.file to determine the varmin,varmax and resolution settings
#   log.dat <- read.log(ref)
# 
#   #variable values
#   varVals <- seq(log.dat$varmin,log.dat$varmax,length.out = log.dat$resolution + 1)
#   
#   #file numbers to be read in
#   fnum <- as.integer(match(f,round(varVals,6)))
#   
#   opfiles <- paste(log.dat$outdata.path,"FLRObjects_",ref,"_",fnum,".dat",sep="")
# 
#   Rec_05 <- Rec_50 <- Rec_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
#   Harvest_05 <- Harvest_50 <- Harvest_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
#   Catch_05 <- Catch_50 <- Catch_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
#   SSB_05 <- SSB_50 <- SSB_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
#   
#   for (fl in 1:length(opfiles)){
#   
#     load(opfiles[fl])
# 
#     cat(opfiles[fl],"\n")
#     
#     Rec_05[,fl]<-apply(op.Recruits/1e6,"year",FUN="quantile",probs=c(0.05))
#     Rec_50[,fl]<-apply(op.Recruits/1e6,"year",FUN="median")
#     Rec_95[,fl]<-apply(op.Recruits/1e6,"year",FUN="quantile",probs=c(0.95))
# 
#     Harvest_05[,fl]<-apply(op.FBar,"year",FUN="quantile",probs=c(0.05))
#     Harvest_50[,fl]<-apply(op.FBar,"year",FUN="median")
#     Harvest_95[,fl]<-apply(op.FBar,"year",FUN="quantile",probs=c(0.95))
# 
#     Catch_05[,fl]<-apply(quantSums(op.CatchWeight)/1e3,"year",FUN="quantile",probs=c(0.05))
#     Catch_50[,fl]<-apply(quantSums(op.CatchWeight)/1e3,"year",FUN="median")
#     Catch_95[,fl]<-apply(quantSums(op.CatchWeight)/1e3,"year",FUN="quantile",probs=c(0.95))
# 
#     SSB_05[,fl]<-apply(quantSums(op.SSB.J1.true)/1e6,"year",FUN="quantile",probs=c(0.05))
#     SSB_50[,fl]<-apply(quantSums(op.SSB.J1.true)/1e6,"year",FUN="median")
#     SSB_95[,fl]<-apply(quantSums(op.SSB.J1.true)/1e6,"year",FUN="quantile",probs=c(0.95))
#     
#   }
#   
#   pdf(fname)
#   
#   m <- matrix(c(1,2,3,4,5,5),ncol=2,byrow=TRUE)
#   layout(m,widths=c(0.5,0.5),heights=c(0.475,0.475,0.05))
#   
#   #outer margin limits in text lines
#   par("oma"=c(0,0,0,1))
#   
#   #individual plot margin sizes
#   par(mar = c(2,4,4,2) + 0.1)
#   
#   #Recruits plot
#   plot(seq(log.dat$startyear,by=1,length=log.dat$years),
#        rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(Rec_05),max(Rec_50),max(Rec_95))),
#        xlab="Year",ylab="Recruits (x10^6)")
#   
#   for (p in 1:length(f)){
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Rec_50[,p],lwd=2,lty=1,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Rec_05[,p],lwd=1,lty=2,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Rec_95[,p],lwd=1,lty=2,col=fcol[p])
#   }
#   
#   #Harvest rate plot
#   plot(seq(log.dat$startyear,by=1,length=log.dat$years),
#        rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(Harvest_05),max(Harvest_50),max(Harvest_95))),
#        xlab="Year",ylab="Harvest")
# 
#   for (p in 1:length(f)){
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Harvest_50[,p],lwd=2,lty=1,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Harvest_05[,p],lwd=1,lty=2,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Harvest_95[,p],lwd=1,lty=2,col=fcol[p])
#   }
# 
#   #individual plot margin sizes
#   par(mar = c(2,4,2,2) + 0.1)
#   
#   #Catch plot
#   plot(seq(log.dat$startyear,by=1,length=log.dat$years),
#        rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(Catch_05),max(Catch_50),max(Catch_95))),
#        xlab="Year",ylab="Catch (kt)")
#   
#   for (p in 1:length(f)){
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Catch_50[,p],lwd=2,lty=1,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Catch_05[,p],lwd=1,lty=2,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),Catch_95[,p],lwd=1,lty=2,col=fcol[p])
#   }
#   
#   #SSB plot
#   plot(seq(log.dat$startyear,by=1,length=log.dat$years),
#        rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(SSB_05),max(SSB_50),max(SSB_95))),
#        xlab="Year",ylab="SSB (Mt)")
#   
#   for (p in 1:length(f)){
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),SSB_50[,p],lwd=2,lty=1,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),SSB_05[,p],lwd=1,lty=2,col=fcol[p])
#     lines(seq(log.dat$startyear,by=1,length=log.dat$years),SSB_95[,p],lwd=1,lty=2,col=fcol[p])
#   }
#   
#   #margin size in text line units
#   par(mar=c(0,0,0,0))
#   #empty plot
#   plot(1,1,type="n",frame.plot=FALSE,axes=FALSE)
#   u<-par("usr")
#   text(1,u[4],labels=ref,pos=1)
#   
#   dev.off()
#   
# }


fCalcMSE2014stats <- function(ref, fname, period, nits=NULL, ssbref=1.24e6){
  
  #nits - iterations on which to base calculation. If NULL then use all
  
  #calculate MSE2014 statistics over the statistical period, save in nominated file
  #the following stats are calculated
  #FBar
  #10th,50th and 90th percentiles of SSB
  #Mean SSB
  #Risk 1,2,3
  #10th, 50th and 90th percentiles of yield
  #Mean Yield
  #10th, 50th and 90th percentiles of recruitment
  #Mean recruitment
  #Yield per Recruitment
  #FailureRate - proportion of iterations failing (stock extinction, defined here as SSB (J1) falling below 1% 
  #of ssbref for the entire reporting period)

  #uses all op files for run ref
  
  if (missing(ref)) {ref<-winDialogString("Run Reference?","")}
  if (is.null(ref)) {stop("Cancelled\n")}
  if (nchar(ref)==0) stop("Exit")
  
  #write header
  if (!file.exists(fname)) {
  write(file=fname,
        c("Ref","TACRef","EggLim","EggGamma","From","To","Blim","FTAC",
          "SSBJ1_0.1","SSBJ1_0.5","SSBJ1_0.9","Mean SSBJ1","Rsk1_J1","Rsk2_J1","Rsk3_J1",
          "SSBST_0.1","SSBST_0.5","SSBST_0.9","Mean SSBST","Rsk1_ST","Rsk2_ST","Rsk3_ST",
          "Yld0.1","Yld0.5","Yld0.9","Mean Yld","Rec0.1","Rec0.5","Rec0.9","Rec","Y/R",
          "TACinc","TACdec","NoChg","AvgInc","AvgDec","FailRate"),
        ncolumns=37,
        sep="\t")
  }

  #read log.file to determine the varmin,varmax and resolution settings
  log.dat <- fread_log(ref)
  niter <- log.dat$nits
  nyear <- length(seq(period[1],period[2]))
  
  opfiles <- paste(log.dat$outdata.path,"FLRObjects_",ref,"_",seq(1,log.dat$resolution+1,by=1),".dat",sep="")
  
  if (is.null(nits)) {sel_nits <- seq(1:niter)} else {sel_nits <- sort(nits)}
  
  for (f in opfiles){
    
    load(f)
    
    #check statistical period is contained in FLR simulation output objects
    if ((period[1]>=dims(op.SSB.ST.true)$minyear)&(period[2]<=dims(op.SSB.ST.true)$maxyear)) {
      
      #select appropriate time slice
      op.SSB.J1.true <- window(op.SSB.J1.true,start=period[1],end=period[2])
      op.SSB.ST.true <- window(op.SSB.ST.true,start=period[1],end=period[2])
      op.CatchWeight <- window(op.CatchWeight,start=period[1],end=period[2])
      op.Recruits <- window(op.Recruits,start=period[1],end=period[2])
      op.FTac <- window(op.FTac,start=period[1],period[2])
      op.fbar <- window(op.fbar,start=period[1],period[2])
      
      #window objects
      lop <- lapply(list("SSB_J1_true" = op.SSB.J1.true, "SSB_ST_true" = op.SSB.ST.true, "CW" = op.CatchWeight,
                         "Rec" = op.Recruits, "FTAC" = op.FTac, "FBar" = op.fbar), window, start = period[1], end = period[2])
      
      #select iterations (if a subset of to be used as a basis for statistical calculations)
      #if (!is.null(nits)){
      #  lop <- lapply(lop,iter,nits)
      #}
      lop <- lapply(lop,iter,sel_nits)
      
      #and the appropriate iterations if a subset are identified in the function call
      #if (!is.null(nits)) {
      # op.SSB.J1.true <- iter(op.SSB.J1.true,nits)
      # op.SSB.ST.true <- iter(op.SSB.ST.true,nits)
      # op.CatchWeight <- iter(op.CatchWeight,nits)
      # op.Recruits <- iter(op.Recruits,nits)
      # op.FTac <- iter(op.FTac,nits)
      #}

      op.SSB.J1.true <- iter(op.SSB.J1.true,sel_nits)
      op.SSB.ST.true <- iter(op.SSB.ST.true,sel_nits)
      op.CatchWeight <- iter(op.CatchWeight,sel_nits)
      op.Recruits <- iter(op.Recruits,sel_nits)
      op.FTac <- iter(op.FTac,sel_nits)
      
      SSBJ1 <- as.vector(apply(quantSums(op.SSB.J1.true),c("iter"),FUN="mean")/1e6)
      SSBST <- as.vector(apply(quantSums(op.SSB.ST.true),c("iter"),FUN="mean")/1e6)
      Yld <- as.vector(apply(quantSums(op.CatchWeight),c("iter"),FUN="mean")/1e3)
      FailRate <- 100*(sum(apply((quantSums(op.SSB.J1.true))<(0.01*ssbref),c("iter"),sum)==nyear)/niter)
      
      qSSBJ1 <- quantile(SSBJ1,prob=c(0,0.1,0.5,0.9,1.0))
      qSSBST <- quantile(SSBST,prob=c(0,0.1,0.5,0.9,1.0))    
      qYld <- quantile(Yld,probs=c(0,0.1,0.5,0.9,1.0))
      
      #risk calculations
      Rsk1J1 <- 100*mean(apply(quantSums(lop$SSB_J1_true)<ssbref,c("year"),FUN="sum")/dim(op.SSB.J1.true)[6])
      Rsk1ST <- 100*mean(apply(quantSums(op.SSB.ST.true)<ssbref,c("year"),FUN="sum")/dim(op.SSB.J1.true)[6])
      
      Rsk2J1 <- 100*sum(apply(quantSums(lop$SSB_J1_true),c("iter"),FUN="min")<ssbref)/dim(op.SSB.J1.true)[6]
      Rsk2ST <- 100*sum(apply(quantSums(op.SSB.ST.true),c("iter"),FUN="min")<ssbref)/dim(op.SSB.J1.true)[6]
      
      Rsk3J1 <- max(100*apply(quantSums(lop$SSB_J1_true)<ssbref,2,sum)/dim(op.SSB.J1.true)[6])
      Rsk3ST <- max(100*apply(quantSums(op.SSB.ST.true)<ssbref,2,sum)/dim(op.SSB.J1.true)[6])
      
      #mean recruitment by iterations
      mnRec <- apply(op.Recruits,c("iter"),FUN="mean")
      qmnRec <- quantile(mnRec,probs=c(0,0.1,0.5,0.9,1.0))
      
      #number of TAC increases/decreases and the average change
      #this doesn't apply to reporting periods of only 1 year (period[1]==period[2])
      if (period[2] > period[1]){
        TACchg <- apply(lop$FTAC,c("iter"),diff)
        #increases
        inc <- mean(colSums(TACchg>0))
        #decreases
        dec <- mean(colSums(TACchg<0))
        #no change
        nochg <- mean(colSums(TACchg==0))
        #average increase
        TACchg[TACchg<=0] <- NA
        avginc <- mean(colMeans(TACchg,na.rm=TRUE),na.rm=TRUE)
        #average decrease
        TACchg <- apply(lop$FTAC,c("iter"),diff)
        TACchg[TACchg>=0] <- NA
        avgdec <- mean(colMeans(TACchg,na.rm=TRUE),na.rm=TRUE)
      } else {
        inc <- dec <- nochg <- avginc <- avgdec <- NA
      }
      
      write(file=fname,
            c(ref,log.dat$TACref,log.dat$egglimit,log.dat$egggamma,
              period[1],period[2],ssbref/1e6,
              mean(op.FTac),
              qSSBJ1[2],qSSBJ1[3],qSSBJ1[4],mean(SSBJ1),
              Rsk1J1,Rsk2J1,Rsk3J1,
              qSSBST[2],qSSBST[3],qSSBST[4],mean(SSBST),
              Rsk1ST,Rsk2ST,Rsk3ST,
              qYld[2],qYld[3],qYld[4],
              mean(Yld),
              qmnRec[2]/1000,qmnRec[3]/1000,qmnRec[4]/1000,
              mean(mnRec)/1000,
              mean(Yld)/(mean(op.Recruits)/1000),
              inc,dec,nochg,avginc,avgdec,FailRate),
            ncolumns=37,
            sep="\t",append=TRUE) 
    }
  }
}


fgenLatex <- function(opt_file) {

  #generate a latex table from stats files
  #reads stats files from stats directory, write .tex file into tables directory
  #directories/stat files need to exist - does not check!
  
  require(xtable)
  require(dplyr)
  
  infile <- paste0(".\\stats\\",opt_file,".dat")
  outfile <- paste0(".\\tables\\",opt_file,"_stats.tex")
  
  df <- read.table(infile,header=TRUE,sep="\t")
  
  #exclude annual stats
  df <- filter(df,!(From==To))

  #TAC variability as percentages, increases/decreases in kt
  df <- mutate(df,TACup = as.integer(round(100*(TACinc/(To-From)))))
  df <- mutate(df,TACdn = as.integer(round(100*(TACdec/(To-From)))))
  df <- mutate(df,Avgup = round(AvgInc/1000))
  df <- mutate(df,Avgdn = round(AvgDec/1000))
  
  df <- mutate(df,SSB = round(SSBST_0.5,2))
  df <- mutate(df,Yld = round(Yld0.5,2))
  df <- mutate(df,Risk1 = round(Rsk1_ST))
  df <- mutate(df,Risk2 = round(Rsk2_ST))
  df <- mutate(df,Risk3 = round(Rsk3_ST))
  
  #convert cols to characters (to append non numerics)
  df[] <- lapply(df,as.character)
  
  df <- mutate(df,TACu = paste0(TACup,"%"))
  df <- mutate(df,TACd = paste0(TACdn,"%"))
  df <- mutate(df,Rsk1 = paste0(Risk1,"%"))
  df <- mutate(df,Rsk2 = paste0(Risk2,"%"))
  df <- mutate(df,Rsk3 = paste0(Risk3,"%"))
  df <- mutate(df,FR = paste0(FailRate,"%"))
  
  df <- select(df,From,To,SSB,Yld,Rsk1,Rsk2,Rsk3,TACu,Avgup,TACd,Avgdn,FR)
  
  t <- xtable(df)
  
  print(t, type = "latex", file = outfile, include.rownames=FALSE, table.placement="H")
  
}

# fgencsv <- function(opt_file) {
#   
#   #generate a csv file from stats files
#   #reads stats files from stats directory
#   #directories/stat files need to exist - does not check!
#   
#   require(dplyr)
#   
#   infile <- paste0(".\\stats\\",opt_file,".dat")
#   cat(infile,"\n")
#   outfile <- ".\\tables\\MSE2014_stats.csv"
#   
#   df <- read.table(infile,header=TRUE,sep="\t")
#   
#   #exclude annual stats
#   df <- filter(df,!(From==To))
#   
#   #TAC variability as percentages, increases/decreases & TACref in kt
#   df <- mutate(df,TACup = as.integer(round(100*(TACinc/(To-From)))))
#   df <- mutate(df,TACdn = as.integer(round(100*(TACdec/(To-From)))))
#   df <- mutate(df,Avgup = round(AvgInc/1000))
#   df <- mutate(df,Avgdn = round(AvgDec/1000))
#   df <- mutate(df,TACRef = TACRef/1000)
#   
#   df <- mutate(df,SSB = round(SSBST_0.5,2))
#   df <- mutate(df,Yld = round(Yld0.5,2))
#   df <- mutate(df,Risk1 = round(Rsk1_ST))
#   df <- mutate(df,Risk2 = round(Rsk2_ST))
#   df <- mutate(df,Risk3 = round(Rsk3_ST))
#   
#   #convert cols to characters (to append non numerics)
#   df[] <- lapply(df,as.character)
#   
#   df <- mutate(df,TACu = paste0(TACup,"%"))
#   df <- mutate(df,TACd = paste0(TACdn,"%"))
#   df <- mutate(df,Rsk1 = paste0(Risk1,"%"))
#   df <- mutate(df,Rsk2 = paste0(Risk2,"%"))
#   df <- mutate(df,Rsk3 = paste0(Risk3,"%"))
#   df <- mutate(df,FR = paste0(FailRate,"%"))
#   
#   df <- select(df,Ref,TACRef,EggLim,EggGamma,From,To,SSB,Yld,Blim,Rsk1,Rsk2,Rsk3,TACu,Avgup,TACd,Avgdn,FR)
#   
#   write.table(df, 
#               file = outfile, 
#               append = file.exists(outfile), 
#               col.names = !file.exists(outfile),
#               row.names = FALSE,
#               sep = ",",
#               quote = FALSE)
#   
# }


