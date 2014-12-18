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
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(FPRESS.Home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.2,0,0))
  
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
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5))
  axis(2)
  
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
  mtext("(a) SSB (Mt)",side=3,adj=0)
  
  #confidence intervals (assumes normal distribution)
  #plot 95%/99% confidence lines?
  if(conf.int==1){
    lines(yr,mean.ssb-1.96*SD.ssb,lty=2)
    lines(yr,mean.ssb+1.96*SD.ssb,lty=2)
    lines(yr,mean.ssb-2.58*SD.ssb,lty=2)
    lines(yr,mean.ssb+2.58*SD.ssb,lty=2)
  }
  
  abline(h=0.634577,lty=2,col="red")
  
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
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(FPRESS.Home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
  #if (!missing(file)) {
  #  pdf(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""))
  #  #png(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""),width=480,height=480)
  #}
  
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.2,0,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,max(Yld),len=length(yr))
  plot(yr,ydum,type="n",xlab="Year",ylab="Yield (kt)",axes=FALSE,ylim=c(0,250))
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5))
  axis(2)
  
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
  mtext("(b) Yield (kt)",side=3,adj=0)
  
  #confidence intervals (assumes normal distribution)
  #plot 95%/99% confidence lines?
  if(conf.int==1){
    lines(yr,mean.yld-1.96*SD.yld,lty=2)
    lines(yr,mean.yld+1.96*SD.yld,lty=2)
    lines(yr,mean.yld-2.58*SD.yld,lty=2)
    lines(yr,mean.yld+2.58*SD.yld,lty=2)
  }
  
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
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(FPRESS.Home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
#   if (!missing(file)) {
#     pdf(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""))
#     #png(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""),width=480,height=480)
#   }
#   
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.2,0,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,0.2,len=length(yr))
  plot(yr,ydum,type="n",xlab="Year",ylab="FBar",axes=FALSE)
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5))
  axis(2)
  
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
  mtext("(c) FBar",side=3,adj=0)
  
  #confidence intervals (assumes normal distribution)
  #plot 95%/99% confidence lines?
  if(conf.int==1){
    lines(yr,mean.fbar-1.96*SD.fbar,lty=2)
    lines(yr,mean.fbar+1.96*SD.fbar,lty=2)
    lines(yr,mean.fbar-2.58*SD.fbar,lty=2)
    lines(yr,mean.fbar+2.58*SD.fbar,lty=2)
  }
  
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
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(FPRESS.Home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
#   if (!missing(file)) {
#     pdf(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""))
#     #png(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""),height=480,width=480)
#   }
  
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
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      } else if (ext=="png"){
        png(filename=paste(FPRESS.Home(),"\\plots\\",paste(file,"png",sep="."),sep=""))
      } else {
        #default is pdf is not otherwise coded
        pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
      }
    } else {
      #default is pdf
      pdf(file=paste(FPRESS.Home(),"\\plots\\",paste(file,"pdf",sep="."),sep=""))
    }
  } 
  
#   if (!missing(file)) {
#     pdf(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""))
#     #png(file=paste(FPRESS.Home(),"\\plots\\",file,sep=""),width=480,height=480)
#   }
  
  #outer margins
  par.omi <- par("omi")
  par(omi=c(0.2,0.2,0,0))
  
  #individual plot margin lines
  par.mar <- par("mar")
  par(mar=c(2,2,1,0))
  
  #empty plot
  yr<-seq(log$startyear,len=log$years)
  ydum<-seq(0,100,len=length(yr))
  plot(yr,ydum,type="n",xlab="Year",ylab="Risk",axes=FALSE)
  
  #axes
  axis(1,at=seq(from=5*ceiling(min(yr)/5),to=5*floor(max(yr)/5),by=5))
  axis(2)
  
  #annual risks
  lines(dat$From[dat$From==dat$To],dat$Rsk3_ST[dat$From==dat$To])
  
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
  mtext("(d) Risk",side=3,adj=0)
    
  if (!missing(file)) {dev.off()}
  
  #restore plotting defaults
  par(omi=par.omi)
  par(mar=par.mar)
  
}


f.PlotFSSB <- function(ref,sfile,pfile,title,subtitle,ylim="missing",xlim="missing"){
  
  #plot the SSB and Risk 3
  Model.res<-read.table(file=sfile,header=TRUE,sep="\t")
  
  plot.title <- title
  plot.subtitle <- subtitle
  
  names(Model.res) <- c("From","To","FBar","SSB0.1","SSB0.5","SSB0.9","MeanSSB","Rsk1","Rsk2","Rsk3","Yld0.1","Yld0.5","Yld0.9","MeanYld","Rec","YPR")
  
  pdf(file=pfile)
  
  par("oma"=c(1,0,0,2))
  
  plot(Model.res$FBar,Model.res$MeanSSB,type="n",col="green",
       xlab="FBar",ylab="SSB (Mt)",axes=FALSE,
       xlim=c(0,0.2),ylim=if (missing(ylim)) {c(0,5)} else {ylim},main=plot.title)
  
  mtext(plot.subtitle,side=3,line=0)
  mtext(ref,side=1,line=5)
  
  lines(Model.res$FBar,Model.res$SSB0.1,col="green",lty=2)
  lines(Model.res$FBar,Model.res$SSB0.5,col="green",lwd=2)
  lines(Model.res$FBar,Model.res$SSB0.9,col="green",lty=2)
  #abline(h=seq(0,5),col="light grey",lty=1)
  
  legend(0,if (missing(ylim)) {5} else {ylim[2]},
         c("Med SSB","10th,90th Pct SSB","Type 3 Risk (MSY Btrigger)"),
         lty=c(1,2,1),
         col=c("green","green","red"),
         lwd=c(2,1,1),
         bty="n")
  
  tck<-seq(0,0.2,by=0.02)
  lbl<-c("0.0","","","","","0.05","","","","","0.1","","","","","0.15","","","","","0.2")
  
  axis(side=1,
       at=tck)
  axis(side=2)
  
  par(new=TRUE)
  
  plot(Model.res$FBar,Model.res$Rsk3,type="l",col="red",axes=FALSE,
       xlim=c(0,0.2),ylim=c(0,100),ylab="",xlab="")
  
  #50% risk line
  abline(h=50,col="red",lty=2)
  
  axis(side=4)
  mtext("Risk 3",side=4,line=2)
  
  dev.off()
  
}


f.PlotFYld <- function(ref,sfile,pfile,title,subtitle,ylim="missing",xlim="missing"){
  
  #read statistics
  Model.res<-read.table(file=sfile,header=TRUE,sep="\t")
  
  plot.title <- title
  plot.subtitle <- subtitle
  
  names(Model.res) <- c("From","To","FBar","SSB0.1","SSB0.5","SSB0.9","MeanSSB","Rsk1","Rsk2","Rsk3","Yld0.1","Yld0.5","Yld0.9","MeanYld","Rec","YPR")
  
  pdf(file=pfile)
  
  par("oma"=c(1,0,0,2))
  
  plot(Model.res$FBar,Model.res$MeanYld,type="n",col="green",
       xlab="FBar",ylab="Yield (kt)",axes=FALSE,
       xlim=if (missing(xlim)) {c(0,0.2)} else {xlim},ylim=if (missing(ylim)) {c(0,400)} else {ylim},main=plot.title)
  
  mtext(plot.subtitle,side=3,line=0)
  mtext(ref,side=1,line=5)
  
  lines(Model.res$FBar,Model.res$Yld0.1,col="green",lty=2)
  lines(Model.res$FBar,Model.res$Yld0.5,col="green",lwd=2)
  lines(Model.res$FBar,Model.res$Yld0.9,col="green",lty=2)
  
  legend(0,if (missing(ylim)) {400} else {ylim[2]},
         c("Med Yield","10th,90th Pct Yield","Type 3 Risk (MSY Btrigger)"),
         lty=c(1,2,1),
         col=c("green","green","red"),
         lwd=c(2,1,1),
         bty="n")
  
  tck<-seq(0,0.2,by=0.02)
  lbl<-c("0.0","","","","","0.05","","","","","0.1","","","","","0.15","","","","","0.2")
  
  axis(side=1,
       at=tck)
  axis(side=2)
  
  par(new=TRUE)
  
  plot(Model.res$FBar,Model.res$Rsk3,type="l",col="red",axes=FALSE,
       xlim=if (missing(xlim)) {c(0,0.2)} else {xlim},ylim=c(0,100),ylab="",xlab="")
  
  axis(side=4)
  mtext("Risk 3",side=4,line=2)
  abline(h=50,col="red",lty=2,lwd=1)
  lines(Model.res$FBar,Model.res$Rsk3,col="red")  

  dev.off()
  
}


#FLR comparison
f.FLRCompPlot <- function(ref,fname,f=c(0,0.05,0.1),fcol=c("black","red","blue")) {
  
  #makes a plot for comparison with FLR output.
  #Recruitment, Harvest Rate, Catch, SSB
  
  #ref - the run reference
  #f - vector of f vals to plot
  #fcol - vector of colours for f vals
  
  if (missing(ref)) {ref<-winDialogString("Run Reference?","")}
  if (is.null(ref)) {stop("Cancelled\n")}
  if (nchar(ref)==0) stop("Exit")

  #determine which data files need to be read in
  #they are post fixed with a number as the simulation progresses through the f or TAC values

  #read log.file to determine the varmin,varmax and resolution settings
  log.dat <- read.log(ref)

  #variable values
  varVals <- seq(log.dat$varmin,log.dat$varmax,length.out = log.dat$resolution + 1)
  
  #file numbers to be read in
  fnum <- as.integer(match(f,round(varVals,6)))
  
  opfiles <- paste(log.dat$outdata.path,"FLRObjects_",ref,"_",fnum,".dat",sep="")

  Rec_05 <- Rec_50 <- Rec_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
  Harvest_05 <- Harvest_50 <- Harvest_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
  Catch_05 <- Catch_50 <- Catch_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
  SSB_05 <- SSB_50 <- SSB_95 <- matrix(data = NA, nrow = log.dat$years, ncol = length(f))
  
  for (fl in 1:length(opfiles)){
  
    load(opfiles[fl])

    cat(opfiles[fl],"\n")
    
    Rec_05[,fl]<-apply(op.Recruits/1e6,"year",FUN="quantile",probs=c(0.05))
    Rec_50[,fl]<-apply(op.Recruits/1e6,"year",FUN="median")
    Rec_95[,fl]<-apply(op.Recruits/1e6,"year",FUN="quantile",probs=c(0.95))

    Harvest_05[,fl]<-apply(op.FBar,"year",FUN="quantile",probs=c(0.05))
    Harvest_50[,fl]<-apply(op.FBar,"year",FUN="median")
    Harvest_95[,fl]<-apply(op.FBar,"year",FUN="quantile",probs=c(0.95))

    Catch_05[,fl]<-apply(quantSums(op.CatchWeight)/1e3,"year",FUN="quantile",probs=c(0.05))
    Catch_50[,fl]<-apply(quantSums(op.CatchWeight)/1e3,"year",FUN="median")
    Catch_95[,fl]<-apply(quantSums(op.CatchWeight)/1e3,"year",FUN="quantile",probs=c(0.95))

    SSB_05[,fl]<-apply(quantSums(op.SSB.J1.true)/1e6,"year",FUN="quantile",probs=c(0.05))
    SSB_50[,fl]<-apply(quantSums(op.SSB.J1.true)/1e6,"year",FUN="median")
    SSB_95[,fl]<-apply(quantSums(op.SSB.J1.true)/1e6,"year",FUN="quantile",probs=c(0.95))
    
  }
  
  pdf(fname)
  
  m <- matrix(c(1,2,3,4,5,5),ncol=2,byrow=TRUE)
  layout(m,widths=c(0.5,0.5),heights=c(0.475,0.475,0.05))
  
  #outer margin limits in text lines
  par("oma"=c(0,0,0,1))
  
  #individual plot margin sizes
  par(mar = c(2,4,4,2) + 0.1)
  
  #Recruits plot
  plot(seq(log.dat$startyear,by=1,length=log.dat$years),
       rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(Rec_05),max(Rec_50),max(Rec_95))),
       xlab="Year",ylab="Recruits (x10^6)")
  
  for (p in 1:length(f)){
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Rec_50[,p],lwd=2,lty=1,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Rec_05[,p],lwd=1,lty=2,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Rec_95[,p],lwd=1,lty=2,col=fcol[p])
  }
  
  #Harvest rate plot
  plot(seq(log.dat$startyear,by=1,length=log.dat$years),
       rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(Harvest_05),max(Harvest_50),max(Harvest_95))),
       xlab="Year",ylab="Harvest")

  for (p in 1:length(f)){
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Harvest_50[,p],lwd=2,lty=1,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Harvest_05[,p],lwd=1,lty=2,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Harvest_95[,p],lwd=1,lty=2,col=fcol[p])
  }

  #individual plot margin sizes
  par(mar = c(2,4,2,2) + 0.1)
  
  #Catch plot
  plot(seq(log.dat$startyear,by=1,length=log.dat$years),
       rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(Catch_05),max(Catch_50),max(Catch_95))),
       xlab="Year",ylab="Catch (kt)")
  
  for (p in 1:length(f)){
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Catch_50[,p],lwd=2,lty=1,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Catch_05[,p],lwd=1,lty=2,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),Catch_95[,p],lwd=1,lty=2,col=fcol[p])
  }
  
  #SSB plot
  plot(seq(log.dat$startyear,by=1,length=log.dat$years),
       rep(0,length=log.dat$years),type="n",ylim=c(0,max(max(SSB_05),max(SSB_50),max(SSB_95))),
       xlab="Year",ylab="SSB (Mt)")
  
  for (p in 1:length(f)){
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),SSB_50[,p],lwd=2,lty=1,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),SSB_05[,p],lwd=1,lty=2,col=fcol[p])
    lines(seq(log.dat$startyear,by=1,length=log.dat$years),SSB_95[,p],lwd=1,lty=2,col=fcol[p])
  }
  
  #margin size in text line units
  par(mar=c(0,0,0,0))
  #empty plot
  plot(1,1,type="n",frame.plot=FALSE,axes=FALSE)
  u<-par("usr")
  text(1,u[4],labels=ref,pos=1)
  
  dev.off()
  
}


fCalc_MSE2014_stats <- function(ref,fname,period,nits=NULL,ssbref=1.24e6){
  
  #nits - iterations on which to base calculation. If NULL then use all
  #if this number is less than the number of simulation iterations
  #then this number are randomly selected (replace=FALSE)
  
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

  #uses all op files for run ref
  
  if (missing(ref)) {ref<-winDialogString("Run Reference?","")}
  if (is.null(ref)) {stop("Cancelled\n")}
  if (nchar(ref)==0) stop("Exit")
  
  #write header
  if (!file.exists(fname)) {
  write(file=fname,
        c("From","To","Blim","FTAC",
          "SSBJ1_0.1","SSBJ1_0.5","SSBJ1_0.9","Mean SSBJ1","Rsk1_J1","Rsk2_J1","Rsk3_J1",
          "SSBST_0.1","SSBST_0.5","SSBST_0.9","Mean SSBST","Rsk1_ST","Rsk2_ST","Rsk3_ST",
          "Yld0.1","Yld0.5","Yld0.9","Mean Yld","Rec0.1","Rec0.5","Rec0.9","Rec","Y/R",
          "TACinc","TACdec","NoChg","AvgInc","AvgDec"),
        ncolumns=32,
        sep="\t")
  }

  #read log.file to determine the varmin,varmax and resolution settings
  log.dat <- fread_log(ref)
  niter <- log.dat$nits
  nyear <- length(seq(period[1],period[2]))
  
  opfiles <- paste(log.dat$outdata.path,"FLRObjects_",ref,"_",seq(1,log.dat$resolution+1,by=1),".dat",sep="")
  
  if (is.null(nits)) {sel_nits <- seq(1:niter)} else {sel_nits <- nits}
  
  cat(period[1],period[2],"\n")
  
  for (f in opfiles){
    
    cat(f,"\n")
    
    load(f)
    
    #select appropriate time slice
    #op.SSB.J1.true <- window(op.SSB.J1.true,start=period[1],end=period[2])
    op.SSB.ST.true <- window(op.SSB.ST.true,start=period[1],end=period[2])
    op.CatchWeight <- window(op.CatchWeight,start=period[1],end=period[2])
    op.Recruits <- window(op.Recruits,start=period[1],end=period[2])
    op.FTac <- window(op.FTac,start=period[1],period[2])
    op.fbar <- window(op.fbar,start=period[1],period[2])
    
    #window objects
    lop <- lapply(list("SSB_J1_true" = op.SSB.J1.true, "SSB_ST_true" = op.SSB.ST.true, "CW" = op.CatchWeight,
                       "Rec" = op.Recruits, "FTAC" = op.FTac, "FBar" = op.fbar), window, start = period[1], end = period[2])
    
    #select iterations (if a subset of to be used as a basis for statistical calculations)
    if (!is.null(nits)){
      lop <- lapply(lop,iter,nits)
    }

    #and the appropriate iterations if a subset are identified in the function call
    if (!is.null(nits)) {
     #op.SSB.J1.true <- iter(op.SSB.J1.true,nits)
     op.SSB.ST.true <- iter(op.SSB.ST.true,nits)
     op.CatchWeight <- iter(op.CatchWeight,nits)
     op.Recruits <- iter(op.Recruits,nits)
     op.FTac <- iter(op.FTac,nits)
    }
    
    SSBJ1 <- as.vector(apply(quantSums(lop$SSB_J1_true),c("iter"),FUN="mean")/1e6)
    SSBST <- as.vector(apply(quantSums(op.SSB.ST.true),c("iter"),FUN="mean")/1e6)
    Yld <- as.vector(apply(quantSums(op.CatchWeight),c("iter"),FUN="mean")/1e3)
    
    qSSBJ1<-quantile(SSBJ1,prob=c(0,0.1,0.5,0.9,1.0))
    qSSBST<-quantile(SSBST,prob=c(0,0.1,0.5,0.9,1.0))    
    qYld<-quantile(Yld,probs=c(0,0.1,0.5,0.9,1.0))
    
    #risk calculations
    Rsk1J1<-100*mean(apply(quantSums(lop$SSB_J1_true)<ssbref,c("year"),FUN="sum")/dim(op.SSB.J1.true)[6])
    Rsk1ST<-100*mean(apply(quantSums(op.SSB.ST.true)<ssbref,c("year"),FUN="sum")/dim(op.SSB.J1.true)[6])
    
    Rsk2J1<-100*sum(apply(quantSums(lop$SSB_J1_true),c("iter"),FUN="min")<ssbref)/dim(op.SSB.J1.true)[6]
    Rsk2ST<-100*sum(apply(quantSums(op.SSB.ST.true),c("iter"),FUN="min")<ssbref)/dim(op.SSB.J1.true)[6]
    
    Rsk3J1<-max(100*apply(quantSums(lop$SSB_J1_true)<ssbref,2,sum)/dim(op.SSB.J1.true)[6])
    Rsk3ST<-max(100*apply(quantSums(op.SSB.ST.true)<ssbref,2,sum)/dim(op.SSB.J1.true)[6])
    
    #mean recruitment by iterations
    mnRec <- apply(op.Recruits,c("iter"),FUN="mean")
    qmnRec <- quantile(mnRec,probs=c(0,0.1,0.5,0.9,1.0))
    
    #number of TAC increases/decreases and the average change
    #this doesn't apply to reporting periods of only 1 year (period[1]==period[2])
    if (period[2]>period[1]){
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
          c(period[1],period[2],ssbref/1e6,
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
            inc,dec,nochg,avginc,avgdec),
          ncolumns=32,
          sep="\t",append=TRUE) 
  }
  
}


fgen_Latex <- function(opt_file) {

  #generate a latex table from stats files
  #reads stats files from stats directory, write .tex file into tables directory
  #directories/stat files need to exist - does not check!
  
  require(xtable)
  require(dplyr)
  
  infile <- paste0(".\\stats\\",opt_file,".dat")
  outfile <- paste0(".\\tables\\",opt_file,"_stats.tex")
  
  df <- read.table(infile,header=TRUE,sep="\t")
  
  #exclude annual stats
  df <- select(filter(df,!(From==To)))

  #TAC variability as percentages, increases/decreases in kt
  df <- mutate(df,TACup = as.integer(round(100*(TACinc/(To-From)))))
  df <- mutate(df,TACdn = as.integer(round(100*(TACdec/(To-From)))))
  df <- mutate(df,Avgup = round(AvgInc/1000))
  df <- mutate(df,Avgdn = round(AvgDec/1000))
  
  df <- mutate(df,SSB = round(SSBST_0.5,2))
  df <- mutate(df,Yld = round(Yld0.5,2))
  df <- mutate(df,Risk = round(Rsk3_ST))
  
  #convert cols to characters (to append non numerics)
  df[] <- lapply(df,as.character)
  
  df <- mutate(df,TACu = paste0(TACup,"%"))
  df <- mutate(df,TACd = paste0(TACdn,"%"))
  df <- mutate(df,Rsk = paste0(Risk,"%"))
  
  df <- select(df,From,To,SSB,Yld,Rsk,TACu,Avgup,TACd,Avgdn)
  
  t <- xtable(df)
  
  print(t, type = "latex", file = outfile, include.rownames=FALSE)
  
}

