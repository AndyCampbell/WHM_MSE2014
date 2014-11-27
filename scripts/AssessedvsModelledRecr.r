#assessed vs modelled recruitment investigations
#recap
#27/11/2014

#copied from sandbox script

#clean up
rm(list=ls())
gc()

#need SADutils.r
source(".//source//SADutils.r")

#observed Stock and Recruit values
SRpairs = read.table(".\\indata\\SADMSE2014_SRPairs_15_09_2014.dat",header=TRUE,sep=",")
datYrs <- sprintf("%02d", seq(1982,2012))
noSpikeYrs <- sprintf("%02d", c(seq(1983,2000),seq(2002,2012)))
residYrs <- sprintf("%02d",seq(2013,length=length(datYrs)))

#Stock Recruit Models
nits <- 1000  #1000 models
years <- 100  #generate 100 years of future residuals
startyear <- 2014
indata <- read.table(".\\indata\\SADMSE2014_WHMParams_15_09_2014.dat",header=TRUE,sep=",")
SR.types<-f.RandSR(nits=nits)
lSR<-lapply(seq(1:nits),FUN=f.SADSR,nits=nits,SADparams=indata,SRpairs=SRpairs,SR.types=SR.types,startyear=startyear,years=years)

#initialize some vectors
rec.obs<-c()
ssb.obs<-c()
#using FLR framework
rec.mod.FLR<-c()
#using FPRESS approach
rec.mod.FPRESS<-c()

#for FPRESS approach we need the function used to draw recruitment
#the function that generates recruitment, copy of recruit34 from recruit.r but vectorised
f.vrecruit34 <- function(log.file,ssb,SRModel,year,debug=0){
  
  #residual
  eta <- SRModel$Resids[as.character(year)]
  
  #initialise return vectors
  #recruitment value
  recr <- vector("numeric",length=length(year))
  #deterministic value
  E <- vector("numeric",length=length(year))
  
  #apply appropriate recruitment formulation
  E = switch(substr(SRModel$model,0,2),
             HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
             RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
             BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
  )
  
  #apply residual
  recr <- E*exp(eta)
  
  #no ssb, no recruitment
  recr[ssb==0.001] <- 0
  E[ssb==0.001] <- 0
  
  ret <- list(recr=recr,eta=eta,E=E)
  
  ret
  
}


#for FLR we need a function for smooth hockey stick model for use in FLSR (Ricker and B&H already available)
myHS <- function(){

  formula <- rec ~ a*(ssb + sqrt(b^2+0.25*g^2) - sqrt((ssb-b)^2+0.25*g^2))
  
  logl <- function(a,b,g,rec,ssb){
    loglAR1(log(rec),log(a*(ssb + sqrt(b^2+0.25*g^2) - sqrt((ssb-b)^2+0.25*g^2))))
  }
  
  initial <- structure(function(rec,ssb){
    a <- 1
    b <- 1
    g <- 1
    
    return(list(a=a,b=b,g=g))
  })
  
  return(list(model = formula,
              initial = initial,
              logl = logl))
}


#perform iterations
for (i in seq(1,nits)){
 
  #observed values
  rec.obs <- c(rec.obs,as.vector(unlist(lSR[[i]]$HistRec[names(lSR[[i]]$HistRec)%in%noSpikeYrs])))
  ssb.obs <- c(ssb.obs,as.vector(unlist(lSR[[i]]$HistSSB[names(lSR[[i]]$HistSSB)%in%noSpikeYrs])))
  
  #create FLSR object
  
  #FLQuants for rec,ssb and residuals
  rec <- FLQuant(as.vector(unlist(lSR[[i]]$HistRec[names(lSR[[i]]$HistRec)%in%datYrs])))
  ssb <- FLQuant(as.vector(unlist(lSR[[i]]$HistSSB[names(lSR[[i]]$HistSSB)%in%datYrs])))
  resids <- FLQuant(as.vector(unlist(lSR[[i]]$Resids[names(lSR[[i]]$Resids)%in%residYrs])))
  
  quant(rec) <- "age"
  quant(ssb) <- "age"
  
  units(rec) <- "10^3"
  units(ssb) <- "t*10^3"
  
  dimnames(rec)$year <- datYrs
  dimnames(ssb)$year <- datYrs
  dimnames(resids)$year <- datYrs
  
  #create the FLSR object
  
  if (lSR[[i]]$FLSRmodel=="segreg") {
    
    sr <- FLSR(model = myHS,rec=rec,ssb=ssb)
    pars <- FLPar(c(lSR[[i]]$AParam,lSR[[i]]$BParam,lSR[[i]]$GParam),c("a","b","g"))
    params(sr) <- pars
    
    #get fmle to fit the model but specify the parameters to use
    sr <- fmle(sr,fixed=list(a=params(sr)[1],b=params(sr)[2],g=params(sr)[3]))
    
  } else {
    
    sr <- FLSR(model=lSR[[i]]$FLSRmodel,rec=rec,ssb=ssb)
    pars <- FLPar(c(lSR[[i]]$AParam,lSR[[i]]$BParam),c("a","b"))
    params(sr) <- pars
    
    #get fmle to fit the model but specify the parameters to use
    sr <- fmle(sr,fixed=list(a=params(sr)[1],b=params(sr)[2]))
  }
  
  #FLR calculation
  rec.mod.FLR <- c(rec.mod.FLR,(fitted(sr)*exp(resids))[,!dimnames(rec(sr))$year%in%c("1982","2001")])
  
  #FPRESS function
  rec.mod.FPRESS <- c(rec.mod.FPRESS,f.vrecruit34(log.file="",ssb=as.vector(unlist(lSR[[i]]$HistSSB[names(lSR[[i]]$HistSSB)%in%noSpikeYrs])),
                                      SRModel=lSR[[i]],year=as.numeric(noSpikeYrs)+31,debug=0)[[1]])  
}

#plot the results
png(filename=".\\plots\\Recr_ECDF_FLR.png",width=12,height=5,units="in",res=72)

par(mfrow=c(1,2))
plot(rec.mod.FLR~ssb.obs,pch=20,col="black",ylim=c(0,max(rec.mod.FLR)),xlim=c(0,max(ssb.obs)),
     xlab="ssb",ylab="rec",main=c("Assessed vs modelled recruitment"))
points(rec.obs~ssb.obs,pch=20,col="red")

plot(ecdf(c(rec.mod.FLR)),pch="o",xlim=c(0,max(rec.mod.FLR)),main="Cumulative recruitment",xlab="rec",ylab="Empirical Cumulative Distribution")
par(new=T)
plot(ecdf(c(rec.obs)),pch="o",col="red",xlim=c(0,max(rec.mod.FLR)),main="",xlab="",ylab="")

dev.off()


png(filename=".\\plots\\Recr_ECDF_FPRESS.png",width=12,height=5,units="in",res=72)

par(mfrow=c(1,2))
plot(rec.mod.FPRESS~ssb.obs,pch=20,col="black",ylim=c(0,max(rec.mod.FPRESS)),xlim=c(0,max(ssb.obs)),
     xlab="ssb",ylab="rec",main=c("Assessed vs modelled recruitment"))
points(rec.obs~ssb.obs,pch=20,col="red")

plot(ecdf(c(rec.mod.FPRESS)),pch="o",xlim=c(0,max(rec.mod.FPRESS)),main="Cumulative recruitment",xlab="rec",ylab="Empirical Cumulative Distribution")
par(new=T)
plot(ecdf(c(rec.obs)),pch="o",col="red",xlim=c(0,max(rec.mod.FPRESS)),main="",xlab="",ylab="")

dev.off()


