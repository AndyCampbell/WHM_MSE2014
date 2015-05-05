#eggcalc.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#V2 - consolidated 07/10/2008
#Sys.putenv("V:eggcalc.r"="2.0")
#Sys.setenv("V:eggcalc.r"="2.0")

#assign the environment var V:eggcalc.r
fset_env_var("V:eggcalc.r","2.0")

# eggcalc<-function(SSB,SSBW,q,b,CVegg,Lambda,debug=0) {
# 
#   if(debug==1){
#     cat("SSB=",SSB,"\n")
#   }
#   
#    #calculate 'true' egg abundance
# 
#    EGG.true <- (q*SSB + b*SSBW)/2000	  #what's this for?
#    EGG.true <- EGG.true*exp(Lambda*qnorm(runif(1)))
# 
#    #incorporate sampling cv
#    EGG.obs <- EGG.true*exp(CVegg*qnorm(runif(1)))
# 
#    return(EGG.obs)
# 
# }
# 
# eggcalc_wkwhmmp2<-function(SSB,wgt,aFec,bFec,qFec,segg,sex.ratio,debug=0){
# 
# 	#SSB - spawning stock biomass (true)
# 	#wgt - stock weights
# 	#aFec,bFec - fecundity parameters
# 	#qFec - realised fecundity parameter
# 	#segg - egg standard deviation
# 	#sex.ratio - male/female ratio
#   
# 	EGG.true <- sum(qFec*(aFec + bFec*wgt)*SSB*sex.ratio)/1000
#   
#   EGG.obs <- EGG.true*exp(segg*rnorm(n=1,mean=0,sd=1))
#   
# }

#eggcalc_mse2014<-function(log.file,SSB,wgt,EggModel,SexRatio=0.5,year,debug=0){
feggcalc_mse2014<-function(ssb,wgt,EggModel,SexRatio=0.5,year){
    
  #ssb - spawning stock biomass (true)
  #wgt - stock weights
  #EggModel - egg model
  #sex.ratio - male/female ratio
  #year - current simulation year
  
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(SSB,wgt,EggModel,SexRatio,year,debug))
#   }
  
  #cat(year,EggModel$qFec,EggModel$aFec,EggModel$bFec,EggModel$ResidFut,"\n")
  
  EGG.true <- sum(EggModel$qFec*(EggModel$aFec + EggModel$bFec*wgt)*ssb*SexRatio)/1000
  
  #cat("EGG.true=",EGG.true,"\n")
  
  EGG.obs <- EGG.true*exp(EggModel$ResidFut[[as.character(year)]])
  
  #cat("Resid=",EggModel$ResidFut[[as.character(year)]],"\n")
  
  ret<-c(EGG.obs,EggModel$ResidFut[[as.character(year)]],EGG.true)
  
#  if(debug==1){log.write.func.exit(log.file,ret)}
  
  ret
  
}

