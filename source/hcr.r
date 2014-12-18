#hcr.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#HCR functions
#V2 - consolidated 07/10/2008
#Sys.putenv("V:hcr.r"="2.0")
#Sys.setenv("V:hcr.r"="2.0")

#assign the environment var V:hcr.r
set.env.var("V:hcr.r","2.0")

############################################################ 
# HCR rule 1
# This function compares the simulated assessment estimate of SSB (oldssb) to 
# a fixed reference level ssbhcr. If oldssb > ssbhcr then a value of hcrmult = 1 
# is returned (virtual fishery is unchanged). If oldssb < ssbhcr then a value of 
# hcrmult = oldssb / ssbhcr (will always be <1) is returned (resulting in a 
# reduction in the virtual fishery relative to how far below the reference level 
# the estimate of the virtual SSB is).

# hcr1<-function(oldssb,ssbhcr){
# 
#    #check arguments supplied are numeric
#    if (!is.numeric(oldssb)) stop ("hcr1::argument oldssb must be numeric\n")
#    if (!is.numeric(ssbhcr)) stop ("hcr1::argument ssbhcr must be numeric\n")
#    if (oldssb<0) stop ("hcr1::argument oldssb must have a value greater than zero\n")
#    if (ssbhcr<=0) stop ("hcr1::argument ssbhcr must have a value greater than zero\n")
# 
#    #default value
#    hcrmulty <- 1
# 
#    #if ssb is less than ssbhcr, set the multiplier to oldssb/ssbhcr
#    if (oldssb < ssbhcr) {
#       hcrmulty <- oldssb/ssbhcr
#    }
# 
#    return(c(hcrmulty,1))
# 
# }

############################################################ 
# HCR rule 2
# This function compares the simulated assessment estimate of SSB (oldssb) to 
# a fixed reference level ssbhcr. If oldssb > ssbhcr then a value of hcrmult = 1 
# is returned. If oldssb < ssbhcr then a value of hcrmult = 0 is returned 

# hcr2<-function(oldssb,ssbhcr){
# 
#    #check arguments supplied are numeric
#    if (!is.numeric(oldssb)) stop ("hcr2::argument oldssb must be numeric\n")
#    if (!is.numeric(ssbhcr)) stop ("hcr2::argument ssbhcr must be numeric\n")
#    if (oldssb<0) stop ("hcr2::argument oldssb must have a value greater than zero\n")
#    if (ssbhcr<0) stop ("hcr2::argument ssbhcr must have a value greater than zero\n")
# 
#    #default value
#    hcrmulty<-1
# 
#    #if ssb is less than ssbhcr, set the multiplier to 0 to close the fishery
#    if (oldssb<ssbhcr) {
#       hcrmulty<-0
#    }
# 
#    return(c(hcrmulty,1))
#   
# }

############################################################ 
# HCR rule 3
# This function compares the simulated assessment estimate of SSB (oldssb) to 
# a fixed reference level ssbhcr. A value of hcrmult = oldssb/ssbhcr is always 
# saved and returned (resulting in an increase or reduction in the virtual fishery 
# depending on whether oldssb > ssbhcr).

# hcr3<-function(oldssb,ssbhcr){
# 
#    #check arguments supplied are numeric
#    if (!is.numeric(oldssb)) stop ("hcr3::argument oldssb must be numeric\n")
#    if (!is.numeric(ssbhcr)) stop ("hcr3::argument ssbhcr must be numeric\n")
#    if (oldssb<0) stop ("hcr3::argument oldssb must have a value greater than or equal to zero\n")
#    if (ssbhcr<=0) stop ("hcr3::argument ssbhcr must have a value greater than zero\n")
# 
#    hcrmulty<-oldssb/ssbhcr
# 
#    return(c(hcrmulty,1))
# 
# }

############################################################ 
# HCR rule 4
# This function compares the simulated assessment estimate of (oldfbar) to a 
# fixed reference level fbarhcr. A value of hcrmult = fbarhcr/oldfbar is
# returned (resulting in an increase or reduction in the virtual fishery 
# depending on whether oldfbar < fbarhcr).

# hcr4<-function(oldfbar,fbarhcr){
# 
#    #check arguments supplied are numeric
#    if (!is.numeric(oldfbar)) stop ("hcr4::argument oldfbar must be numeric\n")
#    if (!is.numeric(fbarhcr)) stop ("hcr4::argument fbarhcr must be numeric\n")
#    if (oldfbar<0) stop ("hcr4::argument oldfbar must have a value greater than zero\n")
#    if (fbarhcr<0) stop ("hcr4::argument fbarhcr must have a value greater than zero\n")
# 
#    #default value
#    hcrmulty<-1
# 
#    if (oldfbar>fbarhcr) {
#       hcrmulty<-fbarhcr/oldfbar
#    }
# 
#    return(c(hcrmulty,1))
# 
# }

############################################################ 
# HCR rule 5
# if the new fbar exceeds that given in the fixed reference fbarhcr the
# fishery is closed

# hcr5<-function(oldfbar,fbarhcr){
# 
#    #check arguments supplied are numeric
#    if (!is.numeric(oldfbar)) stop ("hcr5::argument oldfbar must be numeric\n")
#    if (!is.numeric(fbarhcr)) stop ("hcr5::argument fbarhcr must be numeric\n")
#    if (oldfbar<0) stop ("hcr5::argument oldfbar must have a value greater than zero\n")
#    if (fbarhcr<0) stop ("hcr5::argument fbarhcr must have a value greater than zero\n")
# 
#    #default value
#    hcrmulty<-1
# 
#    if (oldfbar>fbarhcr) {
#       hcrmulty<-0
#    }
# 
#    return(c(hcrmulty,1))
# 
# }

############################################################ 
# HCR rule 6
# the multiplier is set to the ratio of the current fbar to the reference
# fbar, regardless of value

# hcr6<-function(oldfbar,fbarhcr){
# 
#    #check arguments supplied are numeric
#    if (!is.numeric(oldfbar)) stop ("hcr6::argument oldfbar must be numeric\n")
#    if (!is.numeric(fbarhcr)) stop ("hcr6::argument fbarhcr must be numeric\n")
#    if (oldfbar<1) stop ("hcr6::argument oldfbar must have a value greater than zero\n")
#    if (fbarhcr<0) stop ("hcr6::argument fbarhcr must have a value greater than or equal to zero\n")
# 
#    hcrmulty<-fbarhcr/oldfbar
# 
#    return(c(hcrmulty,1))
#   
# }

###########################################################
# HCR rule 7
# different action taken depending in which range SSB sits
# SSB > Bpa            - tac set to constant catch value
# Blim <= SSB <= Bpa   - tac adjusted by SSB/Bpa
# SSB < Blim           - fishery is closed

# hcr7<-function(SSB,Blim,Bpa){
# 
#    #check arguments supplied are numeric
#    if (!is.numeric(SSB)) stop ("hcr7::argument SSB must be numeric\n")
#    if (!is.numeric(Blim)) stop ("hcr7::argument Blim must be numeric\n")
#    if (!is.numeric(Bpa)) stop ("hcr7::argument Bpa must be numeric\n")
# 
#    #Blim needs to be less than or equal to Bpa
#    if (Blim > Bpa) stop ("hcr7::Blim must be less than or equal to Bpa")
#   
#    #all values should be greater than or equal to zero
#    if (SSB < 0) stop ("hcr7::argument SSB must have a value greater than or equal to zero\n")
#    if (Blim < 0) stop ("hcr7::argument Blim must have a value greater than or equal to zero\n")
#    if (Bpa < 0) stop ("hcr7::argument Bpa must have a value greater than or equal to zero\n")
# 
#    if (SSB < Blim) {
#       hcrmulty <- c(0,1)
#    }
# 
#    if ((SSB >= Blim) & (SSB <= Bpa)) {
#       if (Bpa > 0) {
# 	 hcrmulty <- c(SSB/Bpa,2)
#       } else {
# 	 hcrmulty <- c(0,2)
#       }
#    }
# 
#    if(SSB > Bpa){
#       hcrmulty <- 1 + ((SSB-Bpa)/Bpa)
#       hcrmulty <- c(hcrmulty,3)
#    }
# 
#    return(hcrmulty)
# 
# }

###########################################################
# USER DEFINED HCR rules
#HCR rule 8
# hcr8<-function(SSB,SSBtrig,SSBlim,TAC,maxTAC) {
# 
#    hcrmulty<-1
# 
#    if (SSB>SSBtrig) {
# 
#       #SSB above SSBtrig (by definition also above SSBlim)
#       hcrmulty <- SSB/SSBtrig
# 
#    } else {
# 
#       #SSB equal to or below SSBtrig
# 
#       if (SSB>SSBlim) {
# 
#          #SSB above SSBlim
#          hcrmulty <- SSB/SSBtrig
# 
#       } else {
# 
#          #SSB equal to or below SSBlim
# 	 hcrmulty <- TAC^(SSB/SSBlim)/TAC
# 
#       }
# 
#    }
# 
#    #check if maximum TAC will be breached by the multiplier
#    #and if so modify the multiplier in order that the TAC stays within the limit
#    if (TAC*hcrmulty > maxTAC) {hcrmulty <- maxTAC/TAC}
# 
#    return(hcrmulty)
# 
# }

#HCR rule 9
# #hcr9<-function(SSB,SSBtrig,SSBlim,TAC,maxTAC) {
# hcr9<-function(SSB,SSBtrig,TAC,maxTAC) {
# 
#    hcrmulty<-1
# 
#    #SSB above SSBtrig (by definition also above SSBlim)
#    hcrmulty <- SSB/SSBtrig
# 
#    #check if maximum TAC will be breached by the multiplier
#    #and if so modify th multiplier in order that the TAC stays within the limit
#    if (TAC*hcrmulty > maxTAC) {hcrmulty <- maxTAC/TAC}
# 
#    return(hcrmulty)
# 
# }

#HCR rule 10
# hcr10<-function(log.file,simyear,beta,w,TACref,TAC,slope,debug=0) {
# 
#   #HCR based on hybrid fixed TAC and egg slope
#   #simyear - the current simulation year
#   #beta - HCR beta parameter
#   #w - HCR omega parameter
#   #TACref - HCR reference TAC
#   #TAC - current TAC
#   #slope - HCR slope factor, as determined by the previous egg surveys
#   
#   #returns a multiplier to be applied to the existing TAC
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(simyear,beta,w,TACref,TAC,slope,debug))
#   }
#     
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
# 
#   if (TAC==0) {
#     ret <- 0
#   } else {
#     ret <- (new.TAC/TAC)
#   }
#   
#   if(debug==1){log.write.func.exit(log.file,ret)}
#   
#   ret
#   
# }

#25/06/2013
# hcr11<-function(log.file,beta,w,TACref,TAC,slope,eggs,egglimit=0,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,egglimit,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#     
#   if (eggs[3]<egglimit) {new.TAC<-new.TAC*(eggs[3]/500)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if(debug==1){log.write.func.exit(log.file,ret)}
#   
#   ret
#   
# }

#25/06/2013
# hcr12<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr12::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<500) {new.TAC<-new.TAC*(eggs[3]/1000)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr12::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr12::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr12::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#25/06/2013
# hcr13<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr13::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<750) {new.TAC<-new.TAC*(eggs[3]/750)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr13::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr13::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr13::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#25/06/2013
# hcr14<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr14::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<1000) {new.TAC<-new.TAC*(eggs[3]/1000)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr14::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr14::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr14::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#25/06/2013
# hcr15<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr15::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<1000) {new.TAC<-new.TAC*(eggs[3]/2000)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr15::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr15::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr15::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#25/06/2013
# hcr16<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr16::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<1500) {new.TAC<-new.TAC*(eggs[3]/1500)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr16::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr16::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr16::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#25/06/2013
# hcr17<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr17::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<1500) {new.TAC<-new.TAC*(eggs[3]/3000)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr17::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr17::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr17::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#25/06/2013
# hcr18<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr18::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<2000) {new.TAC<-new.TAC*(eggs[3]/2000)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr18::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr18::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr18::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#25/06/2013
# hcr19<-function(log.file,beta,w,TACref,TAC,slope,eggs,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,debug))
#   }
#   
#   new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr19::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (eggs[3]<2000) {new.TAC<-new.TAC*(eggs[3]/4000)}
#   
#   if (debug==1){log.write.ln(log.file,paste(" debug::hcr19::new.TAC = ",new.TAC,sep=""),TRUE)}
#   
#   if (TAC==0) {ret<-0} else {ret<-new.TAC/TAC}
#   
#   if (debug==1){
#     log.write.ln(log.file,"debug::hcr19::exit",TRUE)
#     log.write.ln(log.file,paste("  debug::hcr19::ret = ",ret,sep=""),TRUE)
#   }
#   
#   return(ret)
#   
# }

#30/09/2014
# f.hcr20<-function(log.file,beta,w,TACref,TAC,slope,eggs,egglimit=0,egggamma=1,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(beta,w,TACref,TAC,slope,eggs,egglimit,egggamma,debug))
#   }
#   
#   if (TAC==0) {
#     
#     #prevents division by zero later - should not be here anyway
#     ret <- 0    
#   
#   } else {
#   
#     new.TAC <- beta*(w*TACref + (1-w)*TAC*slope)
#     
#     #additional reduction if the previous egg result is below the limit
#     if (eggs[3] < egglimit){new.TAC <- new.TAC*((eggs[3]/egglimit)^egggamma)}
#     
#     #calculate multiplier to return
#     ret <- new.TAC/TAC
#     
#   }
#   
#   if(debug==1){log.write.func.exit(log.file,ret)}
#   
#   ret
#   
# }


fhcr20<-function(lopt,dfEgg,tac){
  
  #lopt - list of simulation options
  #dfEgg - dataframe with egg counts 2 columns - Year & EggCount
  #tac - the current tac
  
  if (tac==0) {
    
    #prevents division by zero later - should not be here anyway
    ret <- 0    
    
  } else {
    
    new_tac <- lopt$beta*(lopt$w*lopt$TACref + (1-lopt$w)*tac*feggslope(rev(rev(dfEgg$EggCount)[1:3])))
    
    #additional reduction if the previous egg result is below the limit
    if (rev(dfEgg$EggCount)[1] < lopt$egglimit){
      new_tac <- new_tac*((rev(dfEgg$EggCount)[1]/lopt$egglimit)^lopt$egggamma)
    }
    
    #calculate multiplier to return
    ret <- new_tac
    
  }
  
  ret
  
}