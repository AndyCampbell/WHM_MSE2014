#management.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#Sys.putenv("V:management.r"="2.0")
#Sys.setenv("V:management.r"="2.0")

#assign the environment var V:management.r
set.env.var("V:management.r","2.0")

############################################################
#Management model###########################################
############################################################

f.management<-function(log.file,ssb.obs,fbar,ftac,yearno,years,assessdelay,
                     hcrperiod,hcrrule,hcrchange,ssbhcr,
                     oldhcrmult,fbarhcr,ssbtest1,ssbtest2,
                     mintac,maxtac,suspend,simyear,
                     firstmanagementyear,eggs=c(0,0,0),
                     ssb2=0,tac.ref=0,weight.parameter=0,
                     beta=0,egglimit=0,egggamma=1,debug=0){
  
  
  if (debug==1){
     log.write.func.enter(log.file,formals()[-1],list(ssb.obs,fbar,ftac,yearno,years,assessdelay,
                                               hcrperiod,hcrrule,hcrchange,ssbhcr,oldhcrmult,
                                               fbarhcr,ssbtest1,ssbtest2,mintac,maxtac,suspend,
                                               simyear,firstmanagementyear,eggs,ssb2,tac.ref,
                                               weight.parameter,beta,egglimit,egggamma,debug))
  }
  
  #hcr's are not applied unless yearno is greater than the assessdelay
  
  if((yearno-assessdelay)==1){oldhcrmult<-1}

   #check is the hcr to be applied in this year by call to function hcryear()
   if(hcryear(log.file,simyear,hcrperiod,firstmanagementyear,debug)) {

      #and the slope factor
      slope.factor <- eggslope(eggs,debug)
      
      #cat("eggs=",eggs,"\n")
      #cat("slope.factor=",slope.factor,"\n")

      #default values
      HCRsub <- 0

      #check that a suitable HCR function is available
      if ((!exists(paste("f.hcr",as.character(hcrrule),sep=""))) & (hcrrule>0)) {
	      stop (paste("\nNo HCR function exists for index ",hcrrule,"\nUnable to continue\n",sep=""))
      }
   
      #calculate the appropriate multiplier for ftac (given the specified hcr)
      if(hcrrule==0){HCRmult<-1;HCRsub<-0}
      
      if(hcrrule==1){HCRmult<-hcr1(ssb,ssbhcr)}
      
      if(hcrrule==2){HCRmult<-hcr2(ssb,ssbhcr)}
      
      if(hcrrule==3){
        hcrret<-hcr3(ssb,ssbhcr)
        HCRmult<-hcrret[1]
        HCRsub<-hcrret[2]
      }
      
      if(hcrrule==4){HCRmult<-hcr4(fbar,fbarhcr)}
      
      if(hcrrule==5){HCRmult<-hcr5(fbar,fbarhcr)}
      
      if(hcrrule==6){HCRmult<-hcr6(fbar,fbarhcr)}
      
      if(hcrrule==7){
         hcrret<-hcr7(ssb,ssbtest1,ssbtest2)
         HCRmult<-hcrret[1]
         HCRsub<-hcrret[2]
      }

      #USER DEFINED HCRs
      #add extra code here for user-defined harvest control rules
      #insert a suitable if condition, calling the new function in hcr.r
      #and supplying the correct parameters
      #functions should return a multiplier for f/TAC
      if (hcrrule==8) {HCRmult<-hcr8(ssb,ssbtest1,ssbtest2,ftac,maxtac)}

      if (hcrrule==9) {HCRmult<-hcr9(ssb,ssbhcr,ftac,maxtac)}

      if (hcrrule==10) {HCRmult <- hcr10(log.file,simyear,beta,weight.parameter,tac.ref,ftac,slope.factor,debug)}
      
      #25/06/2013
      if (hcrrule==11) {HCRmult <- hcr11(log.file = log.file, beta = beta,
                                         w = weight.parameter, TACref = tac.ref,
                                         TAC = ftac, slope = slope.factor, eggs = eggs,
                                         egglimit = egglimit, debug = debug)}
      if (hcrrule==12) {HCRmult <- hcr12(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==13) {HCRmult <- hcr13(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==14) {HCRmult <- hcr14(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==15) {HCRmult <- hcr15(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==16) {HCRmult <- hcr16(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==17) {HCRmult <- hcr17(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==18) {HCRmult <- hcr18(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==19) {HCRmult <- hcr19(log.file,beta,weight.parameter,tac.ref,ftac,slope.factor,eggs,debug)}
      if (hcrrule==20) {HCRmult <- f.hcr20(log.file = log.file, beta = beta, w = weight.parameter, TACref = tac.ref,
                                         TAC = ftac, slope = slope.factor,eggs = eggs, egglimit = egglimit,
                                         egggamma = egggamma, debug = debug)}
            
      #limit the change in ftac if the hcrchange parameter is set to a non-zero value
      #and ssb is above blim

      if (suspend==1) {
          #suspend the hcrchange limit below ssbtest2
          #so implement the hcrchange rule only when ssb > ssbtest2

	 #AC TEMP CHANGE
          #if ((hcrchange>0) & (ssb>ssbtest2)) {
          if ((hcrchange>0) & (ssb>ssbtest1)) {
 
	      #new minimum value
	      newmin <- oldhcrmult*max(c(0,(1-hcrchange)))
	      #newmin <- hcrmult*max(c(0,(1-hcrchange)))
      
	      #new maximum value
	      newmax <- oldhcrmult*(1+hcrchange)
	      #newmax <- hcrmult*(1+hcrchange)
     
	      #apply these limits
	      if (HCRmult < newmin) {HCRmult<-newmin}
	      if (HCRmult > newmax) {HCRmult<-newmax}
          }

      } else {
          
          #don't suspend the change i.e. it applies thoughout the ssb value range
          #if ((hcrchange>0) & (ssb>ssbtest2)) {
          if (hcrchange>0) {

 
	      #new minimum value
	      newmin <- oldhcrmult*max(c(0,(1-hcrchange)))
	      #newmin <- hcrmult*max(c(0,(1-hcrchange)))
      
	      #new maximum value
	      newmax <- oldhcrmult*(1+hcrchange)
	      #newmax <- hcrmult*(1+hcrchange)
     
	      #apply these limits
	      if (HCRmult < newmin) {HCRmult<-newmin}
	      if (HCRmult > newmax) {HCRmult<-newmax}

	 }
      }

   } else {      #do not apply hcr and use previous value

      HCRmult <- 1
      HCRsub <- 0
      eggs[3] <- NaN
   }
    
  HCRmult<-c(HCRmult,HCRsub,eggs[3])

  if(debug==1){log.write.func.exit(log.file,HCRmult)}
  
  #return the new multiplier
  return(HCRmult)
  
}

