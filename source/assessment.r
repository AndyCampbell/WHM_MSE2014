#assessment.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#V2 - consolidated 07/10/2008
#Sys.putenv("V:assessment.r"="2.0")
#Sys.setenv("V:assessment.r"="2.0")

#assign the environment var V:assessment.r
set.env.var("V:assessment.r","2.0")

############################################################
#Assessment / observation model#############################
############################################################

#function calculates new assessment ssb/f by applying a bias 
#and noise to the existing values
assessment<-function(f,ssb.true,fbar.true,ssbassessbias,
                     ssbassesscv,fbarassessbias,
                     fbarassesscv,debug=0){
  
  if (debug==1){
    log.write.func.enter(f,formals()[-1],list(ssb.true,fbar.true,ssbassessbias,
                                              ssbassesscv,fbarassessbias,
                                              fbarassesscv,debug))
  }
  
  
  #apply bias and cv to ssb assessment,
  #assumes normal error
  #standard deviation = mean * cv
  ssb.obs<--1
  #keep drawing for a positive result
  while(ssb.obs<0){ssb.obs<-rnorm(n=1,
                                  mean=sum(ssb.true)*ssbassessbias,
                                  sd=ssbassesscv*sum(ssb.true)*ssbassessbias)}
  
  #if (debug==1) {cat("assessment SSB =",ssb.obs,"\n")}
  
  #apply bias and cv to f assessment
  #assumes normal error
  #standard deviation = mean * cv
  fbar.obs<--1
  
  #keep drawing for a positive result
  while(fbar.obs<0){fbar.obs<-rnorm(n=1,
                                    mean=fbar.true*fbarassessbias,
                                    sd=fbarassesscv*fbar.true*fbarassessbias)}
  
  #return vector
  ret<-c(ssb.obs,fbar.obs)

  if(debug==1){log.write.func.exit(f,ret)}
  
  return(ret)
  
}
