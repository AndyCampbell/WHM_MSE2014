#eggslope.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#V2 - consolidated 07/10/20
#Sys.putenv("V:eggslope.r"="2.0")
#Sys.setenv("V:eggslope.r"="2.0")

#assign the environment var V:eggslope.r
set.env.var("V:eggslope.r","2.0")

#eggslope <- function(eggs,debug=0) {
feggslope <- function(eggs,debug=0) {
  
   upper.limit<-0.5
   lower.limit<--1.5

   sum.egg <- sum(eggs)
   avg.egg <- sum.egg/3

   sumX<-0
   sumY<-0
   sumXY<-0
   sumX2<-0

   jcc<-c(0,0,0)
   kfac<-0

   for (i in 1:3){
      jcc[i]<-i
      sumX<-sumX+jcc[i]
      sumY<-sumY+(eggs[i]/avg.egg)
      sumXY<-sumXY+jcc[i]*(eggs[i]/avg.egg)
      sumX2<-sumX2+jcc[i]*jcc[i]
   }

   num<-sumXY - (sumX*sumY/3)
   den<-sumX2 - (sumX*sumX/3)

   slope <- (sumXY - (sumX*sumY/3))/(sumX2 - (sumX*sumX/3))
   
   if (!is.nan(slope)) {
      if (slope>upper.limit) {
         kfac <- 1.4 
      } else if ((slope <= upper.limit) && (slope >= 0)) {
	 kfac <- 1 + (slope/upper.limit)*0.4
      } else if ((slope<0) && (slope>lower.limit)) {
	 kfac <- 1 + (slope/(-1*lower.limit)) 
      } else if (slope <= lower.limit) {
	 kfac <- 0
      }
   } else {
      kfac <- 0
   }

   return(kfac)

}

