#ffactor.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#V2 - consolidated 07/10/2008
#Sys.putenv("V:ffactor.r"="2.0")
#Sys.setenv("V:ffactor.r"="2.0")

#assign the environment var V:ffactor.r
fset_env_var("V:ffactor.r","2.0")

######################################
############################################################ 
#Function to calculate f factor from TAC####################
############################################################

#ffactor<-function(tac,pop,fv,disf,wt,m,numage){
ffactor<-function(tac,pop,fv,wt,m,numage){
    
    
   #tac - the desired tac
   #pop - population number vector
   #fv - current fishing mortality vector
   #disf - discard mortality vector
   #wt - weight at age vector
   #m - natural mortality vector
   #numage - number of age groups

   #ensure no f values are zero, by setting any that are equal to a notional minimum (0.00001)
   for(j in 1:numage){if(fv[j]==0){fv[j]<-0.00001}}

   f<-fv

   #current biomass vector
   np<-pop*wt

   #if the tac is zero then set the returned vector of ffactors to zero
   if (tac==0) {

      rat<-0

   } else {

      #if the tac is 95% (or greater) of the current biomass i.e. effectively all the population then
      #set the multiplier to 1000

      if ((0.95*sum(np))<tac) {

         rat<-1000

      } else {

         #factor is calculated by initialising it to 1, calculating the resulting catch and 
         #comparing it with the desired tac. If the difference is greater than 0.1% of the tac
         #then the multiplier is modified by a factor of the tac divided by the total catch weight.

	 #initialise vars
         x<-1            #used to test for convergence
         rat<-1          #the fishing mortality multiplier

         while (x>0.001) {

            #compute modified fishing mortality using the current multiplier value
            f<-rat*fv

            #compute the test catch
            #test_catch<-f*np*(1-exp(-f-m-disf))/(disf+f+m+0.00001)
            test_catch<-f*np*(1-exp(-f-m))/(f+m+0.00001)
            
            #multiplier for the rat value is the ration of the test catch to the desired tac
            rat<-rat*(tac/sum(test_catch))

            #if the total catch weight less the tac is within 0.1% of the desired tac then
            #we can assume we have converged
            x<-abs((sum(test_catch)-tac)/tac)

         }
      }
   }

#return the multiplier
ffac<-rat

}

