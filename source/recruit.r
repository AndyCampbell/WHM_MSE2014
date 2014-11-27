#recruit.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#Sys.putenv("V:recruit.r"="2.0")
#Sys.setenv("V:recruit.r"="2.0")

#assign the environment var V:recruit.r
set.env.var("V:recruit.r","2.0")

#Recruitment functions

#Recruitment function 0 (fixed value)

#This function returns a value for recruits given by the mean recruitment 
#value specified by the recmean parameter

# recruit0<-function(recmean){
# 
#    #recmean is a required parameter that must be a value greater 
#    #than or equal to zero
#    if (!is.numeric(recmean)) stop ("recruit0::argument recmean must be numeric\n")
#    if (recmean<0) stop ("recruit0::argument recmean must be a value greater than or equal to zero\n")
# 
#    recruits<-recmean
# 
# }

#Recruitment function 1 (random from normal):

#This function initially sets the recruits parameter to be negative and 
#continues to select values for recruits from a Normal distribution until
#a non-negative value is chosen and returned (i.e. a truncated Normal 
#distribution is used). The Normal distribution used has mean = recmean and 
#standard deviation = reccv*recmean, where the parameters recmean and reccv 
#are those specified at the start of the run

# recruit1<-function(recmean,reccv){
# 
#    #recmean is a required parameter that must be a numeric value 
#    #greater than or equal to zero
#    #reccv is a required parameter that must be a numeric value 
#    #greater than or equal to zero
# 
#    if (!is.numeric(recmean)) stop ("recruit1::argument recmean must be numeric\n")
#    if (recmean<0) stop ("recruit1::argument recmean must be a value greater than or equal to zero\n")
#    if (!is.numeric(reccv)) stop ("recruit1::argument reccv must be numeric\n")
#    if (reccv<0) stop ("recruit1::argument reccv must be a value greater than or equal to zero\n")
# 
#    #initial value
#    recruits<--1
# 
#    #draw until positive
#    while(recruits<0){
#       recruits<-rnorm(1,recmean,reccv*recmean)
#    }
# 
# }

#Recruitment function 2 (random from previous years):

#This function uses the spwnfrec vector, which stores the historical recruit 
#values (up to the year datarecyear) from the recruits.txt file. The function
#sets up a vector of numbers 1 to k, where k is the length of the spwnfrec vector, 
#and randomly chooses one of these numbers from the vector k. The entry in the 
#spwnfrec vector corresponding to this random number is then chosen and this 
#historical value is returned for the recruits parameter (e.g. if the random number 
#drawn is 7, then the 7th entry in the spwnfrec vector is chosen as recruits). The 
#end result is that a value is randomly drawn from the spwnfrec vector, and all 
#historical values have the same probability of being drawn

# recruit2<-function(spwnfnyr,spwnfnrec){
# 
#    #spwnfnyr and spwnfnrec are both required arguments
#    #spwnfnyear must be a vector of length 1 or more of positive values
#    #spwnfnrec must be a vector of length 1 or more of real positive values
#    #spwnfnyear and spwnfnrec must both be of the same length
# 
#    if (length(spwnfnyr)<1) stop("recruit2::argument spwnfnyr must be a vector of length >= 1\n")
#    if (length(spwnfnrec)<1) stop("recruit2::argument spwnfnrec must be a vector of length >= 1\n")
# 
#    if (!is.numeric(spwnfnyr)) stop ("recruit2::argument spwnfnyr must be of numeric type\n")
#    if (!is.numeric(spwnfnrec)) stop ("recruit2::argument spwnfnrec must be of numeric type\n")
# 
#    if (!(length(spwnfnyr) == length(spwnfnrec))) stop ("recruit2::arguments spwnfnyr and 
#    spwnfnrec must be vectors of the same length\n")
# 
#    if (sum(spwnfnyr<0)>0) stop ("recruit2::all spwnfnyr values must be greater than zero\n")
#    if (sum(spwnfnrec<0)>0) stop ("recruit2::all spwnfnrec values must be greater than zero\n")
# 
# 
#    n<-length(spwnfnrec)
#    k<-1:n
# 
#    #randomly draw one of the elements
#    rn<-sample(k,1)
#    yearn<-spwnfnyr[rn]
#    recruits<-spwnfnrec[rn]
# 
# }

#Recruitment function 3 (random from previous years with two groupings for ssb):

#As recruitment function 2, except that the parameter spwnfnssb corresponding to 
#the column of historical SSB values (up to datarecyear) from the recruits.txt file is used. 
#If the virtual SSB is above ssbcut then only historical recruitment with corresponding 
#historical SSB above this ssbcut level will be drawn and returned as recruits. 
#A similar process applies if the virtual SSB is below ssbcut. 

# recruit3<-function(ssb,spwnfnyr,spwnfnrec,spwnfnssb,ssbcut){
# 
#    #all arguments are required
#    #all arguments must be numeric
#    #spwnfnyear, spwnfnrec & spwnfnssb must be vectors of length >= 1
#    #spwnfnyear, spwnfnrec & spwnfnssb must all be of the same length
#    #spwnfnssb needs contain values that are below ssbcut if ssb < ssbcut
#    #spwnfnssb needs to contain values that are above ssbcut if ssb >= ssbcut
# 
#    if (length(spwnfnyr)<1) stop("recruit3::argument spwnfnyr must be a vector of length >= 1\n")
#    if (length(spwnfnrec)<1) stop("recruit3::argument spwnfnrec must be a vector of length >= 1\n")
#    if (length(spwnfnssb)<1) stop("recruit3::argument spwnfnssb must be a vector of length >= 1\n")
# 
#    if (!is.numeric(ssb)) stop ("recruit3::argument ssb must be of numeric type\n")
#    if (!is.numeric(spwnfnyr)) stop ("recruit3::argument spwnfnyr must be of numeric type\n")
#    if (!is.numeric(spwnfnrec)) stop ("recruit3::argument spwnfnrec must be of numeric type\n")
#    if (!is.numeric(spwnfnssb)) stop ("recruit3::argument spwnfnssb must be of numeric type\n")
#    if (!is.numeric(ssbcut)) stop ("recruit3::argument ssbcut must be of numeric type\n")
# 
#    if (!(length(spwnfnyr) == length(spwnfnrec))) stop ("recruit3::arguments spwnfnyr and 
#    spwnfnrec must be vectors of the same length\n")
#    if (!(length(spwnfnyr) == length(spwnfnssb))) stop ("recruit3::arguments spwnfnyr and 
#    spwnfnssb must be vectors of the same length\n")
# 
#    if (sum(spwnfnyr<0)>0) stop ("recruit3::all spwnfnyr values must be greater than zero\n")
#    if (sum(spwnfnrec<0)>0) stop ("recruit3::all spwnfnrec values must be greater than zero\n")
#    if (sum(spwnfnssb<0)>0) stop ("recruit3::all spwnfnssb values must be greater than zero\n")
# 
#    if ((ssb<ssbcut) & (sum(spwnfnssb<ssbcut)==0)) stop ("recruit3::if ssb<ssbcut the vector 
#    spwnfnssb needs to contain at least one value lower than ssbcut\n")
#    
#    if ((ssb>=ssbcut) & (sum(spwnfnssb>=ssbcut)==0)) stop ("recruit3::if ssb>=ssbcut the vector 
#    spwnfnssb needs to contain at least one value >= ssbcut\n")
#    
#    n<-length(spwnfnrec)
#    k<-1:n
#    recruits<-0
# 
#    if (ssb < ssbcut) {
# 
#       ssbn <- 2*ssbcut
#     
#       while (ssbn > ssbcut) {
# 
# 	 rn <- sample(k,1)
# 	 yearn <- spwnfnyr[rn]
# 	 ssbn <- spwnfnssb[rn]
# 	 recruits <- spwnfnrec[rn]}
#   
#    } else {
# 
#       ssbn <- 0.5*ssbcut
#     
#       while (ssbn < ssbcut) {
# 
# 	 rn <- sample(k,1)
# 	 yearn <- spwnfnyr[rn]
# 	 ssbn <- spwnfnssb[rn]
# 	 recruits <- spwnfnrec[rn]}
#   
#    }
#   
#    return(recruits)
#   
# }

#Recruitment function 4 (Ricker map fitted to data from previous years):

#This function fits a Ricker model to the historical SSB-recruitment 
#data in the recruits.txt file (up to datarecyear). As Ricker is an exponential 
#model, a linear fit (lm command in R) is made to the log of the data. The 
#Ricker model parameters are then calculated from the output of the linear fit, 
#and using the virtual SSB level, a value of recruits is calculated and returned.

# recruit4<-function(ssb,spwnfnrec,spwnfnssb){
# #recruit4<-function(ssb,params){
# 
#    #all arguments are required
#    #all arguments must be numeric
#    #ssb must be >= 0
#    #spwnfnrec and spenfnssb must be > 0
#    #spwnfnrec and spwnfnssb must be of the same length
# 
#    if (length(spwnfnrec)<1) stop("recruit3::argument spwnfnyr must be a vector of length >= 1\n")
#    if (length(spwnfnssb)<1) stop("recruit3::argument spwnfnrec must be a vector of length >= 1\n")
# 
#    if (!is.numeric(ssb)) stop ("recruit4::argument ssb must be of numeric type\n")
#    if (!is.numeric(spwnfnrec)) stop ("recruit4::argument spwnfnrec must be of numeric type\n")
#    if (!is.numeric(spwnfnssb)) stop ("recruit4::argument spwnfnssb must be of numeric type\n")
# 
#    if (ssb<-0) stop ("recruit4::argument ssb must be > 0\n")
#    if (sum(spwnfnrec<0)>0) stop ("recruit3::all spwnfnrec values must be greater than zero\n")
#    if (sum(spwnfnssb<0)>0) stop ("recruit3::all spwnfnssb values must be greater than zero\n")
# 
#    if (!(length(spwnfnrec == length(spwnfnssb)))) stop ("recruit4::arguments spwnfnrec and 
#    spwnfnssb must be vectors of the same length\n")
# 
#    y <- log(spwnfnrec/spwnfnssb)
#    x <- spwnfnssb
#    z <- coef(lm(y ~ x))
#    alpha <- exp(z[1])
#    beta <- z[2]
# 
#    recruits<-ssb*alpha*exp(beta*ssb)
# 
#    return(recruits)
# 
#   #recruits<-ssb*params[1]*exp(-1*params[2]*ssb)
# 
# }

#Recruitment function 5 (Ricker map fitted to data from previous years with added noise):

#As recruitment function 4 except noise is added to the Ricker value of recruits. 
#A CV value (reccv) is calculated from the historical data (up to datarecyear). 
#The calculated value for recruitment from the Ricker model is then used as the mean 
#value of a Normal distribution with a standard deviation calculated from reccv. A 
#non-negative value for recruits is then randomly drawn from this Normal distribution 
#and returned. 

# #recruit5<-function(ssb,spwnfnrec,spwnfnssb){
# recruit5<-function(ssb,params){
# 
#   #y<-log(spwnfnrec/spwnfnssb)
#   #x<-spwnfnssb
#   #lsq<-lm(y~x)
#   #z<-coef(lsq)
# 
#   #junk<-summary(lsq)
# 
#   ##shouldn't really be calculating this every time
#   #reccv<-sqrt(var(spwnfnrec,y=NULL,na.rm=FALSE))/mean(spwnfnrec) #cv from absolute recruit values
#   
#   #alpha<-exp(z[1])
#   #beta<-z[2]
# 
#   recruitmn<-ssb*params[1]*exp(-1*params[2]*ssb)
# 
#   recruits<--1
#   while(recruits<0){
#     #recruits<-rnorm(1,recruitmn,reccv*recruitmn)
#     recruits<-rnorm(1,recruitmn,params[3]*recruitmn)
#   }
# }

#Recruitment function 6 (Segmented regression - data already in function):

#This function uses the ssbcut, recmean and seggrad parameter values 
#to calculate a 'hockey-stick' stock-recruit relation. In principle, it should 
#be possible to calculate these parameter values from the historical data in the 
#recruits.txt file but the program does not yet do this. If the virtual SSB is 
#greater than the specified SSB level, ssbcut, then recruitment is just taken to be 
#the value given by recmean. If the virtual SSB < ssbcut, then the recruitment is 
#calculated as a linear relation to SSB (from zero), where the gradient of the linear 
#function is given by seggrad. Note that to be correct the following relation must 
#hold: ssbcut*seggrad = recmean. The calculated recruitment value is then returned as 
#recruits.

# recruit6<-function(ssb,ssbcut,recmean,seggrad){
# 
# if(ssb==0){recruits<-0}else{
#    if(ssb>ssbcut){recmean1<-recmean} else{recmean1<-ssb*seggrad}}
#    recruits<-recmean1
# }

#Recruitment function 7 (Segmented regression with added noise - data already in function):

#As recruitment function 6, except noise is added to the recruitment value. The 
#recruitment value from the 'hockey-stick' function is used as the mean value of a 
#Normal distribution with standard deviation calculated from this mean value and the 
#specified value of reccv from the FPRESSopt.r file. A non-negative value is then 
#drawn from this Normal distribution and returned as recruits.

# recruit7<-function(ssb,ssbcut,recmean,seggrad,reccv){
# 
#   if(ssb==0){recruits<-0}else{
#     if(ssb>ssbcut){recmean<-recmean} else{recmean<-ssb*seggrad}
# 
#     rectst<--recmean
#     while(rectst<0){
#       rectst<-rnorm(1,recmean,recmean*reccv)
#     }
#     recruits<-rectst
#   }
# }

#user defined recruitment functions
#Recruitment function 8 (random from previous years including simulated years):
#(Bootstrap with addition)

#Function takes the recruitment data provided in the recruits input data file
#and the simulated recruits to date and randomly chooses a recruitment value.

# recruit8<-function(spwnfnyr,spwnfnrec,simrecruits="missing"){
# 
#    if (missing(simrecruits)) { 
#       totrecs<-spwnfnrec
#    } else {
#       totrecs<-c(spwnfnrec,simrecruits)
#    }
# 
#    n<-length(spwnfnrec)
#    k<-1:n
# 
#    rn<-sample(k,1)
#    yearn<-spwnfnyr[rn]
#    recruits<-spwnfnrec[rn]
# }
#user defined recruitment functions

#Recruitment function 9 (Segmented regression with added noise - data already in function):
#LOG NORMAL distribution
# recruit9<-function(ssb,ssbcut,recmean,seggrad,reccv){
# 
#    if (ssb==0) {
# 
#       recruits<-0
# 
#    } else {
# 
#       if (ssb>ssbcut) {
# 	 recmean<-recmean
#       } else {
# 	 recmean<-ssb*seggrad
#       }
# 
#       rectst<-recmean*exp(rnorm(1,0,reccv))
#       recruits<-rectst
# 
#       flush.console()
# 
#    }
# 
#    return(recruits)
# 
# }


#Recruitment function 10 (Segmented regression with added noise - data already in function):
#LOG NORMAL distribution
# recruit10<-function(ssb,ssbcut,recmean,seggrad,reccv,recmax){
# 
#    if (ssb==0) {
# 
#       recruits<-0
# 
#    } else {
# 
#       #draw the deterministic value
#       if (ssb>ssbcut) {
# 	 recmean<-recmean
#       } else {
# 	 recmean<-ssb*seggrad
#       }
# 
#       #now apply an error term which is lognormally distributed about the deterministic value
# 
#       #rectst<--recmean
# 
#       #while (rectst<0) {
# 	 #err<-rlnorm(1,14.43382,0.4228567)-2027405
# 	 #rectst<-recmean+err
#       #}
# 
#       rectst <- recmean*exp(0.765*qnorm(runif(1)))
#       #rectst <- recmean*exp(reccv*qnorm(runif(1)))
#       recruits<-rectst
# 
#       if (recmax>0) {
# 	 recruits <- min(recruits,recmax)
#       }
# 
#    }
# 
#    return(recruits)
# 
# }

#Recruitment function 11
#used by FPRESS.Run2() for WHM deterministic hindcasts
#if ssb exceeds ssb1982 then use the actual recruitment
#otherwise draw from the slope section of the hockey stick model

# recruit11<-function(year,ssb){
# 
#    if (ssb>1240000) {
# 
#       if (year==1983) {
# 	 recruits<-498609
#       } else if (year==1984) {
# 	 recruits<-1409320
#       } else if (year==1985) {
# 	 recruits<-2617180
#       } else if (year==1986) {
# 	 recruits<-3844100
#       } else if (year==1987) {
# 	 recruits<-5248690
#       } else if (year==1988) {
# 	 recruits<-2121140
#       } else if (year==1989) {
# 	 recruits<-2270800
#       } else if (year==1990) {
# 	 recruits<-2071230
#       } else if (year==1991) {
# 	 recruits<-4426580
#       } else if (year==1992) {
# 	 recruits<-7812090
#       } else if (year==1993) {
# 	 recruits<-9037740
#       } else if (year==1994) {
# 	 recruits<-9605170
#       } else if (year==1995) {
# 	 recruits<-5323020
#       } else if (year==1996) {
# 	 recruits<-2818950
#       } else if (year==1997) {
# 	 recruits<-2515750
#       } else if (year==1998) {
# 	 recruits<-3506740
#       } else if (year==1999) {
# 	 recruits<-4629310
#       } else if (year==2000) {
# 	 recruits<-3146240
#       } else if (year==2001) {
# 	 recruits<-17843300
#       } else if (year==2002) {
# 	 recruits<-8150480
#       } else if (year==2003) {
# 	 recruits<-7065880
#       } else if (year==2004) {
# 	 recruits<-3485070
#       }
# 
#    } else {
#       
#       recruits <- ssb*3.12835
# 
#    }
# 
#    return(recruits)
# 
# }

#Recruitment function 12
#seg reg with correlation, as per BR's paper
# recruit12<-function(ssb,ssbcut,recmean,seggrad,sigmaR,eta,cors,recmax){
# 
#    #argument list
#    #ssb - current SSB
#    #ssbcut - SSB elbow for segmented regression
#    #recmean - recr when SSB above elbow
#    #seggrad - gradient of line from origin to elbow
#    #sigmaR
#    #eta
#    #cors
#    #recmax - maximum recruitment (=0 if not enforced)
# 
#    #draw the number of recruits based on the ssb
#    if (ssb<ssbcut) {
#       recr<-seggrad*ssb
#    } else {
#       recr<-recmean
#    }
# 
#    new.eta <- (cors*eta) + (sqrt(1-(cors*cors))*rnorm(1))
# 
#    recr <- recr*exp(sigmaR*new.eta)
# 
#    if (recmax>0) {
#       recr<-min(recmax,recr)
#    }
# 
#    return(c(recr,new.eta))
# 
# }

#WKWHMMP2 recruitment function
#Ricker, no autocorrelation
# recruit13 <- function(a_param,b_param,sig_param,ssb,debug=0){
# 
#   if(debug==1){cat("recruit13::\n\ta_param = ",a_param,"\n\tb_param = ",b_param,
#   "\n\tsig_param=",sig_param,"\n\tssb = ",sum(ssb),"\n")}
#   
# 	#recruits
# 	rck <- sum(ssb)*a_param*exp(-1*b_param*sum(ssb))
# 	#add LN error
# 	recr <- rck*exp(sig_param*rnorm(n=1,mean=0,sd=1))
# 
#   if(debug==1){cat("\trecr = ",recr,"\n")}
#   
# 	return(recr)
# }

#WKWHMMP2 recruitment function
#Beverton & Holt, no autocorrelation
# recruit14 <- function(a_param,b_param,sig_param,ssb,debug=0){
# 
#   if(debug==1){cat("recruit14::\n\ta_param = ",a_param,"\n\tb_param = ",b_param,
#   "\n\tsig_param=",sig_param,"\n\tssb = ",sum(ssb),"\n")}
#   
# 	#recruits
# 	#bh <- 1/(a_param + (b_param/sum(ssb)))
#   bh <- (a_param*sum(ssb))/(b_param+sum(ssb))
# 	#add LN error
# 	recr <- bh*exp(sig_param*rnorm(n=1,mean=0,sd=1))
# 
#   if(debug==1){cat("\trecr = ",recr,"\n")}
#   
# 	return(recr)
# }

#WKWHMMP2 recruitment function
#Smooth Hockey Stick, no autocorrelation
# recruit15 <- function(a_param,b_param,g_param,sig_param,ssb,debug=0){
# 
#   if(debug==1){cat("recruit15::\n\ta_param = ",a_param,"\n\tb_param = ",b_param,
#                    "\n\tg_param=",g_param,"\n\tsig_param=",sig_param,"\n\tssb = ",sum(ssb),"\n")}
#   
# 	#recruits
# 	shs <- a_param*(sum(ssb)+sqrt(b_param^2+g_param)-sqrt((sum(ssb)-b_param)^2+g_param))
# 	#add LN error
# 	recr <- shs*exp(sig_param*rnorm(n=1,mean=0,sd=1))
# 
#   if(debug==1){cat("\trecr = ",recr,"\n")}
#   
# 	return(recr)
# }

#WKWHMMP2 recruitment function
#Ricker, including autocorrelation
# recruit16 <- function(a_param,b_param,sig_param,auto,eta,ssb,debug=0){
# 
#   if(debug==1){cat("recruit16::\n\ta_param = ",a_param,"\n\tb_param = ",b_param,
#                    "\n\tsig_param = ",sig_param,"\n\tauto = ",auto,
#                    "\n\teta = ",eta,"\n\tssb = ",sum(ssb),"\n")}
# 
# 	#recruits
# 	rck <- sum(ssb)*a_param*exp(-1*b_param*sum(ssb))
#   
# 	#autocorrelation
#   if (sig_param==0) {
#     eta <- auto*eta + sqrt(1-auto^2)*rnorm(n=1,mean=0,sd=0)
#   } else {
# 	  eta <- auto*eta + sqrt(1-auto^2)*rnorm(n=1,mean=0,sd=1)
#   }
#   
# 	#add error
# 	recr <- rck*exp(eta*sig_param)
# 
#   if(debug==1){cat("\trecr = ",recr,"\n\teta = ",eta,"\n")}
#   
# 	return(c(recr,eta))
# }

#WKWHMMP2 recruitment function
#Beverton & Holt, including autocorrelation
# recruit17 <- function(a_param,b_param,sig_param,auto,eta,ssb,debug=0){
# 
#   if(debug==1){cat("recruit17::\n\ta_param = ",a_param,"\n\tb_param = ",b_param,
#                    "\n\tsig_param = ",sig_param,"\n\tauto = ",auto,
#                    "\n\teta = ",eta,"\n\tssb = ",sum(ssb),"\n")}
#   
# 	#recruits
#   bh <- (a_param*sum(ssb))/(b_param+sum(ssb))
#   #autocorrelation
# 	eta <- auto*eta + sqrt(1-auto^2)*rnorm(n=1,mean=0,sd=1)
# 	#add error
# 	recr <- bh*exp(eta*sig_param)
# 
# 	if(debug==1){cat("\trecr = ",recr,"\n\teta = ",eta,"\n")}
# 	
# 	return(c(recr,eta))
#   
# }

#WKWHMMP2 recruitment function
#Smooth Hockey Stick, including autocorrelation
# recruit18 <- function(a_param,b_param,g_param,sig_param,auto,eta,ssb,debug=0){
# 
#   if(debug==1){cat("recruit16::\n\ta_param = ",a_param,"\n\tb_param = ",b_param,
#                    "\n\tg_param = ",g_param,
#                    "\n\tsig_param = ",sig_param,"\n\tauto = ",auto,
#                    "\n\teta = ",eta,"\n\tssb = ",sum(ssb),"\n")}
#   
# 	#recruits
# 	shs <- a_param*(ssb+sqrt(b_param^2+g_param)-sqrt((ssb-b_param)^2+g_param))
# 	#autocorrelation
# 	eta <- auto*eta + sqrt(1-auto^2)*rnorm(n=1,mean=0,sd=1)
# 	#add error
# 	recr <- shs*exp(eta*sig_param)
# 
# 	if(debug==1){cat("\trecr = ",recr,"\n\teta = ",eta,"\n")}
# 	
# 	return(c(recr,eta))
# 
# }

#WKWHMMP2 - Bootstrap of historic recruits, linear reduction
#below lowest observed SSB to origin
# recruit19 <- function(ssb,debug=0){
#   
#   #2011 assessment output
#   year <- seq(1982,2009)
#   recr.2011 <- c(72201900,512710,1486590,2722860,3912810,5268630,2094050,2227910,2022310,3841870,
#                  7550070,9646330,10837500,7575330,4243180,3703400,5442220,5529480,5394950,23945400,
#                  4776310,3129910,1511900,994976,623672,1021800,1570380,616047)
#   
#   ssb.2011 <- c(1480040,1468930,1367890,2429610,3198840,3848070,4401200,4061760,3444800,3305000,
#                 2734040,2578760,2175360,1699330,1631960,1530860,1472070,1614400,1690230,1278520,
#                 1629840,1717980,1929200,2594120,2563760,2299980,2491150,2701310)
#   
#   #no autocorrelation
#   eta <- 0
#   
#   #total ssb
#   ssb.tot<-sum(ssb)
#   
#   if(debug==1){cat("recruit19::\n\tssb = ",sum(ssb),"\n")}
#   
#   if (ssb.tot<min(ssb.2011)){
#     
#     #linear reduction in recr below lowest observed SSB
#     #recr <- ssb.tot*(recr.2011[ssb.2011==min(ssb.2011)]/min(ssb.2011))
#     
#     #13/06/2013 
#     #recruitment associated with lowest observed SSB is taken as the mean
#     #of the 'non-pulse' recruitments i.e. the mean of the time series excluding
#     #the 1982 and 2001 year classes
#     recr <- ssb.tot*(mean(recr.2011[!(year==1982 | year==2001)]))/min(ssb.2011)
#     
#   } else {
#     #bootstrap from recr vector
#     recr <- sample(recr.2011,1)
#   }
#   
#   return(c(recr,eta,recr))
#   
# }

#WKWHMMP2 - Ricker Normal - from Thomas Brunel work
# recruit20 <- function(ssb,a=4.983028,b=2.239222e-7,sigma=13602923,debug=0){
# 
#   if(debug==1){cat("recruit20::\n\tssb = ",sum(ssb),"\n")}
# 
#   #no autocorrelation
#   eta <- 0
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,eta,0))
#   
#   #deterministic draw
#   E <- a*sum(ssb)*exp(-b*sum(ssb))
#  
#   #add stochastic variation - any negative draws rejected
#   #can seriously bias results if sigma is big
#   recr <- -1
#   while(recr<0){
#     recr <- rnorm(1,E,sigma)
#   }
#   
#   return(c(recr,0,E))
#   
# }

#WKWHMMP2 - Ricker Log Normal - from Thomas Brunel work
# recruit21 <- function(ssb,a=1.028955e1,b=5.273009e-7,sigma=10668142,debug=0){
# 
#   if(debug==1){cat("recruit20::\n\tssb = ",sum(ssb),"\n")}
#   
#   #no autocorrelation
#   eta<-0
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,eta,0))
#   
#   #draw a mean recruitment from the ricker model 
#   E <- a*sum(ssb)*exp(-b*sum(ssb)) 
#   
#   #apply log normal residual
#   Var <- log(1+((sigma^2)/(E^2)))
#   mu <- log(E) - 0.5*Var
#   
#   recr <- -1
#   while(recr<0){recr <- exp(rnorm(1,mu,Var^0.5))}
# 
#   return(c(recr,eta,E))
#   
# }

#Bayesian fit to SR data
# recruit22 <- function(ssb,model,A.param,B.param,
#                       lowlim=-999,uplim=999,sigma,
#                       debug=0) {
# 
#   if(debug==1){cat("recruit22::\n\tssb = ",sum(ssb),"\n")}
#   #cat("recruit22::\n\tssb = ",sum(ssb),"\n")
# 
#   #no autocorrelation
#   eta <- 0
#   
#   #temp - no noise
#   sigma <- 0
# 
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,eta,0))
#   
#   #SSB
#   #ssb<-sum(ssb)/1000000
#   ssb<-sum(ssb)
#   
#   #model - the model code (HSL,RKL)
#   #A.param, B.param - model parameters
#   #sigma - standard deviation
#   
#   #Returned recruits in 1000 millions  
#   E = switch(substr(model,0,2),
#               HS = A.param*(ssb>=B.param)+A.param*ssb/B.param*(ssb<B.param),
#               RK = A.param*ssb*exp(-B.param*ssb)
#   )
#   
#   cat("E=",E,"\n")
#  
#   
#   #apply log normal residual
#   Var <- log(1+((sigma^2)/(E^2)))
#   mu <- log(E) - 0.5*Var
#   
#   recruits <- -1
#   while(recruits<0){recruits <- exp(rnorm(1,mu,Var^0.5))}
#   
#   
#   #error term
#   #R.err<--1000
#   #R.err.min<-lowlim
#   #R.err.max<-uplim
#   
#   #draw until error is within the specified limits
#   #while (R.err<R.err.min||R.err>R.err.max) {
#   #  R.err<-rnorm(1,0,sigma)
#   #}
#   
#   #recruits = switch(substr(model,3,3),
#   #                  N = mu*(1+R.err),
#   #                  L = exp(mu+R.err)
# #)  
#  
#   #if(debug==1){cat("recruit22::\n\trecruits = ",1000000*recruits,"\n")}
#   cat("recruit22::\n\trecruits = ",1000000*recruits,"\n")
#   
#   return(c(1000000*recruits,eta,1000000*mu))
#   #return(c(recruits,eta,mu))
# }

#Bayesian fit to SR data, no autocorrelation
# recruit23 <- function(f,ssb,model,A.param,B.param,
#                       lowlim=-999,uplim=999,sigma,
#                       debug=0) {
# 
#   if (debug==1){
#     log.write.func.enter(f,formals()[-1],list(ssb,model,A.param,B.param,lowlim,
#                                               uplim,sigma,debug))
#   }
#   
#   #no auto
#   eta <- 0
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   E = switch(substr(model,0,2),
#              HS = 1000000*(A.param*(ssb>=B.param)+A.param*ssb/B.param*(ssb<B.param)),
#              RK = A.param*ssb*exp(-B.param*ssb),
#              BH = A.param*ssb/(B.param+ssb)
#   )
# 
#   #add LN noise
#   R2 <- rnorm(n=1,mean=0,sd=sigma)
#   recr <- exp(log(E)+R2)
# 
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   return(ret)
#   
#   }

#Bayesian fit to SR data, including autocorrelation
# recruit24 <- function(ssb,model,A.param,B.param,
#                       lowlim=-999,uplim=999,sigma,
#                       auto,eta,debug=0) {
# 
#   #autocorrelation
#   eta <- auto*eta + sqrt(1-auto^2)*rnorm(n=1,mean=0,sd=1)
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,eta,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   E = switch(substr(model,0,2),
#              HS = 1000000*(A.param*(ssb>=B.param)+A.param*ssb/B.param*(ssb<B.param)),
#              RK = A.param*ssb*exp(-B.param*ssb)
#   )
# 
#   #add LN noise
#   R2 <- rnorm(n=1,mean=0,sd=sigma)
#   recr <- E*exp(R2*eta)
#   
#   if(debug==1){cat("Recruit24:recr,eta,E",recr,eta,E,"\n")}
#   
#   return(c(recr,eta,E))
#   
# }

#SAD recruit models
#recruit25 <- function(f,ssb,SRModel,eta,debug=0,trunc=FALSE,trunc.upper=2,trunc.lower=-Inf){
f.recruit25 <- function(log.file,ssb,SRModel,trunc=FALSE,trunc.upper=2,trunc.lower=-Inf,debug=0){
    
  if (debug==1){
    log.write.func.enter(log.file,
                         formals()[-1],
                         list(ssb,SRModel$model,trunc,trunc.upper,trunc.lower,debug))}

  #SSB
  ssb<-sum(ssb)
  
  #no SSB - no recruits!
  if (ssb<0.001) return(c(0,0,0))
  
  E = switch(substr(SRModel$model,0,2),
             HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
             RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
             BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
  )
  
  #add LN noise
  #R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR)
  #recr <- exp(log(E)+R2)

  #or this way, more useful for truncation
  if (trunc){ 
    #truncation, need to add a check that if this is TRUE then at least one of the limits is supplied
    if (trunc.upper<Inf) {
      err <- trunc.upper + 1
    } else if (trunc.lower>-Inf){
      err <- trunc.lower - 1
    }
    
    while (err>trunc.upper | err<trunc.lower){
      err <- rnorm(n=1,mean=0,sd=1)
    }
    
  } else {
    #no truncation
    err <- rnorm(n=1,mean=0,sd=1)
  }
  
  R2 <- SRModel$SigR*err
  recr <- exp(log(E)+R2)
  
  #or this way even, using rlnorm
  #R2 <- rlnorm(n=1,mean=0,sdlog=sigR)
  #recr <- exp(log(E)+R2)
  
  ret<-c(recr,0,E)
  
  if (debug==1){log.write.func.exit(log.file,ret)}
  
  ret
  
}

#SAD recruit models, including autocorrelation
# recruit26 <- function(f,ssb,SRModel,eta,debug=0){
#   
#   #cat("Recruit26\n")
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$A.param,SRModel$B.param,sigma,debug))}
#   
#   #autocorrelation
#   eta <- SRModel$scor*eta + sqrt(1-SRModel$scor^2)*rnorm(n=1,mean=0,sd=SRModel$SigR)
#   
#   
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = SRModel$AParam*ssb*exp(-SRModel$BParam*ssb),
#              BH = SRModel$AParam*ssb/(SRModel$BParam+ssb)
#   )
#   
#   #add LN noise, autocorrelation
#   R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR)
#   recr <- E*exp(R2*eta)
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }

#SAD recruit models
# recruit27 <- function(f,ssb,SRModel,eta,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$A.param,SRModel$B.param,SRModel$sigma,eta,debug))}
#   
#   #no auto
#   eta <- 0
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
#              BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
#   )
#   
#   #add LN noise (1/10th)
#   R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR/10)
#   recr <- exp(log(E)+R2)
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }

#SAD recruit models
# recruit28 <- function(f,ssb,SRModel,eta,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$A.param,SRModel$B.param,SRModel$sigma,eta,debug))}
#   
#   #no auto
#   eta <- 0
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
#              BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
#   )
#   
#   #add LN noise (1/2)
#   R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR/2)
#   recr <- exp(log(E)+R2)
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }

#SAD recruit models
# recruit29 <- function(f,ssb,SRModel,eta,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$A.param,SRModel$B.param,SRModel$sigma,eta,debug))}
#   
#   #no auto
#   eta <- 0
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
#              BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
#   )
#   
#   #add LN noise (75%)
#   R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR*0.75)
#   recr <- exp(log(E)+R2)
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }

#SAD recruit models
# recruit30 <- function(f,ssb,SRModel,eta,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$A.param,SRModel$B.param,SRModel$sigma,eta,debug))}
#   
#   #no auto
#   eta <- 0
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
#              BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
#   )
#   
#   #add LN noise (90%)
#   R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR*0.90)
#   recr <- exp(log(E)+R2)
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }

#SAD recruit models, including autocorrelation
# recruit31 <- function(f,ssb,SRModel,eta,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$A.param,SRModel$B.param,sigma,debug))}
#   
#   #autocorrelation (50%)
#   scor <- 0.5*SRModel$scor
#   sigR <- SRModel$SigR
#   eta <- scor*eta + sqrt(1-scor^2)*rnorm(n=1,mean=0)*sigR
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   #cat("ssb=",ssb,"\n")
#   
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
#              BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
#   )
#   
#   #add LN noise, autocorrelation
#   #R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR)
#   #recr <- E*exp(R2*eta)
#   recr <- E*exp(eta)
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }


#SAD recruit models, including autocorrelation
# recruit32 <- function(f,ssb,SRModel,eta,debug=0,trunc=FALSE,trunc.upper=2,trunc.lower=-Inf){
# 
#   #cat("recruit32\n")
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$AParam,SRModel$BParam,eta,debug))}
#   
#   #autocorrelation (100%)
#   scor <- SRModel$scor
#   sigR <- SRModel$SigR
# 
#   #cat("scor=",scor,"\n")
#   
#   #or this way, more useful for truncation
#   if (trunc){
#     
#     #truncation, need to add a check that if this is TRUE then at least one of the limits is supplied
#     if (trunc.upper<Inf) {
#       err <- trunc.upper + 1
#     } else if (trunc.lower>-Inf){
#       err <- trunc.lower - 1
#     }
#     
#     #draws <- 0
#     
#     while (err>trunc.upper | err<trunc.lower){
#       err <- rnorm(n=1,mean=0,sd=1)
#       #draws <- draws + 1
#     }
#     
#     #if (draws>1) {cat ("Truncation\n")} else {cat("No truncation necessary\n")}
#     
#   } else {
#     #no truncation
#     err <- rnorm(n=1,mean=0,sd=1)
#   }
#   
#   #eta <- scor*eta + sqrt(1-scor^2)*rnorm(n=1,mean=0)*sigR
#   eta <- scor*eta + sqrt(1-scor^2)*err*sigR
#   
#   #SSB
#   ssb<-sum(ssb)
#   
#   if (ssb<0.001) return(c(0,eta,0))
#   
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
#              BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
#   )
#   
#   #add LN noise, autocorrelation
#   #R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR)
#   #recr <- E*exp(R2*eta)
#   recr <- E*exp(eta)
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }

#SAD5 recruit models - historic residuals, no autocorrelation

# recruit33 <- function(f,ssb,SRModel,eta,debug=0){
#   
#   if (debug==1){
#     log.write.func.enter(f,
#                          formals()[-1],
#                          list(ssb,SRModel$model,SRModel$A.param,SRModel$B.param,SRModel$sigma,eta,debug))}
#   
#   #no auto
#   eta <- 0
#     
#   #SSB
#   ssb<-sum(ssb)
#   
#   #no SSB - no recruits!
#   if (sum(ssb)<0.001) return(c(0,0,0))
# 
#   #apply appropriate SR model
#   E = switch(substr(SRModel$model,0,2),
#              HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
#              RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
#              BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
#   )
#   
#   #add LN noise
#   #R2 <- rnorm(n=1,mean=0,sd=SRModel$SigR)
#   #recr <- exp(log(E)+R2)
# 
#   #add residual sampled from historical resids
#   recr <- E*exp(sample(SRModel$HistResids,size=1))
#   
#   ret<-c(recr,eta,E)
#   
#   if (debug==1){log.write.func.exit(f,ret)}
#   
#   ret
#   
# }

#SAD5 recruit model, historic residuals, including autocorrelation

#recruit34 <- function(f,ssb,SRModel,eta,debug=0){
f.recruit34 <- function(log.file,ssb,SRModel,year,debug=0){
  
  #log.file - debug log file
  #ssb - the ssb to be used in drawing the recruitment
  #SR Model - the SR data 
  #year - current simulation year
  #debug - debug flag
    
  if (debug==1){
    log.write.func.enter(log.file,
                         formals()[-1],
                         list(ssb,SRModel$model,year,debug))}
  
  #residual
  eta <- SRModel$Resids[[as.character(year)]]
  
  #SSB
  ssb<-sum(ssb)
  
  if (ssb<0.001) {
    
    #no SSB - no recruits!
    ret <- c(0,eta,0)

  } else {
    
    #apply appropriate recruitment formulation
    E = switch(substr(SRModel$model,0,2),
               HS = SRModel$AParam*(ssb + sqrt(SRModel$BParam^2 + 0.25*SRModel$GParam^2) - sqrt((ssb - SRModel$BParam)^2 + 0.25*SRModel$GParam^2)),
               RK = as.numeric(SRModel$AParam)*as.numeric(ssb)*exp(-as.numeric(SRModel$BParam)*as.numeric(ssb)),
               BH = as.numeric(SRModel$AParam)*as.numeric(ssb)/(as.numeric(SRModel$BParam)+as.numeric(ssb))
    )
  
    #apply residual
    recr <- E*exp(eta)
    
    ret<-c(recr,eta,E)

  }
  
  if (debug==1){log.write.func.exit(log.file,ret)}
  
  ret
  
}

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
