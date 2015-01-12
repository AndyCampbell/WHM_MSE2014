rm(list=ls())
gc()

#1000 parameter sets (the data I've been using)
indata <- read.table(file=".//indata//SADMSE2014_WHMParams_15_09_2014.dat",header=T,sep=",")

#Jose's objects
load(".//..//Jose//10_12_2014//srdev_spk0.RData")
#load(".//..//Jose//10_12_2014//whmproj_ryend.RData")
load(".//..//Jose//17_12_2014//whmproj_ryend.RData")
load(".//..//Jose//10_12_2014//srPar.rData")

#append Jose's ricker parameters to the indata data frame
indata$srPar_rk_a <- subset(as.data.frame(srPar_rk),params=="a")$data
indata$srPar_rk_b<- subset(as.data.frame(srPar_rk),params=="b")$data

#calculate the 2013 deterministic ricker recruitment
indata$RickerDet <- indata$ark*indata$Bsp13*exp(-indata$brk*indata$Bsp13)
#applying the deviation as supplied by Jose in his srDev_rk object
indata$Ricker <- indata$RickerDet*as.vector(srDev_rk[,'2013',,,])

#add the 2013 recruitment from Jose's FLR object
indata$RickerRec <- as.vector(ryendrk)

#now compare the two
#a parameters
table(indata$srPar_rk_a/indata$ark)
#b parameters
table(indata$srPar_rk_b/indata$brk)

#and a plot of the recruitments
plot(indata$Ricker,indata$RickerRec,type="p",pch=".")
abline(a=0,b=1)
hist(indata$Ricker/indata$RickerRec)
#bit of rounding required
table(round(indata$Ricker/indata$RickerRec,5))
table(round(indata$Ricker/indata$RickerRec,4))


#Comparison with Beverton & Holt
indata$srPar_bh_a <- subset(as.data.frame(srPar_bh),params=="a")$data
indata$srPar_bh_b <- subset(as.data.frame(srPar_bh),params=="b")$data
#deterministic draw
indata$BHDet <- indata$Bsp13*(indata$abh/(indata$bbh+indata$Bsp13))
#applying the deviation
indata$BH <- indata$BHDet*as.vector(srDev_bh[,'2013',,,])
#add the 2013 recruitment from Jose's FLR object
indata$BHRec <- as.vector(ryendbh)

#now compare the two
#a parameters
table(indata$srPar_bh_a/indata$abh)
#b parameters
table(indata$srPar_bh_b/indata$bbh)

plot(indata$BH,indata$BHRec,type="p",pch=".")
abline(a=0,b=1)
hist(indata$BH/indata$BHRec)

#bit of rounding required
table(round(indata$BH/indata$BHRec,5))
table(round(indata$BH/indata$BHRec,4))


#finally the hockey stick model
indata$srPar_hs_a <- subset(as.data.frame(srPar_hs),params=="a")$data
indata$srPar_hs_b <- subset(as.data.frame(srPar_hs),params=="b")$data

#3 parameter model (smooth hockey stick)
indata$SHS3Det <- indata$ahs*(indata$Bsp13 + sqrt(indata$bhs^2+0.25*indata$ghs^2) - sqrt((indata$Bsp13-indata$bhs)^2 + 0.25*indata$ghs^2))
indata$SHS3 <- indata$SHS3Det*as.vector(srDev_hs[,'2013',,,])
#2 parameter model 
indata$SHS2Det <- ifelse(indata$Bsp13<indata$srPar_hs_b,indata$srPar_hs_a*indata$Bsp13,indata$srPar_hs_a*indata$srPar_hs_b)
indata$SHS2 <- indata$SHS2Det*as.vector(srDev_hs[,'2013',,,])
#add the 2013 recruitment from Jose's FLR object
indata$SHSRec <- as.vector(ryendhs)

#3 parameter formulation
plot(indata$SHS3,indata$SHSRec,type="p",pch=".")
abline(a=0,b=1)
hist(indata$SHS3/indata$SHSRec)

#2 parameter formulation
plot(indata$SHS2,indata$SHSRec,type="p",pch=".")
abline(a=0,b=1)
hist(indata$SHS2/indata$SHSRec)

