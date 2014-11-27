#functions to support the use of WHM SAD assessment model output in FPRESS

#15/09/2014
#fRandSR <- function(props=c(0.49,0.28,0.23),nits){
f.RandSR <- function(props=c(0.46,0.32,0.22),nits){
    
  #model proportions for BH,RK and HS (in THAT order!)
  #randomise SR models according to the proportions supplied
  
  SR.types <- vector("character",nits)
  
  SR.types[1:nits] <- "HS"
  SR.types[1:round((props[1]*nits))] <- "BH"
  SR.types[(round((props[1]*nits)) + 1):(round((props[1]*nits)) + 1 + round((props[2]*nits)))] <- "RK"
    
  SR.types
}

f.SADEgg <- function(iter,nits,SADparams,EggHist,StockWeights,NatMor,Mat,
                    NumatAge,FatAge,SexRatio=0.5,PM=0.45,PF=0.45,startyear,years){
  
  #generate list of egg production models, including future residuals

  #iter is the current iteration number
  #nits is the total number of iterations
  #SADparams is the df containing the SAD input vectors    
  #EggHist is the df containing the historic egg count data
  #StockWeights is the df containing the stock weights
  #NatMor  is the df containing the natural mortalities
  #Mat  is the df containing the maturities
  #NumAtAge is the df containing the historic numbers at age
  #FatAge is the df containing the historic fishing mortality at age
  #SexRatio - defaults to 0.5
  #PF - proportion fishing mortality prior to spawning, defaults to 0.45
  #PM - proportion natural mortality priot to spawning, defaults to 0.45
  #startyear is the first year of the simulation
  #years number of years of simulation

  #data years correspond to the years in the EggHist data frame
  datYrs<-EggHist$Year
  #future years
  futYrs<-seq(2016,startyear+years,by=3)
  
  #vectors for historic and future residuals
  ResidHist <- vector("numeric",length=length(datYrs))
  ResidFut <- vector("numeric",length=length(futYrs))
    
  #cat("datYrs=",datYrs,"\n")
  #cat("futYrs=",futYrs,"\n")
  
  #historic residuals
  for (d in (1:length(datYrs))){
    
    #extract vectors from data frames

    #stock weights (use earliest available if no SW data in file for egg survey year)
    if (sum(StockWeights$Year==datYrs[d])==0) {
      SW <- unlist(StockWeights[1,2:13])
    } else {
      SW <- unlist(StockWeights[StockWeights$Year==datYrs[d],2:13])
    }
    
    #cat("SW=",SW,"\n")
    
    #Num at age
    Num <- unlist(NumatAge[iter,paste('y',datYrs[d],'a',seq(0,11),sep="")])
    
    #cat("Num=",Num,"\n")
    
    #Maturity
    maturity <- unlist(Mat[Mat$Year==datYrs[d],2:13])
    
    #cat("maturity=",maturity,"\n")
    
    #Fishing Mortality
    f <- unlist(FatAge[iter,paste('y',datYrs[d],'a',seq(0,11),sep="")])
    #Natural mortaliy
    m <- unlist(NatMor[NatMor$Year==datYrs[d],2:13])
    
    Egg.mod <- sum(SADparams$qFec[iter]*(SADparams$aFec[iter] + SADparams$bFec[iter]*SW)*SexRatio*Num*SW*maturity*exp(-1*(PF*f+PM*m)))
    
    #cat("Egg.mod=",Egg.mod,"\n")
    
    #residual (log)
    ResidHist[d] <- log(1000.0*EggHist$EggCount[EggHist$Year==EggHist$Year[d]]) - log(Egg.mod)
    
  }
  
  names(ResidHist) <- datYrs
  
  #future residuals
  eta <- ResidHist[length(ResidHist)]
  for (d in (1:length(futYrs))){
    err <- rnorm(n=1,mean=0,sd=1)
    ResidFut[d] <- SADparams$scoregg[iter]*eta + sqrt(1-SADparams$scoregg[iter]^2)*err*SADparams$segg[iter]
    eta <- ResidFut[d]
  }
  
  names(ResidFut) <- futYrs
  
  #cat("ResidHist",ResidHist,"\n")
  
  ret <- list(aFec = SADparams$aFec[iter],
              bFec = SADparams$bFec[iter],
              qFec = SADparams$qFec[iter],
              SigR = SADparams$segg[iter],
              scor = SADparams$scoregg[iter],
              ResidHist = ResidHist,
              ResidFut = ResidFut)
  
}

f.SADSR <- function(iter,nits,SADparams,SRpairs,SR.types,startyear,years){
    
  #function returns SR details in a list
  
  #iter is the current iteration number
  #nits is the total number of iterations
  #SADparams is the df containing the SAD input vectors    
  #SRpairs is the df containing the historic SAD stock and recruit pairs
  #SR.types is a character vector specifying which model to use in each iteration,
  #usually supplied by function fRandSR
  #startyear is the first year of the simulation
  #years number of years of simulation
  
  #data years (no 1982,2001 spike years as models were not fit to data including these points)
  datYrs<-c(as.character(seq(1982,startyear-2)))
  
  #vectors for historic and future residuals
  Resids <- vector("numeric",length=length(datYrs)+years+1)
  names(Resids) <- seq(1982,length=length(Resids))
  
  ResidFut <- vector("numeric",length=years+1)  #additional year for year prior to start
  
  #Beverton & Holt
  if (SR.types[iter] == 'BH') {
    
    #historic residuals
    for (d in 1:length(datYrs)){
      Resids[d] <- log(SRpairs[iter,paste("Rec_",datYrs[d],sep="")]) - log(as.numeric(SADparams$abh[iter])*as.numeric(SRpairs[iter,paste("Bsp_",datYrs[d],sep="")])/(as.numeric(SADparams$bbh[iter])+as.numeric(SRpairs[iter,paste("Bsp_",datYrs[d],sep="")])))
    }
    
    #sample from historic residuals, ignoring 1982 and 2001
    ResidDraws <- sample(Resids[!(names(Resids)=='1982' | names(Resids)=='2001' | as.integer(names(Resids)) > datYrs[length(datYrs)])],size=years+1,replace=TRUE)
    
    #previous residual is initially the final observed one
    last.resid <- Resids[length(datYrs)]
    
    for (y in ((length(datYrs)+1):length(Resids))){
      Resids[y]<-SADparams$scorbh[iter]*last.resid + sqrt(1-SADparams$scorbh[iter]^2)*ResidDraws[y-length(datYrs)]
      last.resid <- Resids[y]
    }
    
    names(Resids) <- seq(1982,length=length(Resids))
    
    #historic SSB
    HistSSB <- SRpairs[iter,paste("Bsp_",c(as.character(seq(1982,startyear))),sep="")]
    names(HistSSB) <- as.character(seq(1982,startyear))

    #historic recruits
    HistRec <- SRpairs[iter,paste("Rec_",c(as.character(seq(1982,startyear-2))),sep="")]
    names(HistRec) <- as.character(seq(1982,startyear-2))
    
    ret <- list(model = 'BH',
                FLSRmodel = 'bevholt',
                AParam = SADparams$abh[iter],
                BParam = SADparams$bbh[iter],
                GParam = NA,
                SigR = SADparams$sigRbh[iter],
                scor = SADparams$scorbh[iter],
                Rec1982 = SADparams$Rec1982[iter],
                Rec2001 = SADparams$Rec2001[iter],
                SSB1982 = SADparams$SSB1982[iter],
                SSB2001 = SADparams$SSB2001[iter],
                Bloss = SADparams$Bloss[iter],
                Resids = Resids,
                HistSSB = HistSSB,
                HistRec = HistRec)

  } else if (SR.types[iter] == 'RK'){
    
    #historic residuals
    for (d in 1:length(datYrs)){
      Resids[d] <- log(SRpairs[iter,paste("Rec_",datYrs[d],sep="")]) - log(as.numeric(SADparams$ark[iter])*as.numeric(SRpairs[iter,paste("Bsp_",datYrs[d],sep="")])*exp(-as.numeric(SADparams$brk[iter])*as.numeric(SRpairs[iter,paste("Bsp_",datYrs[d],sep="")])))
    }
        
    #sample from historic residuals, ignoring 1982 and 2001
    ResidDraws <- sample(Resids[!(names(Resids)=='1982' | names(Resids)=='2001' | as.integer(names(Resids)) > datYrs[length(datYrs)])],size=years+1,replace=TRUE)
    
    #previous residual is initially the final observed one
    last.resid <- Resids[length(datYrs)]

    for (y in ((length(datYrs)+1):length(Resids))){
      Resids[y] <- SADparams$scorbh[iter]*last.resid + sqrt(1-SADparams$scorrk[iter]^2)*ResidDraws[y-length(datYrs)]
      last.resid <- Resids[y]
    }
    
    names(Resids) <- seq(1982,length=length(Resids))

    #historic SSB
    HistSSB <- SRpairs[iter,paste("Bsp_",c(as.character(seq(1982,startyear))),sep="")]
    names(HistSSB) <- as.character(seq(1982,startyear))
    
    #historic recruits
    HistRec <- SRpairs[iter,paste("Rec_",c(as.character(seq(1982,startyear-2))),sep="")]
    names(HistRec) <- as.character(seq(1982,startyear-2))
    
    ret <- list(model = 'RK',
                FLSRmodel = 'ricker',
                AParam = SADparams$ark[iter],
                BParam = SADparams$brk[iter],
                GParam = NA,
                SigR = SADparams$sigRrk[iter],
                scor = SADparams$scorrk[iter],
                Rec1982 = SADparams$Rec1982[iter],
                Rec2001 = SADparams$Rec2001[iter],
                SSB1982 = SADparams$SSB1982[iter],
                SSB2001 = SADparams$SSB2001[iter],
                Bloss = SADparams$Bloss[iter],
                Resids = Resids,
                HistSSB = HistSSB,
                HistRec = HistRec)
  } else {
    
    #Smooth Hockey Stick
    
    #historic residuals
    for (d in 1:length(datYrs)){
      Resids[d] <- log(SRpairs[iter,paste("Rec_",datYrs[d],sep="")]) - log(as.numeric(SADparams$ahs[iter])*(as.numeric(SRpairs[iter,paste("Bsp_",datYrs[d],sep="")]) + sqrt(as.numeric(SADparams$bhs[iter])^2 + 0.25*as.numeric(SADparams$ghs[iter])^2) - sqrt((as.numeric(SRpairs[iter,paste("Bsp_",datYrs[d],sep="")]) - as.numeric(SADparams$bhs[iter]))^2 + 0.25*as.numeric(SADparams$ghs[iter])^2)))
    }
    
    #sample from historic residuals, ignoring 1982 and 2001
    ResidDraws <- sample(Resids[!(names(Resids)=='1982' | names(Resids)=='2001' | as.integer(names(Resids)) > datYrs[length(datYrs)])],size=years+1,replace=TRUE)
    
    #previous residual is initially the final observed one
    last.resid <- Resids[length(datYrs)]

    for (y in ((length(datYrs)+1):length(Resids))){
      Resids[y] <- SADparams$scorbh[iter]*last.resid + sqrt(1-SADparams$scorhs[iter]^2)*ResidDraws[y-length(datYrs)]
      last.resid <- Resids[y]
    }
    
    names(Resids) <- seq(1982,length=length(Resids))
    
    #historic SSB
    HistSSB <- SRpairs[iter,paste("Bsp_",c(as.character(seq(1982,startyear))),sep="")]
    names(HistSSB) <- as.character(seq(1982,startyear))
    
    #historic recruits
    HistRec <- SRpairs[iter,paste("Rec_",c(as.character(seq(1982,startyear-2))),sep="")]
    names(HistRec) <- as.character(seq(1982,startyear-2))
    
    ret <- list(model = 'HS',
                FLSRmodel = 'segreg',
                AParam = SADparams$ahs[iter],
                BParam = SADparams$bhs[iter],
                GParam = SADparams$ghs[iter],
                SigR = SADparams$sigRhs[iter],
                scor = SADparams$scorhs[iter],
                Rec1982 = SADparams$Rec1982[iter],
                Rec2001 = SADparams$Rec2001[iter],
                SSB1982 = SADparams$SSB1982[iter],
                SSB2001 = SADparams$SSB2001[iter],
                Bloss = SADparams$Bloss[iter],
                Resids = Resids,
                HistSSB = HistSSB,
                HistRec = HistRec)
  }
  
  ret
  
}
