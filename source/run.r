fFPRESS_run <- function(runref = "missing", saveref = "missing", debug = 0, det = FALSE, iterations = "missing", years = "missing",
                        varmin = "missing", varmax = "missing", resolution = "missing") {

  if (missing(runref)) {runref <- winDialogString("Run Reference?","")}  
  if (missing(saveref)) {saveref<-runref}
  
  optionsfile <- paste(FPRESS.Home(),"\\options\\","options",runref,".xml",sep="")
  
  #check options file exists
	if (!(file.exists(optionsfile))) {stop(paste("\nOptions file",optionsfile,"doesn't exist\nUnable to continue\n",sep=" "))}
	
  logfile <- paste(FPRESS.Home(),"\\runlogs\\",saveref,".log",sep="")

  file.create(logfile)
  flog_write_systime(logfile,TRUE)
  
  #read options
  lopt <- fread_options(optionsfile,match.call())
  flog_write_options(lopt = lopt, logfile = logfile)
  
  #create a vector of output filenames. The number of output files will be resolution+1
  FLRfiles <- paste("FLRObjects_",saveref,"_",1:(lopt$resolution+1),".dat",sep="")
	   
  #read in the initialisation data
  #file contains initialisation for aFec,bFec,qFec,sC,segg,spg,scoregg,Rec12,Bsp12,Bsp13,Bsp14,N2-N11,
  #F0-F10,ark,brk,sigRrk,scorrk,nllRrk,abh,bbh,sigRbh,scorbh,nllRbh,ahs,bhs,ghs,sigRhs,scorhs,nllRhs,neglnL,
  #Rec1982,Rec2001,SSB1982,SSB2001,Bloss with a new value for each iteration
  indata <- read.table(file=paste(FPRESS.Home(),"\\indata\\SADMSE2014_WHMParams_15_09_2014.dat",sep=""),header=TRUE,sep=",")
  
  #stock weights
  #historic stock weights
  dfSW <- read.table(file = paste(FPRESS.Home(),".\\indata\\WHM_StockWeights.dat",sep=""),
	                   header = FALSE,
	                   sep = ",",
	                   col.names = c("Year",seq(0,11,by=1)))
	
  spn.wgt <- data.matrix(dfSW)
  rownames(spn.wgt)<-spn.wgt[,1]
  spn.wgt<-spn.wgt[,-1]
  colnames(spn.wgt)<-c("Age0","Age1","Age2","Age3","Age4","Age5","Age6","Age7","Age8","Age9","Age10","Age11+")
	
	
  #catch weights
  cat.wgt <- data.matrix(read.table(file=paste(FPRESS.Home(),".\\indata\\WHM_CatchWeights.dat",sep=""),header=FALSE,sep=","))
  rownames(cat.wgt)<-cat.wgt[,1]
  cat.wgt<-cat.wgt[,-1]
  colnames(cat.wgt)<-c("Age0","Age1","Age2","Age3","Age4","Age5","Age6","Age7","Age8","Age9","Age10","Age11+")
	
  #weights to be selected from 1998-2013
  common.years <- seq(1998,2013)
	
  wgt.year <- common.years[sample.int(n=length(common.years),size=lopt$nits*lopt$years,replace=TRUE)]
  #convert to 2d, first dimension for iteration, second for year
  dim(wgt.year)<-c(lopt$nits,lopt$years)
	
  #egg counts
  dfEgg <- read.table(file = paste(FPRESS.Home(),".\\indata\\WHM_Eggs.dat",sep=""),
	                    header = TRUE,
	                    sep = ",")
	
  #historic numbers at age
  dfNum <- read.table(file = paste(FPRESS.Home(),".\\indata\\SADMSE2014_NumAtAge_15_09_2014.dat",sep=""),
	                    header = TRUE,
	                    sep = ",",
	                    col.names = paste("y",rep(seq(1982,2013,by=1),each=12),"a",seq(0,11),sep=""))
	
  #historic fishing mortality
  dfF <- read.table(file = paste(FPRESS.Home(),".\\indata\\SADMSE2014_FAtAge_15_09_2014.dat",sep=""),
	                  header = TRUE,
	                  sep = ",",
	                  col.names = paste("y",rep(seq(1982,2013,by=1),each=12),"a",seq(0,11),sep=""))
	
  #historic natural mortality
  dfNatMor  <- read.table(file = paste(FPRESS.Home(),".\\indata\\WHM_NaturalMortality.dat",sep=""),
	                        header = FALSE,
	                        sep = ",",
	                        col.names = c("Year",seq(0,11,by=1)))
	
  #historic maturity
  dfMat  <- read.table(file = paste(FPRESS.Home(),".\\indata\\WHM_Maturity.dat",sep=""),
	                     header = FALSE,
	                     sep = ",",
	                     col.names = c("Year",seq(0,11,by=1)))
  
  #stock-recruit pairs
  dfSR <- read.table(file = ".\\indata\\SADMSE2014_SRPairs_15_09_2014.dat",
                     header = TRUE,sep=",")
    
  #the number of distinct age classes.....
  numage <- 12
  
  #want the same residuals for each run
  set.seed(1)    
  lSR <- lapply(lopt$simiters, 
                FUN = fSADsr, 
                SADparams = indata, 
                SRpairs = dfSR,
                SR.types = f.RandSR(props = c(0.46,0.32,0.22), nits=lopt$nits),
                startyear = lopt$startyear,
                years = lopt$years,
                det = det)
    
  cat("lSR generated\n")

  fprop <- rep(0.45,numage)
  mprop <- rep(0.45,numage)
  m <- rep(0.15,numage)
  mature <- c(0,0,0.05,0.25,0.70,0.95,1.0,1.0,1.0,1.0,1.0,1.0)

  #egg models
  set.seed(1)
  lEgg <- lapply(lopt$simiters,
                 FUN = fSADegg,
                 nits = lopt$nits,
                 SADparams = indata,
	               EggHist = dfEgg,
                 StockWeights = dfSW,
                 NatMor = dfNatMor, 
                 Mat = dfMat,
	               NumatAge = dfNum, 
                 FatAge = dfF, 
                 SexRatio = 0.5, 
                 PM = 0.45, 
                 PF = 0.45, 
	               startyear = lopt$startyear,
                 years = lopt$years)
		
  cat("lEgg generated\n")

  #recruitment spikes, if required
  if (lopt$onceoff) {
    lrec_spikes <- lapply(seq(1:lopt$nits),function (x) fgenerate_spikes(mean_interval = 19, spike_var = 0.5, projyr = lopt$years, 
                                                                         offset = lopt$startyear - 2001, spike_years = c(1982,2001), 
                                                                         spike_probs = c(0.5,0.5)))
  }
  
  #dimension a number of arrays to store the quantities to be saved
  aCW <- aSSB.J1.true <- aSSB.ST.true <- apop.J1 <- apop.ST <- apop.YE <- af <- arf <- array(0,
                                                                                             dim = c(numage,lopt$years,1,1,1,lopt$nits),
                                                                                             dimnames = list(age=lopt$simages,year=lopt$simyears,unit='unique',season='all',area='all',iter=1:lopt$nits))
  
  arec <- aftac <- afbar <- array(0,
                                  dim = c(1,lopt$years,1,1,1,lopt$nits),
                                  dimnames = list(age=0,year=lopt$simyears,unit='unique',season='all',area='all',iter=1:lopt$nits))
  
  #loop over values of F,TAC
  for(res in 0:lopt$resolution) {

    #calculate the appropriate FTAC value for this resolution
    if (lopt$resolution>0) {

      resolution.ftac <- c((lopt$varmax-lopt$varmin)*res/lopt$resolution)+lopt$varmin

			#ensure we don't proceed with a 0 value
			#if(resolution.ftac==0){resolution.ftac <- 0.0001}

      cat(resolution.ftac,"\n")

    } else {
      
			resolution.ftac <- lopt$varmin
		
		}

		#update the console to ouput above messages
		flush.console()
    
		########################################################  
		#main iterative loop####################################
		#iter is the counter variable###########################
		########################################################
		for (iter in lopt$simiters) {
      
      #2013 recruitment
      Rec.lastyear <- frecruit34(log.file="", ssb=indata[iter,'Bsp13'], SRModel=lSR[[iter]], year=2013, debug=debug)[1]

      #initial population vector, age 1 derived from 2013 recruitment
			popint <- c(0,Rec.lastyear*exp(-1*(mprop[1]*m[1] + indata[iter,'F0'])),
                  indata[iter,'N2'],indata[iter,'N3'],indata[iter,'N4'],
                  indata[iter,'N5'],indata[iter,'N6'],indata[iter,'N7'],
                  indata[iter,'N8'],indata[iter,'N9'],indata[iter,'N10'],
                  indata[iter,'N11'])
            
			#selection
      fint <- c(indata[iter,'F0'],indata[iter,'F1'],indata[iter,'F2'],
                indata[iter,'F3'],indata[iter,'F4'],indata[iter,'F5'],
                indata[iter,'F6'],indata[iter,'F7'],indata[iter,'F8'],
                indata[iter,'F9'],indata[iter,'F10'],indata[iter,'F10'])

			#initial fbar
			fbar <- mean(fint[(lopt$fbarmin-lopt$minage+1):(lopt$fbarmax-lopt$minage+1)])
			
      #at start of iteration remove any predicted eggcounts i.e. those for years after the startyear
      dfEgg <- filter(dfEgg, Year<lopt$startyear)

			#if the resolution is zero then the simulation is running for a single 
			#TAC or F value. In this case report progress on basis of the number of iterations
			#completed. Issue report every 10%
			if (lopt$resolution == 0) {
				percent <- 100*iter/lopt$nits
				if ((percent%%10) == 0) {
					cat(percent,"% complete","\n",sep="")
				}
			}

			flush.console()
	
			#initialise ftac value
			ftac <- resolution.ftac
      
			for (year in 1:lopt$years) {

				#update the current simulation year
				simyear <- lopt$simyears[year]
                
        if (year==1) {          
          pop.J1 <- popint
        } else {
          #generate pop.J1 from previous pop.YE
          pop.J1 <- vector("numeric",length=numage)
          #recruits assigned later (at spawning time)
          pop.J1[1] <- 0
          pop.J1[2:(numage-1)] <- pop.YE[1:(numage-2)]
          #plus group
          pop.J1[numage] <- sum(pop.YE[(numage-1):numage])
        }
        
        #if the current year is a management year then apply the appropriate HCR and derive a new ftac
				if (fis_hcr_year(year = simyear, hcr_period = lopt$hcrperiod, first_management_year = lopt$firstmanagementyear)) {
				 
          #HCR requires an egg survey result
				  if (!(sum(dfEgg$Year==(simyear-1))==1)) {
            
				    #egg data already exists for 2013 data i.e. the first year of the simulation
				    dfEgg <- rbind(dfEgg,data.frame("Year" = (simyear-1),"EggCount" = feggcalc_mse2014(ssb = pop.ST*mature*spn.wgt[as.character(wgt.year[iter,year]),],
				                                                                                       wgt = spn.wgt[as.character(wgt.year[iter,year]),],
				                                                                                       EggModel = lEgg[[iter]],
				                                                                                       SexRatio = 0.5,
				                                                                                       year = simyear - 1)[1]))
				  }
          
          #new ftac
          ftac <- fhcr20(lopt = lopt, dfEgg = dfEgg, tac = ftac)
          
				}
        
        
        #if simyear is 2014,2015 or 2016 then TAC is 133220,99304,99304 respectively
        if (simyear==2014) {ftac <- 133220}
				if (simyear==2015 | simyear==2016) {ftac <- 99304}
        
				#scale fishing mortality to give requied ftac
				if (lopt$variable==0) {
				  #TAC controlled fishery
				  f <- fint*ffactor(tac = ftac, pop = pop.J1, fv = fint,
				                    wt = cat.wgt[as.character(wgt.year[iter,year]),],
				                    m = m, numage = numage)
				} else {
				  #f controlled
				  f <- fint*(ftac/fbar)  
				}
				
				fbar <- mean(f[(lopt$fbarmin-lopt$minage+1):(lopt$fbarmax-lopt$minage+1)])
        
        #proportion of fishing mortality that takes place before spawning
        fbef <- fprop*f
      
        #proportion of fishing mortality that takes place after spawning
        faft <- (1-fprop)*f
      
        #proportion of natural mortality that takes place before spawning
        mbef <- m*mprop
      
			  #proportion of natural mortality that takes place after spawning
			  maft <- m-mbef

			  #calculate the population at spawning time 
        pop.ST <- pop.J1*exp(-1*(m*mprop + f*fprop))
			
        #and catch from J1 to ST
	  		catch.ST <- (pop.J1*(f*fprop/(f*fprop+m*mprop))*(1-exp(-f*fprop-m*mprop)))
			
        #Calculate recruitment
        rec_ret <- frecruit34(log.file="", ssb = pop.ST*mature*spn.wgt[as.character(wgt.year[iter,year]),],
                              SRModel = lSR[[iter]], year = simyear, debug = debug)

        #override recruitment with spike if spikes are included and there is to be a spike this year
        if (lopt$onceoff) {          
          #if (rec_spikes[[iter]][year] > 0) {    
            #calculate residual to add to modelled recruitment
            if (lrec_spikes[[iter]][year] == 1982 | lrec_spikes[[iter]][year] == 2001){
                
              #calculate residual for iteration specific SR model
              rec_resid <- log(lSR[[iter]][[paste("Rec",lrec_spikes[[iter]][year],sep="")]]) - 
                log(frecruit25(log.file = "", ssb = lSR[[iter]][[paste("SSB",lrec_spikes[[iter]][year],sep="")]],
                               SRModel = lSR[[iter]], debug = debug, trunc = FALSE)[3])
              
              #overwrite value returned by recruit function
              rec_ret[1] <- rec_ret[3]*exp(rec_resid)
            }
          #}
        }

        #Add recruits to population
        pop.ST[1] <- rec_ret[1]

        #Project to year end
			  pop.YE <- pop.ST*exp(-1*(m*(1-mprop) + f*(1-fprop)))
			
			  catch.YE <- (pop.ST*(f*(1-fprop)/(f*(1-fprop)+m*(1-mprop)))*(1-exp(-f*(1-fprop)-m*(1-mprop))))
			      
        #output arrays
			  aftac[1,year,1,1,1,iter] <- ftac  
			  af[,year,1,1,1,iter] <- f
        arf[,year,1,1,1,iter] <- c(-1*log(pop.YE[1]/pop.ST[1])-m[1]*(1-mprop[1]),-1*log(pop.YE[-1]/c(pop.J1[-1]))-m[-1])
			  apop.J1[,year,1,1,1,iter] <- pop.J1
        apop.ST[,year,1,1,1,iter] <- pop.ST
        apop.YE[,year,1,1,1,iter] <- pop.YE
        aSSB.J1.true[,year,1,1,1,iter] <- pop.J1*mature*spn.wgt[as.character(wgt.year[iter,year]),]
        aSSB.ST.true[,year,1,1,1,iter] <- pop.ST*mature*spn.wgt[as.character(wgt.year[iter,year]),]
        aCW[,year,1,1,1,iter] <- (catch.ST + catch.YE)*cat.wgt[as.character(wgt.year[iter,year]),]
        arec[1,year,1,1,1,iter] <- rec_ret[1]
        afbar[1,year,1,1,1,iter] <- fbar

		  }#end year loop

	  }#end iter loop

    #Save data
	  op.CatchWeight <- FLQuant(aCW, dimnames = list(age=lopt$simages,year=lopt$simyears), units = 'Kgs')
    op.SSB.J1.true <- FLQuant(aSSB.J1.true,units='Kgs')
    op.SSB.ST.true <- FLQuant(aSSB.ST.true,units='Kgs')
    op.Pop.J1 <- FLQuant(apop.J1,units='000s')
    op.Pop.ST <- FLQuant(apop.ST,units='000s')
    op.Pop.YE <- FLQuant(apop.YE,units='000s')
    op.f <- FLQuant(af,units='F')
    op.fbar <- FLQuant(afbar,units='F')
    op.realisedf <- FLQuant(arf,units='F')
    op.Recruits <- FLQuant(arec,units='000s')
    op.FTac <- FLQuant(aftac,units='ftac')
    op.lSR <- lSR
    op.wgt.year <- wgt.year
    op.spn.wgt <- spn.wgt
    op.cat.wgt <- cat.wgt

    savedobjs <- c("op.CatchWeight","op.SSB.J1.true","op.Recruits","op.FTac","op.lSR",
                   "op.Pop.J1","op.Pop.ST","op.Pop.YE","op.wgt.year","op.spn.wgt",
                   "op.cat.wgt","op.SSB.ST.true","op.f","op.realisedf","op.fbar")

		#save specified objects
		if (substr(lopt$outdata.path,nchar(lopt$outdata.path),nchar(lopt$outdata.path))=="\\") {
			outfile <- paste(lopt$outdata.path,FLRfiles[res+1],sep="") 
		} else {
			outfile <- paste(lopt$outdata.path,"\\",FLRfiles[res+1],sep="")
		}

		#write output filename
		log.write.ln(logfile,paste("outfile::",outfile,sep=""))
		save(list=savedobjs,file=outfile)

	}#end resolution loop

}
