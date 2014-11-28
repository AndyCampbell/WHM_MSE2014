#FPRESS.run.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#V1 10/07/2006\
#V2 10/10/2006	  tacopt,simtype,tacmin,tacmax,tacbias,taccv,fmin,fmax,fbarcv,fbias replaced with variable,resolution,varmin,varmax,varcv,varbias
#V2 16/10/2006	  function changed to FPRESS.Run(), runref missing, output directory changed to outdata
#V2.1 16/10/2008 - removed recruits bug (when recruits are added to pop vector)
#V2.11 24/11/2011 - update for WKWHMMP1 - hardcoded b egg parameter updated from 0.5 to 1.5
#					refreshed egg vector at the start of each iteration - should have new versio for this!!
#V2.12 31/01/2012 - added out.subfolder parameter. This permits the caller to specify a subfolder for output data.
#                   This subfolder will be created in the outdata folder if it doesn't already exist and the output will be
#                   written there. This functionality supports multiple runs using the same options file (such as for repeatability tests)
#                   avoiding conflicts in output.

#V2.11 24/11/2011
#V2.12 31/01/2012
#Sys.putenv("V:FPRESS.run.r"="2.1")
#Sys.putenv("V:FPRESS.run.r"="2.11")
#Sys.putenv("V:FPRESS.run.r"="2.12")
#assign the environment var V.FPRESS.run.r
set.env.var("V:FPRESS.run.r","2.12")

f.FPRESS.Run<-function(runref="missing",saveref="missing",batch=0,debug=0,runlog=TRUE,det=FALSE,
                     out.subfolder="",opiter="",iterations="missing",years="missing",
                     assessdelay="missing",rec="missing",TAC.ref="missing",weight.parameter="missing",
                     beta="missing",varmin="missing",varmax="missing",resolution="missing",histcatch="missing",
                     MSYBtrigger="missing",Fmsy="missing",Blim="missing",Bpa="missing",Flim="missing",Fpa="missing",
                     egglimit="missing",egggamma="missing")
{
	#runref - the simulation reference. Function will examine file options<runref>.xml for model parameters
	#batch - batch flag. If true, the function will not prompt user for any settings and will use defaults.
	#debug flag - if true, print debug information to the screen
	#runlog - if true invokes runlog checking functionality (for parallel runs)
  #det - if true all noise sources ignored (deterministic run)
  
	if (!(is.numeric(batch))) stop("FPRESS.Run.r::parameter batch must be of type numeric \n")
	if (!(is.numeric(debug))) stop("FPRESS.Run.r::parameter debug must be of type numeric \n")
	if (!(batch==1) & !(batch==0)) stop("FPRESS.Run.r::parameter batch must be 0 or 1 \n")
	if (!(debug==1) & !(debug==0)) stop("FPRESS.Run.r::parameter debug must be 0 or 1 \n")
	if (!(runlog==1) & !(runlog==0)) stop("FPRESS.Run.r::parameter debug must be 0 or 1 \n")

	#check for simulation reference in runlogs directory. Implemented for parallel runs
  #or for simply overwriting existing log file
	#if (file.exists(paste(FPRESS.Home(),"\\runlogs\\",runref,sep=""))) {
	if (missing(saveref)) {saveref<-runref}
	#log.file <- paste(FPRESS.Home(),"\\runlogs\\",saveref,".log",sep="")
	
	if (opiter=="") {
	  log.file <- paste(FPRESS.Home(),"\\runlogs\\",saveref,".log",sep="")
	} else {
	  log.file <- paste(FPRESS.Home(),"\\runlogs\\",saveref,"_",opiter,".log",sep="")
	}
	
	if (file.exists(log.file)) {
		if (runlog & batch) {
			stop(runref," currently running or logfile exists\n")
		} else {
      #remove current log file
      file.remove(log.file)
      #create a new one & start logging
      file.create(log.file)
      log.write.systime(log.file,TRUE)
		}
	} else {
		file.create(log.file)
		log.write.systime(log.file,TRUE)
	}	

	#log the current file versions
	vers <- Sys.getenv()
	for (i in 1:length(names(vers))){
		if (substr(names(vers)[i],1,2)=="V:") {
			ver <- Sys.getenv(names(vers)[i])
			log.write.ln(log.file,paste(names(vers)[i],ver,sep=" = "),TRUE)
		}
	}
 
  #write the function call
  #ll <- as.list(match.call())[-1]  
  #myfor <- formals(FPRESS.Run)
  #for (v in names(myfor)){
  #  if (!(v %in% names(ll))) ll <- append(ll,myfor[v])
  #}
  #cat(names(ll),"\n")
  #log.write.ln(log.file,ll)
     
	#save the current working directory
	currwd <- getwd()

	#get the FPRESS home directory
	home <- FPRESS.Home()

	#check that the environment variable FPRESSHome is not empty
	if (nchar(home)==0) {
    log.write.ln(log.file,"Exiting - no FPRESSHome environment variable")
    stop("Exiting - no FPRESSHome environment variable")
	}
  
	log.write.ln(log.file,paste("home::",home,sep=""))

	#prompt for the run reference (only if not in batch mode)
	if (batch==0) {
		if (missing(runref)) { 
			runref <- winDialogString("Run Reference?","")
		}
	} else {
		if (missing(runref)) stop("No run reference")
	}

  if (missing(saveref)) saveref<-runref
  
	if (nchar(runref)==0) stop("Exiting - no run reference provided")

	#runref
	log.write.ln(log.file,paste("runref::",runref,sep=""))
	#saveref
	log.write.ln(log.file,paste("saveref::",saveref,sep=""))
	#batch mode
	#log.write.ln(log.file,paste("batch::",batch,sep=""))
	#debug flag
	#log.write.ln(log.file,paste("debug::",debug,sep=""))
	#out.subfolder
	#log.write.ln(log.file,paste("out.subfolder::",if (missing(out.subfolder)) {"missing"} else {out.subfolder},sep=""))
	#iterations
	#log.write.ln(log.file,paste("iterations::",if (missing(iterations)) {"missing"} else {iterations},sep=""))
	#years
	#log.write.ln(log.file,paste("years::",if (missing(years)) {"missing"} else {years},sep=""))
	#assessdelay
	#log.write.ln(log.file,paste("assessdelay::",if (missing(assessdelay)) {"missing"} else {assessdelay},sep=""))
	#beta
	#log.write.ln(log.file,paste("beta::",if (missing(beta)) {"missing"} else {beta},sep=""))
	#call args
  
	#create an empty vector into which the names of the objects to be saved will be written
	savedobjs <- c()

	#construct the name of the relevant xml file containing simulation parameters
	#the file is located in the folder given in variable options.path which is set
	#in the header script (header.r). <runref> is appended to "options" to construct
	#the filename
	optionsfile = paste(FPRESS.Home(),"\\options\\","options",runref,".xml",sep="")
  
	#check options file exists
	if (!(file.exists(optionsfile))) {
		stop(paste("\nOptions file",optionsfile,"doesn't exist\nUnable to continue\n",sep=" "))
	}

	log.write.ln(log.file,paste("optfile::",optionsfile,sep=""),TRUE)
	#log.write.options(log.file,optionsfile,TRUE)
	
	#load the simulation options xml document into memory using the xml library xmlTreeParse function
	doc <- xmlTreeParse(optionsfile)

	#root xmlNode object
	root <- xmlRoot(doc)

	#read the simulation options into local variables, casting to the appropriate data type where necessary
	#stockname - a general comment describing the simulation
	stockname <- nodeval(root,"stockname","")
	log.write.ln(log.file,paste("stockname::",stockname,sep=""),TRUE)
	
	#outfolder
	outfolder <- nodeval(root,"outfolder",FPRESS.Home())
	log.write.ln(log.file,paste("outfolder::",outfolder,sep=""),TRUE)
	
	#indata.path - the folder where the input data files are stored
	indata.path <- paste(FPRESS.Home(),"\\indata\\",sep="")
	log.write.ln(log.file,paste("indata.path::",indata.path,sep=""),TRUE)
  if (debug==1) {cat("indata.path=",indata.path,"\n")}

	#outdata.path - the folder where the output data files are stored
	outdata.path <- paste(outfolder,"\\outdata\\",sep="")
  #V2.12 changes - check for out.subfolder
  if (!(missing(out.subfolder))) {
    outdata.path <- paste(outdata.path,out.subfolder,"\\",sep="")
    d<-dir.create(outdata.path,recursive=TRUE)
  } 
	log.write.ln(log.file,paste("outdata.path::",outdata.path,sep=""),TRUE)
	if (debug==1) {cat("outdata.path=",outdata.path,"\n")}

	#datfile - the name of the data file containing the initialisation data profiles
	datfile <- nodeval(root,"datfile","")
	log.write.ln(log.file,paste("datfile::",datfile,sep=""),TRUE)
	if (debug==1) {cat("datfile=",datfile,"\n")}

	#recfile - the name of the file containing the historic stock and recruitment data   
	recfile <- nodeval(root,"recfile","")
	log.write.ln(log.file,paste("recfile::",recfile,sep=""),TRUE)
	if (debug==1) {cat("recfile=",recfile,"\n")}

	#catchfile - the name of the file containing the historic catch data
	catchfile <- nodeval(root,"catchfile","")
	log.write.ln(log.file,paste("catchfile::",catchfile,sep=""),TRUE)
	if (debug==1) {cat("catchfile=",catchfile,"\n")}
  
  #24/06/2013
  #SRfile - the name of the file containing SR models
	SRfile <- nodeval(root,"SRfile","")
	log.write.ln(log.file,paste("SRfile::",SRfile,sep=""),TRUE)
	if (debug==1) {cat("SRfile=",SRfile,"\n")}
  
  #10/02/2014
  #EGGfile - the name of the file containing the egg count time series
  Eggfile <- nodeval(root,"Eggfile","")
	log.write.ln(log.file,paste("Eggfile::",Eggfile,sep=""),TRUE)
	if (debug==1) {cat("Eggfile=",Eggfile,"\n")}
  #Stock Weights file
  SWfile  <- nodeval(root,"SWfile","")
	log.write.ln(log.file,paste("SWfile::",SWfile,sep=""),TRUE)
	if (debug==1) {cat("SWfile=",SWfile,"\n")}
  #Number at Age file
	Numfile  <- nodeval(root,"Numfile","")
	log.write.ln(log.file,paste("Numfile::",Numfile,sep=""),TRUE)
	if (debug==1) {cat("Numfile=",Numfile,"\n")}
	#Fishing Mortality at Age file
	Ffile  <- nodeval(root,"Ffile","")
	log.write.ln(log.file,paste("Ffile::",Ffile,sep=""),TRUE)
	if (debug==1) {cat("Ffile=",Ffile,"\n")}
	#Natural Mortality at Age file
	NatMorfile  <- nodeval(root,"NatMorfile","")
	log.write.ln(log.file,paste("NatMorfile::",NatMorfile,sep=""),TRUE)
	if (debug==1) {cat("NatMorfile=",NatMorfile,"\n")}
	#Maturity at Age file
	Matfile  <- nodeval(root,"Matfile","")
	log.write.ln(log.file,paste("Matfile::",Matfile,sep=""),TRUE)
	if (debug==1) {cat("Matfile=",Matfile,"\n")}
	
	#minage - the minimum age group for the stock in the simulation
	minage <- as.integer(nodeval(root,"minage",1))
	log.write.ln(log.file,paste("minage::",minage,sep=""),TRUE)
	if (debug==1) {cat("minage=",minage,"\n")}

	#maxage - the maximum age group for the stock in the simulation (treated as a plus group)
	maxage <- as.integer(nodeval(root,"maxage",1))
	log.write.ln(log.file,paste("maxage::",maxage,sep=""),TRUE)
	if (debug==1) {cat("maxage=",maxage,"\n")}

	#fbarmin - the youngest age class to be used in the calculation of fbar
	fbarmin <- as.integer(nodeval(root,"fbarmin",0))
	log.write.ln(log.file,paste("fbarmin::",fbarmin,sep=""),TRUE)
	if (debug==1) {cat("fbarmin=",fbarmin,"\n")}

	#fbarmax - the oldest age class to be used in the calculation of fbar
	fbarmax <- as.integer(nodeval(root,"fbarmax",1))
	log.write.ln(log.file,paste("fbarmax::",fbarmax,sep=""),TRUE)
	if (debug==1) {cat("fbarmax=",fbarmax,"\n")}

	#ssbtest1 - virtual ssb test level 1 (often corresponding to Blim)
	ssbtest1 <- as.numeric(nodeval(root,"ssbtest1",0))
	log.write.ln(log.file,paste("ssbtest1::",ssbtest1,sep=""),TRUE)
	if (debug==1) {cat("ssbtest1=",ssbtest1,"\n")}

	#ssbtest2 - virtual ssb test level 2 (often corresponding to Bpa)
	ssbtest2 <- as.numeric(nodeval(root,"ssbtest2",0))
	log.write.ln(log.file,paste("ssbtest2::",ssbtest2,sep=""),TRUE)
	if (debug==1) {cat("ssbtest2=",ssbtest2,"\n")}

	#resolution
	if (missing(resolution)) resolution <- as.numeric(nodeval(root,"resolution",0))
	log.write.ln(log.file,paste("resolution::",resolution,sep=""),TRUE)
	if (debug==1) {cat("resolution=",resolution,"\n")}

	#startyear - the start year for the simulation
	startyear <- as.integer(nodeval(root,"startyear",1))
	log.write.ln(log.file,paste("startyear::",startyear,sep=""),TRUE)	
	if (debug==1) {cat("startyear=",startyear,"\n")}

	#years - the number of years the simulation is to run for
  if (missing(years)){years <- as.integer(nodeval(root,"years",1))}
	log.write.ln(log.file,paste("years::",years,sep=""),TRUE)
	if (debug==1) {cat("years=",years,"\n")}

	#iterations - the number of iterations to be performed for each TAC/F value
	if (missing(iterations)) {iterations <- as.integer(nodeval(root,"iterations",1))}
	log.write.ln(log.file,paste("iterations::",iterations,sep=""),TRUE)	
	if (debug==1) {cat("iterations=",iterations,"\n")}

	#histrec - the historical recruitment flag. Can only be 0 or 1. If 1 then historical recruitment
	#values as given in the recfile are used, otherwise not.
	histrec <- as.integer(nodeval(root,"histrec",0))
	log.write.ln(log.file,paste("histrec::",histrec,sep=""),TRUE)
	if (debug==1) {cat("histrec=",histrec,"\n")}

	#histreccv - the coefficient of variation to be applied when adding in noise to the 
	#historical recruitment values. Only used when histrec=1
	histreccv <- as.numeric(nodeval(root,"histreccv",0))
	log.write.ln(log.file,paste("histreccv::",histreccv,sep=""),TRUE)
	if (debug==1) {cat("histreccv=",histreccv,"\n")}

	#histcatch - the historical catch flag. Can only be 0 or 1. If 1 then the historical catch
	#values as given in the catchfile are used, otherwise not.
	if (missing(histcatch)) histcatch <- as.integer(nodeval(root,"histcatch",0))
	log.write.ln(log.file,paste("histcatch::",histcatch,sep=""),TRUE)
	if (debug==1) {cat("histcatch=",histcatch,"\n")}

	#histcatchcv - the coefficient of variation to be applied when adding noise to the
	#historical catch values. Only used when histcatch=1
	histcatchcv <- as.numeric(nodeval(root,"histcatchcv",0))
	log.write.ln(log.file,paste("histcatchcv::",histcatchcv,sep=""),TRUE)
	if (debug==1) {cat("histcatchcv=",histcatchcv,"\n")}

	#rec - the recruitment model to be used
	if (missing(rec)){rec <- as.integer(nodeval(root,"rec",0))}
	log.write.ln(log.file,paste("rec::",rec,sep=""),TRUE)
	if (debug==1) {cat("rec=",rec,"\n")}

	#recmean - the mean recruitment values (in thousands). 
  #Only used for some of the recruitment models (see recruit.r).
	recmean <- as.numeric(nodeval(root,"recmean",0))
	log.write.ln(log.file,paste("recmean::",recmean,sep=""),TRUE)
	if (debug==1) {cat("recmean=",recmean,"\n")}

	#recmax - maximum permitted recruitment
	recmax <- as.numeric(nodeval(root,"recmax",0))
	log.write.ln(log.file,paste("recmax::",recmax,sep=""),TRUE)
	if (debug==1) {cat("recmax=",recmax,"\n")}

	#reccv - the coefficient of variation to be used when adding in noise to the caluclated
	#recruitment value. Only used for some of the recruitment models (see recruit.r).
	reccv <- as.numeric(nodeval(root,"reccv",0))
	log.write.ln(log.file,paste("reccv::",reccv,sep=""),TRUE)
	if (debug==1) {cat("reccv=",reccv,"\n")}

	#seggrad - the gradient of the recruitment vs SSB line on a stock-recruitment diagram. Only
	#used for some of the recruitment models (see recruit.r).
	seggrad <- as.numeric(nodeval(root,"seggrad",1))
	log.write.ln(log.file,paste("seggrad::",seggrad,sep=""),TRUE)
	if (debug==1) {cat("seggrad=",seggrad,"\n")}

	#ssbcut - the SSB cutoff point to use when calculating the number of recruits. Only used for some
	#of the recruitment models (see recruit.r).
	ssbcut <- as.numeric(nodeval(root,"ssbcut",0))
	log.write.ln(log.file,paste("ssbcut::",ssbcut,sep=""),TRUE)
	if (debug==1) {cat("ssbcut=",ssbcut,"\n")}

	#datarecyear - the final year of historical recruitment data contained in recfile that is to be 
	#included in the simulation (including the bootstrapping options). This parameter permits the use of 
	#only a portion of the historical recruitment data. If greater than the final year's data then all 
	#data in the file will be used
	datarecyear <- as.integer(nodeval(root,"datarecyear",0))
	log.write.ln(log.file,paste("datarecyear::",datarecyear,sep=""),TRUE)
	if (debug==1) {cat("datarecyear=",datarecyear,"\n")}

	#variable - indicates whether the fishery should be TAC controlled (0) or F controlled (1)
	variable <- as.numeric(nodeval(root,"variable",0))
	log.write.ln(log.file,paste("variable::",variable,sep=""),TRUE)
	if (debug==1) {cat("variable=",variable,"\n")}

	#varcv - the coefficient of variation to used when adding in noise to the controlling variable
	varcv <- as.numeric(nodeval(root,"varcv",0))
	log.write.ln(log.file,paste("varcv::",varcv,sep=""),TRUE)
	if (debug==1) {cat("varcv=",varcv,"\n")}
   
	#varbias - the amount of bias to apply when randomising the controlling variable
	varbias <- as.numeric(nodeval(root,"varbias",1))
	log.write.ln(log.file,paste("varbias::",varbias,sep=""),TRUE)
	if (debug==1) {cat("varbias=",varbias,"\n")}

	#varmin - the minimum (first) or only (for resolution=0) value to use for the controlling variable
	if (missing(varmin)) varmin <- as.numeric(nodeval(root,"varmin",1))
	log.write.ln(log.file,paste("varmin::",varmin,sep=""),TRUE)
	if (debug==1) {cat("varmin=",varmin,"\n")}

	#varmax - for simulations with resolution>0, he maximum (final) value to use for the controlling variable
	if (missing(varmax)) varmax <- as.numeric(nodeval(root,"varmax",1))
	log.write.ln(log.file,paste("varmax::",varmax,sep=""),TRUE)
	if (debug==1) {cat("varmax=",varmax,"\n")}

	#assessdelay - this parameter controls the timing of assessment events. The assessment will be based
	#on simulated data corresponding to the current year less the number of years given in assessdelay.
	if (missing(assessdelay)) {assessdelay<- as.integer(nodeval(root,"assessdelay",1))}
	log.write.ln(log.file,paste("assessdelay::",assessdelay,sep=""),TRUE)
	if (debug==1) {cat("assessdelay=",assessdelay,"\n")}
   
	#ssbassessbias - the amount of bias to apply to the ssb value during the assessment
	ssbassessbias <- as.numeric(nodeval(root,"ssbassessbias",1))
	log.write.ln(log.file,paste("ssbassessbias::",ssbassessbias,sep=""),TRUE)
	if (debug==1) {cat("ssbassessbias=",ssbassessbias,"\n")}
   
	#ssbassesscv - the coefficient of variation to use with the ssb during the assessment
	ssbassesscv <- as.numeric(nodeval(root,"ssbassesscv",0))
	log.write.ln(log.file,paste("ssbassesscv::",ssbassesscv,sep=""),TRUE)
	if (debug==1) {cat("ssbassesscv=",ssbassesscv,"\n")}
   
	#fbarassessbias - the amount of bias to apply to F during the assessment
	fbarassessbias <- as.numeric(nodeval(root,"fbarassessbias",1))
	log.write.ln(log.file,paste("fbarassessbias::",fbarassessbias,sep=""),TRUE)
	if (debug==1) {cat("fbarassessbias=",fbarassessbias,"\n")}
   
	#fbarassesscv - the coefficient of variation to use with F during the assessment
	fbarassesscv <- as.numeric(nodeval(root,"fbarassesscv",0))
	log.write.ln(log.file,paste("fbarassesscv::",fbarassesscv,sep=""),TRUE)
	if (debug==1) {cat("fbarassesscv=",fbarassesscv,"\n")}
   
	#hcrrule - an integer value corresponding to the appropriate harvest control rule function (see hcr.r)
	hcrrule <- as.integer(nodeval(root,"hcrrule",0))
	log.write.ln(log.file,paste("hcrrule::",hcrrule,sep=""),TRUE)
	if (debug==1) {cat("hcrrule=",hcrrule,"\n")}
   
	#hcrperiod - the period (in years) between harvest control rule actions being applied to the assessment
	hcrperiod <- as.integer(nodeval(root,"hcrperiod",1))
	log.write.ln(log.file,paste("hcrperiod::",hcrperiod,sep=""),TRUE)
	if (debug==1) {cat("hcrperiod=",hcrperiod,"\n")}
  
	#firstmanagementyear - the first year in which the HCR is to be activated
	firstmanagementyear <- as.integer(nodeval(root,"firstmanagementyear",startyear))
	log.write.ln(log.file,paste("firstmanagementyear::",firstmanagementyear,sep=""),TRUE)
	if (debug==1) {cat("firstmanagementyear=",firstmanagementyear,"\n")}
 
	#hcrchange - the proportional change to TAC of F (as a result of an HCR) permitted in a single year
	hcrchange <- as.numeric(nodeval(root,"hcrchange",1))
	log.write.ln(log.file,paste("hcrchange::",hcrchange,sep=""),TRUE)
	if (debug==1) {cat("hcrchange=",hcrchange,"\n")}

	#suspend - suspend any hcrchange restrictions below ssbhcr?
	suspend <- as.integer(nodeval(root,"suspend",0))
	log.write.ln(log.file,paste("suspend::",suspend,sep=""),TRUE)
	if (debug==1) {cat("suspend=",suspend,"\n")}

	#ssbhcr - reference SSB level used by some HCRs for comparison with simulated SSB levels
	ssbhcr <- as.numeric(nodeval(root,"ssbhcr",0))
	log.write.ln(log.file,paste("ssbhcr::",ssbhcr,sep=""),TRUE)
	if (debug==1) {cat("ssbhcr=",ssbhcr,"\n")}

	#fbarhcr - reference fbar value used by some HCRs for comparison with simulated fbar values
	fbarhcr <- as.numeric(nodeval(root,"fbarhcr",0))
	log.write.ln(log.file,paste("fbarhcr::",fbarhcr,sep=""),TRUE)
	if (debug==1) {cat("fbarhcr=",fbarhcr,"\n")}

	#minTAC - if a minimum TAC must be taken then this value will be greater than zero
	#applies only with variable=0 
	mintac <- as.numeric(nodeval(root,"minimumtac",0))
	log.write.ln(log.file,paste("mintac::",mintac,sep=""),TRUE)
	if (debug==1) {cat("mintac=",mintac,"\n")}

	#maxTAC
	#applies only with variable=0 
	maxtac <- as.numeric(nodeval(root,"maximumtac",0))
	log.write.ln(log.file,paste("maxtac::",maxtac,sep=""),TRUE)
	if (debug==1) {cat("maxtac=",maxtac,"\n")}

	#WHM sims - extra recruitment parameters
	#allow once off (specified) recruitments?
	onceoff <- as.integer(nodeval(root,"onceoff",0))
	log.write.ln(log.file,paste("onceoff::",onceoff,sep=""),TRUE)
	if (debug==1) {cat("onceoff=",onceoff,"\n")}

	#if once off recruitments are permitted, this is the size of the recruitment
	onceoffrec <- as.numeric(nodeval(root,"onceoffrec",0))
	log.write.ln(log.file,paste("onceoffrec::",onceoffrec,sep=""),TRUE)
	if (debug==1) {cat("onceoffrec=",onceoffrec,"\n")}

	#if once off recruitments are allowed, this is how often they occur
	onceoffevery <- as.integer(nodeval(root,"onceoffevery",0))
	log.write.ln(log.file,paste("onceoffevery::",onceoffevery,sep=""),TRUE)
	if (debug==1) {cat("onceoffevery=",onceoffevery,"\n")}
  
  #truncate residuals
  truncateresid <- as.integer(nodeval(root,"truncateresid",0))
	log.write.ln(log.file,paste("truncateresid::",truncateresid,sep=""),TRUE)
	if (debug==1) {cat("truncateresid=",truncateresid,"\n")}
  
	#is the HCR based on a targetyield or a moveable TAC?
	targetyield<-as.integer(nodeval(root,"targetyield",1))
	log.write.ln(log.file,paste("targetyield::",targetyield,sep=""),TRUE)
	if (debug==1) {cat("targetyield=",targetyield,"\n")}

	#HCR10 (slope strategy) parameters 
	#reference TAC
	if (missing(TAC.ref)) TAC.ref <- as.numeric(nodeval(root,"tacref",0))
	log.write.ln(log.file,paste("tacref::",TAC.ref,sep=""),TRUE)
	#weight parameter
	if (missing(weight.parameter)) weight.parameter <- as.numeric(nodeval(root,"weightparameter",0))
	log.write.ln(log.file,paste("weight.parameter::",weight.parameter,sep=""),TRUE)
	#beta value
  if (missing(beta)) beta <- as.numeric(nodeval(root,"beta",0))
	log.write.ln(log.file,paste("beta::",beta,sep=""),TRUE)

	#opindex
	opindex <- as.numeric(nodeval(root,"opindex",0))

	if (opindex >= 1024) {
		savedobjs <- c(savedobjs,"op.YearEndStock")
		opindex <- opindex - 1024
	}

	if (opindex >= 512) {
		savedobjs <- c(savedobjs,"op.CatchWeight")
		opindex <- opindex - 512
	}

	if (opindex >= 256) {
		savedobjs <- c(savedobjs,"op.Catch")
		opindex <- opindex - 256
	}

	if (opindex >= 128) {
		savedobjs <- c(savedobjs,"op.FBar")
		opindex <- opindex - 128
	}

	if (opindex >= 64) {
		#savedobjs <- c(savedobjs,"NewSpawnMass","SSB.st.true","SSB.J1.true")
		savedobjs <- c(savedobjs,"op.SSB.st.true","op.SSB.J1.true")
		opindex <- opindex - 64
	}
   
	if (opindex >= 32) {
		savedobjs <- c(savedobjs,"op.FishingMortalityMult")
		opindex <- opindex - 32
	}

	if (opindex >= 16) {
		savedobjs <- c(savedobjs,"op.FishingMortality")
		opindex <- opindex - 16
	}

	if (opindex >= 8) {
		savedobjs <- c(savedobjs,"op.FTac")
		opindex <- opindex - 8
	}

	if (opindex >= 4) {
		savedobjs <- c(savedobjs,"op.Pop")
		opindex <- opindex - 4
	}

	if (opindex >= 2) {
		savedobjs <- c(savedobjs,"op.Recruits")
		savedobjs <- c(savedobjs,"op.RecruitsDet")
		opindex <- opindex - 2
	}

	if (opindex >= 1) {
		savedobjs <- c(savedobjs,"op.FTacMult")
		opindex <- opindex - 1 
	}

	#additional object for HCR10
	savedobjs <- c(savedobjs,"op.EggCounts")
  
  #SRModel,EggModel details
  #savedobjs <- c(savedobjs,"SRModels")
	savedobjs <- c(savedobjs,"op.lSR","op.lEgg")

	#cat("savedobjs=",savedobjs,"\n")
	
	if (debug==1) {
    log.write.ln(log.file,paste("debug::savedobjs::",savedobjs,sep=""))
  }
	
  
  #29/09/2014
  #reference points
  #MSYBtrigger
	if (missing(MSYBtrigger)) MSYBtrigger <- as.numeric(nodeval(root,"MSYBtrigger",0))
	log.write.ln(log.file,paste("MSYBtrigger::",MSYBtrigger,sep=""),TRUE)
	if (debug==1) {cat("MSYBtrigger=",MSYBtrigger,"\n")}
  #Fmsy
	if (missing(Fmsy)) Fmsy <- as.numeric(nodeval(root,"Fmsy",0))
	log.write.ln(log.file,paste("Fmsy::",Fmsy,sep=""),TRUE)
	if (debug==1) {cat("Fmsy=",Fmsy,"\n")}
	#Blim
	if (missing(Blim)) Blim <- as.numeric(nodeval(root,"Blim",0))
	log.write.ln(log.file,paste("Blim::",Blim,sep=""),TRUE)
	if (debug==1) {cat("Blim=",Blim,"\n")}
	#Bpa
	if (missing(Bpa)) Bpa <- as.numeric(nodeval(root,"Bpa",0))
	log.write.ln(log.file,paste("Bpa::",Bpa,sep=""),TRUE)
	if (debug==1) {cat("Bpa=",Bpa,"\n")}
	#Flim
	if (missing(Flim)) Flim <- as.numeric(nodeval(root,"Flim",0))
	log.write.ln(log.file,paste("Flim::",Flim,sep=""),TRUE)
	if (debug==1) {cat("Flim=",Flim,"\n")}
	#Fpa
	if (missing(Fpa)) Fpa <- as.numeric(nodeval(root,"Fpa",0))
	log.write.ln(log.file,paste("Fpa::",Fpa,sep=""),TRUE)
	if (debug==1) {cat("Fpa=",Fpa,"\n")}
  
  #egg hcr 20 protection rule parameters
  if (missing(egglimit)) egglimit <- as.numeric(nodeval(root,"egglimit",0))
	log.write.ln(log.file,paste("egglimit::",egglimit,sep=""),TRUE)
	if (debug==1) {cat("egglimit=",egglimit,"\n")}
  
	if (missing(egggamma)) egggamma <- as.numeric(nodeval(root,"egggamma",1))
	log.write.ln(log.file,paste("egggamma::",egggamma,sep=""),TRUE)
	if (debug==1) {cat("egggamma=",egggamma,"\n")}
  
	datainfile <- datfile
	recruitinfile <- recfile
	catchinfile <- catchfile
	
	############################################################################################################
	#####################----------SIMULATION EXECUTION-----------------------##################################
	############################################################################################################

	#print some information to the screen & store the start time in a local variable.
	#on completion of the simulation this can be compared with the system time
	#to calculate the elapsed time for the run.
	print(stockname)
	print(Sys.time())
	startdate<-as.POSIXlt(Sys.time(),Sys.timezone())
	
	#create a vector of output filenames. The number of output files will be resolution+1
	#FLRfiles <- paste("FLRObjects_",runref,"_",1:(resolution+1),".dat",sep="")
	#FLRfiles <- paste("FLRObjects_",saveref,"_",1:(resolution+1),".dat",sep="")
  if (opiter=="") {
    FLRfiles <- paste("FLRObjects_",saveref,"_",1:(resolution+1),".dat",sep="")
  } else {
    FLRfiles <- paste("FLRObjects_",saveref,"_",opiter,"_",1:(resolution+1),".dat",sep="")  
  }
	   
	#CHANGE HERE - this code is not necessary and should be moved within the resolution/iteration loop
	#initialise the local variable ftac to the minimum value of the controlling variable (varmin)
	ftac <- varmin

	#the ftac value used to control the fishery is modified in various ways e.g. by the assessment and management
	#module. This is usually done by calculating a multiplier. Here, a couple of local variables used to store
	#the multiplier are declared and initialised to unity.
	#the multiplier
	ftacmult <- 1
	#temporal storage
	oldftacmult <- 1
 
	#initialise a variable to hold the final year for the projection
	endyear <- startyear + years - 1

	#check initialisation data file exists
	if (!(file.exists(datainfile))) {
		stop(paste("\nInitialisation file",datainfile,"doesn't exist\nUnable to continue\n",sep=" "))
	}

	#read in the initialisation data from file datainfile
	#WKWHMMP2 new input data format
	#indata <- matrix(scan(datainfile,0,quiet=TRUE),ncol=17,byrow=TRUE)
	#file now contains initialisation for 45 different parameters (not all used), with a new value for each iteration
  
	#15/09/2014
  #file now contains initialisation for aFec,bFec,qFec,sC,segg,spg,scoregg,Rec12,Bsp12,Bsp13,Bsp14,N2-N11,
  #F0-F10,ark,brk,sigRrk,scorrk,nllRrk,abh,bbh,sigRbh,scorbh,nllRbh,ahs,bhs,ghs,sigRhs,scorhs,nllRhs,neglnL,
  #Rec1982,Rec2001,SSB1982,SSB2001,Bloss with a new value for each iteration
  indata <- read.table(file=datainfile,header=TRUE,sep=",")

	#the number of distinct age classes.....
	#WKWHMMP2
	#numage <- length(indata[,1])
	numage <- 12

	#check that the number of ages in the initialistion data file matches the range given
	#by minage - maxage (the initialisation data file age limits)
	if ((maxage-minage+1)!=numage) {
	stop(paste("\nThe age class data supplied in the initialisation data file (",datainfile,")\ndoes not match the age class range (",minage,"-",maxage,") in the options file (",optionsfile,")\n",sep=""))
	}
	
	#dimension a number of arrays to store the various parameters of interest. 
	#Each array will hold all values i.e. over all age groups, 
	#simulations years and iterations for each resolution. Initial values are all zero
	#the arrays are designed to look like FLQuant objects, hence the age, year, unit, season and 
	#area dimension names and some redundancy
	#(with unit and season). The area dimension is being used in this context as the iteration dimension

	#temporary arr_dim vector used to initialise arrays
	#arr_dim <- c(numage,years,1,1,iterations)
	arr_dim <- c(numage,years,1,1,1,iterations)
	#temporary arr_dimnames list
	#arr_dimnames = list(age=minage:maxage,year=startyear:endyear,unit='unique',season='all',area=1:iterations)
	arr_dimnames = list(age=minage:maxage,year=startyear:endyear,unit='unique',season='all',area='all',iter=1:iterations)
	
	catchweightarray <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	catcharray <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	newspawnmassarray <- array(0,dim=arr_dim,dimnames=arr_dimnames)
  ssb.st.true.array <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	ssb.J1.true.array <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	yearendstockarray <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	poparray <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	fishingmortalityarray <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	fishingmortalitymultarray <- array(0,dim=arr_dim,dimnames=arr_dimnames)
	farray <- array(0,dim=arr_dim,dimnames=arr_dimnames)

	#the following arrays do not include age-disaggregated data and so do not expand this dimension
	#arr_dim_noage <- c(1,years,1,1,iterations)
	arr_dim_noage <- c(1,years,1,1,1,iterations)
	#arr_dimnames_noage <- list(age=0,year=startyear:endyear,unit='unique',season='all',area=1:iterations)
	arr_dimnames_noage <- list(age=0,year=startyear:endyear,unit='unique',season='all',area='all',iter=1:iterations)

	recruitsarray <- array(0,dim=arr_dim_noage,dimnames=arr_dimnames_noage)
	recruitsdetarray <- array(0,dim=arr_dim_noage,dimnames=arr_dimnames_noage)
	fbararray <- array(0,dim=arr_dim_noage,dimnames=arr_dimnames_noage)
  ftacarray <- array(0,dim=arr_dim_noage,dimnames=arr_dimnames_noage)
	ftacmultarray <- array(0,dim=arr_dim_noage,dimnames=arr_dimnames_noage)
	hcrsubarray <- array(0,dim=arr_dim_noage,dimnames=arr_dimnames_noage)
	eggsarray <- array(0,dim=arr_dim_noage,dimnames=arr_dimnames_noage)

	SRModels <- data.frame(type=rep(NA,1000),AParam=rep(NA,1000),BParam=rep(NA,1000),
                         GParam=rep(NA,1000),SigR=rep(NA,1000),scor=rep(NA,1000))
  
  
	#initial egg values
	#full time series is 513(1983),1762(1989),1712(1992),1265(1995),1136(1998),821(2001),889(2004),1640(2007),1093(2010),397(2013)
  #2004,2007,2010
  eggs <- c(889,1640,1093)
	
  eta<-0
  
	#read in stock recruit pairs data ...
  #data file without header
  #input <- scan(recruitinfile,list(0,0,0),quiet=TRUE)
  
	#...and assign to local vectors
	#recyear <- input[[1]]   #recruitment data year vector
	#recr <- input[[2]]      #recruitment data recruitment numbers vector
	#rssbin <- input[[3]]    #recruitment data ssb vector

  dfSRPairs <- read.table(recruitinfile,header=TRUE)
  recyear <- dfSRPairs$Year
  recr <- dfSRPairs$Recruits
  rssbin <- dfSRPairs$SSB
  
  
	#initialise some variables to hold the length of the historical recruitment dataset
	#(lenrecyr) and the final year for which data is available (maxrecyr). In the event
	#that this data is not to be used set the maximum year to 0

	lenrecyr <- length(recyear)
	maxrecyr <- 0

	if (histrec==1) {maxrecyr<-max(recyear)}

	#calculate the number of year between the start of the simulation and the final year of recruitment
	#data and store it in local variable diffxs
   
	diffxs <- max(recyear) - startyear + 1
  
	#create three new vectors to hold the recruitment data offset so that the recruitment value 
	#corresponds to the year that produced 
	#this age-class (the spawning year) rather than the year it entered the fishery. 
	#Depending on the value of minage, this is likely to require recruitment data for years 
	#that are after the start date of the projection (e.g. if the simulation starts in 2000 and minage = 2, 
	#then recruitment data will be required for at least 1998, 1999, 2000 and 2001). 
  
	spwnyr <- seq(1,1,length=lenrecyr-max(minage,diffxs))
	spwnssb <- spwnyr
	spwnrec <- spwnyr

	for(i in 1:(lenrecyr-max(minage,diffxs))){
		spwnyr[i] <- recyear[i]
		spwnssb[i] <- rssbin[i]
		spwnrec[i] <- recr[i+minage]
	}

	#These lines are similar to the above - vectors are set up with the historical recruitment data 
	#and the vector spwnfrec is offset so that it corresponds to the spawning year rather than the year 
	#of entry to the fishery. However, the vectors may be of a different length than those above 
	#depending on the value of datarecyear. This parameter gives the maximum year in the recfile that 
	#data is to be used from in the recruitment functions. This allows fictional future recruitment to 
	#be entered into the recruits data file, but only the real historical data to be used in fitting 
	#parametric functions or bootstrapping. If datarecyear > max(recyear) (the oldest year in the recruits 
	#data file), then all data in the file is used in the recruitment functions. The vectors spwnfnyr,spwnfnssb,
	#spwnfnrec are used in the recruitment functions

	#calculate the number of years between the last year of recruitment data and the final year that is to 
	#be used in the simulations

	diffxt <- max(recyear) - datarecyear + 1

	spwnfnyr <- seq(1,1,length=lenrecyr-max(minage,diffxt))
	spwnfnssb <- spwnfnyr
	spwnfnrec <- spwnfnyr

	for(i in 1:(lenrecyr-max(minage,diffxt))){
		spwnfnyr[i] <- recyear[i]
		spwnfnssb[i] <- rssbin[i]
		spwnfnrec[i] <- recr[i+minage]
	}
	
	#if using a ricker model, calculate the parameters
	#ricker.params <- rickerfit(spwnfnrec,spwnfnssb)

	#read in historical catch data
	input <- scan(catchinfile,list(0,0,0),quiet=TRUE,skip=1)
  dfCatches <- read.table(catchinfile,header=TRUE)
   
	#and assign to local vectors for
	#year,
	#catyear <- input[[1]]
  catyear <- dfCatches$Year
	#catch
	#ccatch <- input[[2]]
  ccatch <- dfCatches$Catch
	#fbar
	#cfbar <- input[[3]]
  cfbar <- dfCatches$FBar

	if(histcatch==1){
		#last year of catch data
		maxcatyr <- max(catyear)
		#number of years of catch data
		lencatyr <- length(catyear)
	} else  {
		#set maximum year to zero
		maxcatyr <- 0
	}

	#create initialisation vectors using data from datafile

	#initialise vars with data from datinfile

	#WKWHMMP2 - not required for this implementation
	#age class
	#initage <- indata[,1]
	initage <- seq(0,11)
	#population (000s)
	#initpopint <- indata[,2]
	#population cv
	#initpopcv <- indata[,3]
	initpopcv <- rep(0,numage)
	#spawning weight (kg)
	#initspwnweightint <- indata[,4]
	#spawning weight cv
	#initspwnweightcv <- indata[,5]
	initspwnweightcv <- rep(0,numage)
	#catch weight (kg)
	#initcatweightint <- indata[,6]
	#catch weight cv
	#initcatweightcv <- indata[,7]
	initcatweightcv <- rep(0,numage)
	#maturity
	#initmatureint <- indata[,8]
	initmatureint <- c(0,0,0.05,0.25,0.70,0.95,1.0,1.0,1.0,1.0,1.0,1.0)
	#maturity cv
	#initmaturecv <- indata[,9]
	initmaturecv <- rep(0,numage)
	#fishing mortality
	#initfint <- indata[,10]
	#fishing mortality cv
	#initfcv <- indata[,11]
	initfcv <- rep(0,numage)
	#proportion of fishing mortality before spawning
	#initfprop <- indata[,12]
	initfprop <- rep(0.45,numage)
	#discard mortality
	#initdisfint <- indata[,13]
	initdisfint <- rep(0,numage)
	#discard mortality cv
	#initdisfcv <- indata[,14]
	initdisfcv <- rep(0,numage)
	#natural mortality
	#initmint <- indata[,15]
	initmint <- rep(0.15,numage)
	#natural mortality cv
	#initmcv <- indata[,16]
	initmcv <- rep(0,numage)
	#proportion of natural mortality before spawning
	#initmprop <- indata[,17]
	initmprop <- rep(0.45,numage)


	#spawning weights
	#24/06/2013
	#spn.wgt <- data.matrix(read.table(file=paste(FPRESS.Home(),"\\indata\\WHM_west.dat",sep=""),header=FALSE,sep=","))
	#spn.wgt <- data.matrix(read.table(file=paste(FPRESS.Home(),"\\indata\\WHM_west_yr.dat",sep=""),header=FALSE,sep=","))
  #27/05/2014
  spn.wgt <- data.matrix(read.table(file=paste(FPRESS.Home(),"\\indata\\WHM_StockWeights.dat",sep=""),header=FALSE,sep=","))
  
	rownames(spn.wgt)<-spn.wgt[,1]
  spn.wgt<-spn.wgt[,-1]
	colnames(spn.wgt)<-c("Age0","Age1","Age2","Age3","Age4","Age5","Age6","Age7","Age8","Age9","Age10","Age11+")
  #cat("read spawning weights",dim(spn.wgt),"\n")
	
	#catch weights
  #24/06
	#cat.wgt <- data.matrix(read.table(file=paste(FPRESS.Home(),"\\indata\\WHM_weca.dat",sep=""),header=FALSE,sep=","))
	#cat.wgt <- data.matrix(read.table(file=paste(FPRESS.Home(),"\\indata\\WHM_weca_yr.dat",sep=""),header=FALSE,sep=","))
  #27/05/2014
	cat.wgt <- data.matrix(read.table(file=paste(FPRESS.Home(),"\\indata\\WHM_CatchWeights.dat",sep=""),header=FALSE,sep=","))
	rownames(cat.wgt)<-cat.wgt[,1]
	cat.wgt<-cat.wgt[,-1]
	colnames(cat.wgt)<-c("Age0","Age1","Age2","Age3","Age4","Age5","Age6","Age7","Age8","Age9","Age10","Age11+")
	
  common.years<-seq(max(as.integer(range(rownames(cat.wgt)))[1],
                        as.integer(range(rownames(spn.wgt)))[1]),
                    min(as.integer(range(rownames(cat.wgt)))[2],
                        as.integer(range(rownames(spn.wgt)))[2]))
  
  #weights to be selected from 1998-2013
  common.years <- seq(1998,2013)
  
  #randomly draw a vector of years
  #23/09/2014 bug fix
  #wgt.year <- ceiling(runif(iterations,common.years[1]-1,rev(common.years)[1]))
	wgt.year <- common.years[sample.int(n=length(common.years),size=iterations,replace=TRUE)]
  
  #wgt.year[1]<-"2002"
	#cat(wgt.year[1],"\n")
  
  #recruitment models
  #excluding 1982
	#SRfile <- paste(FPRESS.Home(),"\\indata\\SRbayes_1000models_excluding1982.csv",sep="")
  #excluding 1982 & 2001
	#SRfile <- paste(FPRESS.Home(),"\\indata\\SRbayes_1000models_excluding1982and2001.csv",sep="")
	dfSR <- read.table(SRfile,header=TRUE,sep=",")
  
  #egg counts
  dfEgg <- read.table(Eggfile,header=TRUE,sep=",")
  #cat(unlist(dfEgg),"\n")
  
  #historic stock weights
  dfSW <- read.table(SWfile,header=FALSE,sep=",",col.names=c("Year",seq(0,11,by=1)))
  
  #historic numbers at age
	dfNum <- read.table(Numfile,header=TRUE,sep=",",col.names=paste("y",rep(seq(1982,2013,by=1),each=12),"a",seq(0,11),sep=""))
  
  #historic fishing mortality
	dfF <- read.table(Ffile,header=TRUE,sep=",",col.names=paste("y",rep(seq(1982,2013,by=1),each=12),"a",seq(0,11),sep=""))
  
  #historic natural mortality
	dfNatMor  <- read.table(NatMorfile,header=FALSE,sep=",",col.names=c("Year",seq(0,11,by=1)))
  
  #historic maturity
	dfMat  <- read.table(Matfile,header=FALSE,sep=",",col.names=c("Year",seq(0,11,by=1)))
	
  #spikes output
  #write("Iter\tYear\tSSB\tBloss\tRec\tModel",
  #      file = paste(FPRESS.Home(),"\\outdata\\Spikes_",runref,".dat",sep=""))
	#write("Iter\tYear\tSSB\tBloss\tRec\tModel",
	#      file = paste(FPRESS.Home(),"\\outdata\\NoSpikes_",runref,".dat",sep=""))
	
		
	#################################################
	#START OF MAIN BODY##############################
	#################################################

	#################################################
	#loop over values of F,TAC as appropriate########
	#res is the counter variable#####################
	#################################################
	for(res in 0:resolution) {

	  #spike.model <- spike.iter <- spike.yr <- spike.ssb <- spike.bloss <- spike.rec <- c()
	  #nospike.model <- nospike.iter <- nospike.yr <- nospike.ssb <- nospike.bloss <- nospike.rec <- c()
	  
		if (debug==1) {
      cat("res=",res,"\n")
      log.write.ln(log.file,paste("debug::res::",res,sep=""))      
    }

		#calculate the appropriate FTAC value for this resolution
		if (resolution>0) {

			resolution.ftac<-c((varmax-varmin)*res/resolution)+varmin

			#ensure we don't proceed with a 0 value
			#if(resolution.ftac==0){resolution.ftac <- 0.0001}

			#calculate the percentage of the simulation that is complete
			#for reporting purposes
			percent <- round((res/(resolution+1))*100,digits=2)

			if (variable==1) {
				str <- paste("fval = ",resolution.ftac," ",percent,"% complete","\n",sep="")
			} else {
				str <- paste("tacval = ",resolution.ftac," ",percent,"% complete","\n",sep="")
			}

			cat(str,"\n")
			log.write.ln(log.file,str,TRUE,timestamp=TRUE)

		} else {
			resolution.ftac <- varmin
		}

		#update the console to ouput above messages
		flush.console()

    init.eta<-c()

    #SR, Egg models
    if (startyear==2013){
      
      lSR <- lapply(seq(1,iterations),FUN = f.SADSR,nits = iterations,SADparams = indata,
                    SRpairs = read.table("C:\\FPRESS\\HOM-WEST\\MSE2014\\Baseline\\indata\\SADMSE2014_SRPairs_29_07_2014.dat",header=TRUE,sep=","),
                    SR.types = f.RandSR(props=c(0.49,0.28,0.23),nits=iterations),startyear = startyear,years = years)
      
    } else if (startyear==2014) {
      
      lSR <- lapply(seq(1,iterations),FUN = f.SADSR,nits = iterations,SADparams = indata,
                    SRpairs = read.table("C:\\FPRESS\\HOM-WEST\\MSE2014\\Baseline\\indata\\SADMSE2014_SRPairs_15_09_2014.dat",header=TRUE,sep=","),
                    SR.types = f.RandSR(props=c(0.46,0.32,0.22),nits=iterations),startyear = startyear,years = years)      

    } else {
      
      stop("invalid startyear, cannot generate lSR\n")
    
    }

    cat("lSR generated\n")
    
		lEgg <- lapply(seq(1,iterations),FUN = f.SADEgg,nits = iterations,SADparams = indata,
		               EggHist = dfEgg,StockWeights = dfSW,NatMor = dfNatMor, Mat = dfMat,
		               NumatAge = dfNum, FatAge = dfF, SexRatio = 0.5, PM = 0.45, PF = 0.45, 
		               startyear = startyear,years = years)
		
    cat("lEgg generated\n")
    
		########################################################  
		#main iterative loop####################################
		#iter is the counter variable###########################
		########################################################
		for (iter in 1:iterations) {

      #cat("Iter:",iter,"\n")
		  if (debug==1) {log.write.ln(log.file,paste("######iteration number ",iter," ######",sep=""),TRUE)}

			#initialise egg count data
      #2001,2004,2007
      #eggs <- c(821,889,1640)      
			#2007,2010,2013
			#eggs <- c(1640,1093,397)
      #2004,2007,2010
			eggs <- c(889,1640,1093)
      
      eta <- 0
      
			#assign initial values for this iteration based on contents of file SAD parameters
			#which have been scanned into dataframe indata

      #Stock and recruit parameters
      if (rec == 13 | rec == 16) {

        #Ricker parameters
        SR_A <- indata[iter,'ark']
        SR_B <- indata[iter,'brk']
        SR_Sigma <- indata[iter,'sigRrk']
        SR_Auto <- indata[iter,'scorrk']
        
        if (rec==16){
          #initial residual for autocorrelation (2009)
          eta <- log(indata[iter,'Rec09'])-log(SR_A*indata[iter,'Bsp09']*exp(-1*SR_B*indata[iter,'Bsp09']))
        }
      
        #cat("2009 eta=",eta,"\n")
        #2010 recruits
        tmp<-recruit16(SR_A,SR_B,if (det) {0} else {SR_Sigma},SR_Auto,eta,ssb=indata[j,'Bsp10'],debug)
        Rec10<-tmp[1]
        eta<-tmp[2]
        
        #cat("2010 Recruits,eta = ",Rec10,eta,"\n")
        #cat("eta=",eta,"\n")
      }
      
      if (rec == 14 | rec == 17) {
        #Beverton&Holt
        SR_A <- indata[iter,'abh']
        SR_B <- indata[iter,'bbh']
        SR_Sigma <- indata[iter,'sigRbh']
        SR_Auto <- indata[iter,'scorbh']
      }
      
      if (rec == 15 | rec == 18) {
        #Smoothed hockey stick
        SR_A <- indata[iter,'ahs']
        SR_B <- indata[iter,'bhs']
        SR_G <- indata[iter,'ghs']
        SR_Sigma <- indata[iter,'sigRhs']
        SR_Auto <- indata[iter,'scorhs']
      }
      
      if (rec == 19) {
        #Bootstrap
        Rec10 <- recruit19(ssb=indata[j,'Bsp10'],debug)[1]
      }

      if (rec == 20) {
        #Ricker Normal
        Rec10 <- recruit20(ssb=indata[j,'Bsp10'],debug=debug)[1]
      }

      if (rec == 21) {
        #Ricker LogNormal
        Rec10 <- recruit21(ssb=indata[j,'Bsp10'],debug=debug)[1]
      }
      
      if(rec == 22) {
        #Bayesian
        Rec10 <- recruit22(ssb = indata[iter,'Bsp10'],
                           model = as.character(dfSR$mod[iter]),
                           A.param = dfSR$A[iter],
                           B.param = dfSR$B[iter],
                           sigma = dfSR$sigma[iter],
                           debug=debug)[1]
      }

      if(rec == 23) {
        #Bayesian
        #Rec10 <- recruit23(log.file,ssb = indata[iter,'Bsp10'],
        #                   model = as.character(dfSR$mod[iter]),
        #                   A.param = dfSR$A[iter],
        #                   B.param = dfSR$B[iter],
        #                   sigma = dfSR$sigma[iter],
        #                   debug=debug)[1]
        #Bayesian
        Rec13 <- recruit23(log.file,ssb = indata[iter,'Bsp13'],
                           model = as.character(dfSR$mod[iter]),
                           A.param = dfSR$A[iter],
                           B.param = dfSR$B[iter],
                           sigma = dfSR$sigma[iter],
                           debug=debug)[1]
      }

      if (rec==24){
        
        #initial residual for autocorrelation (2009)
        #eta <- log(indata[iter,'Rec09'])-log(SR_A*indata[iter,'Bsp09']*exp(-1*SR_B*indata[iter,'Bsp09']))
        #modelled recruitment is a deterministic draw from the current Bayes iter
        eta <- log(indata[iter,'Rec09'])-log(recruit23(log.file,ssb=indata[iter,'Bsp09'],
                                                       model = as.character(dfSR$mod[iter]),
                                                       A.param = dfSR$A[iter],
                                                       B.param = dfSR$B[iter],
                                                       sigma = 0)[1])

        #temp
        #eta<-0
        init.eta<-c(init.eta,eta)
        
        if(debug==1){
          cat("Initial eta value=",eta,"\n")
          log.write.ln(log.file,paste("debug::Initial eta value::",eta,sep=""))      
        }
        
        #Bayesian
        tmp <- recruit24(ssb = indata[iter,'Bsp10'],
                         model = as.character(dfSR$mod[iter]),
                         A.param = dfSR$A[iter],
                         B.param = dfSR$B[iter],
                         sigma = dfSR$sigma[iter],
                         auto=dfSR$ac_in2001[iter],
                         eta=eta,
                         debug=debug)
        
        Rec10 <- tmp[1]
        eta <- tmp[2]
                
      }
      
      if (rec==25){
        
        #get the SR model details
        SR <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)

        #save the model details
        SRModels$type[iter]<-SR$model
        SRModels$AParam[iter]<-SR$AParam
        SRModels$BParam[iter]<-SR$BParam
        SRModels$GParam[iter]<-SR$GParam
        SRModels$SigR[iter]<-SR$SigR
        SRModels$scor[iter]<-SR$scor
        
        #calculate a recruitment for 2013
        #Rec13 <- recruit25(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
        #Rec12 <- recruit25(log.file,ssb=indata[iter,'Bsp12'],SR,eta=0,debug=debug)[1]

        #calculate a recruitment for 2014
        Rec14 <- recruit25(log.file,ssb=indata[iter,'Bsp14'],SR,eta=0,debug=debug,trunc=truncateresid)[1]
        Rec13 <- recruit25(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug,trunc=truncateresid)[1]
        
      }

			if (rec==26){
			  #SR models from SAD parameters file
			  #Use the Bev Holt parameters for the first iteration

        #draw recruits for 2011 based on 2011 ssb from input file, using appropriate SR model
        #use function recruit25 which implements the SAD SR models without autocorrelation
        #sr model
        sr <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
        t <- recruit25(log.file, ssb = indata[iter,'Bsp11'], SRModel = sr, eta = 0, debug = debug,trunc=truncateresid)[1]
        #cat("t=",t,"\n")
			  #calculate the initial residual for autocorrelation (2011)
			  eta.2011 <- log(indata[iter,'Rec11'])-log(t)
        
        #cat("eta.2011=",eta.2011,"\n")
        
        #now calculate eta for 2012
        eta.2012 <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$scor*eta.2011+sqrt(1-f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$scor^2)*rnorm(n=1,mean=0,sd=f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$SigR)
        #Q - should the scor on the end be squared?
        
        #Use the Bev Holt parameters for the first iteration
			  Rec13 <- recruit26(log.file,ssb = indata[iter,'Bsp13'],
			                     f.SADSR(iter,iterations,indata,dfHistSR,SR.types),
			                     eta=eta.2012,
			                     debug=debug)[1]
			  Rec12 <- recruit26(log.file,ssb = indata[iter,'Bsp12'],
			                     f.SADSR(iter,iterations,indata,dfHistSR,SR.types),
			                     eta=0,
			                     debug=debug)[1]
			}

			if (rec==27){
			  
			  #get the SR model details
			  SR <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
			  
			  #save the model details
			  SRModels$type[iter]<-SR$model
			  SRModels$AParam[iter]<-SR$AParam
			  SRModels$BParam[iter]<-SR$BParam
			  SRModels$GParam[iter]<-SR$GParam
			  SRModels$SigR[iter]<-SR$SigR
			  SRModels$scor[iter]<-SR$scor
			  
			  #calculate a recruitment for 2013
			  #Rec13 <- recruit27(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  #Rec12 <- recruit27(log.file,ssb=indata[iter,'Bsp12'],SR,eta=0,debug=debug)[1]

			  #calculate a recruitment for 2014
			  Rec14 <- recruit27(log.file,ssb=indata[iter,'Bsp14'],SR,eta=0,debug=debug)[1]
			  Rec13 <- recruit27(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  
			}

			if (rec==28){
			  
			  #get the SR model details
			  SR <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
			  
			  #save the model details
			  SRModels$type[iter]<-SR$model
			  SRModels$AParam[iter]<-SR$AParam
			  SRModels$BParam[iter]<-SR$BParam
			  SRModels$GParam[iter]<-SR$GParam
			  SRModels$SigR[iter]<-SR$SigR
			  SRModels$scor[iter]<-SR$scor
			  
			  #calculate a recruitment for 2013
			  #Rec13 <- recruit28(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  #Rec12 <- recruit28(log.file,ssb=indata[iter,'Bsp12'],SR,eta=0,debug=debug)[1]

			  #calculate a recruitment for 2014
			  Rec14 <- recruit28(log.file,ssb=indata[iter,'Bsp14'],SR,eta=0,debug=debug)[1]
			  Rec13 <- recruit28(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  
			}
			
			if (rec==29){
			  
			  #get the SR model details
			  SR <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
			  
			  #save the model details
			  SRModels$type[iter]<-SR$model
			  SRModels$AParam[iter]<-SR$AParam
			  SRModels$BParam[iter]<-SR$BParam
			  SRModels$GParam[iter]<-SR$GParam
			  SRModels$SigR[iter]<-SR$SigR
			  SRModels$scor[iter]<-SR$scor
			  
			  #calculate a recruitment for 2013
			  #Rec13 <- recruit29(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  #Rec12 <- recruit29(log.file,ssb=indata[iter,'Bsp12'],SR,eta=0,debug=debug)[1]

			  #calculate a recruitment for 2014
			  Rec14 <- recruit29(log.file,ssb=indata[iter,'Bsp14'],SR,eta=0,debug=debug)[1]
			  Rec13 <- recruit29(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  
			}
			
      
			if (rec==30){
			  
			  #get the SR model details
			  SR <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
			  
			  #save the model details
			  SRModels$type[iter]<-SR$model
			  SRModels$AParam[iter]<-SR$AParam
			  SRModels$BParam[iter]<-SR$BParam
			  SRModels$GParam[iter]<-SR$GParam
			  SRModels$SigR[iter]<-SR$SigR
			  SRModels$scor[iter]<-SR$scor
			  
			  #calculate a recruitment for 2013
			  #Rec13 <- recruit30(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  #Rec12 <- recruit30(log.file,ssb=indata[iter,'Bsp12'],SR,eta=0,debug=debug)[1]

			  #calculate a recruitment for 2014
			  Rec14 <- recruit30(log.file,ssb=indata[iter,'Bsp14'],SR,eta=0,debug=debug)[1]
			  Rec13 <- recruit30(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug)[1]
			  
			}

			if (rec==31){
			  #SR models from SAD parameters file
			  #Use the Bev Holt parameters for the first iteration
			  
			  ##draw recruits for 2011 based on 2011 ssb from input file, using appropriate SR model
			  #draw recruits for 2012 based on 2012 ssb from input file, using appropriate SR model
			  #use function recruit25 which implements the SAD SR models without autocorrelation
			  #sr model
			  sr <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
        #cat(indata[iter,'Bsp11'],"\n")
        
			  #t <- recruit25(log.file, ssb = indata[iter,'Bsp11'], SRModel = sr, eta = 0, debug = debug)[1]
			  t <- recruit25(log.file, ssb = indata[iter,'Bsp12'], SRModel = sr, eta = 0, debug = debug,trunc=truncateresid)[1]

        ##calculate the initial residual for autocorrelation (2011)
			  #calculate the initial residual for autocorrelation (2012)
			  #eta.2011 <- log(indata[iter,'Rec11'])-log(t)
			  eta.2012 <- log(indata[iter,'Rec12'])-log(t)
			  
			  ##now calculate eta for 2012
			  #now calculate eta for 2013
			  #eta.2012 <- (0.5*f.SADSR(iter,iterations,indata,SR.types)$scor)*eta.2011+
        #  sqrt(1-(0.5*f.SADSR(iter,iterations,indata,SR.types)$scor)^2)*rnorm(n=1,mean=0)*f.SADSR(iter,iterations,indata,SR.types)$SigR
			  eta.2013 <- (0.5*f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$scor)*eta.2012+
			    sqrt(1-(0.5*f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$scor)^2)*rnorm(n=1,mean=0)*f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$SigR
			  
			  #Use the Bev Holt parameters for the first iteration
			  #Rec13 <- recruit31(log.file,ssb = indata[iter,'Bsp13'],
			  #                    f.SADSR(iter,iterations,indata,SR.types),
			  #                  eta=eta.2012,
			  #                 debug=debug)[1]
			  Rec14 <- recruit31(log.file,ssb = indata[iter,'Bsp14'],
			                     f.SADSR(iter,iterations,indata,dfHistSR,SR.types),
			                     eta=eta.2013,
			                     debug=debug)[1]
			  
# 			  Rec12 <- recruit31(log.file,ssb = indata[iter,'Bsp12'],
# 			                     f.SADSR(iter,iterations,indata,SR.types),
# 			                     eta=0,
# 			                     debug=debug)[1]
			  Rec13 <- recruit31(log.file,ssb = indata[iter,'Bsp13'],
			                     f.SADSR(iter,iterations,indata,dfHistSR,SR.types),
			                     eta=0,
			                     debug=debug)[1]
			          
			}

			if (rec==32){

        #SR models from SAD parameters file
			  #Use the Bev Holt parameters for the first iteration
			  
			  ##draw recruits for 2011 based on 2011 ssb from input file, using appropriate SR model
			  #draw recruits for 2012 based on 2012 ssb from input file, using appropriate SR model
			  #use function recruit25 which implements the SAD SR models without autocorrelation
			  #sr model
			  #sr <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
			  
			  #t <- recruit25(log.file, ssb = indata[iter,'Bsp11'], SRModel = sr, eta = 0, debug = debug)[1]
			  #t <- recruit25(log.file, ssb = indata[iter,'Bsp12'], SRModel = sr, eta = 0, debug = debug,trunc=truncateresid)[1]
			  
			  ##calculate the initial residual for autocorrelation (2011)
			  #calculate the initial residual for autocorrelation (2012)
			  #eta.2012 <- log(indata[iter,'Rec12'])-log(t)

        if (startyear==2013){
          #eta <- lSR[[iter]]$HistResids['11']  
          eta <- lSR[[iter]]$Resids['2011']  
        } else if (startyear==2014) {
          #eta <- lSR[[iter]]$HistResids['12']
          eta <- lSR[[iter]]$Resids['2012']
        }
        
        #cat("eta=",eta,"\n")
        
			  ##now calculate eta for 2012
			  #now calculate eta for 2013
			  #eta.2012 <- (1.0*f.SADSR(iter,iterations,indata,SR.types)$scor)*eta.2011+
			  # sqrt(1-(1.0*f.SADSR(iter,iterations,indata,SR.types)$scor)^2)*rnorm(n=1,mean=0)*f.SADSR(iter,iterations,indata,SR.types)$SigR
			  #eta.2013 <- (1.0*f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$scor)*eta.2012+
			  # sqrt(1-(1.0*f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$scor)^2)*rnorm(n=1,mean=0)*f.SADSR(iter,iterations,indata,dfHistSR,SR.types)$SigR
        eta <- lSR[[iter]]$scor*eta + sqrt(1-lSR[[iter]]$scor^2)*rnorm(n=1,mean=0)*lSR[[iter]]$SigR
			  #cat("eta=",eta,"\n")
			  
        
			  #Use the Bev Holt parameters for the first iteration
# 			  Rec13 <- recruit32(log.file,ssb = indata[iter,'Bsp13'],
# 			                     f.SADSR(iter,iterations,indata,SR.types),
# 			                     eta=eta.2012,
# 			                     debug=debug)[1]
# 			  Rec14 <- recruit32(log.file,ssb = indata[iter,'Bsp14'],
# 			                     f.SADSR(iter,iterations,indata,dfHistSR,SR.types),
# 			                     eta=eta.2013,
# 			                     debug=debug,trunc=truncateresid)[1]

# 			  Rec12 <- recruit32(log.file,ssb = indata[iter,'Bsp12'],
# 			                     f.SADSR(iter,iterations,indata,SR.types),
# 			                     eta=0,
# 			                     debug=debug)[1]
		
#         Rec13 <- recruit32(log.file,ssb = indata[iter,'Bsp13'],
#                            f.SADSR(iter,iterations,indata,dfHistSR,SR.types),
#                            eta=0,
#                            debug=debug,trunc=truncateresid)[1]


        if (startyear==2013) {
          t <- recruit32(log.file,ssb = indata[iter,'Bsp12'],lSR[[iter]],eta=eta,debug=debug,trunc=truncateresid)
        } else if (startyear==2014){
          t <- recruit32(log.file,ssb = indata[iter,'Bsp13'],lSR[[iter]],eta=eta,debug=debug,trunc=truncateresid)
        } else {
          t <- NA
        }

        Rec.lastyear <- t[1]
        eta <- t[2]

        if (startyear==2013) {
          Rec.startyear <- recruit32(log.file,ssb = indata[iter,'Bsp13'],lSR[[iter]],eta=eta,debug=debug,trunc=truncateresid)[1]
        } else if (startyear==2014){
          Rec.startyear <- recruit32(log.file,ssb = indata[iter,'Bsp14'],lSR[[iter]],eta=eta,debug=debug,trunc=truncateresid)[1]
        } else {
          Rec.startyear <- NA
        }

			}


      if (rec==33){
        #SRSAD5, historic residuals, no autocorrelation
        
        #get the SR model details
        SR <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
  
        #save the model details
        SRModels$type[iter]<-SR$model
        SRModels$AParam[iter]<-SR$AParam
        SRModels$BParam[iter]<-SR$BParam
        SRModels$GParam[iter]<-SR$GParam
        SRModels$SigR[iter]<-SR$SigR
        SRModels$scor[iter]<-SR$scor
        
        #calculate a recruitment for 2014
        Rec14 <- recruit25(log.file,ssb=indata[iter,'Bsp14'],SR,eta=0,debug=debug,trunc=truncateresid)[1]
        Rec13 <- recruit25(log.file,ssb=indata[iter,'Bsp13'],SR,eta=0,debug=debug,trunc=truncateresid)[1]
  
      }

      if (rec==34){
        
        #SRSAD5, historic residuals, including autocorrelation

        #get Stock Recruit model details
        #SR <- f.SADSR(iter,iterations,indata,dfHistSR,SR.types)
        
        #initial residual for autocorrelation (2012)
        #eta.2012 <- log(indata[iter,'Rec12'])-log(recruit25(log.file, ssb = indata[iter,'Bsp12'], SRModel = SR, eta = 0, debug = debug,trunc=truncateresid)[1])
        #eta <- lSR[[iter]]$HistResid['12']
        
        #now calculate eta for 2013
        #eta.2013 <- SR$scor*eta.2012 + sqrt(1-SR$scor^2)*sample(SR$HistResids,size=1,replace=TRUE)
        #eta <- lSR[[iter]]$scor + sqrt(1-lSR[[iter]]$scor^2)*sample(lSR[[iter]]$HistResids,size=1)
        
        #to complete the initial population vector with age 0 and age 1 we now 
        #calculate recruits for 2013 and 2014 based on the current SR model and the SSB from the assessment
        #t <- f.recruit34(log.file,ssb=indata[iter,'Bsp13'],SRModel=SR,eta=eta.2013,debug=debug)
        #t <- f.recruit34(log.file,ssb=indata[iter,'Bsp13'],SRModel=lSR[[iter]],eta=eta,debug=debug)
        #Rec13 <- t[1]
        #eta <- t[2]
        #Rec13 <- f.recruit34(log.file,ssb=indata[iter,'Bsp13'],SRModel=lSR[[iter]],year=2013,debug=debug)[1]
        #cat("Rec13=",Rec13,"\n")
        
        #Rec14 <- f.recruit34(log.file,ssb=indata[iter,'Bsp14'],SRModel=SR,eta=eta,debug=debug)[1]
        #don't need to store the eta value (residual)
        #Rec14 <- f.recruit34(log.file,ssb=indata[iter,'Bsp14'],SRModel=lSR[[iter]],eta=eta,debug=debug)[1]
        #Rec14 <- f.recruit34(log.file,ssb=indata[iter,'Bsp14'],SRModel=lSR[[iter]],year=2014,debug=debug)[1]
        #cat("Rec14=",Rec14,"\n")
        
        
        if (startyear==2013) {
          Rec.lastyear <- f.recruit34(log.file,ssb=indata[iter,'Bsp12'],SRModel=lSR[[iter]],year=2012,debug=debug)[1]
          Rec.startyear <- f.recruit34(log.file,ssb=indata[iter,'Bsp13'],SRModel=lSR[[iter]],year=2013,debug=debug)[1]
        } else if (startyear==2014) {
          Rec.lastyear <- f.recruit34(log.file,ssb=indata[iter,'Bsp13'],SRModel=lSR[[iter]],year=2013,debug=debug)[1]
          Rec.startyear <- f.recruit34(log.file,ssb=indata[iter,'Bsp14'],SRModel=lSR[[iter]],year=2014,debug=debug)[1]
        } else {
          stop("Invalid startyear\n")
        }
        
      }

      #numbers at age 0 in startyear
      tmpN0 <- Rec.startyear; #this will actually be overwritten before it has any effect
      #calculate numbers at age 1 in startyear using the previous year recruitment and the natural and fishing mortality at age 0
      tmpN1 <- Rec.lastyear*exp(-1*(initmint[1] + indata[iter,'F0']))
			
			initpopint <- c(tmpN0,tmpN1,
                      indata[iter,'N2'],indata[iter,'N3'],indata[iter,'N4'],
                      indata[iter,'N5'],indata[iter,'N6'],indata[iter,'N7'],
                      indata[iter,'N8'],indata[iter,'N9'],indata[iter,'N10'],
                      indata[iter,'N11'])

      #cat("initpopint=",initpopint,"\n")
      
      #randomly select weights from spwn.wgt,cat.wgt
      if (det) {
        
        #deterministic run - use average of spawning/catch weights
        initspwnweightint <- apply(spn.wgt,MARGIN=2,FUN=mean)
        initcatweightint <- apply(cat.wgt,MARGIN=2,FUN=mean)
      
      } else {

        #24/06/2013
        #select a single year from which to extract both vectors
        initspwnweightint <- spn.wgt[as.character(wgt.year[iter]),]
        initcatweightint <- cat.wgt[as.character(wgt.year[iter]),]
        
        #stochastic run - randomly select from data
        #initspwnweightint <- spn.wgt[floor(runif(1,1,nrow(spn.wgt))),]
        #initcatweightint <- cat.wgt[floor(runif(1,1,nrow(cat.wgt))),]
      }


      #22/09/2014
      #Catch weights are the average of those in the separable period (2008-2013)
      #initcatweightint <- c(0.040,0.049,0.080,0.109,0.135,0.162,0.180,0.205,0.224,0.250,0.268,0.326)
      #initspwnweightint <- c(0.000,0.020,0.081,0.096,0.123,0.151,0.165,0.184,0.202,0.217,0.237,0.267)
            
			initfint <- c(indata[iter,'F0'],indata[iter,'F1'],indata[iter,'F2'],
			              indata[iter,'F3'],indata[iter,'F4'],indata[iter,'F5'],
			              indata[iter,'F6'],indata[iter,'F7'],indata[iter,'F8'],
			              indata[iter,'F9'],indata[iter,'F10'],indata[iter,'F10'])
      
			#egg parameters
			aFec <- indata[iter,'aFec']
			bFec <- indata[iter,'bFec']
			qFec <- indata[iter,'qFec']
			segg <- indata[iter,'segg']

			sex.ratio <- 0.5
		
			#end WKWHMMP2

			#if the resolution is zero then the simulation is running for a single 
			#TAC or F value. In this case report progress on basis of the number of iterations
			#completed. Issue report every 10%
			if (resolution == 0) {
				percent <- 100*iter/iterations
				if ((percent%%10) == 0) {
					cat(percent,"% complete","\n",sep="")
					log.write.ln(log.file,paste(percent,"% complete",sep=""),TRUE,timestamp=TRUE)
				}
			}

			flush.console()
	
			#initialise ftac value
			ftac <- resolution.ftac
	
      if(debug==1){
        #cat("ftac=",ftac,"\n")
        log.write.ln(log.file,paste("debug::ftac::",ftac,sep=""),TRUE)
      }
      
			#initial ssb
      ssb.J1.true <- initpopint*initspwnweightint*initmatureint
			ssb <- sum(initpopint*initspwnweightint*initmatureint)
      ssb.spwnweight <- sum(initpopint*initspwnweightint*initspwnweightint)
      
			#initial fbar
			fbar <- mean(initfint[(fbarmin-minage+1):(fbarmax-minage+1)])
      
      #generate recruitment spikes, if they are required
      if (onceoff) {
        rec.spikes <- generate.spikes(mean_interval = 19,
                                      spike_var = 0.5,
                                      projyr = years,
                                      offset = startyear - 2001,
                                      spike.years = c(1982,2001),
                                      spike.probs = c(0.5,0.5))
      }
      
			############################################################
			#year loop##################################################
			#year is the counter variable###############################
			############################################################

			for (year in 1:years) {

				#update the current simulation year
				simyear <- year + startyear - 1

        #cat(iter,simyear,"\n")
        
				if (debug==1){log.write.ln(log.file,paste("######year number ",year," (",simyear,")"," ######",sep=""),TRUE)}
				          
				#initialise vectors for start of year
				#age class
				age <- initage
        #population
				if (year>1) {popint <- newyearplus1} else {popint <- initpopint}
				popcv <- initpopcv
				#spawning weight/cv
				spwnweightint <- initspwnweightint
				spwnweightcv <- initspwnweightcv
				#catch weight/cv
				catweightint <- initcatweightint
				catweightcv <- initcatweightcv
				#maturity/cv
				matureint <- initmatureint
				maturecv <- initmaturecv
				#fishing mortality/cv
				fint <- initfint
				fcv <- initfcv
				#proportion of fishing mortality prior to spawning
				fprop <- initfprop
				#discard mortality/cv
				disfint <- initdisfint
				disfcv <- initdisfcv
				#natural mortality/cv
				mint <- initmint
				mcv <- initmcv
				#proportion of natural mortality before spawning
				mprop <- initmprop
				      
        #apply the noise        
				#Randomising parameters - adding noise / stochasticity to account for 
				#environmental/operational variability
				#Initial population (randomised in first year of projection only)
				pop <- popint
				popintsd <- ((if (det) {0} else {popcv})*popint)
         
        #cat("popint",popint,"\n")
        
				if (year==1) { 
				  pop <- -popint
				  #keep drawing until a positive result is obtained
				  while (min(pop)<0) {
				    for (i in 1:numage) {
				      pop[i] <- rnorm(1,popint[i],popintsd[i])
				    }
				  } 
				}
				
				#if minage>0 then take the fishrecruited value calculated above for the first element
				#of the population vector
				if (minage>0) {pop[1] <- fishrecruited}
				
				#Spawning weight
				spwnweight <- -spwnweightint
				spwnweightsd <- (if (det) {0} else {spwnweightcv})*spwnweightint 
				
				while (min(spwnweight)<0) {
				  for (i in 1:numage) {
				    spwnweight[i] <- rnorm(1,spwnweightint[i],spwnweightsd[i])
				  }
				} 

				#Catch weight
				catweight <- -catweightint
				catweightsd <- (if (det) {0} else {catweightcv})*catweightint 
				
				while (min(catweight)<0) {
				  for (i in 1:numage) {
				    catweight[i] <- rnorm(1,catweightint[i],catweightsd[i])
				  } 
				} 
				
				#Maturity
				mature <- -matureint
				maturesd <- (if(det) {0} else {maturecv})*matureint
				
				while (min(mature)<0) {
				  for (i in 1:numage) {
				    mature[i] <- min(rnorm(1,matureint[i],maturesd[i]),1)
				  }
				} 
				
				#Fishing mortality
				f <- -fint
				fsd <- (if (det) {0} else {fcv})*fint
				
				while (min(f)<0) { 
				  for (i in 1:numage) {
				    f[i] <- rnorm(1,fint[i],fsd[i])
				  }
				} 
				
				#Discard mortality
				disf <- -disfint
				disfsd <- (if (det) {0} else {disfcv})*disfint
				
				while (min(disf)<0) { 
				  for (i in 1:numage) {
				    disf[i] <- fmult*rnorm(1,disfint[i],disfsd[i])
				  }
				} 
				
				#Natural mortality
				m <- -mint
				msd <- (if (det) {0} else {mcv})*mint
				
				while (min(m)<0) {
				  for (i in 1:numage) {
				    m[i]<-rnorm(1,mint[i],msd[i])
				  }
				} 
				
        SSB.J1.true <- pop*mature*spwnweight
        
        #24/05/2013
        #calculate recruits
        
				#RECRUITMENT
				#calculate a value for the number of new recruits using the specific recruitment model. 
				
				#USER DEFINED RECRUITMENT MODELS - add new recruitment model to switch construct.
        rec_ret <- switch(rec+1,
                          recruit0(recmean),
                          recruit1(recmean,(if (det) {0} else {reccv})),
                          recruit2(spwnfnyr,spwnfnrec),
                          recruit3(ssb,spwnfnyr,spwnfnrec,spwnfnssb,ssbcut),
                          recruit4(ssb,ricker.params),
                          recruit5(ssb,ricker.params),
                          recruit6(ssb,ssbcut,recmean,seggrad),
                          recruit7(ssb,ssbcut,recmean,seggrad,(if (det) {0} else {reccv})),
                          if (year>1) {
                            #if in the first year there will be no modelled recruitment data
                            #recruit8(spwnfnyr,spwnfnrec,recruitsarray[1,1:year-1,1,1,iter])
                            recruit8(spwnfnyr,spwnfnrec,recruitsarray[1,1:year-1,1,1,1,iter])
                          } else {
                            recruit8(spwnfnyr,spwnfnrec)
                          },
                          recruit9(ssb,ssbcut,recmean,seggrad,(if (det) {0} else {reccv})),
                          recruit10(ssb,ssbcut,recmean,seggrad,(if (det) {0} else {reccv}),recmax),
                          recruit11(),
                          recruit12(ssb,ssbcut,recmean,seggrad,0.75,eta,0.5,recmax),
                          recruit13(SR_A,SR_B,(if (det) {0} else {SR_Sigma}),ssb=ssb.J1.true,debug),
                          recruit14(SR_A,SR_B,(if (det) {0} else {SR_Sigma}),ssb=ssb.J1.true,debug),
                          recruit15(SR_A,SR_B,SR_G,(if (det) {0} else {SR_Sigma}),ssb=ssb.J1.true,debug),
                          recruit16(SR_A,SR_B,(if (det) {0} else {SR_Sigma}),SR_Auto,eta,ssb=ssb.J1.true,debug),
                          recruit17(SR_A,SR_B,(if (det) {0} else {SR_Sigma}),SR_Auto,eta,ssb=ssb.J1.true,debug),
                          recruit18(SR_A,SR_B,SR_G,(if (det) {0} else {SR_Sigma}),SR_Auto,eta,ssb=ssb.J1.true,debug),
                          recruit19(ssb=ssb.J1.true,debug),
                          recruit20(ssb=ssb.J1.true,debug=debug),
                          recruit21(ssb=ssb.J1.true,debug=debug),
                          recruit22(ssb=ssb.J1.true,model=as.character(dfSR$mod[iter]),A.param=dfSR$A[iter],B.param=dfSR$B[iter],sigma=dfSR$sigma[iter],debug=debug),
                          recruit23(log.file,ssb=ssb.J1.true,model=as.character(dfSR$mod[iter]),A.param=dfSR$A[iter],B.param=dfSR$B[iter],sigma=dfSR$sigma[iter],debug=debug),
                          recruit24(ssb=ssb.J1.true,model=as.character(dfSR$mod[iter]),A.param=dfSR$A[iter],B.param=dfSR$B[iter],sigma=dfSR$sigma[iter],auto=dfSR$ac_in2001[iter],eta=eta,debug=debug),
                          recruit25(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types,trunc=truncateresid),eta=eta,debug=debug),
                          recruit26(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types),eta=eta,debug=debug),
                          recruit27(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types),eta=eta,debug=debug),
                          recruit28(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types),eta=eta,debug=debug),                         
                          recruit29(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types),eta=eta,debug=debug),                          
                          recruit30(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types),eta=eta,debug=debug),
                          recruit31(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types),eta=eta,debug=debug),
                          recruit32(log.file,ssb=ssb.J1.true,lSR[[iter]],eta=eta,debug=debug,trunc=truncateresid),
                          recruit33(log.file,ssb=ssb.J1.true,f.SADSR(iter,iterations,indata,dfHistSR,SR.types),eta=eta,debug=debug),
                          f.recruit34(log.file=log.file, ssb=ssb.J1.true, SRModel=lSR[[iter]], year=simyear, debug=debug)
        )
								
				#override recruitment if a once-off pulse recruitment is randomly selected
				#if (onceoff) {if (ceiling(runif(1,min=0,max=onceoffevery)) == 1) {rec_ret <- onceoffrec}}

				#updated eta value in second element
				eta<-rec_ret[2]
				
        #override recruitment with spike 
        if (onceoff) {
          if (rec.spikes[year]>0) {
              
            #calculate residual to add to modelled recruitment
            if (rec.spikes[year]==1982 | rec.spikes[year]==2001){
                
              #calculate residual for iteration specific SR model
              rec.res <- log(lSR[[iter]][[paste("Rec",rec.spikes[year],sep="")]]) - 
                         log(f.recruit25(log.file=log.file, ssb = lSR[[iter]][[paste("SSB",rec.spikes[year],sep="")]],
                                         SRModel=lSR[[iter]], debug=debug, trunc=FALSE)[3])
              
              #overwrite value returned by recruit** function
              rec_ret[1] <- rec_ret[3]*exp(rec.res)
                
#             #alternative (SRSAD3 versions)
#             #if ssb > bloss just use the appropriate 1982 value              
#             if (sum(ssb.J1.true)>lSR[[iter]]$Bloss) {
#               rec_ret[1] <- lSR[[iter]][[paste("Rec",rec.spikes[year],sep="")]]
#             } else {
#               rec_ret[1] <- sum(ssb.J1.true)*lSR[[iter]][[paste("Rec",rec.spikes[year],sep="")]]/lSR[[iter]]$Bloss
#             }

#             #save details
#             spike.iter <- c(spike.iter,iter)
#             spike.yr <- c(spike.yr,rec.spikes[year])
#             spike.ssb <- c(spike.ssb,sum(ssb.J1.true))
#             spike.bloss <- c(spike.bloss,lSR[[iter]]$Bloss)
#             spike.rec <- c(spike.rec,rec_ret[1])
#             spike.model <- c(spike.model,lSR[[iter]]$model)
#             write(c(rec.spikes[year],sum(ssb.J1.true),lSR[[iter]]$Bloss,rec_ret[1]),
#                   file = paste(FPRESS.Home(),"\\outdata\\Spikes_",runref,".dat",sep=""),
#                   append=TRUE)
          } 
        } #else {
#               nospike.iter <- c(nospike.iter,iter)
#               nospike.yr <- c(nospike.yr,NA)
#               nospike.ssb <- c(nospike.ssb,sum(ssb.J1.true))
#               nospike.bloss <- c(nospike.bloss,lSR[[iter]]$Bloss)
#               nospike.rec <- c(nospike.rec,rec_ret[1])
#               nospike.model <- c(nospike.model,lSR[[iter]]$model)
#               write(c(NA,sum(ssb.J1.true),f.SADSR(iter,iterations,indata,SR.types)$Bloss,rec_ret[1]),
#                     file = paste(FPRESS.Home(),"\\outdata\\NoSpikes_",runref,".dat",sep=""),
#                     append=TRUE)      
#            }
#          } 
        }

        #end spike recruitment logic

				#number of recruits returned in first element
				recruitsnew <- rec_ret[1]
        #deterministic value
        recruitsdetnew <- rec_ret[3]

				#add recruits to population
				pop[1]<-recruitsnew
        
        #######assessment & management#############################
				#WKWHMMP2 - the first year is also an assessment year
				#in this instance. The code below would normally skip 
				#this because there is no year for results which the management
				#can be based upon
        #if ((year-assessdelay)>0) {
				#if (((year-assessdelay)>0) | (simyear==2013)) {
				if (((year-assessdelay)>0) | (simyear==2014)) {
				    
					#if we're in a management year then generate eggs 
					#from SSB at spawning time in previous year (HCR10 specific)
					#if (hcryear(simyear,hcrperiod,firstmanagementyear,debug)) {
					 if (hcryear(log.file,simyear,hcrperiod,firstmanagementyear,debug)) {
						eggs[1] <- eggs[2]
						eggs[2] <- eggs[3]
						#q <- 1.24
						#V2_11 24_11_2011
						#b <- 0.5
						#b <- 1.5
						#cvegg <- 0.3
						lambda <- 0.48
						#WKWHMMP2 - NEW FUNCTION eggcalc_wkwhmmp2
						#eggs[3] <- eggcalc(ssb,ssb.spwnweight,q,b,cvegg,lambda)
						#eggs[3] <- eggcalc_wkwhmmp2(if(simyear==2011) ssb=ssb.2010.st else ssb=ssb.st.true,
						#                            initspwnweightint,
						#                            aFec,bFec,qFec,
						#                            segg,sex.ratio,debug)
						
            if (simyear==2014){
              #assign 2013 egg count
              eggs[3] <- 397
            } else {
              #calculate a value
              #eggs[3] <- eggcalc_wkwhmmp2(SSB=ssb.st.true,
              #                            wgt=initspwnweightint,
              #                            aFec,bFec,qFec,
              #                            if (det) {0} else {segg},
              #                            sex.ratio,debug)              
              eggs[3] <- eggcalc_mse2014(log.file = log.file,
                                         SSB = ssb.st.true,
                                         wgt = initspwnweightint,
                                         EggModel = lEgg[[iter]],
                                         SexRatio = sex.ratio,
                                         year = simyear-1,
                                         debug = debug)[1]
            }
            
            #cat(simyear,eggs,"\n")
						            
	       	}

					#apply the assessment model to calculate new values for ssb and fbar (see assessment.r for details)
					assres<-assessment(log.file,ssb.true = ssb.J1.true,
                             fbar,
                             if (det) {1} else {ssbassessbias},
                             if (det) {0} else {ssbassesscv},
                             if (det) {1} else {fbarassessbias},
                             if (det) {0} else {fbarassesscv},
                             debug)

					#ssb is stored in the first element of the returned vector...
					ssb.J1.obs <- assres[1]
          
					#...and fbar in the second
					fbar <- assres[2]
          
					#pass the modified (assessment) ssb and fbar values to the management routine 
					#which will calculate an ftac multiplier via the HCR
					man_ret <- f.management(log.file,ssb.obs=ssb.J1.obs,fbar,
                                ftac,year,
                                years,assessdelay,
                                hcrperiod,hcrrule,
                                hcrchange,ssbhcr,
                                oldftacmult,fbarhcr,
                                ssbtest1,ssbtest2,
                                mintac,maxtac,
                                suspend,simyear,
                                firstmanagementyear,eggs,
                                ssb.spwnweight,TAC.ref,
                                weight.parameter,beta,
                                debug,egglimit=egglimit,egggamma=egggamma)

					#the ftac multiplier is returned as the first element...
					ftacmult <- man_ret[1]
					#...and a second value of interest can be returned as the second (not currently used)
					hcrsub <- man_ret[2]

				} else {

					#no assessment and management this year so
					#the multiplier is simply set to unity so no change is effected

					ftacmult <- 1
					hcrsub <- 0
	    
				} #end of assessment management loop

				#update the local ftacmult and hcrsub arrays using the year and iter counters to assign the values
				#to the correct slots
				#ftacmultarray[1,year,1,1,iter] <- ftacmult
        ftacmultarray[1,year,1,1,1,iter] <- ftacmult
				#hcrsubarray[1,year,1,1,iter] <- hcrsub
        hcrsubarray[1,year,1,1,1,iter] <- hcrsub
				#eggsarray[1,year,1,1,iter] <- eggs[3]
        eggsarray[1,year,1,1,1,iter] <- eggs[3]

				#apply the multiplier to calculate the new ftac, 
				#first saving the original value in temporary variable origftac
				origftac <- ftac
        
				#apply the ftac multiplier calculated by the HCR
				ftac <- ftacmult*ftac
            
        #cat("ftac=",ftac,"\n")
        
#        #initialise vectors for start of year
#				#age class
#				age <- initage
#				#population. If past the first year of the simulation, use the population as simulated 
#				#at the end of the previous year. If in the first year of the simulation, used the populations
#				#as specified in the datafile
#				if (year>1) {popint <- newyearplus1} else {popint <- initpopint}
#				#population cv
#				popcv <- initpopcv
#         #spawning weight
# 				spwnweightint <- initspwnweightint
# 				#spawning weight cv
# 				spwnweightcv <- initspwnweightcv
# 				#catch weight
# 				catweightint <- initcatweightint
# 				#catch weight cv
# 				catweightcv <- initcatweightcv
# 				#maturity
# 				matureint <- initmatureint
# 				#maturity cv
# 				maturecv <- initmaturecv
# 				#fishing mortality
# 				fint <- initfint
# 				#fishing mortality cv
# 				fcv <- initfcv
# 				#proportion of fishing mortality prior to spawning
# 				fprop <- initfprop
# 				#discard mortality
# 				disfint <- initdisfint
# 				#discard mortality cv
# 				disfcv <- initdisfcv
# 				#natural mortality
# 				mint <- initmint
# 				#natural mortality cv
# 				mcv <- initmcv
# 				#proportion of natural mortality before spawning
# 				mprop <- initmprop

				#recruits entering fishery
				#population will only be supplemented here if minage>0. When minage=0 the fish enter immediately

				if (minage>0) {

					#we also must have some historical recruitment values here since entry to the fishery
					#is after spawning and the model execution period may not cover the year for which the
					#recruitment is originally calculated. For example, if minage=3 and the model starts
					#execution in 2000, the first year that the model can calculate recruitment for is 2003.
					#thus historical values are required for 2000, 2001 and 2002

					#if (histrec==1) {
					#if (simyear <= maxrecyr) {			  
					#BUG - should be simyear-minage to properly offset data in SSB/R file
					if (simyear-minage <= maxrecyr) {

						#loop through historical data to find the value we need
						for (i in 1:lenrecyr) {
							#if (recyear[i]==simyear) {index<-i}		
							#BUG - should be simyear-minage to properly offset data in SSB/R file
							if (recyear[i]==simyear-minage) {index <- i}
						}

						#initialise value to -1
						frec <- -1

						#apply historical recruitment cv value
						while(frec<0){frec <- rnorm(1,recr[index],recr[index]*histreccv)}

						#recruitsarray[1,year,1,1,iter] <- frec
						recruitsarray[1,year,1,1,1,iter] <- frec
						fishrecruited <- frec

					} else {

						#simulation year has passed end of historical value dataset and
						#recruitment value should already be in the recruitsarray
						#frec <- as.numeric(recruitsarray[1,(year-minage),1,1,iter])
						frec <- as.numeric(recruitsarray[1,(year-minage),1,1,1,iter])

					}

					fishrecruited <- frec 

					#}

				} #end of minage>0 loop

# 				#Randomising parameters - adding noise / stochasticity to account for 
# 				#environmental/operational variability
# 				#Initial population (randomised in first year of projection only)
# 				pop <- popint
# 				popintsd <- ((if (det) {0} else {popcv})*popint)
# 				if (year==1) { 
# 					pop <- -popint
# 					#keep drawing until a positive result is obtained
# 					while (min(pop)<0) {
# 						for (i in 1:numage) {
# 							pop[i] <- rnorm(1,popint[i],popintsd[i])
# 						}
# 					} 
# 				}
# 
# 				#if minage>0 then take the fishrecruited value calculated above for the first element
# 				#of the population vector
# 				if (minage>0) {pop[1] <- fishrecruited}
# 
# 				#Spawning weight
# 				spwnweight <- -spwnweightint
# 				spwnweightsd <- (if (det) {0} else {spwnweightcv})*spwnweightint 
# 				
# 				while (min(spwnweight)<0) {
# 					for (i in 1:numage) {
# 						spwnweight[i] <- rnorm(1,spwnweightint[i],spwnweightsd[i])
# 					}
# 				} 
# 
# 				#Catch weight
# 				catweight <- -catweightint
# 				catweightsd <- (if (det) {0} else {catweightcv})*catweightint 
# 				
# 				while (min(catweight)<0) {
# 					for (i in 1:numage) {
# 						catweight[i] <- rnorm(1,catweightint[i],catweightsd[i])
# 					} 
# 				} 
# 	   	 
# 				#Maturity
# 				mature <- -matureint
# 				maturesd <- (if(det) {0} else {maturecv})*matureint
# 				
# 				while (min(mature)<0) {
# 					for (i in 1:numage) {
# 						mature[i] <- min(rnorm(1,matureint[i],maturesd[i]),1)
# 					}
# 				} 
# 
# 				#Fishing mortality
# 				f <- -fint
# 				fsd <- (if (det) {0} else {fcv})*fint
# 				
# 				 
# 				while (min(f)<0) { 
# 					for (i in 1:numage) {
# 						f[i] <- rnorm(1,fint[i],fsd[i])
# 					}
# 				} 
# 
# 				#Discard mortality
# 				disf <- -disfint
# 				disfsd <- (if (det) {0} else {disfcv})*disfint
# 				
# 				while (min(disf)<0) { 
# 					for (i in 1:numage) {
# 						disf[i] <- fmult*rnorm(1,disfint[i],disfsd[i])
# 					}
# 				} 
#                   
# 				#Natural mortality
# 				m <- -mint
# 				msd <- (if (det) {0} else {mcv})*mint
# 				
# 				while (min(m)<0) {
# 					for (i in 1:numage) {
# 						m[i]<-rnorm(1,mint[i],msd[i])
# 					}
# 				} 

				#newfbar - calculated by taking the mean of f over the age groups defined by the 
				#parameters fbarmin and fbarmax
				newfbar <- mean(f[(fbarmin-minage+1):(fbarmax-minage+1)])

				#Calculate FTAC
				#If historical catches are to be used and the current simulation year is within the available
				#data period then find the appropriate catch or fbar from the historical dataset. This value
				#will then be used as the mean value for the normal distribution from which a randomised value
				#for ftac will be drawn with the cv value given by histcatchcv

				#if historical values are used for catch then the bias and cv associated with 
				#F/TAC will be set to 1 and 0 respectively

				newvarcv <- varcv
				newvarbias <- varbias

        #if historic catches/fishing mortalities are to be used and the simlation year is less than or equal
        #to the final year for which hostoric catch data is available

				if ((histcatch == 1) & (simyear <= maxcatyr)) {

					#identify the relevant year
					index <- 0
					for (i in 1:lencatyr) {if (catyear[i] == simyear){index <- i}}

					#now perform the random draw
          
					#F controlled fishery 
					if (variable == 1) {
						#initialise
						ftac <- -1
						#randomise
						while (ftac < 0) {ftac <- rnorm(1,cfbar[index],cfbar[index]*(if (det) {0} else {histcatchcv}))} 
						#reset cv, bias parameters
						newvarcv <- 0
						newvarbias <- 1
					}
            
					#TAC controlled fishery
					if (variable == 0) { 
						#initialise
						ftac <- -1
						#randomise
						while (ftac < 0) {ftac <- rnorm(1,ccatch[index],ccatch[index]*(if (det) {0} else {histcatchcv}))} 
						#reset cv, bias parameters
						newvarcv <- 0
						newvarbias <- 1
					} 
				} 


        #Update the ftacarray value
        #ftacarray[1,year,1,1,iter] <- ftac  
        ftacarray[1,year,1,1,1,iter] <- ftac  

        #sometimes, the value is over-ridden
        #For TAC controlled WHM simulations the following assumtions are made for years 2014-2016
        #2014 - the TAC is 133,220t. Based on the IMY catch as calculated by WGWIDE 2014
        #2015 - the TAC is the advice from ADGWIDE2014 = 99,304t
        #2016 - TAC assumed same as 2015
        #2016 - we assume that the MSY approach will be used. So, if SSB<MSYBtrigger the MSY fishing mortality (0.13)
        #is reduced by the proportion of SSB/MSYBtrigger. Should SSB be greater than MSTBtrigger then the Fmsy is applied
        #and a TAC derived from it. For F controlled fisheries (such as the long term simulations), no override is done

        #TAC controlled fishery
#         if (variable==0 & simyear==2016) {
#           if (sum(ssb.J1.true)>634577) {
#             #fish at Fmsy
#             ftarget <- Fmsy
#           } else {
#             #fish at a reduced F under MSY rules
#             ftarget <- Fmsy*sum(ssb.J1.true)/MSYBtrigger 
#           } 
#           #update ftac array
#           ftacarray[1,year,1,1,iter] <- sum(ssb.J1.true) - sum(ssb.J1.true)*exp(-1*(m[1] + ftarget))
#           #cat(Fmsy,ftarget,ftacarray[1,year,1,1,iter],"\n")
#         }

        #cat(ftacarray[1,year,1,1,iter],"\n")

				#Apply F multiplier to F vectors (only required if simulation is based on f)
				#The specified level of F given by ftac is altered by possible bias (overfishing) given by varbias 	
				#and then randomised by drawing from a Normal distribution. The randomised value of F (freq) is the 
				#required fishing mortality for this year. A multiplier, fmult, is calculated from newfbar so that 
				#when multiplied by the current F vector f it will give the required value of freq. Finally, the F 
				#vector f is multiplied by this multiplier along with the discard F vector disf.
				#In reality, discard F may not change in the same way as F when catches are reduced.

        #F controlled fishery
				if (variable == 1){

          #apply bias
				  #fnewval <- ftac*(if (det) {1} else {newvarbias})
				  #fnewval <- ftacarray[1,year,1,1,iter]*(if (det) {1} else {newvarbias})
				  fnewval <- ftacarray[1,year,1,1,1,iter]*(if (det) {1} else {newvarbias})
          
					#randomise fnewval
					nf <- -1        
				  while (nf<0) {nf <- rnorm(1,fnewval,fnewval*(if (det) {0} else {newvarcv}))}
				  
					#this is the required f vector
					freq <- nf

					#calculate f multiplier
					fmult <- freq/newfbar
       
					#apply multiplier to f
					f <- fmult*f

          #apply multiplier to discard f
					disf <- disf*fmult
				}

				#If using a TAC, calculate the appropriate F multiplier.
				#The specified level of TAC given by FTAC is altered by possible bias (overfishing) given by varbias 
				#and then randomised (this occurs each year) by drawing from a Normal distribution. The randomised 
				#value of TAC given by tac is the required catch for this year. Function ffactor is then used to 
				#calculate an F multiplier, ffac, that when applied to the current F vector f, will produce the 
				#required catch equal to the TAC (so that fishing mortality in the simulation is always actually 
				#calculated using F even when the virtual fishery uses TAC). Finally, the F vector f is multiplied
				#by this multiplier along with the discard F vector disf.

				if (variable == 0) {
          
					#apply bias			  
				  #tacval <- ftacarray[1,year,1,1,iter]*(if (det) {1} else {newvarbias})  #=1 no bias, <1 under, >1 over
				  tacval <- ftacarray[1,year,1,1,1,iter]*(if (det) {1} else {newvarbias})  #=1 no bias, <1 under, >1 over
          
					#randomise
					ntac <- -1
				  while (ntac<0) {ntac <- rnorm(1,tacval,tacval*(if (det) {0} else {newvarcv}))}
				  
					#this is the requied tac vector
					tac <- ntac
          
					#calculate f vector
					ffac <- ffactor(tac,pop,f,disf,catweight,m,numage)

					#apply multipler to f
					f <- f*ffac
					#apply multiplier to discard f
					disf <- disf*ffac
					fmult <- ffac
          
				}

        #Update fishing mortality and fishing mortality multiplier arrays
				#fishingmortalityarray[,year,1,1,iter] <- f
        fishingmortalityarray[,year,1,1,1,iter] <- f
				#fishingmortalitymultarray[,year,1,1,iter] <- fmult
        fishingmortalitymultarray[,year,1,1,1,iter] <- fmult

				##################################################################
				#POPULATION DYNAMICS
				##################################################################

				#PRIOR TO SPAWNING
				if (debug==1) {log.write.ln(log.file,"debug::dynamics before spawning",TRUE)} 

				#proportion of fishing mortality that takes place before spawning
				fbef <- (f+disf)*fprop
      
				#proportion of fishing mortality that takes place after spawning
				faft <- (f+disf)*(1-fprop)
      
				#proportion of natural mortality that takes place before spawning
				mbef <- m*mprop
      
				#proportion of natural mortality that takes place after spawning
				maft <- m-mbef

				#calculate the new population due to the fishing and natural mortality that 
				#occurs before spawning
				newstock <- (pop*exp(-mbef-fbef))

				#new spawning population is the total population multiplied by the maturity vector
				newspawnpop <- (newstock*mature)
      
				#new spawning biomass is the new spawning population multiplied by the spawning weight vector
				newspawnmass <- newspawnpop*spwnweight 
				newspawnmass2 <- newspawnpop*spwnweight*spwnweight
      
				#update the local array with the new spawning biomass for this iteration/year
				#newspawnmassarray[,year,1,1,iter] <- newspawnmass
        newspawnmassarray[,year,1,1,1,iter] <- newspawnmass
        #ssb.st.true.array[,year,1,1,iter] <- newspawnmass
        ssb.st.true.array[,year,1,1,1,iter] <- newspawnmass
				#ssb.J1.true.array[,year,1,1,iter] <- ssb.J1.true
        ssb.J1.true.array[,year,1,1,1,iter] <- ssb.J1.true
      
				#cat("***pop.J1=",pop,"\n")
				#cat("***ssb.J1.true = ",ssb.J1.true,sum(ssb.J1.true),"\n")
        #cat("***pop.st=",newstock,"\n")
        #cat("***ssb.st.true = ",newspawnmass,sum(newspawnmass),"\n")
        
				#calculate the total spawning stock biomass by summing over all age classes
        ssb.st.true <- newspawnmass
        ssb <- sum(newspawnmass)
				ssb.spwnweight <- sum(newspawnmass2)        
        
				#if the minimum age is zero then the fish enter the fishery from age 0 
				#i.e. new recruits enter the fishery immediately after spawning so we add them now
				if (minage==0) {                      
      
					if (simyear > maxrecyr) {         
        
						#If the current simulation year is greater than the oldest year in the recruitment data file, 
						#then set the recruitment (frec) = recruitsnew
						frec <- recruitsnew
        
					} else {
        
						#if the current simulation year is contained in the recruitment data file
						#then the expected recruitment is set to the corresponding value given in the file
        
						for (i in 1:lenrecyr) {
							if (recyear[i]==simyear) {num <- i}
						}
        
						#randomise the number of recruits
						frec <- -1
						while (frec < 0) {frec <- rnorm(1,recr[num],recr[num]*(if (det) {0} else {histreccv}))}
						
					}
       
					#BUGfix 16/10/2008
					#assign the new recruits to the first population vector element
					yearrecruits <- frec

					#write data to local recruitsarray
					#recruitsarray[1,year,1,1,iter] <- yearrecruits
					recruitsarray[1,year,1,1,1,iter] <- yearrecruits
          #recruitsdetarray[1,year,1,1,iter] <- recruitsdetnew
					recruitsdetarray[1,year,1,1,1,iter] <- recruitsdetnew
					
				} else {
	 
					#the value recruitsnew applies to a future year
					#write this into a future slot in the recruitsarray (provided there is one - 
					#no point in writing beyond end of simulation period)

					if ((year+minage) <= years) {
						#recruitsarray[1,year+minage,1,1,iter] <- recruitsnew
						recruitsarray[1,year+minage,1,1,1,iter] <- recruitsnew
					}

				}	#end of minage==0 if construct

			
				#TAKE THE CATCH
				if (debug==1) {log.write.ln(log.file,"debug::removing catch",TRUE)} 
        
				fbarz <- c()
				for (i in (fbarmin-minage+1):(fbarmax-minage+1)) {
					fbarz <- c(fbarz,f[i])
				}

				fbar <- mean(fbarz)

				#update local array
				#fbararray[1,year,1,1,iter] <- fbar
        fbararray[1,year,1,1,1,iter] <- fbar
        #cat("fbar=",fbar,"\n")

				#calculate the catch in number
				catch <- (pop*f/(f+disf+m+0.00001)*(1-exp(-f-disf-m)))
      
				#calculate the catch weight
				catchwght <- catch*catweight
      
				#update local array
				#catchweightarray[,year,1,1,iter] <- catchwght
        catchweightarray[,year,1,1,1,iter] <- catchwght

				#AFTER SPAWNING
				#calculate the year end population by applying the fishing and natural mortality that occurs after spawning
				yearendstock <- newstock*exp(-maft-faft)
     
				#update local arrays
				#yearendstockarray[,year,1,1,iter] <- yearendstock
        yearendstockarray[,year,1,1,1,iter] <- yearendstock
				#poparray[,year,1,1,iter] <- pop
        poparray[,year,1,1,1,iter] <- pop
        #catcharray[,year,1,1,iter] <- catch
        catcharray[,year,1,1,1,iter] <- catch
				#farray[,year,1,1,iter] <- f
        farray[,year,1,1,1,iter] <- f
      
				#plus group
				#BUGfix V2.1 16/10/2008
        #24/05/2013
				#yearplus1 <- yearrecruits
        yearplus1 <- recruitsnew

				#add in older age classes
				for (i in 1:(numage-2)) {yearplus1 <- c(yearplus1,yearendstock[i])}
      
				#add on the plus group
				yearplus1 <- c(yearplus1, yearendstock[numage-1]+yearendstock[numage])

				#save vals in temp vars for initialisation of population in next year
				newyearplus1 <- yearplus1
        ssb.J1.true <- newyearplus1*mature*spwnweight

        #no longer required (targetyield setting)
				#if in the next year of the simulation an HCR is going to be applied then revert
				#ftac back to the resolution.ftac value
				#if ((hcryear(simyear+1,hcrperiod,firstmanagementyear)) & (targetyield==1))
				#{
				#ftac <- resolution.ftac
				#}

  
			}    
			###########################################################
			#end of year loop##########################################
			###########################################################

		}   
		##########################################################
		#end of main iterative loop###############################
		##########################################################

    #print(lsos());

		#browser();
 
		#Save data
		#Create FLQuant objects
		#FLQuant() is the creator function for FLQuant objects
		op.CatchWeight <- FLQuant(catchweightarray,units='Kgs')
    op.SSB.st.true <- FLQuant(ssb.st.true.array,units='Kgs')
    op.SSB.J1.true <- FLQuant(ssb.J1.true.array,units='Kgs')
		NewSpawnMass <- FLQuant(newspawnmassarray,units='Kgs')
		op.FBar <- FLQuant(fbararray,units='F')
		op.Recruits <- FLQuant(recruitsarray,units='000s')
		op.RecruitsDet <- FLQuant(recruitsdetarray,units='000s')
		op.YearEndStock <- FLQuant(yearendstockarray,units='Kgs')
		Pop <- FLQuant(poparray,units='Kgs')
		Catch <- FLQuant(catcharray,units='Kgs')
		op.FishingMortality <- FLQuant(fishingmortalityarray,units='F')
		FishingMortalityMult <- FLQuant(fishingmortalitymultarray,units='F')
		F <- FLQuant(farray,units='F')
		op.FTac <- FLQuant(ftacarray,units='ftac')
		op.FTacMult <- FLQuant(ftacmultarray,units='mult')
		op.EggCounts <- FLQuant(eggsarray,units='eggs')
    op.lSR <- lSR
    op.lEgg <- lEgg

		#stats calculations
		SSB <- quantSums(NewSpawnMass)

		#save specified objects
		if (substr(outdata.path,nchar(outdata.path),nchar(outdata.path))=="\\") {
			outfile <- paste(outdata.path,FLRfiles[res+1],sep="") 
		} else {
			outfile <- paste(outdata.path,"\\",FLRfiles[res+1],sep="")
		}

#     write.table(data.frame(spike.iter,spike.yr,spike.ssb,spike.bloss,spike.rec,spike.model),
#                 file = paste(FPRESS.Home(),"\\outdata\\Spikes_",runref,".dat",sep=""),
#                 append=TRUE,row.names=FALSE,col.names=FALSE)
#     
#     write.table(data.frame(nospike.iter,nospike.yr,nospike.ssb,nospike.bloss,nospike.rec,nospike.model),
#                 file = paste(FPRESS.Home(),"\\outdata\\NoSpikes_",runref,".dat",sep=""),
#                 append=TRUE,row.names=FALSE,col.names=FALSE)

    #write output filename
		log.write.ln(log.file,paste("outfile::",outfile,sep=""))
    
		save(list=savedobjs,file=outfile)

	}     
	############################################################
	#end of resolution loop#####################################
	############################################################

	#some end logging
	print(Sys.time())
	log.write.systime(log.file,TRUE)
	#calculate the end time and the difference with the starttime and
	#print duration of run to screen
	enddate <- as.POSIXlt(Sys.time(),Sys.timezone())
	diff <- difftime(enddate,startdate,tz="GMT",units="secs")
	cat("Execution Time:",diff[[1]]," secs", "\n")
	log.write.ln(log.file,paste("Execution Time:",diff[[1]]," secs"),TRUE)
  log.write.ln(log.file,"Complete")

	#restore working directory
	setwd(currwd)

  #return the name of the log file
  return(log.file)
  
# EOF
}
