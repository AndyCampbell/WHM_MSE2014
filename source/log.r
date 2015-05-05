#log.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie

#assign the environment var V:log.r
fset_env_var("V:log.r","2.0")

#various logging functions for FPRESS

fFPRESS_home<-function()
{
  Sys.getenv("FPRESSHome")
}

# tst<-function(){
#   log.enter()
#   log.exit()
# }

# log.enter<-function(){
#   #cat("enter\n")
#   #print(sys.parent(sys.parent()))
#   cat(sys.call(sys.parent())[[1]],"ENTER\n",sep="::")
# }
# 
# log.exit<-function(){
#   #cat("exit\n")
#   #print(sys.parent(sys.parent()))
#   cat(sys.call(sys.parent(sys.parent()))[[1]],"EXIT\n",sep="::")  
# }

# curfnfinder<-function(skipframes=0,skipnames="(FUN)|(.+apply)|(replicate)",
#                       retIfNone="Not in function",retStack=FALSE,extraPrefPerLevel="\t")
# {
#   prefix <- sapply(3+skipframes+1:sys.nframe(),function(i){
#     currv<-sys.call(sys.parent(n=i))[[1]]
#     return(currv)
#   })
#   
#   prefix[grep(skipnames,prefix)]<-NULL
#   prefix<-gsub("function \\(.*","do.call",prefix)
#   if(length(prefix)==0)
#   {
#     return(retIfNone)
#   }
#   else if(retStack)
#   {
#     return(paste(rev(prefix),collapse="|"))
#   }
#   else
#   {
#     retval<-as.character(unlist(prefix[1]))
#     if(length(prefix)>1)
#     {
#       retval<-paste(paste(rep(extraPrefPerLevel,length(prefix)-1),collapse="|"))
#     }
#     return(retval)
#   }
# }


#various logging functions for FPRESS
# log.write.func.enter <- function(log.file,args,vals){
#   
#   write(paste("\n",sys.call(sys.parent())[[1]],"::ENTER with",sep=""),
#         file=log.file,
#         append=TRUE)
#   
#   #log function arguments and values
#   for (i in 1:length(args)){
#     write(paste(" ",names(args)[i]," = ",toString(vals[[i]]),sep=""),
#           file=log.file,
#           append=TRUE)
#     }
# }
# 
# log.write.func.exit <- function(log.file,ret){
#   
#   write(paste("\n",sys.call(sys.parent())[[1]],"::EXIT with",sep=""),
#         file=log.file,
#         append=TRUE)
#   
#   #log function arguments and values
#   write(paste("  ",toString(ret),sep=""),
#         file=log.file,
#         append=TRUE)
#   
# }

flog_write_ln <- function(in.log.file,in.line="",in.append=TRUE,timestamp=FALSE) {
	#write the supplied text line to the log file
  
  if (is.list(in.line)){
    if(timestamp){log.write.systime(in.log.file,in.append=TRUE)}
    lapply(in.line,print)
  } else {
    if(timestamp){
      write(paste(as.character(as.POSIXlt(Sys.time(),Sys.timezone())),"-",in.line,sep=""),file=in.log.file,append=in.append)  
    } else {
      write(in.line,file=in.log.file,append=in.append)  
    }
  }
  
}

flog_write_systime <- function(in.log.file,in.append=TRUE) {
	#write the current sys time to the log file
	write(as.character(as.POSIXlt(Sys.time(),Sys.timezone())),file=in.log.file,append=in.append)
}

# log.write.options <- function(in.log.file,in.options.file,in.append=TRUE) {
# 
# 	#write out the simulation options to the log file
# 
# 	doc<-xmlTreeParse(in.options.file)
# 	root<-xmlRoot(doc)
# 
# 	for (i in 1:(length(doc$doc$children$FPRESS))){
# 		name = names(doc$doc$children$FPRESS)[i]
# 		val = nodeval(root,name,"NA")
# 		str <- paste(name,val,sep="::")
# 		write(str,file=in.log.file,append=in.append)
# 	}
# 
# }

flog_write_options <- function(lopt,logfile,append=TRUE){
  
  #lopt - list of options
  for (i in 1:length(lopt)){
    write(paste(names(lopt)[i],paste(lopt[[i]],collapse=","),sep="::"), file = logfile, append = append)
  }

}


fread_log <- function(ref){
  
  #read log file and return list containing info
  log <- paste(fFPRESS_home(),"\\runlogs\\",ref,".log",sep="")
  if(!file.exists(log)){stop(paste("\nLog file",log,"doesn't exist\nUnable to continue\n",sep=" "))}
  
  #read the log file
  log.dat<-readLines(log)

  #any lines containing the double colon are tag/value pairs
  #return a list with names corresponding the tags and values to value
  
  #remove data rows that do not contain ::
  log.dat <- log.dat[grepl("::",log.dat)]
  
  n <- unlist(lapply(strsplit(log.dat,"::"),"[",1))
  v <- unlist(lapply(strsplit(log.dat,"::"),"[",2))

  ret<-as.list(v)
  names(ret)<-n
  
  #convert to numeric where appropriate
  #(needs work)
  ret$startyear <- as.numeric(ret$startyear)
  ret$years <- as.numeric(ret$years)
  ret$minage <- as.numeric(ret$minage)
  ret$maxage <- as.numeric(ret$maxage)
  ret$fbarmin <- as.numeric(ret$fbarmin)
  ret$fbarmax <- as.numeric(ret$fbarmax)
#   ret$ssbtest1 <- as.numeric(ret$ssbtest1)
#   ret$ssbtest2 <- as.numeric(ret$ssbtest2)
  ret$resolution <- as.numeric(ret$resolution)
  ret$nits <- as.numeric(ret$nits)
#   ret$histrec <- as.numeric(ret$histrec)
#   ret$histreccv <- as.numeric(ret$histreccv)
#   ret$histcatch <- as.numeric(ret$histcatch)
#   ret$histcatchcv <- as.numeric(ret$histcatchcv)
#   ret$rec <- as.numeric(ret$rec)
#   ret$recmean <- as.numeric(ret$recmean)
#   ret$recmax <- as.numeric(ret$recmax)
#   ret$reccv <- as.numeric(ret$reccv)
#   ret$seggrad <- as.numeric(ret$seggrad)
#   ret$ssbcut <- as.numeric(ret$ssbcut)
#   ret$datarecyear <- as.numeric(ret$datarecyear)
#   ret$variable <- as.numeric(ret$variable)
#   ret$varcv <- as.numeric(ret$varcv)
#   ret$varbias <- as.numeric(ret$varbias)
#   ret$varmin <- as.numeric(ret$varmin)
#   ret$varmax <- as.numeric(ret$varmax)
#   ret$assessdelay <- as.numeric(ret$assessdelay)
#   ret$ssbassessbias <- as.numeric(ret$ssbassessbias)
#   ret$ssbassesscv <- as.numeric(ret$ssbassesscv)
#   ret$fbarassessbias <- as.numeric(ret$fbarassessbias)
#   ret$fbarassesscv <- as.numeric(ret$fbarassesscv)
#   ret$hcrrule <- as.numeric(ret$hcrrule)
#   ret$hcrperiod <- as.numeric(ret$hcrperiod)
#   ret$firstmanagementyear <- as.numeric(ret$firstmanagementyear)
#   ret$hcrchange <- as.numeric(ret$hcrchange)
#   ret$suspend <- as.numeric(ret$suspend)
#   ret$ssbhcr <- as.numeric(ret$ssbhcr)
#   ret$fbarhcr <- as.numeric(ret$fbarhcr)
#   ret$mintac <- as.numeric(ret$mintac)
#   ret$maxtac <- as.numeric(ret$maxtac)
#   ret$onceoff <- as.numeric(ret$onceoff)
#   ret$onceoffrec <- as.numeric(ret$onceoffrec)
#   ret$onceoffevery <- as.numeric(ret$onceoffevery)
#   ret$targetyield <- as.numeric(ret$targetyield)
#   ret$tacref <- as.numeric(ret$tacref)
#   ret$weight.parameter <- as.numeric(ret$weight.parameter)
#   ret$beta <- as.numeric(ret$beta)
  
  return(ret)
  
}

# f.savedobjs <- function(opindex=0,debug=0){
#   
#   savedobjs <- c()
#   
#   if (opindex >= 1024) {
#     savedobjs <- c(savedobjs,"op.YearEndStock")
#     opindex <- opindex - 1024
#   }
#   
#   if (opindex >= 512) {
#     savedobjs <- c(savedobjs,"op.CatchWeight")
#     opindex <- opindex - 512
#   }
#   
#   if (opindex >= 256) {
#     savedobjs <- c(savedobjs,"op.Catch")
#     opindex <- opindex - 256
#   }
#   
#   if (opindex >= 128) {
#     savedobjs <- c(savedobjs,"op.FBar")
#     opindex <- opindex - 128
#   }
#   
#   if (opindex >= 64) {
#     savedobjs <- c(savedobjs,"op.SSB.st.true","op.SSB.J1.true")
#     opindex <- opindex - 64
#   }
#   
#   if (opindex >= 32) {
#     savedobjs <- c(savedobjs,"op.FishingMortalityMult")
#     opindex <- opindex - 32
#   }
#   
#   if (opindex >= 16) {
#     savedobjs <- c(savedobjs,"op.FishingMortality")
#     opindex <- opindex - 16
#   }
#   
#   if (opindex >= 8) {
#     savedobjs <- c(savedobjs,"op.FTac")
#     opindex <- opindex - 8
#   }
#   
#   if (opindex >= 4) {
#     savedobjs <- c(savedobjs,"op.Pop.J1","op.Pop.ST","op.Pop.YE")
#     opindex <- opindex - 4
#   }
#   
#   if (opindex >= 2) {
#     savedobjs <- c(savedobjs,"op.Recruits")
#     savedobjs <- c(savedobjs,"op.RecruitsDet")
#     opindex <- opindex - 2
#   }
#   
#   if (opindex >= 1) {
#     savedobjs <- c(savedobjs,"op.FTacMult")
#     opindex <- opindex - 1 
#   }
#   
#   #additional object for HCR10
#   savedobjs <- c(savedobjs,"op.EggCounts")
#   
#   #SRModel,EggModel details
#   savedobjs <- c(savedobjs,"op.lSR","op.lEgg")
#   
#   if (debug==1) {log.write.ln(log.file,paste("debug::savedobjs::",savedobjs,sep=""))}
#   
#   return(savedobjs)
#   
# }

fread_options <- function(optionsfile,call) {
  
  c <- as.list(call)
  
  #load the simulation options xml document into memory using the xml library xmlTreeParse function
  doc <- xmlTreeParse(optionsfile)
  root <- xmlRoot(doc)

  nits <- if ('iterations' %in% names(c)) {c$iterations} else {as.integer(fnodeval(root,"iterations",1))}
  
  list('outfolder' = fnodeval(root,"outfolder",fFPRESS_home()),
       'outdata.path' = paste(fnodeval(root,"outfolder",fFPRESS_home()),"\\outdata\\",sep=""),
       'Paramfile' = fnodeval(root,"datfile",""),
       'SWfile' = fnodeval(root,"SWfile",""),
       'CWfile' = fnodeval(root,"CWfile",""),
       'Eggfile' = fnodeval(root,"Eggfile",""),
       'Numfile' = fnodeval(root,"Numfile",""),
       'Ffile' = fnodeval(root,"Ffile",""),
       'Mfile' = fnodeval(root,"NatMorfile",""),
       'Matfile' = fnodeval(root,"Matfile",""),
       'SRfile' = fnodeval(root,"SRfile",""),
       'minage' = as.integer(fnodeval(root,"minage",1)),
       'maxage' = as.integer(fnodeval(root,"maxage",1)),
       'simages' = as.integer(fnodeval(root,"minage",1)):as.integer(fnodeval(root,"maxage",1)),
       'fbarmin' = as.integer(fnodeval(root,"fbarmin",0)),
       'fbarmax' = as.integer(fnodeval(root,"fbarmax",1)),
       'resolution' = (if ('resolution' %in% names(c)) {c$resolution} else {as.numeric(fnodeval(root,"resolution",0))}),
       'startyear' = as.integer(fnodeval(root,"startyear",1)),
       'years' = as.integer(fnodeval(root,"years",1)),
       'endyear' = as.integer(fnodeval(root,"startyear",1)) + as.integer(fnodeval(root,"years",1)) + 1,
       'simyears' = as.integer(fnodeval(root,"startyear",1)):(as.integer(fnodeval(root,"startyear",1)) + as.integer(fnodeval(root,"years",1)) - 1),
       'nits' = nits,
       'simiters' = 1:nits,
       'variable' = as.numeric(fnodeval(root,"variable",0)),
       'varmin' = (if ('varmin' %in% names(c)) {c$varmin} else {as.numeric(fnodeval(root,"varmin",1))}),
       'varmax' = (if ('varmax' %in% names(c)) {c$varmax} else {as.numeric(fnodeval(root,"varmax",1))}),
       'onceoff' = as.integer(fnodeval(root,"onceoff",0)),
       'hcrrule' = as.integer(fnodeval(root,"hcrrule",0)),
       'hcrperiod' = as.integer(fnodeval(root,"hcrperiod",1)),
       'firstmanagementyear' = as.integer(fnodeval(root,"firstmanagementyear",as.integer(fnodeval(root,"startyear",1)))),
       'egglimit' = as.numeric(fnodeval(root,"egglimit",0)),
       'egggamma' = as.numeric(fnodeval(root,"egggamma",1)),
       'TACref' = as.numeric(fnodeval(root,"tacref",0)),
       'w' = as.numeric(fnodeval(root,"weightparameter",0)),
       'beta' = as.numeric(fnodeval(root,"beta",0)),
       'spikevar' = as.numeric(fnodeval(root,"spikevar",0.5))
  )
}