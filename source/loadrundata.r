#loadrundata.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#V2 - consolidated 07/10/2008
#V2.1 - added option to override the drive in FPRESS.Home()
#and specify a subfolder of FPRESS.Home/outdata

#Sys.putenv("V:loadrundata.r"="2.1")
#Sys.setenv("V:loadrundata.r"="2.1")

#assign the environment var V:loadrundata.r
set.env.var("V:loadrundata.r","2.1")

#load the FLR data objects from a single f/tac simulation

fload_run_data <- function(runref = "missing",
                        saveref,
                        resref = "missing",
                        outdata.drive = "missing",
                        outdata.subfolder = "missing"){

   if (missing(runref)) {runref<-winDialogString("Run Reference","")}

   if (nchar(runref)==0) stop("Exit - invalid run reference")

   #construct options filename
   optionsfile = paste(FPRESS.Home(),"\\options\\options",runref,".xml",sep="")

   #check options file exists
   if (!(file.exists(optionsfile))) {
      stop(paste("\nOptions file",optionsfile,"doesn't exist\nUnable to continue\n",sep=" "))
   }

   opt <- readoptions(runref)
   
   #load xml tree
   doc<-xmlTreeParse(optionsfile)

   #root xmlNode object
   root<-xmlRoot(doc)

   #node values
   resolution <- as.integer(nodeval(root,"resolution",0))

   if (missing(resref)) {
      if (resolution > 1) {
         resref <- winDialogString(paste("Resolution Reference (1-",resolution+1,")",sep=""),"")
      } else {
	 resref <- "1"
      }
   }

	if (missing(outdata.drive)) {
		if (missing(outdata.subfolder)) {
			FLRFile <- paste(opt$outfolder,
				"\\outdata\\FLRObjects_",
				runref,
				"_",
				resref,
				".dat",
				sep="")
		} else {
			FLRFile <- paste(opt$outfolder,
				"\\outdata\\",
				outdata.subfolder,
				"\\FLRObjects_",
				runref,
				"_",
				resref,
				".dat",
				sep="")
		} 
	} else {
		if (missing(outdata.subfolder)) {
			FLRFile <- paste(outdata.drive,
				substr(FPRESS.Home(),3,100000),
				"\\outdata\\FLRObjects_",
				runref,
				"_",
				resref,
				".dat",
				sep="")
		} else {
			FLRFile <- paste(outdata.drive,
				substr(FPRESS.Home(),3,100000),
				"\\outdata\\",
				outdata.subfolder,
				"\\FLRObjects_",
				runref,
				"_",
				resref,
				".dat",
				sep="")
		}
	}

   #check FLR file exists
   if (!(file.exists(FLRFile))) {
      stop(paste("\nFLR data file",FLRFile,"doesn't exist\nUnable to continue\n",sep=" "))
   }

  cat(FLRFile,"\n");
  rm(list=ls(pattern="^op[.]"))
  load(FLRFile,envir=parent.frame())

}

