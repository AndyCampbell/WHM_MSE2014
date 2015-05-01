#update the GUI prompt 
options(prompt = "hom-west MSE2014 Baseline>>")

#function to source all files in path
fsourceDir <- function(path="C:\\FPRESS\\HOM-WEST\\MSE2014\\Baseline\\source",trace=TRUE,...) {
   for (nm in list.files(path,pattern = "\\.[RrSsQq]$")) {
      if(trace) cat(nm,":")
      source(file.path(path,nm), ...)
      if(trace) cat("\n")
   }
}

#load required 3rd party libraries
floadLibs <- function() {
   library(utils)
   library(FLCore)
   #library(FLEDA)
   library(XML)
   library(dplyr)
   library(pbapply)
   #library(FPRESS)
}

fset_env_var<-function(name,val) {
   ver <- R.Version()$minor
   if ((ver=="6.1") || (ver=="8.0") || (ver=="14.1")) {
	   Sys.setenv(name=val)
   } else {
	   Sys.setenv(name=val)
   }
}

.ls.objects <- function(pos=1,pattern,order.by,decreasing=FALSE,head=FALSE,n=5) {

   napply <- function(names,fn) sapply(names,function(x) fn(get(x,pos=pos)))

   #names <- ls(pos=pos,patten=pattern)
   names <- ls(pos=pos)
   obj.class <- napply(names,function(x) as.character(class(x))[1])
   obj.mode <- napply(names,mode)
   obj.type <- ifelse(is.na(obj.class),obj.mode,obj.class)
   obj.size <- napply(names, object.size)
   obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
   vec <- is.na(obj.dim)[,1] & (obj.type != "function")
   obj.dim[vec,1] <- napply(names,length)[vec]
   out <- data.frame(obj.type,obj.size,obj.dim)
   names(out) <- c("Type","Size","Rows","Columns")
   if (!missing(order.by))
      out <- out[order(out[[order.by]],decreasing=decreasing), ]
   if (head)
      out <- head(out,n)
   out
}

#shorthand
lsos <- function(..., n=10) {
   .ls.objects(...,order.by="Size",decreasing=TRUE,head=TRUE,n=n)
}

.First <- function() 
{

   #assign the environment variable FPRESSHome
	ver <- R.Version()$minor

	if ((ver=="6.1") || (ver=="8.0") || (ver=="14.1")) {
		Sys.setenv("FPRESSHome"="C:\\FPRESS\\HOM-WEST\\MSE2014\\Baseline")
	} else {
		Sys.setenv("FPRESSHome"="C:\\FPRESS\\HOM-WEST\\MSE2014\\Baseline")
	}

   #uncomment this line and insert the correct path 
   #if source is required
   #this will result in the functions as defined in the source files
   #masking those in the FPRESS package installation
   fsourceDir(paste(Sys.getenv("FPRESSHome"),"\\source",sep=""))

   #test files
   fsourceDir(paste(Sys.getenv("FPRESSHome"),"\\test",sep=""))

   #call loadlibs
   floadLibs()

   #greeting
   cat("Hello\n")

	#if(file.exists("Runs.r")) {source("Runs.r")}

}
.Last <- function()
{
  cat("\n   Goodbye!\n\n")
}
