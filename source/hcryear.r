#Sys.putenv("V:hcryear.r"="2.0")
#Sys.setenv("V:hcryear.r"="2.0")

#assign the environment var V:hcryear.r
set.env.var("V:hcryear.r","2.0")

#hcryear<-function(log.file,year,hcr.period,first.management.year,debug=0) {
fis_hcr_year<-function(year,hcr_period,first_management_year) {
    
  #argument list
  #log.file - the log file
  #year - the year (not year number)
  #hcr.period - the period of the harvest control rule
  #first.management.year - the first year in which the harvest control rule is to be applied

#   if (debug==1){
#     log.write.func.enter(log.file,formals()[-1],list(year,hcr.period,first.management.year,debug))
#   }
  
  if (year<first_management_year) {
    
    ret <- FALSE
  
  } else {

    yearmod<-(year-first_management_year)%%hcr_period
    
    #trap NA
    if(is.na(yearmod)==TRUE){yearmod<-1}
    
    ret <- yearmod==0
  }
  
#  if(debug==1){log.write.func.exit(log.file,ret)}
  
  ret
}
