#Conduct FPRESS runs for WHM MSE 2014-2015

#clean up
rm(list=ls())
gc()

#get the source code. Functions in files in source directory.
source(".Rprofile")
fsourceDir()

#create runlogs directory
runlogs.dir <- paste(fFPRESS_home(),"\\runlogs",sep="");
#create the directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(runlogs.dir, recursive = TRUE, showWarnings = FALSE)

#create outdata directory
outdata.dir <- paste(fFPRESS_home(),"\\outdata",sep="");
#create the directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(outdata.dir, recursive = TRUE, showWarnings = FALSE)

#create plots directory
plots.dir <- paste(fFPRESS_home(),"\\plots",sep="");
#create the directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(plots.dir, recursive = TRUE, showWarnings = FALSE)

#create stats directory
stats.dir <- paste(fFPRESS_home(),"\\stats",sep="");
#create the directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(stats.dir, recursive = TRUE, showWarnings = FALSE)

#create tables directory
tables.dir <- paste(fFPRESS_home(),"\\tables",sep="");
#create the directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(tables.dir, recursive = TRUE, showWarnings = FALSE)

#long term (MSY investigation) runs, no HCR applied
#options files - two different assessments are used for conditioning. 
#Runs are conducted for each with and without spikes in recruitment.
loptfiles <- c("MSE2014_SRSAD11_1scor_0spikes_1sigR","MSE2015_SRSAD11_1scor_0spikes_1sigR",
               "MSE2014_SRSAD11_1scor_1spikes_1sigR","MSE2015_SRSAD11_1scor_1spikes_1sigR")

loptfiles <- c("MSE2014_Final_MSY_NoSpikes","MSE2014_Final_MSY_Spikes")

#do the runs, very high resolution as these are to be the final runs
lapply(loptfiles,
       function(x) fFPRESS_run(runref = x,
                               iterations = 1000,
                               varmin = 0.0,
                               varmax = 0.2,
                               resolution = 200))

#calculate the statistics. SSBref vals correspond to SSBloss from the assessment upon which the 
#model initialisation is derived, divided by 1.4
lapply(loptfiles,
       function(x) fCalcMSE2014stats(ref = x,
                                     fname = paste(".\\stats\\",x,".dat",sep=""),
                                     period = c(2151,2200),
                                     ssbref = {if(grepl("2014",x)){0.453269e6}else{0.600700e6}}))

#generate yield plot for each scenario
lapply(loptfiles,
       function(x) fPlotFYld(ref = x,
                             sfile = paste(".\\stats\\",x,".dat",sep=""),
                             pfile = paste(".\\plots\\FYld_",x,".pdf",sep=""),
                             title = "MSE2014 - Yield & Risk vs FBar",
                             subtitle = {if(grepl("0spikes",x)){"Serial Correlation, Excluding Spike, 100% SigR"}else{"Serial Correlation, Including Spike, 100% SigR"}},
                             ylim = c(0,250),
                             xlim = c(0,0.2)))

#generate ssb plot
lapply(loptfiles,
       function(x) fPlotFSSB(ref = x,
                             sfile = paste(".\\stats\\",x,".dat",sep=""),
                             pfile = paste(".\\plots\\FSSB_",x,".pdf",sep=""),
                             title = "MSE2014 - SSB & Risk vs FBar",
                             subtitle = {if(grepl("0spikes",x)){"Serial Correlation, Excluding Spike, 100% SigR"}else{"Serial Correlation, Including Spike, 100% SigR"}},
                             ylim = c(0,7),
                             xlim = c(0,0.2)))


fPlotMSYJose(ref = "MSE2014_SRSAD11_1scor_1spikes_1sigR")
fPlotMSYJose(ref = "MSE2014_SRSAD11_1scor_0spikes_1sigR")
fPlotMSYJose(ref = "MSE2014_Final_MSY_NoSpikes")
fPlotMSYJose(ref = "MSE2014_Final_MSY_Spikes")


#low resolution runs to allow comparison of number of iterations on statistical outputs
loptfiles <- c("MSE2014_SRSAD11_1scor_1spikes_1sigR_lowres")

lapply(loptfiles,
       function(x) fFPRESS_run(runref = x,
                               iterations = 1000,
                               varmin = 0.0,
                               varmax = 0.2,
                               resolution = 4))


#50 years stats
#1000 stats calculations using a sample of 100 iterations each time
#generate a list of length 1000 with each element storing a vector of iteration numbers to be used

fopt <- "MSE2014_SRSAD11_1scor_1spikes_1sigR_lowres"

#based on 100 iterations
l100 <- vector("list",1000)
assign("l100",lapply(l100, function(x){sample.int(n=1000, size=100, replace=FALSE)}))

pblapply(l100,function(x) fCalcMSE2014stats(ref = fopt,
                                          fname = paste(".\\stats\\",fopt,"_100.dat",sep=""),
                                          period = c(2151,2200),
                                          nits = x,
                                          ssbref = {if(grepl("2014",fopt)){0.453269e6}else{0.600700e6}}))

#based on 250 iterations
l250 <- vector("list",1000)
assign("l250",lapply(l250, function(x){sample.int(n=1000, size=250, replace=FALSE)}))

pblapply(l250,function(x) fCalcMSE2014stats(ref = fopt,
                                            fname = paste(".\\stats\\",fopt,"_250.dat",sep=""),
                                            period = c(2151,2200),
                                            nits = x,
                                            ssbref = {if(grepl("2014",fopt)){0.453269e6}else{0.600700e6}}))  

#based on 500 iterations
l500 <- vector("list",1000)
assign("l500",lapply(l500, function(x){sample.int(n=1000, size=500, replace=FALSE)}))

pblapply(l500,function(x) fCalcMSE2014stats(ref = fopt,
                                            fname = paste(".\\stats\\",fopt,"_500.dat",sep=""),
                                            period = c(2151,2200),
                                            nits = x,
                                            ssbref = {if(grepl("2014",fopt)){0.453269e6}else{0.600700e6}}))

#based on 750 iterations
l750 <- vector("list",1000)
assign("l750",lapply(l750, function(x){sample.int(n=1000, size=750, replace=FALSE)}))

pblapply(l750,function(x) fCalcMSE2014stats(ref = fopt,
                                            fname = paste(".\\stats\\",fopt,"_750.dat",sep=""),
                                            period = c(2151,2200),
                                            nits = x,
                                            ssbref = {if(grepl("2014",fopt)){0.453269e6}else{0.600700e6}}))


#based on 1000 iterations (only 1 sample required)
l1000 <- vector("list",1)
assign("l1000",lapply(l1000, function(x){sample.int(n=1000, size=1000, replace=FALSE)}))

pblapply(l1000,function(x) fCalcMSE2014stats(ref = fopt,
                                             fname = paste(".\\stats\\",fopt,"_1000.dat",sep=""),
                                             period = c(2151,2200),
                                             nits = x,
                                             ssbref = {if(grepl("2014",fopt)){0.453269e6}else{0.600700e6}}))

#options files for all HCR runs (based on 2014 WG assessment output)
opt_files <- c("MSE2014_300","MSE2014_301","MSE2014_302","MSE2014_303","MSE2014_304",
               "MSE2014_310","MSE2014_311","MSE2014_312","MSE2014_313","MSE2014_314",
               "MSE2014_320","MSE2014_321",
               "MSE2014_330","MSE2014_331","MSE2014_332","MSE2014_333","MSE2014_334",
               "MSE2014_340","MSE2014_341","MSE2014_342","MSE2014_343","MSE2014_344",
               "MSE2014_350","MSE2014_351","MSE2014_352","MSE2014_353","MSE2014_354",
               "MSE2014_360","MSE2014_361","MSE2014_362","MSE2014_363","MSE2014_364",
               "MSE2014_370","MSE2014_371","MSE2014_372","MSE2014_373","MSE2014_374",
               "MSE2014_380","MSE2014_381","MSE2014_382","MSE2014_383","MSE2014_384",
               "MSE2014_390","MSE2014_391","MSE2014_392","MSE2014_393","MSE2014_394",
               "MSE2014_400","MSE2014_401","MSE2014_402","MSE2014_403","MSE2014_404")

#runs with updated assessment (increased weighting of egg survey)
opt_files <- c("MSE2015_300","MSE2015_301","MSE2015_302","MSE2015_303","MSE2015_304",
               "MSE2015_310","MSE2015_311","MSE2015_312","MSE2015_313","MSE2015_314",
               "MSE2015_320","MSE2015_321",
               "MSE2015_330","MSE2015_331","MSE2015_332","MSE2015_333","MSE2015_334",
               "MSE2015_340","MSE2015_341","MSE2015_342","MSE2015_343","MSE2015_344",
               "MSE2015_350","MSE2015_351","MSE2015_352","MSE2015_353","MSE2015_354",
               "MSE2015_360","MSE2015_361","MSE2015_362","MSE2015_363","MSE2015_364",
               "MSE2015_370","MSE2015_371","MSE2015_372","MSE2015_373","MSE2015_374",
               "MSE2015_380","MSE2015_381","MSE2015_382","MSE2015_383","MSE2015_384",
               "MSE2015_390","MSE2015_391","MSE2015_392","MSE2015_393","MSE2015_394",
               "MSE2015_400","MSE2015_401","MSE2015_402","MSE2015_403","MSE2015_404")


opt_files <- c("MSE2014_320_Final","MSE2014_321_Final")
opt_files <- c("MSE2014_300_Final","MSE2014_301_Final","MSE2014_302_Final","MSE2014_303_Final","MSE2014_304_Final")
opt_files <- c("MSE2014_310_Final","MSE2014_311_Final","MSE2014_312_Final","MSE2014_313_Final","MSE2014_314_Final")
opt_files <- c("MSE2014_330_Final","MSE2014_331_Final","MSE2014_332_Final","MSE2014_333_Final","MSE2014_334_Final")
opt_files <- c("MSE2014_340_Final","MSE2014_341_Final","MSE2014_342_Final","MSE2014_343_Final","MSE2014_344_Final")
opt_files <- c("MSE2014_350_Final","MSE2014_351_Final","MSE2014_352_Final","MSE2014_353_Final","MSE2014_354_Final")
opt_files <- c("MSE2014_360_Final","MSE2014_361_Final","MSE2014_362_Final","MSE2014_363_Final","MSE2014_364_Final")
opt_files <- c("MSE2014_370_Final","MSE2014_371_Final","MSE2014_372_Final","MSE2014_373_Final","MSE2014_374_Final")
opt_files <- c("MSE2014_380_Final","MSE2014_381_Final","MSE2014_382_Final","MSE2014_383_Final","MSE2014_384_Final")
opt_files <- c("MSE2014_390_Final","MSE2014_391_Final","MSE2014_392_Final","MSE2014_393_Final","MSE2014_394_Final")
opt_files <- c("MSE2014_400_Final","MSE2014_401_Final","MSE2014_402_Final","MSE2014_403_Final","MSE2014_404_Final")

for (opt_file in opt_files){

  fFPRESS_run(runref = opt_file)

  lapply(list("st" = c(2014,2023), "mt" = c(2014,2033), "lt" = c(2034,2053),"lt1" = c(2054,2073),"lt2" = c(2074,2093)),
         function(x) fCalcMSE2014stats(ref = opt_file,
                                         fname = paste(".\\stats\\",opt_file,".dat",sep=""),
                                         period = c(x[1],x[2]),
                                         ssbref = 0.634577e6))
  
  lapply(seq(2014,2104),
         function(x) fCalcMSE2014stats(ref = opt_file,
                                         fname = paste(".\\stats\\",opt_file,".dat",sep=""),
                                         period = c(x,x),
                                         ssbref = 0.634577e6))
  
  fMSE2014ssbplot(opt_file, file = paste0(opt_file,"_SSB"), ext = "png", ymaxval = 6)
  fMSE2014yieldplot(opt_file, file = paste0(opt_file,"_Yld"), ext = "png")
  fMSE2014fbarplot(opt_file, file = paste0(opt_file,"_FBar"), ext = "png")
  fMSE2014TACplot(opt_file, file = paste0(opt_file,"_TAC"), ext = "png")
  fMSE2014riskplot(opt_file, file = paste0(opt_file,"_Risk"), ext = "png")

}


#23/04/2015
#runs exploring spike variability parameter (MSE2014_331_2 s=0.4 and MSE2014_331_2 with s=0.6)
#BE AWARE - this parameter is not read in, it is changed within the fFPRESS_run function
#in file run.r

opt_files <- c("MSE2014_331_1","MSE2014_331_2","MSE2014_331_3")

for (opt_file in opt_files){
  
  fFPRESS_run(runref = opt_file)
  
  lapply(list("st" = c(2014,2023), "mt" = c(2014,2033), "lt" = c(2034,2053),"lt1" = c(2054,2073),"lt2" = c(2074,2093)),
         function(x) fCalcMSE2014stats(ref = opt_file,
                                       fname = paste(".\\stats\\",opt_file,".dat",sep=""),
                                       period = c(x[1],x[2]),
                                       ssbref = 0.634577e6))
  
  lapply(seq(2014,2104),
         function(x) fCalcMSE2014stats(ref = opt_file,
                                       fname = paste(".\\stats\\",opt_file,".dat",sep=""),
                                       period = c(x,x),
                                       ssbref = 0.634577e6))
  
  fMSE2014ssbplot(opt_file, file = paste0(opt_file,"_SSB"), ext = "png", ymaxval = 6)
  fMSE2014yieldplot(opt_file, file = paste0(opt_file,"_Yld"), ext = "png")
  fMSE2014fbarplot(opt_file, file = paste0(opt_file,"_FBar"), ext = "png")
  fMSE2014TACplot(opt_file, file = paste0(opt_file,"_TAC"), ext = "png")
  fMSE2014riskplot(opt_file, file = paste0(opt_file,"_Risk"), ext = "png")
  
}

#create stats tables for report
lapply(opt_files,fgenLatex)

#create stats csv
lapply(opt_files,fgencsv)
