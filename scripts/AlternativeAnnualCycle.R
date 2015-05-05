#alternative annual cycle

rm(list=ls())
gc()

source(".Rprofile")
fsourceDir()


#50 years stats for various numbers of iterations
lapply(list("100" = sample.int(n=1000, size=100, replace=FALSE),
            "250" = sample.int(n=1000, size=250, replace=FALSE),
            "500" = sample.int(n=1000, size=500, replace=FALSE),
            "1000" = c(1:1000)),
       function(x) fCalc_MSE2014_stats(ref = opt_file,
                                       fname = paste(".\\stats\\",opt_file,"_",length(x),".dat",sep=""),
                                       period = c(2151,2200),
                                       nits = x,
                                       ssbref = 1.24e6))

#annual stats
lapply(list("100" = sample.int(n=1000, size=100, replace=FALSE),
            "250" = sample.int(n=1000, size=250, replace=FALSE),
            "500" = sample.int(n=1000, size=500, replace=FALSE),
            "1000" = c(1:1000)), function(y) {
  lapply(seq(2014,2025), function(x) fCalc_MSE2014_stats(ref = opt_file,
                                                         fname = paste(".\\stats\\",opt_file,"_",length(y),"_ann.dat",sep=""),
                                                         nits = y,
                                                         period = c(x,x),
                                                         ssbref = 1.24e6))})



opt_files <- c("MSE2014_170","MSE2014_171","MSE2014_173","MSE2014_174","MSE2014_176","MSE2014_178","MSE2014_180",
               "MSE2014_181","MSE2014_183","MSE2014_184","MSE2014_176","MSE2014_186","MSE2014_188","MSE2014_190",
               "MSE2014_191","MSE2014_192","MSE2014_195","MSE2014_200","MSE2014_210","MSE2014_211","MSE2014_212",
               "MSE2014_213","MSE2014_214","MSE2014_215","MSE2014_216","MSE2014_217")

opt_files <- c("MSE2014_300","MSE2014_301","MSE2014_302","MSE2014_303","MSE2014_304",
               "MSE2014_310","MSE2014_311","MSE2014_312","MSE2014_313","MSE2014_314",
               "MSE2014_320","MSE2014_321")

#including spikes
opt_files <- c("MSE2014_310","MSE2014_311","MSE2014_312","MSE2014_313","MSE2014_314",
               "MSE2014_330","MSE2014_331","MSE2014_332","MSE2014_333","MSE2014_334",
               "MSE2014_340","MSE2014_341","MSE2014_342","MSE2014_343","MSE2014_344",
               "MSE2014_350","MSE2014_351","MSE2014_352","MSE2014_353","MSE2014_354",
               "MSE2014_360","MSE2014_361","MSE2014_362","MSE2014_363","MSE2014_364",
               "MSE2014_370","MSE2014_371","MSE2014_372","MSE2014_373","MSE2014_374",
               "MSE2014_380","MSE2014_381","MSE2014_382","MSE2014_383","MSE2014_384",
               "MSE2014_390","MSE2014_391","MSE2014_392","MSE2014_393","MSE2014_394",
               "MSE2014_400","MSE2014_401","MSE2014_402","MSE2014_403","MSE2014_404")

#all
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

#10/03/2015
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
               "MSE2015_400","MSE2015_401","MSE2015_402","MSE2015_403","MSE2015_404");


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




#long term runs
#options files for long term (MSY) simulations
loptfiles <- c("MSE2014_SRSAD11_1scor_0spikes_1sigR","MSE2015_SRSAD11_1scor_0spikes_1sigR",
                "MSE2014_SRSAD11_1scor_1spikes_1sigR","MSE2015_SRSAD11_1scor_1spikes_1sigR")

#do the runs
lapply(loptfiles,
       function(x) fFPRESS_run(runref = x,
                               iterations = 1000,
                               varmin = 0.0,
                               varmax = 0.2,
                               resolution = 100))

#calculate the statistics
lapply(loptfiles,
       function(x) fCalcMSE2014stats(ref = x,
                                     fname = paste(".\\stats\\",x,".dat",sep=""),
                                     period = c(2151,2200),
                                     ssbref = {if(grepl("2014",x)){0.453269e6}else{0.600700e6}}))

#generate yield plot
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

#50 years stats for various numbers of iterations
lapply(loptfiles,
       function(fopt) {
         lapply(list("100" = sample.int(n=1000, size=100, replace=FALSE),
            "250" = sample.int(n=1000, size=250, replace=FALSE),
            "500" = sample.int(n=1000, size=500, replace=FALSE),
            "1000" = c(1:1000)),
       function(x) fCalcMSE2014stats(ref = fopt,
                                     fname = paste(".\\stats\\",fopt,"_",length(x),".dat",sep=""),
                                     period = c(2151,2200),
                                     nits = x,
                                     ssbref = {if(grepl("2014",fopt)){0.453269e6}else{0.600700e6}}))})
