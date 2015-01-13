#alternative annual cycle

rm(list=ls())
gc()

source(".Rprofile")
sourceDir()

#opt_file <- "MSE2014_SRSAD99_1scor_0spikes_1sigR"       #no spikes
opt_file <- "MSE2014_SRSAD99_1scor_1spikes_1sigR"       #spikes

fFPRESS_run(runref = opt_file, varmin = 0.0, varmax = 0.2, resolution = 40)

fCalc_MSE2014_stats(ref = opt_file, fname = paste(".\\stats\\",opt_file,".dat",sep=""),
               period = c(2151,2200), ssbref = 1.24e6)

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

for (opt_file in opt_files){

  #fFPRESS_run(runref = opt_file)

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





#f.MSE2014Stats(ref = opt_file,
#               fname = paste(".\\stats\\",opt.file,".dat",sep=""),
#               period = c(2151,2200),
#               ssbref = 634557)
# 
# 
# f.MSE2014Stats(ref = opt_file,
#                fname = paste(".\\stats\\",opt.file,"ann.dat",sep=""),
#                period = c(2151,2200),
#                ssbref = 634557)
# 
# f.PlotFYld(ref = opt_file,
#            sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
#            pfile = paste(".\\plots\\FYld_",opt.file,".pdf",sep=""),
#            title = "MSE2014 - Yield & Risk vs FBar",
#            subtitle = "Serial Correlation, No Spike, 100% SigR",
#            ylim = c(0,130),
#            xlim = c(0,0.2))
# 
# f.PlotFSSB(ref = opt_file,
#            sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
#            pfile = paste(".\\plots\\FSSB_",opt.file,".pdf",sep=""),
#            title = "MSE2014 - SSB & Risk vs FBar",
#            subtitle = "Serial Correlation, No Spike, 100% SigR",
#            ylim = c(0,3.4),
#            xlim = c(0,0.2))
# 
# 
# opt_file <- "MSE2014_SRSAD99_1scor_1spikes_1sigR"
# 
# f.FPRESS.Run(runref = opt_file,
#              iterations = 100,
#              varmin = 0.0,
#              varmax = 0.2,
#              resolution = 40)
# 
# 
# 
# f.MSE2014Stats(ref = opt_file,
#                fname = paste(".\\stats\\",opt.file,".dat",sep=""),
#                period = c(2151,2200),
#                ssbref = 634557)
# 
# f.PlotFYld(ref = opt_file,
#            sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
#            pfile = paste(".\\plots\\FYld_",opt.file,".pdf",sep=""),
#            title = "MSE2014 - Yield & Risk vs FBar",
#            subtitle = "Serial Correlation, No Spike, 100% SigR",
#            ylim = c(0,130),
#            xlim = c(0,0.2))
# 
# f.PlotFSSB(ref = opt_file,
#            sfile = paste(".\\stats\\",opt.file,".dat",sep=""),
#            pfile = paste(".\\plots\\FSSB_",opt.file,".pdf",sep=""),
#            title = "MSE2014 - SSB & Risk vs FBar",
#            subtitle = "Serial Correlation, No Spike, 100% SigR",
#            ylim = c(0,3.4),
#            xlim = c(0,0.2))
