#setwd("") # set directory to the folder where the folders "data", "models" and "panels" are
library(Hmsc)
library(colorspace)
library(corrplot)
library(writexl)

nChains = 6
samples = 250
thin = 100

filename = paste("models/models_thin_", as.character(thin),
                 "_samples_", as.character(samples),
                 "_chains_",as.character(nChains),
                 ".Rdata",sep = "")
load(filename)
nm = length(models)

file2 = paste("models/MF_thin_", as.character(thin),
              "_samples_", as.character(samples),
              "_chains_",as.character(nChains),
              ".Rdata",sep = "")
load(file2)


for(j in 1:nm){
  m = models[[j]]
  # the variables are grouped in the variance partitioning
  VP = computeVariancePartitioning(m,group = c(rep(1,8), 2,3,3,4,5,6),
                                   groupnames = c("landcover",
                                                  "precipitation", 
                                                  "temperature", 
                                                  "VFA",
                                                  "altitude",
                                                  "prop_surv"))
  vals = VP$vals
  preds = computePredictedValues(m)
  MF_2 = MF[[j]]
  MFCV_2 = MFCV[[j]]
  
  R2 = NULL
  if(!is.null(MF_2$TjurR2)){
    TjurR2 = MF_2$TjurR2
    TjurR2_pred = MFCV_2$TjurR2
    vals = rbind(vals,TjurR2, TjurR2_pred)
    R2=TjurR2
  }
  if(!is.null(MF_2$R2)){
    R2=MF_2$R2
    R2_pred = MFCV_2$R2
    vals = rbind(vals,R2, R2_pred)
  }
  
  filename =  paste0("parameter_estimates_VP_",modelnames[[j]],".csv")
  write.csv(vals,file=filename)
  }
