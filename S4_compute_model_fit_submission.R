#setwd("") # set directory to the folder where the folders "data", "models" and "panels" are
library(Hmsc)

#You may wish to loop over samples_list and thinning_list
nChains = 6
thin_list = c(100)
samples_list = c(250)
nst = length(thin_list)

MF = list()
MFCV = list()
WAIC = list()

for (Lst in 1:nst) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
  filename = paste("models/models_thin_", as.character(thin),
                   "_samples_", as.character(samples),
                   "_chains_",as.character(nChains),".Rdata",sep = "")
  load(filename)
  nm = length(models)
  for(model in 1:nm){
    print(paste0("model = ",as.character(model)))
    m = models[[model]]
    # model prediction
    preds = computePredictedValues(m,updater = list(GammaEta=F))
    MF[[model]] = evaluateModelFit(hM=m, predY=preds)
    # cross validation
    partition = createPartition(m, nfolds = 2, column = "plot")
    preds_cv = computePredictedValues(m,partition=partition, nParallel = nChains,updater = list(GammaEta=F)) #nParallel = nChains
    MFCV[[model]] = evaluateModelFit(hM=m, predY=preds_cv)
    WAIC[[model]] = computeWAIC(m)
  }
  filename_out = paste("models/MF_thin_", as.character(thin),
                       "_samples_", as.character(samples),
                       "_chains_",as.character(nChains),
                       ".Rdata",sep = "")
  save(MF,MFCV,WAIC,modelnames,file = filename_out)
  
}

