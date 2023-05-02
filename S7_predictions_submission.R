library(Hmsc)
library(tidyverse)
library(ggplot2)
# this script makes the marginal predictions for the interpretation of the AHCV results.
# Load the model
nChains = 6
samples = 250
thin = 100
filename = paste("models/models_thin_", as.character(thin),
                 "_samples_", as.character(samples),
                 "_chains_",as.character(nChains),
                 ".Rdata",sep = "")
load(filename)
nm = length(models)

# Presence absence model
j = 1
m = models[[j]]
covariates = all.vars(m$XFormula)
ex.sp = which.max(colMeans(m$Y,na.rm = TRUE))

# Select the right covariate
k = 10

covariate = covariates[[k]]
# Construct the gradients, marginal and total effect
Gradient_marg = constructGradient(m,focalVariable = covariate,non.focalVariables = 1)
Gradient_tot = constructGradient(m,focalVariable = covariate)

# predict the marginal and total effect
predY_marg = predict(m, Gradient=Gradient_marg, expected = TRUE)  
predY_tot = predict(m, Gradient=Gradient_tot, expected = TRUE)  

# Reformat the data
predY_marg = predY_marg %>% lapply(function(x){
  data.frame(vt = row.names(x), x) %>%
    pivot_longer(cols = 2:71, names_to = "species", values_to = "pred") %>% 
    pivot_wider(names_from = vt, values_from = pred)
})
predY_marg = do.call(rbind.data.frame, predY_marg)
predY_marg$pred_dif = predY_marg$`2` - predY_marg$`1`

predY_tot = predY_tot %>% lapply(function(x){
  data.frame(vt = row.names(x), x) %>%
    pivot_longer(cols = 2:71, names_to = "species", values_to = "pred") %>% 
    pivot_wider(names_from = vt, values_from = pred)
})
predY_tot = do.call(rbind.data.frame, predY_tot)
predY_tot$pred_dif = predY_tot$`2` - predY_tot$`1`

save(predY_tot,predY_marg, file = "predictedAHCV_PA.RData")

# Abundance COP model
j = 2
m = models[[j]]
covariates = all.vars(m$XFormula)
ex.sp = which.max(colMeans(m$Y,na.rm = TRUE))

# Select the right covariate
k = 10

covariate = covariates[[k]]
# Construct the gradients, marginal and total effect
Gradient_marg = constructGradient(m,focalVariable = covariate,non.focalVariables = 1)
Gradient_tot = constructGradient(m,focalVariable = covariate)

# predict the marginal and total effect
predY_marg = predict(m, Gradient=Gradient_marg, expected = TRUE)  
predY_tot = predict(m, Gradient=Gradient_tot, expected = TRUE)  

# Reformat the data
predY_marg = predY_marg %>% lapply(function(x){
  data.frame(vt = row.names(x), x) %>%
    pivot_longer(cols = 2:71, names_to = "species", values_to = "pred") %>% 
    pivot_wider(names_from = vt, values_from = pred)
})
predY_marg = do.call(rbind.data.frame, predY_marg)
predY_marg$pred_dif = predY_marg$`2` - predY_marg$`1`

predY_tot = predY_tot %>% lapply(function(x){
  data.frame(vt = row.names(x), x) %>%
    pivot_longer(cols = 2:71, names_to = "species", values_to = "pred") %>% 
    pivot_wider(names_from = vt, values_from = pred)
})
predY_tot = do.call(rbind.data.frame, predY_tot)
predY_tot$pred_dif = predY_tot$`2` - predY_tot$`1`

save(predY_tot,predY_marg, file = "predictedAHCV_Abu.RData")

