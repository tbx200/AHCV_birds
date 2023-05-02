#setwd("") # set directory to the folder where the folders "data", "models" and "panels" are
library(Hmsc)
library(colorspace)
library(vioplot)

#include in samples_list and thin_list only those models that you have actually fitted!
samples_list = c(250)
thin_list = c(100)
nChains = 6
nst = length(thin_list)


ma = NULL
ess = NULL
na = NULL
nae=NULL
ma.o = NULL
na.o = NULL
nae.o = NULL
ess.o = NULL
ma.g = NULL
na.g = NULL
nae.g=NULL
ess.g = NULL
ma.r = NULL
na.r = NULL
nae.r = NULL
ess.r = NULL
for (Lst in 1:nst) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
  
  filename = paste("models/models_thin_", as.character(thin),
                   "_samples_", as.character(samples),
                   "_chains_",as.character(nChains),".Rdata",sep = "")
  load(filename)
  nm = length(models)
  for(j in 2:2){
    print(paste0("model = ",modelnames[j]))
    mpost = convertToCodaObject(models[[j]], spNamesNumbers = c(T,F), covNamesNumbers = c(T,F))
    psrf.beta = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
    ess.beta = effectiveSize(mpost$Beta)
    sppairs = matrix(sample(x = 1:70^2, size = 100))
    tmp.ome = mpost$Omega[[1]]
    for (chain in 1:length(tmp.ome)){
      tmp.ome[[chain]] = tmp.ome[[chain]][,sppairs]
      }
    psrf.omega = gelman.diag(tmp.ome,multivariate=FALSE)$psrf
    ess.omega = effectiveSize(tmp.ome)
    psrf.gamma = gelman.diag(mpost$Gamma,multivariate=FALSE)$psrf
    ess.gamma = effectiveSize(mpost$Gamma)
    print(effectiveSize(mpost$Rho))
    print(gelman.diag(mpost$Rho)$psrf)
    tmp = summary(psrf.beta)
    tmp.o = summary(psrf.omega)
    tmp.g = summary(psrf.gamma)
    if(is.null(ma)){
      ma=psrf.beta[,1]
      na = paste0(as.character(thin),",",as.character(samples))
    } else {
      ma = cbind(ma,psrf.beta[,1])
      if(j==1){
        na = c(na,paste0(as.character(thin),",",as.character(samples)))
      } else {
        na = c(na,"")
      }
    }
    if(is.null(ma.o)){
      ma.o=psrf.omega[,1]
      na.o = paste0(as.character(thin),",",as.character(samples))
    } else {
      ma.o = cbind(ma.o,psrf.omega[,1])
      if(j==1){
        na.o = c(na.o,paste0(as.character(thin),",",as.character(samples)))
      } else {
        na.o = c(na.o,"")
      }
    }
    if(is.null(ma.g)){
      ma.g=psrf.gamma[,1]
      na.g = paste0(as.character(thin),",",as.character(samples))
    } else {
      ma.g = cbind(ma.g,psrf.gamma[,1])
      if(j==1){
        na.g = c(na.g,paste0(as.character(thin),",",as.character(samples)))
      } else {
        na.g = c(na.g,"")
      }
    }
    if(is.null(ess)){
      ess=ess.beta
      nae = paste0(as.character(thin),",",as.character(samples))
    } else {
      ess = cbind(ess,ess.beta)
      if(j==1){
        nae = c(nae,paste0(as.character(thin),",",as.character(samples)))
      } else {
        nae = c(nae,"")
      }
    }
    if(is.null(ess.g)){
      ess.g=ess.gamma
      nae.g = paste0(as.character(thin),",",as.character(samples))
    } else {
      ess.g = cbind(ess.g,ess.gamma)
      if(j==1){
        nae.g = c(nae.g,paste0(as.character(thin),",",as.character(samples)))
      } else {
        nae.g = c(nae.g,"")
      }
    }
    if(is.null(ess.o)){
      ess.o=ess.omega
      nae.o = paste0(as.character(thin),",",as.character(samples))
    } else {
      ess.o = cbind(ess.o,ess.omega)
      if(j==1){
        nae.o = c(nae.o,paste0(as.character(thin),",",as.character(samples)))
      } else {
        nae.o = c(nae.o,"")
      }
    
    }
  }
}



# write the convergence values and graphs to pdf
pdf(file=paste("panels/MCMC_convergence.pdf"))
par(mfrow=c(2,1))
vioplot(ma,col=rainbow_hcl(nm),names=na,ylim=c(0,max(ma)),main="psrf(beta)")
vioplot(ma,col=rainbow_hcl(nm),names=na,ylim=c(0.9,1.1),main="psrf(beta)")
vioplot(ma.o,col=rainbow_hcl(nm),names=na.o,ylim=c(0,max(ma.o)),main="psrf(omega)")
vioplot(ma.o,col=rainbow_hcl(nm),names=na.o,ylim=c(0.9,1.1),main="psrf(omega)")
vioplot(ma.g,col=rainbow_hcl(nm),names=na.g,ylim=c(0,max(ma.g)),main="psrf(gamma)")
vioplot(ma.g,col=rainbow_hcl(nm),names=na.g,ylim=c(0.9,1.1),main="psrf(gamma)")
vioplot(ess, col=rainbow_hcl(nm),names=nae ,main="Effective sample size(beta)")
vioplot(ess.g, col=rainbow_hcl(nm),names=nae.g ,main="Effective sample size(gamma)")
vioplot(ess.o, col=rainbow_hcl(nm),names=nae.o ,main="Effective sample size(omega)")
dev.off()

