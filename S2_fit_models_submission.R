# We first set the working directory to the directory that has all the files for this case study
# We recommend that you create always the subdirectories "data" and "models" to the main directory
# If running the script in another computer, all that needs to be changed in the script below is the main directory
library(Hmsc)
library(tidyverse)
library(ape)

localDir = "."
model.directory = file.path(localDir, "models")

# We load the Hmsc package, and set the random seed, so that all results from this script are reproducable
set.seed(1)

# We read in the data, set the factors to factors, and have a look at it
da$sample.id <- paste(da$karta,da$yr,sep = ".")
da$karta = as.factor(da$karta)
da$sample.id = as.factor(da$sample.id)
head(da)

# We next pre-processing the data to make a number of simplifing choices.
# we selectenvironmental covariates 
XData = data.frame(evergreen=da$evergreen,
                   deciduous = da$deciduous,
                   mixed = da$mixed,
                   clearcut = da$clearcut,
                   wetland = da$wetland,
                   open_veg = da$open_veg,
                   water = da$water, 
                   sum_temp = da$temp_amjja,
                   ann_precip = da$tp_yr,
                   alt = da$mean_altit, 
                   vt = da$vardetrakt, # vt = AHCV
                   prop_surv = da$prop_surv)
# label each observation with the correct sample id
rownames(XData) = da$sample.id
# make sure the AHCV levels are properly implemented as factor
XData$vt <- factor(XData$vt, levels = c("Outside", "Within"))



# Third, select species abundance data replace NA with 0.
Y = da[,4:73]

Y = Y %>% mutate(across(1:70, replace_na, 0)) %>%
  as.matrix
# add the sample id as rownames so that x and y data can later be linked
rownames(Y) = da$sample.id


# define the spatial structure of the data
sp.str =  da[,c(1,119,120)] 
# filter out only the distinct locations
sp.str = distinct(sp.str)
# extract the x and y coordinates as a matrix
xy = as.matrix(cbind(sp.str$x,sp.str$y))
# as we cannot disclose the locations of the routes, we create random coordinates
xy[,1] = runif(length(xy[,1]), min = 0, max = 5000)
xy[,2] = runif(length(xy[,2]), min = 0, max = 5000)

# the rownames here are the route names
rownames(xy)=sp.str$karta
colnames(xy)=c("x-coordinate","y-coordinate")

# create the temporal structure
yr = as.matrix(cbind(unique(da$yr)))
rownames(yr)= unique(da$yr)
colnames(yr)=c("year")

# finally, make sure the phylogeny is there
phyloTree <-c_phylo

# Now we have:
# the community data matrix Y,
# the environmental data frame XData,
# the coordinates of the sampling locations xy,
# the unique surveying years,
# and the phylogenetic tree PhyloTree.
# It is always a good idea to eyeball the data. Let us first have a look at the community data.
dim(Y)

# There are data for 3880 sampling units and 70 species

head(Y[,c(1,25,50)])
summary(Y)

# We next explore the distribution of species richness S as the row sums, and the distribution species prevalence as column means

S = rowSums(Y>0,na.rm = T)
A = colSums(Y, na.rm = T)
range(S)
range(A)
par(mfrow=c(1,2))
hist(S, xlab = "Species richness (S)")
hist(A, xlab = "Species abundance (P)")


# Let us then look at the environmental data

head(XData)
# the following also show the types (factor, num) of variables:
str(XData)
# and a full summary
summary(XData)

# spatial data
head(xy)
par(mfrow=c(1,1))
plot(xy, asp=1) # show the map (NB., equal aspect ratio in the map)

# The matrix xy contains the the coordinates of the survey routes.
# The identities of the survey routes are given by row names

# For each of the 50 species, the data frame includes data on the body mass
# (which is log-transformed to make it suit better as a predictor),
# and their migratory strategy classified as long-distance migrant (L),
# short-distance migrant (S), or resident species (R).

phyloTree
plot(phyloTree,cex = 0.5)
str(phyloTree)


# We are now ready to define the HMSC model. 
# To define a spatial random effect at the level of the route, we need to include the route id:s in the studyDesign

studyDesign = data.frame(sample = da$sample.id, plot = da$karta, year = da$yr)
studyDesign$year = factor(studyDesign$year, 
                          levels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))

# We next define the random level object.
# NNGP spatial random level for the spatial structure
rL.spat = HmscRandomLevel(sData=xy, sMethod = "NNGP", nNeighbours = 10)
rL.spat = setPriors(rL.spat,nfMin=1,nfMax=1)
# Simple random level for the temporal structure
rL.temp = HmscRandomLevel(sData = yr)


# define the formula for the model
XFormula = ~ evergreen + deciduous + mixed + clearcut + open_veg + wetland + water + ann_precip + poly(sum_temp,degree = 2,raw = TRUE) + vt + alt + prop_surv

# split the Y data into presence absence and abundance conditional on presence
Ypa = 1*(Y>0)
Yabu = Y
Yabu[Yabu==0] = NA
# log-transform to approach normality
Yabu = log(Yabu)
summary(Yabu)

# We are now ready to define the model. Note that in ranLevels=list(plot=rL.spat, year=rL.temp), 
# "plot" and "year" refer to a column name
# of the studyDesign

# presence absence
m1 = Hmsc(Y=Ypa, XData = XData, XFormula=XFormula,
         phyloTree = phyloTree, 
         distr="probit", studyDesign=studyDesign,
         ranLevels=list(plot=rL.spat, year = rL.temp) )
# abundance cop
m2 = Hmsc(Y=Yabu, XData = XData, XFormula=XFormula,
          phyloTree = phyloTree, 
          distr="normal", studyDesign=studyDesign,
          ranLevels=list(plot=rL.spat, year = rL.temp))


# As always, it is a good idea to explore the model object

m1
m2

# This should give "Hmsc object with 3880 sampling units, 70 species, 14 covariates, 1 traits and 2 random levels"

head(m1$X)
# scaled variables will be used
head(m1$XScaled)

# combine the unfitted models into a list and save them
models <- list(m1,m2)
names(models) <- c("Pres_Abs", "Abun")
modelnames <- names(models)
save(models, file = "models/unfitted_models.Rdata")

# define the samples per chain, the thinning and the number of chains for the MCMC procedure.
samples_list = c(250)
thin_list = c(100)
nChains = 6
for(Lst in 1:length(samples_list)){
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
  nm = length(models)
  # fit the models
  for (model in 1:nm) {
    print(paste0("model = ",modelnames[model]))
    m = models[[model]]
    m = sampleMcmc(m, samples = samples, thin=thin,
                   adaptNf=rep(ceiling(0.4*samples*thin),m$nr), 
                   transient = ceiling(0.5*samples*thin),
                   nChains = nChains,
                   nParallel = nChains,
                   updater = list(GammaEta=F)) 
    models[[model]] = m
  }
  # save the models
  filename = paste("models/models_thin_", as.character(thin),
                   "_samples_", as.character(samples),
                   "_chains_",as.character(nChains),
                   ".Rdata",sep = "")
  save(models,modelnames,file=filename)
}
