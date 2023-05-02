# load packages
library(tidyverse)
library(Hmsc)
library(ape)

# load data
load("hmscdata_publication.Rdata")
#load("C:/Users/Tristan.Bakx/OneDrive - Lund University/PhD/Projects/Biodiversity/Biodiv_R/HMSC output/Solon_Hurdle_ForSplit_6C_170S_100Th/hmscdata.Rdata")
# observations and environmental data
da <- hmscdatalist[[1]]
# phylogeny
c_phylo <- hmscdatalist[[2]]


# The data gets split into
# S: study design, including units of study and their possible coordinates (named as Route_x and Route_y to indicate that they relate to the units of Route)
# X: covariates to be used as predictors
# Y: species data
S=da[,c(1,3,119,120)]
colnames(S) <- c("route", "year", "route_x", "route_y")
X=da[,c(74:118)]
Y=da[,4:73]


# Check that the data looks as it should!
View(S)
View(X)
View(Y)

# check that community data are numeric and have finite numbers. If the script
# writes "Y looks OK", you are ok.
if (is.numeric(as.matrix(Y)) || is.logical(as.matrix(Y)) && is.finite(sum(Y, na.rm=TRUE))) {
    print("Y looks OK")
} else {
	print("Y should be numeric and have finite values")	}
# Check that the stydy design data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(S))) {
  print("S has NA values - not allowed for")
} else {
  print("S looks ok")	}
# Check that the covariate data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(X))) {
  print("X has NA values - not allowed for")
} else {
  print("X looks ok")	}


# have a look if the phylogeny is correct
if(all(sort(c_phylo$tip.label) == sort(colnames(Y)))){
  print("species names in P and SXY match")
} else{
  print("species names in P and SXY do not match")
}
# Check that the data looks as it should!
plot(c_phylo, cex=0.5)


