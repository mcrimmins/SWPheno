

library(pheno)
data(Simple)
connectedSets(Simple)

data(DWD)

data(DWD)
M <- raw2matrix(DWD) # conversion to matrix
D1 <- matrix2raw(M) # back conversion, but with different level names
D2 <- matrix2raw(M,c(1951:1998),c(1:9)) # with original level names

data(DWD)
R <- pheno.flm.fit(DWD) # parameter estimation


data(Searle)
getConnectedSets(Searle)

data("Size")
data("SizeClimate")