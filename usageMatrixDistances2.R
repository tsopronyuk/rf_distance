library("Rcpp")
library(data.table)
# Example Usage ----

# treeAge from file for tests
treeAgeT <- read.csv("treeAge.csv")
treeAgeT <-treeAgeT [-1]

# membership from file for tests
membership <- read.csv("membership.csv")
membership <-membership [-1]

# source cpp code within parallel calculation and without parallel calculation  
sourceCpp("parallelMatrixAndOther.cpp")

t<-Sys.time()
parallelMatrixDistance<-getParallelMatixDistances(treeAgeT, membership, 0)
Sys.time()-t

# create csv files for parallelMatrixDistance
write.csv(parallelMatrixDistance, file="parallelMatrixDistanceW0.csv")

t<-Sys.time()
matrixDistance<-getMatixDistances(treeAgeT,  membership, 0)
Sys.time()-t

# create csv files for matrixDistance
write.csv(matrixDistance, file="matrixDistanceW0.csv")

#compare 2 results
all(abs(parallelMatrixDistance - matrixDistance) < 0.00000000000001)

# new getDist() for first tree 
t<-Sys.time()
treeAge <- treeAgeT[treeAgeT $treeID== 1, ]
DistTreeOne <- getDistForTreesCPP(treeAge,1)
Sys.time()-t

head(DistTreeOne,3)

# getDist() for the 1-5 trees 
t<-Sys.time()
DistTreeAll<-getDistForTreesCPP(treeAgeT, 5)
Sys.time()-t

head(DistTreeAll,3)


