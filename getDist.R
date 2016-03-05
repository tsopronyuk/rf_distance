library("Rcpp")
getDist <- function (x) {
  
  myv <- is.na(x$contPT)
  mynv <- which(myv)
  nodeID <- x$nodeID[mynv]
  
  g <- c(0, 0)
  myil <- list()
  for (i in 2:length(myv)) {
    if (myv[i - 1]) {
      myil <- append(myil, list(g))
      g <- c(g[length(g)], g[2:(length(g) - 1)])
    } else {
      g <- c(g[1] + 1, g[2:length(g)], g[1] + 1)
    }
  }
  myil <- append(myil, list(g))
  
  
  # get pairwise distance; save nodeID as RF return in prediction nodeID
  # length: (length(mynv)^2 - length(mynv)) / 2; 
  l <- (length(mynv)^2 - length(mynv)) / 2
  myDist <- data.frame(x=rep(0L, l), y=rep(0L, l), dist=rep(0, l))
  k <- 1
  for (i in 1:(length(mynv) - 1)) {
    for (j in (i + 1):length(mynv)) {
      id <- which.min(myil[[i]][2:length(myil[[i]])] %in% myil[[j]][2:length(myil[[j]])])
      myDist$x[k] <- nodeID[i]
      myDist$y[k] <- nodeID[j]
      myDist$dist[k] <- myil[[i]][1] + myil[[j]][1] - 2 * myil[[i]][2:length(myil[[i]])][id] + 2    
      k <- k + 1
    }
  }
  return(myDist)
}

# Example Usage ----
library(randomForestSRC)
library(data.table)
library(dplyr)
data(veteran, package = "randomForestSRC")

# source cpp code
sourceCpp("getDistCPP.cpp")
nTree <- 100

# fit random forrest
rfobj <- rfsrc(Surv(time, status) ~ age, 
               data       = veteran, 
               ntree      = nTree, 
               membership = T,
               proximity  = T, 
               forest     = T)
rfobj -> v.obj

# get the number of knotes for each end node and for each tree and save it in
# a data.table; 
# Tetyana: you may replace following loop by your C++-Code.
treeAge <- v.obj$forest$nativeArray[v.obj$forest$nativeArray$treeID == 1, ]
DistTreeCPP <- getDistCPP(treeAge)
names(DistTreeCPP)[3] <- "tree_1"
system.time(
for (i in 2:nTree) {
  treeAge <- v.obj$forest$nativeArray[v.obj$forest$nativeArray$treeID == i, ]
  tmp <- getDistCPP(treeAge)
  names(tmp)[3] <- paste0("tree_", i)
  DistTreeCPP <- full_join(DistTreeCPP, tmp, by=c("x", "y"))
})
DistTreeCPP <- as.data.table(DistTreeCPP)
setkeyv(DistTreeCPP, cols = c("x", "y"))

# Description for Tetyana: This is the most critical performance part of the whole 
# calculations:
# predict; in this case, we also can use v.obj$membership
prf <- predict(rfobj)
# the membership matrix (n x nTree) with n = cases contains the end node 
# membership of each case. Example output:
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] ...
# [1,]   25   19   22   24   24   25   24   26   26    26    27    29    24   
# [2,]   19   22    4   19   19   15   19   21   19    17    22    23    20  
# [3,]   10    2    8    6    5    2    2    7    4     6    10     7     7    
# 
# Let's have a look at tree 1. Case 1 is in end node 25 and case 2 in end node 
# 19 the number of knotes between this nodes can be extracted from DistTreeCPP 
# by DistTreeCPP[J(19, 25), 3, with=F]. The smaller number has to be always 
# in the first field of J(., .) as DistTreeCPP is ordered by x then by y and the
# (x, y) pairs are unique. Say you get there the value 6. On this value apply 
# 1 / exp(w * g_ijt) = 1 / exp(w * 6), where w is a parameter that can be
# set as parameter (default: w = 2). Now, we have two possibilities
# 1. loop over trees or loop over cases. As this algorithm should be applied 
# on large data sets and n >> nTree, I would suggest to parallelize over cases 
# with (n^2 - n) / 2 final fields or iterations). You may have to research a good 
# matrix representation in the matrix package. 

prf$membership







# compare to R-Code
system.time(
  for (i in 1:100) {
    treeAge <- v.obj $forest$nativeArray[v.obj$forest$nativeArray$treeID == i, ]
    DistTreeCPP<-getDist(treeAge)
})

DistTree <- getDist(treeAge)
identical(DistTree,DistTreeCPP)

