# Q1
# This solution based on one assumption is that the ant always make progress even after it reaches the edges of the grid.

# For 31 by 23 grid, there are simply too many combinations to be able to generated. Probably don't need to have all combinations generated since only the maximal distance is needed.
# Change digit display
options(digits=10)

dist <- function(n, m){
  # total steps = n times north + m times east
  steps <- m+n
  # combination c(n, k)
  direc <- combn(seq(1:steps), n)
  # number of total paths
  npath <- choose(steps,n)
  # create movement matrix for each path (north=1, east=0)
  path <- matrix(0, npath, steps)
  for(i in 1:npath) {
    path[i,direc[,i]] <- 1
  }
  # Cartesian coordinate after each move 
  xpath <- ypath <- matrix(0, npath, (steps+1))
  for(i in 2:(steps+1)){
    xpath[,i] <- xpath[,i-1] + as.integer(path[,i-1]==0)
    ypath[,i] <- ypath[,i-1] + path[,i-1]
  }
  # assign column names
  xpath <- as.data.frame(xpath)
  ypath <- as.data.frame(ypath)
  colnames(xpath) <- paste0("x", 0:steps)
  colnames(ypath) <- paste0("y", 0:steps)
  # calculate maximal distance
  dp_dist <- abs(xpath/m - ypath/n)
  dp_dist$max <- apply(dp_dist, 1, max)
  return(dp_dist$max)
}

d1 <- dist(7,11)
mean(d1)
sqrt(sum((d1 - mean(d1))^2)/(length(d1)))

sum(d1 > 0.6)/sum(d1 > 0.2)
