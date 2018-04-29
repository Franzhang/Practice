# ants and traffic
# Change digit display

options(digits=10)

n <- 7
m <- 11



npoint <- rep(0:(m+n+1), each=2)
xys <- rep(c("x","y"), (m+n+1))
xycoord <- paste0(xys, npoint)
# n times north + m times east. combination c(n, k)
npath <- choose(18,11)
combn(seq(1:18), 7)


# test: start small
options(digits=10)
n <- 2
m <- 3
direc <- combn(seq(1:5), 2)
npath <- choose((m+n),m)
path <- matrix(0, npath, (m+n))
for(i in 1:npath) {
  path[i,direc[,i]] <- 1
}

xpath <- ypath <- matrix(0, npath, (m+n+1))

for(i in 2:6){
xpath[,i] <- xpath[,i-1] + as.integer(path[,i-1]==0)
ypath[,i] <- ypath[,i-1] + path[,i-1]
}

xpath <- as.data.frame(xpath)
ypath <- as.data.frame(ypath)
colnames(xpath) <- paste0("x", 0:(m+n))
colnames(ypath) <- paste0("y", 0:(m+n))
dp_dist <- abs(xpath/m - ypath/n)
dp_dist$max <- apply(dp_dist, 1, max)
