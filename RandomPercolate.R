
hitCounts <- rep(0, 916)

percolate <- function(i)
{
  
  hitCounts[i] <<- hitCounts[i] + 1
  juice_i <<- juice_i - 0.1*juice_i
  if(juice_i > 0.01)
  {
    outAdjList <- unlist(adjacent_vertices(inputGraph, i, mode = "out"))
    d <- length(outAdjList)  # try with variable size ie randomly choose size
    indices <- sample(c(1:d), size = floor(sqrt(d)), replace = FALSE)
    toBeTraversed <- outAdjList[indices]
    invisible(sapply(toBeTraversed, percolate))
    
  }
}

### optimize the following part with lapply or sapply ###

seedNodes <- sample(c(1:13633), size = 10000, replace = TRUE)
              
for (i in c(1:10000)) {
  juice_i <- 1
  percolate(seedNodes[i])
}

srcnd <- numeric(0)
destnd <- numeric(0)
srcHC <- rep(0, 916)
destHC <- rep(0, 916)

for(i in c(1:13633))
{
  s <- ends(inputGraph, i, names = FALSE)[1]
  d <- ends(inputGraph, i, names = FALSE)[2]
  srcnd <- append(srcnd, s)
  destnd <- append(destnd, d)
}

df <- data.frame("node_id" = c(1:13633), "hit_counts" = hitCounts,
                 "source" = srcnd, "dest" = destnd)

##  remove nodes for which there are NA values in M2Metrics ##

M2nodes <- read.csv("M2MetricsCompleteCases.csv")
t <- M2nodes$node_id
df <- df[t,]

write.csv(df, "M2PercolationHitCounts.csv", row.names = FALSE)


##  count source node hit counts from percolation on M2 network  ##

for(i in c(1:7365))
{
  j <- df[i,]$source
  k <- df[i,]$dest
  srcHC[j] <- srcHC[j] + df[i,]$hit_counts
  destHC[k] <- destHC[k] + df[i,]$hit_counts
}



seedNodes <- sample(c(1:916), size = 10000, replace = TRUE)

for (i in c(1:10000)) {
  juice_i <- 1
  percolate(seedNodes[i])
}

df2 <- data.frame("node_id" = c(1:916), "hit_counts" = hitCounts)
write.csv(df2, "OriginalNW-PercolationHC.csv", row.names = FALSE)
