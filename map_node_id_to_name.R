i <- 0
n <- length(comment_top500issues[,1])
name <- character(0)
id <- numeric(0)
'%ni%' <- Negate('%in%')
for(k in c(1:n))
{
  t <- as.character(comment_top500issues[k,]$sender)
  if(t %ni% name)
  {
    i <- i+1
    name <- append(name, t)
    id <- append(id, i)
  }
}

node_id_names <- data.frame("node_id" = id, "email" = name)
node_id_names <- cbind(node_id_names, id, name)
