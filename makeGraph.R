devGraph <<- graph.empty(n = 916, directed = TRUE)

getId <- function(v)
{
  id <- node_id_names[which(node_id_names$email == v),]$node_id
  #print(id)
  if(length(id) == 0)
  {
    t <- as.numeric(tail(node_id_names, 1)[,1]) + 1
    id <- t
    devGraph <<- devGraph + vertex(id)
    df <- data.frame(t, v)
    names(df) <- c("node_id", "email")
    node_id_names <<- rbind(node_id_names, df)
  }
  return(id)
}

for(i in c(1:22424)){
  sourceNode <- getId(as.character(comment_top500issues[i,]$sender))
  print(i)
  rec <- strsplit(as.character(comment_top500issues$filtered_recipients[i]), split = ",")
  #print(rec)
  for (k in unlist(rec)) {
    #print(k)
    destNode <- getId(k)
    #print(destNode)
    if(sourceNode == destNode) next
    if(are_adjacent(devGraph, sourceNode, destNode) || are_adjacent(devGraph, destNode, sourceNode))
      next
    else
      devGraph <- devGraph + edge(sourceNode, destNode)
  }
}

copyGraph <- devGraph

write_graph(devGraph, "devCommentGraph500.net", format = "pajek")
write.csv(node_id_names, "NodeId-Email.csv", row.names = FALSE)
