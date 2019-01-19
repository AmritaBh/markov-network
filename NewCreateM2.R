input_graph <- read_graph("TS50_clean.net", format = "pajek")

m2graph <- graph.empty(n = 95951, directed = TRUE)

for(i in V(input_graph))
{
  cat(i, file = "log.txt", append = TRUE, sep = "\n")
  inAdjList <- adjacent_vertices(input_graph, i, mode = "in")
  outAdjList <- adjacent_vertices(input_graph, i, mode = "out")
  for(j in unlist(inAdjList))
  {
    for(k in unlist(outAdjList))
    {
      sourceNode <- get.edge.ids(input_graph, c(j, i), directed = TRUE)
      destNode <- get.edge.ids(input_graph, c(i, k), directed = TRUE)
      m2graph <- m2graph + edge(sourceNode, destNode)
    }
  }
}

write_graph(m2graph, "M2Graph.net", format = "pajek")