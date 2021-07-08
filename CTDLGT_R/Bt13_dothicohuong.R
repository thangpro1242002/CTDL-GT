library(igraph)

ch = make_graph(c(1, 3, 6, 3, 3, 1, 5, 6, 4,1, 2,4, 4,5, 6,1), directed = TRUE)
plot(ch)