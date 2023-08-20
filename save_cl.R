library(igraph)

dir = "/Users/Mattia/Desktop/YouTube/"
setwd(dir)

load("temp_comments.RData")
load("temp_videos_stats.RData")


com_in_sample = comments

edges = data.frame("from" = com_in_sample$video_id, "to" = com_in_sample$user_name, stringsAsFactors = F )

g <- graph.data.frame(edges, directed = F)
V(g)$type <- V(g)$name %in% edges[,2]
g_comm <- g
g = g_comm
bipartite_matrix <- as_incidence_matrix(g_comm)
video_matrix <- tcrossprod(bipartite_matrix)
diag(video_matrix) <- 0
g_video_c = graph_from_adjacency_matrix(video_matrix, 
                                        mode = "undirected", 
                                        weighted = TRUE)
g_video_c = simplify(g_video_c)


#fastgreedy
cl_fg = cluster_fast_greedy(g_video_c)
save(cl_fg, file="cl_fastgreedy.RData")

#louvain
c_louvain = cluster_louvain(g_video_c)
save(c_louvain, file="cl_louvain.RData")


#leadign_eigen
cl_le = cluster_leading_eigen(g_video_c)
save(cl_le, file="cl_leadingeigen.RData")