library(varhandle)
rm.all.but(c("comments", "dir", "videos"))


library(igraph)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(sqldf)
library(R0)
library(tibble)
library(varhandle)
dir = "/Users/MeijiTenno/Desktop/YouTube/"
setwd(dir)
load("temp_comments.RData")
load("temp_videos_stats.RData")

sample_videos_id = sample(videos$video_id, 2000, replace = F)
sample_videos= videos[videos$video_id %in% sample_videos_id,]
#prendo i comments che fanno parte del sample
com_in_sample = comments[comments$video_id %in% sample_videos_id,]


save(com_in_sample, file = "com_in_sample.RData")
load("com_in_sample.RData")


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

#louvain
c_louvain = cluster_louvain(g_video_c)

#compare di fastgreedy e louvain
compare(cl_fg, c_louvain, "adjusted.rand")

#prendo il cluster più grande
clfg_sizes <- sizes(cl_fg)
clouv_sizes <- sizes(c_louvain)


vec_list <- list()
#for (i in 1:4) {
  #mi prendo dal cluster fast greedy il più grande disponibile
  biggest_fg_index <- which(clfg_sizes==max(clfg_sizes))
  biggest_fg <- cl_fg[biggest_fg_index][[1]]#impo
  
  clfg_sizes[biggest_fg_index] <- 0
  
  #mi prendo dal cluster louvain il più grande disponibile
  louv_ind <- which((clouv_sizes==max(clouv_sizes)))
  biggest_louv <- c_louvain[louv_ind][[1]]#impo
  clouv_sizes[louv_ind] <- 0
  
  #interseco i due cluster 
  biggest_cluster <- intersect(biggest_fg, biggest_louv)
  if(length(biggest_cluster) == 0){
    if(length(biggest_louv) > length(biggest_cluster)){
      biggest_cluster <- biggest_louv
    }else{
      biggest_cluster <- biggest_cluster
    }
  }
  print(c(length(biggest_fg), length(biggest_louv), length(biggest_cluster)))
  
  #prendo le righe di comments che hanno i video_id che fanno parte del cluster
  com_in_cluster = comments[comments$video_id %in% biggest_cluster,]
  
  #Cumulativa del cluster
  comments3 <- com_in_cluster
  comments3 <- cbind(comments3, 
                     data_date = as.Date(substr(comments3$published_at, 1, 10), format = "%Y-%m-%d"))
  data_count <- comments3 %>% group_by(data_date) %>% tally()
  
  
  #mi creo i named num vector del cluster e lo salvo su una lista 
  vec <- as.vector(data_count$n, mode = "numeric")
  #names(vec) <- data_count$data_date
#}
  
  
  
  print(vec[5:27])
  vec = vec/10
  print(vec)
  
  max(vec)
  min(vec)
 
  
  GTs1<-generation.time("empirical", val = vec[5:27])
  SB <- est.R0.SB(vec[5:27], GTs1)  #R0 parziale per intervalli
  plotfit(SB)
  print(SB$R)
  
#}