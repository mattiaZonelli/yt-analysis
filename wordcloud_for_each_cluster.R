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

#sample
sample_videos_id = sample(videos$video_id, 1000, replace = F)
sample_videos= videos[videos$video_id %in% sample_videos_id,]
#prendo i comments che fanno parte del sample
com_in_sample = comments[comments$video_id %in% sample_videos_id,]

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

#cl_le = cluster_leading_eigen(g_video_c)
#cl_o = cluster_optimal(g_video_c)

#fastgreedy
cl_fg = cluster_fast_greedy(g_video_c)

#louvain
c_louvain = cluster_louvain(g_video_c)

#compare di fastgreedy e louvain
compare(cl_fg, c_louvain, "adjusted.rand")

#prendo il cluster più grande --->> andrà fatto anche per c_louvain e poi intersecare
clfg_sizes <- sizes(cl_fg)
clouv_sizes <- sizes(c_louvain)
texts_list <- list()
for (i in 1:4) {
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
  
  texts <- com_in_cluster$comment_text_original
  texts_list[[i]] <- texts
 
}


for (i in 1:4) {
  texts <- texts_list[[i]]
  texts <- iconv(texts, 'UTF-8', 'ASCII') # encoding for emojis
  corpus <- Corpus(VectorSource(texts))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  
  #Creo una DTM  a matrix that lists all occurrences of words in the corpus, by document
  dtm = DocumentTermMatrix(corpus)
  freq <- colSums(as.matrix(dtm))
  length(freq) == dim(dtm)[2]
  ord <- order(freq, decreasing = TRUE)
  
  #prima si crea un dataframe
  wf = data.frame(word = names(freq),
                  freq = freq)
  wf <- wf[order(-wf$freq),]
  
  filename <- paste("wordcloud_of_cl_", i, ".pdf", sep="")
  pdf(filename)
  wordcloud(wf$word[1:200], wf$freq, 
                  scale=c(4,0.5), min.freq = 8, 
                  random.order = F, 
                  rot.per = 0.15, 
                  colors = brewer.pal(8,"Dark2")
  )
  dev.off()
  
}

