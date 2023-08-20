library(igraph)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(sqldf)
library(R0)
library(tibble)
dir = "/Users/MeijiTenno/Desktop/YouTube/"
setwd(dir)

load("tbl/temp_comments.RData")
load("tbl/temp_videos_stats.RData")

{
  require(varhandle)
  rm.all.but(c("comments", "videos"))
}

#sample
sample_videos_id = sample(videos$video_id, 1100, replace = F)
sample_videos= videos[videos$video_id %in% sample_videos_id,]
#prendo i comments che fanno parte del sample
com_in_sample = comments[comments$video_id %in% sample_videos_id,]

#### confronto views, likes, share, comments ####
videos2 <- videos
videos2[is.na(videos2)] <- 0
videos2 <- cbind(videos2, 
                   data_date = as.Date(substr(videos2$published_at, 1, 10), format = "%Y-%m-%d"))
big_tbl <- videos2 %>% group_by(data_date) %>% summarise(n_view=sum(view_count))
tbl1 <- videos2 %>% group_by(data_date) %>% summarise(n_like=sum(like_count))
tbl2 <- videos2 %>% group_by(data_date) %>% summarise(n_comment=sum(comment_count))
tbl3 <- videos2 %>% group_by(data_date) %>% summarise(n_dislike=sum(dislike_count))

big_tbl <- cbind(big_tbl, n_like=tbl1$n_like)
big_tbl <- cbind(big_tbl, n_comment=tbl2$n_comment)
big_tbl <- cbind(big_tbl, n_dislike=tbl3$n_dislike)
last_day <- as.Date(substr(max(videos2$published_at),1,10))
first_day <- as.Date(substr(min(videos2$published_at),1,10))
rm(videos2, tbl1, tbl2, tbl3)

pdf("view_like_dislike_comment.pdf")

ggplot(big_tbl[big_tbl$data_date > "2020-01-10",], aes(x=data_date)) +
  geom_smooth(aes(y = n_comment/4, group=1, colour="Comment"), span=0.4,size=2, se=F) +
  geom_smooth(aes(y = n_like/20, group=1, colour="Like"), span=0.4, size=2, se=F) +
  geom_smooth(aes(y = n_dislike, group=1, colour="Dislike"), span=0.4, size=2, se=F) +
  geom_smooth(aes(y = n_view/800, group=1, colour="View"), span=0.3, size=2, se=F) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_date(breaks = seq(as.Date(first_day), as.Date(last_day), by="3 days")) +
  scale_colour_manual("", 
                      breaks = c("Comment", "Dislike", "Like", "View"),
                      values = c("sienna2", "lightskyblue1", "olivedrab2", "slateblue4")) +
  theme(legend.position=c(0.9, 0.9)) +
  ylab("Number of interactions") + xlab("Days")

dev.off()


#### fine confronto

#### WORDCLOUD ####
#completo
#texts <- comments$comment_text_original 

#con sample
texts <- com_in_sample$comment_text_original 

rm(sample_videos_id, sample_videos, com_in_sample)

texts <- iconv(texts, 'UTF-8', 'ASCII') # encoding for emojis
corpus <- Corpus(VectorSource(texts))
# Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove English common stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)

#Creo una DTM  a matrix that lists all occurrences of words in the corpus, by document
dtm = DocumentTermMatrix(corpus)
rm(texts, corpus)
# Ottengo la frequenza delle occorrenze di ogni parola
freq <- colSums(as.matrix(dtm))
#controllo che la trasformazione sia andata a buon fine in modo corretto, deve restituire TRUE
length(freq) == dim(dtm)[2]
#ordino in modo decrescente
ord <- order(freq, decreasing = TRUE)

#prima si crea un dataframe
wf = data.frame(word = names(freq), 
                freq = freq)
wf <- wf[order(-wf$freq),]
rm(dtm, freq, ord)

subwf <- wf[1:10,]
pdf("wordfreq_hist.pdf")
ggplot(subwf, aes(x=word, y=subwf$freq, fill=word)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d(option="cividis") +
  ylab("frequency") +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.title = element_blank())
dev.off()

View(subwf)


pdf("wordtag_800sample.pdf")
wordcloud(wf$word[1:100], wf$freq, 
          scale=c(4,0.5), min.freq = 8, 
          random.order = F, 
          rot.per = 0.15, 
          colors = brewer.pal(8,"Dark2")
)
dev.off()
### fine WORDCLOUD ###


#### Quante persone commentano ogni giorno ?/Cumulata and R0 ####

#nuovi utenti ogni giorno
comments2 <- comments
comments2 <- cbind(comments2, 
                 data_date = as.Date(substr(comments2$published_at, 1, 10), format = "%Y-%m-%d"))

data_count <- comments2 %>% group_by(data_date) %>% tally(n_distinct(user_name))

#nuovi video ogni giorno
videos2 <- videos
videos2 <- cbind(videos2, data_date = as.Date(substr(videos2$published_at, 1,10), format="%Y-%m-%d"))
data_count2 <- videos2 %>% group_by(data_date) %>% tally()
data_count2 <- rbind(data_count2, data_count2[length(data_count2$n),])
data_count2 <- rbind(data_count2, data_count2[length(data_count2$n),])

data_count <- cbind(data_count, n_video = data_count2$n)

last_day <- as.Date(substr(max(comments2$published_at),1,10))
first_day <- as.Date(substr(min(comments2$published_at),1,10))

pdf("cumulativa_new_userVSvideo_no_head.pdf")
ggplot(data_count[data_count$data_date > "2020-01-14",], aes(x=data_date)) +
  geom_line(aes(y=n/40, group = 1, colour="User"), size = 1.2, linetype ="solid") + 
  geom_line(aes(y=n_video, group = 1, colour="Video"), size = 1.2, linetype ="solid") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_date(breaks = seq(as.Date(first_day), as.Date(last_day), by="3 days"))+
  scale_y_continuous()  +
  scale_colour_manual("", 
                      breaks = c("User", "Video"),
                      values = c("navyblue","coral2")) +
  theme(legend.position=c(0.9, 0.9)) +
  ylab(" ") + xlab("Days")
dev.off()
#### fine 
#### R0 della cumulativa totale ####
vector <- as.vector(data_count$n[51:77], mode = "numeric")
names(vector) <- data_count$data_date[51:77]

#GTs1<-generation.time("empirical", vector)
GTs1 <- generation.time("empirical", c(2.45, 1.38))
size <- length(vector)
R0 =  est.R0.ML(vector, GTs1, end = size) #R0
pdf("R0_secco.pdf")
  plotfit(R0)
dev.off()


print(vector)
vector <- vector/1000
max(vector)
#GTs1<-generation.time("empirical", vector)
GTs1 <- generation.time("empirical", c(2.45, 1.38))
SB <- est.R0.SB(vector, GTs1)  #R0 parziale per intervalli
plotfit(SB, xscale="w")
#### fine

#### Bipartita commentatori-video e Proiezione video ####
#completo
#edges = data.frame("from" = comments$video_id, "to" = comments$user_name, stringsAsFactors = F )

#con sample
edges = data.frame("from" = com_in_sample$video_id, "to" = com_in_sample$user_name, stringsAsFactors = F )


g <- graph.data.frame(edges, directed = F)
V(g)$type <- V(g)$name %in% edges[,2]
g_comm <- g
g = g_comm

# plot bipartita
{
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
E(g)$color <- "grey"
#pdf("bipartita.pdf")
plot(g, vertex.label = NA, 
     vertex.size=6,
     edge.width=0.2,
     edges.size = 0.1,
     layout = layout_as_bipartite)
#dev.off()
}
rm(sample_videos_id, sample_videos, edges)

bipartite_matrix <- as_incidence_matrix(g_comm)
video_matrix <- tcrossprod(bipartite_matrix)
diag(video_matrix) <- 0
g_video_c = graph_from_adjacency_matrix(video_matrix, 
                                       mode = "undirected", 
                                       weighted = TRUE)
g_video_c = simplify(g_video_c)

# plot proiezione sui video
V(g_video_c)$size <- degree(g_video_c)*0.2
V(g_video_c)$color <- "navyblue"
V(g_video_c)$frame.color <- "white"

E(g_video_c)$color <- "grey33"
E(g_video_c)$width <- E(g_video_c)$weight/10

 
l <- layout_in_circle(g_video_c)

#pdf("proj_video_sample1k.pdf")
  plot(g_video_c, 
       vertex.label = NA,
       edge.lty = 1,
       edge.curved = 0.4,
       layout=l
  ) 
#dev.off()

#### fine bipartita 

#### Community Detection plot####
#completo
#edges = data.frame("from" = comments$video_id, "to" = comments$user_name, stringsAsFactors = F )

#con sample 
edges = data.frame("from" = com_in_sample$video_id, "to" = com_in_sample$user_name, 
                   stringsAsFactors = F )

#
rm(com_in_sample,sample_videos,videos, comments)
#
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

#
rm(edges,g, g_comm, bipartite_matrix, video_matrix)
#

cl_fg = cluster_fast_greedy(g_video_c)
sizes(cl_fg)


ids <- which(sizes(cl_fg)>1)
subgraph <- induced.subgraph(g_video_c, which(membership(cl_fg) %in% ids))

cl_fg = cluster_fast_greedy(subgraph)
sizes(cl_fg)

video_matrix <- as.matrix(as_adjacency_matrix(subgraph))

#
rm(subgraph, g_video_c)
#

require(circlize)
pdf("templot/comm_det.pdf")
chordDiagram(video_matrix,
             transparency = 0.5,
             grid.col = membership(cl_fg),
             annotationTrack = "grid",
             link.sort = TRUE, link.decreasing = TRUE
)
dev.off()



c1 = fastgreedy.community(g_video_c)
V(g_video_c)$community <- c1$membership
c1_nodes <- data.frame(id = V(g_video_c)$name, title = V(g_video_c)$name, group = V(g_video_c)$community)
c1_nodes <- c1_nodes[order(c1_nodes$id, decreasing = F),]
c1_edges <- get.data.frame(g_video_c, what="edges")[1:2]
library(visNetwork)
visNetwork(c1_nodes, c1_edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
length(c1)


#metodo originale
plot(g_video_c, 
     #vertex.color=membership(cl_fg),
     vertex.label = NA,
     vertex.size = 3,
     edge.color=membership(cl_fg),
     edge.curved = TRUE,
     edge.width = 1,
     layout=layout.circle(g_video_c)
)

#### fine.

####Community and R0 ####
#con sample 
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

#load("cl_fastgreedy.RData")
#load("cl_louvain.RData")

#compare di fastgreedy e louvain
compare(cl_fg, c_louvain, "adjusted.rand")

#prendo il cluster più grande --->> andrà fatto anche per c_louvain e poi intersecare
clfg_sizes <- sizes(cl_fg)
clouv_sizes <- sizes(c_louvain)

plot_list <- list()
vec_list <- list()
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
  
  #Cumulativa del cluster
  comments3 <- com_in_cluster
  comments3 <- cbind(comments3, 
                     data_date = as.Date(substr(comments3$published_at, 1, 10), format = "%Y-%m-%d"))
  data_count <- comments3 %>% group_by(data_date) %>% tally()
  last_day <- as.Date(substr(max(comments3$published_at),1,10))
  first_day <- as.Date(substr(min(comments3$published_at),1,10))

  p <- ggplot(data_count, aes(data_date, n)) +
    geom_line(aes(group = 1), size = 1.2, linetype ="solid", color= "navyblue") + 
    geom_point(size = 1.5) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    scale_x_date(breaks = seq(as.Date(first_day), as.Date(last_day), by="3 days"))+
    scale_y_continuous(breaks=c(0, 100, 200, 300, 400, 500,600,700,800,900,100))
  
  plot_list[[i]] <- p
  
  #mi creo i named num vector del cluster e lo salvo su una lista 
  vec <- as.vector(data_count$n, mode = "numeric")
  names(vec) <- data_count$data_date
  #print(vec)
  
  vec_list[[i]] <- vec
  
}

#o sarebbe più bello fare 4 linee di colore diverso sullo stesso plot?
require(ggpubr)

pdf("cumulative_of_4biggest_cl_1ksample.pdf")
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
          labels = c("1", "2", "3", "4"),
          ncol=2, nrow=2)
dev.off()



#R0
pdf("R0_4biggestCluster_sample1k.pdf")
par(mfrow = c(2,2))
for (i in 1:4) {
  GT<-generation.time("empirical", vec_list[[i]])
  size <- length(vec_list[[i]])
  r0 =  est.R0.ML(vec_list[[i]], GT, end = size)
  print(r0$R)
  plotfit(r0, GT)
}
dev.off()

#SB
#par(mfrow = c(2,2))
#for (i in 1:4) {
  
  vec <- vec_list[[2]]
  print(vec)
  min(vec)
  
  vec <- vec/10
  GT<-generation.time("empirical", vec)
  
  sb <- est.R0.SB(vec, GT)
  plotfit(sb, GT)
  
#}
#dev.off()


#### fine 