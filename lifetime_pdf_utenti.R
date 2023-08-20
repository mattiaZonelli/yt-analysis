# per svuotare
{
  library(varhandle)
  rm.all.but(c("dir","df"))
}
#


require(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(igraph)
library(Matrix)
library(circlize)
library(disparityfilter)
library(dplyr)
dir = "/Users/MeijiTenno/Desktop/YouTube/"
setwd(dir)
{
  # non eseguire sempre
  load("tbl/temp_videos_stats.RData")
  df = videos
  setDT(df)
  df = df[,.(video_id, channel_id)]
  
  load("tbl/temp_comments.RData")
  setDT(comments)
  # keep all comments where video_id is in our list of videos
  comments = comments[video_id %in% df$video_id,]
  # get user id, better than user name
  comments$user_channel_id = gsub(pattern = "http://www.youtube.com/channel/", replacement = "", x = comments$user_channel_url)
  comments = comments[user_channel_id != "", ]
  
  # unisco la tabella comments e videos attraverso la colonna video_id
  df = merge(comments, df, by = "video_id")
  #View(df)
  df = df[,.(user_name, user_channel_id,comment_id, published_at, video_id, channel_id)]
  save(df, file="tbl/df.RData")
  #
}

load("tbl/df.RData")
load("tbl/yt_g_projection_commenter_channel.RData")


fg <- fastgreedy.community(g)

ids <- which(sizes(fg)>3)
subgraph <- induced.subgraph(g, which(membership(fg) %in% ids))

g_video_c <- subgraph

rm(g, fg, ids, subgraph)

#fastgreedy
cl_fg = cluster_fast_greedy(g_video_c)

#louvain
c_louvain = cluster_louvain(g_video_c)


#prendo l'elenco(vettore) delle dimensiouìni dei vari cluster
clfg_sizes <- sizes(cl_fg)
clouv_sizes <- sizes(c_louvain)

plot_list <- list()

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
  if(length(biggest_cluster) < 20){
    if(length(biggest_louv) > length(biggest_cluster)){
      biggest_cluster <- biggest_louv
    }else{
      biggest_cluster <- biggest_cluster
    }
  }
  print(c(length(biggest_fg), length(biggest_louv), length(biggest_cluster)))
  
  # prendo le righe di comments che rappresentano i commenti fatti sotto un video di channel_id
  # che fanno parte del cluster
  com_in_cluster = df[df$channel_id %in% biggest_cluster,]
  rm(biggest_fg_index, biggest_fg, louv_ind, biggest_louv, biggest_cluster)
  
  #
  comments3 <- com_in_cluster
  comments3 <- cbind(comments3, 
                     datetime = as.POSIXct(strptime(substr(comments3$published_at, 1, 19), "%Y-%m-%dT%H:%M:%S"), tz="GMT"))
  rm(com_in_cluster)
  
  #per ogni utente calcolo primo e ultimo commento e lifetime di quel utente
  user_lt <- comments3 %>% 
    group_by(user_channel_id) %>% 
    summarise(last_val=max(datetime), first_val=min(datetime), lifetime=round(last_val-first_val, digits = 0))
  
  #per ogni lifetime conto quanti ce ne sono
  user_lt_pdf <- user_lt %>% group_by(lifetime) %>% tally()
  names(user_lt_pdf)[2] <- "PDF" 
  names(user_lt_pdf)[1] <- "Lifetime" 
  user_lt_pdf$Lifetime <- as.numeric(user_lt_pdf$Lifetime)
  rm(user_lt)
  
  #mi salvo i plot su una variabile e poi su una lista
  p <- ggplot(user_lt_pdf, aes(Lifetime, PDF)) +
    geom_line(aes(group = 1), size = 0.8, linetype ="solid", color= "mediumblue") + 
    scale_x_continuous(#breaks = seq(1,50, by=log(20)*1.4),
      trans="log10")+
    scale_y_continuous(breaks = c(10, 1000, 100000),
      trans="log10")
  #lista di plot
  plot_list[[i]] <- p
  
  
}


require(ggpubr)

pdf("lifetime_PDF_utenti.pdf")
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
          labels = c("1", "2", "3", "4"),
          ncol=2, nrow=2)
dev.off()


