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
load("tbl/temp_videos_stats.RData")

{
  # non eseguire sempre
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



#prendo il cluster più grande 
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
  
  video_lt <- comments3 %>% 
    group_by(video_id) %>% 
    summarise(max_val=max(datetime), min_val=min(datetime), lifetime=round(max_val-min_val, digits = 0))
  
  video_lt2 <- video_lt
  
  lt_tally <- video_lt2 %>% group_by(lifetime) %>% tally()
  video_lt2 = merge(videos, video_lt2, by = "video_id")
  video_lt2 = video_lt2[,.(video_id, lifetime, view_count, like_count,dislike_count, comment_count)]
  video_lt2[is.na(video_lt2)] <- 0
  #video_lt2 <- cbind(video_lt2, ty=lt_tally$n)
  
  
  lt_df <- video_lt2 %>%
      group_by(lifetime) %>%
      summarise(view_count=sum(view_count), 
                like_count=sum(like_count), 
                dislike_count=sum(dislike_count),
                comment_count=sum(comment_count))
  #lt_pdf <- merge(lt_pdf, lt_tally, by="lifetime")
  #lt_pdf$comment_count <- lt_pdf$comment_count/lt_pdf$ty
  #lt_pdf$view_count <- lt_pdf$view_count/lt_pdf$ty
  #lt_pdf$like_count <- lt_pdf$like_count/lt_pdf$ty
  
  lt_df$lifetime <- as.numeric(lt_df$lifetime)
  
  p <- ggplot(lt_df, aes(x=lifetime)) +
    geom_line(aes(y=dislike_count, group = 1), size = 0.6, linetype ="solid", colour= "lightskyblue1") +
    geom_line(aes(y=comment_count, group = 1), size = 0.6, linetype ="solid", colour= "sienna2") +
    geom_line(aes(y=like_count/10, group = 1), size = 0.6, linetype ="solid", colour= "olivedrab2") +
    geom_line(aes(y=view_count/400, group = 1), size = 0.6, linetype ="solid", colour= "slateblue4") +
    scale_x_continuous(#breaks = seq(0,100, by=10),
                       trans="log10")+
    scale_y_continuous(#breaks = seq(0,100000, by=5000),
                       trans="log10") +
    xlab("Lifetime") + 
    ylab(" ")
  #p
    
  plot_list[[i]] <- p
  
  
}


#rm(clfg_sizes, clouv_sizes, first_day, last_day, i, 
 #  c_louvain, cl_fg, com_in_cluster, data_count, p, g_video_c)

require(ggpubr)

pdf("lifetime_likes-views-comments_log-log.pdf")
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
          labels = c("1", "2", "3", "4"),
          ncol=2, nrow=2)
dev.off()


