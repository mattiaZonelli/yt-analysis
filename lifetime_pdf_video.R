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
  lt_pdf <- video_lt %>% group_by(lifetime) %>% tally()
  names(lt_pdf)[2] <- "PDF" 
  names(lt_pdf)[1] <- "Lifetime" 
  lt_pdf$Lifetime <- as.numeric(lt_pdf$Lifetime)
  rm(video_lt)
  
  p <- ggplot(lt_pdf, aes(Lifetime, PDF)) +
    geom_line(aes(group = 1), size = 0.8, linetype ="solid", color= "chocolate1") + 
    #stat_smooth(aes(group =1), se = F, method = "lm", formula = y ~ poly(x, 10)) +
    #theme(axis.text.x=element_text(angle=90, hjust=1)) +
    scale_x_continuous(#breaks = c(1,3,10,20, 30),
                        trans="log10")+
    scale_y_continuous(#breaks = c(1,5,10,50,100,500,1000),
                       trans="log10")
  
  plot_list[[i]] <- p
  

}


#require(ggpubr)

pdf("lifetime_PDF2.pdf")
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
          labels = c("1", "2", "3", "4"),
          ncol=2, nrow=2)
dev.off()


