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
# jump to line 134


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

cluster_list <- list()


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
  
  # salvo l'intersezione in una lista
  cluster_list[[i]] <- biggest_cluster
  
}

# prendo le righe di comments che rappresentano i commenti fatti sotto un video di tutti channel_id
# che fanno parte del cluster
rm(i, biggest_fg_index, biggest_fg, louv_ind, biggest_louv, biggest_cluster, clfg_sizes, clouv_sizes, g_video_c)

channelid_vec <- unlist(cluster_list, use.names = F)

com_in_cluster = df[df$channel_id %in% channelid_vec,]
user_ncomments <- com_in_cluster %>% group_by(user_channel_id) %>% tally(n_distinct(comment_id))

#solo quelli con almeno 2 commenti
unc_2 <- user_ncomments[user_ncomments$n > 2,]
names(unc_2)[2] <- "n_comm"
rm(channelid_vec, com_in_cluster, user_ncomments)
combined <- unc_2

for (i  in 1:4) {
  #prendo solo i commenti nei video in cluster i-esimo
  com_in_cluster1 <- df[df$channel_id %in% cluster_list[[i]],]
  #per ogni utente conto quanti commenti diversi ha
  unc_in_cl1 <- com_in_cluster1 %>% group_by(user_channel_id) %>% tally(n_distinct(comment_id))
  names(unc_in_cl1)[2] <- paste0("comm_cl", i)
  #left join con la tabella user-tutti i commenti fatti e user-commenti fatti nel cluster i-esimo
  combined <- merge(x = combined, y = unc_in_cl1, by = "user_channel_id", all.x = TRUE)
  combined[is.na(combined)] <- 0
}
rm(com_in_cluster1, unc_in_cl1, unc_2)

#per ogni colonna ne aggiungo una a frazione
combined <- cbind(combined, ccl1_rate=round(combined$comm_cl1/combined$n_comm, 3))
combined <- cbind(combined, ccl2_rate=round(combined$comm_cl2/combined$n_comm, 3))
combined <- cbind(combined, ccl3_rate=round(combined$comm_cl3/combined$n_comm, 3))
combined <- cbind(combined, ccl4_rate=round(combined$comm_cl4/combined$n_comm, 3))

save(combined, file = "pre_polarizazzione.RData")
load("pre_polarizzazione.RData")

p_pdf <- combined %>% group_by(ccl1_rate) %>% tally()
#p_pdf2 <- combined %>% group_by(ccl1_rate) %>% tally(n_distinct(user_channel_id))
names(p_pdf)[2] <- "pdf" 
names(p_pdf)[1] <- "p"

pdf("testing2.pdf")
ggplot(combined) +
  geom_density( aes(x=combined$ccl1_rate, colour="1"), alpha=0.1, fill="purple4") +
  geom_density( aes(x=combined$ccl4_rate, colour="4"), alpha=0.1, fill="seagreen") + 
  geom_density( aes(x=combined$ccl2_rate, colour="2"), alpha=0.1, fill="chocolate1") +
  geom_density( aes(x=combined$ccl3_rate, colour="3"), alpha=0.1, fill="gold") +
  
  scale_colour_manual("", 
                      breaks = c("1", "2", "3", "4"),
                      values = c("purple4","chocolate1","gold", "seagreen")) +
  theme(legend.position="top") +
  xlab("p") + ylab ("PDF")+
  theme(
    panel.background = element_rect(fill = "grey95", colour = "grey27",
                                    size = 1.2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey84"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey84")
  )
dev.off()

# p <- combined$ccl1_rate
# # rimuovo gli zeri
# indices<-which(p>0)
# p<-p[indices]
# # rimuovo i duplicati
# p <- unique(p)
# #ordino in senso crescente
# sort(p, decreasing = F)









