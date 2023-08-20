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
df = df[,.(channel_id, user_channel_id, published_at, video_id)]

load("tbl/yt_g_projection_commenter_channel.RData")
load("tbl/df.RData")

#### communities comparison ####
message("Community Comparison  Analysis")
fg=fastgreedy.community(g)
wt=walktrap.community(g)
ml=multilevel.community(g)
lp=label.propagation.community(g)
fg_fg = compare(fg, fg, method = "rand")
fg_wt = compare(fg, wt, method = "rand")
fg_ml = compare(fg, ml, method = "rand")
fg_lp = compare(fg, lp, method = "rand")
wt_wt = compare(wt, wt, method = "rand")
wt_ml = compare(wt, ml, method = "rand")
wt_lp = compare(wt, lp, method = "rand")
ml_ml = compare(ml, ml, method = "rand")
ml_lp = compare(ml, lp, method = "rand")
lp_lp = compare(lp, lp, method = "rand")
message("Comparing against FG...")
message("FG: ",fg_fg)
message("WT: ",fg_wt)
message("ML: ",fg_ml)
message("LP: ",fg_lp)
message("Comparing against WT...")
message("WT: ",wt_wt)
message("ML: ",wt_ml)
message("LP: ",wt_lp)
message("Comparing against ML...")
message("ML: ",ml_ml)
message("LP: ",ml_lp)
message("Comparing against SG...")
message("LP: ",lp_lp)
rm(wt, ml, lp, fg_fg, fg_wt, fg_ml, fg_lp, wt_wt, wt_ml, wt_lp, ml_ml, ml_lp, lp_lp, fg)

#### fine comparison 

#### community selection and etc ####

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
  
  #Cumulativa del cluster
  #nuovi utenti
  comments3 <- com_in_cluster
  comments3 <- cbind(comments3, 
                     data_date = as.Date(substr(comments3$published_at, 1, 10), format = "%Y-%m-%d"))
  data_count <- comments3 %>% group_by(data_date) %>% tally(n_distinct(user_channel_id))
  
  #nuovi video ogni giorno
  videos2 <- videos
  videos2 <- cbind(videos2, data_date = as.Date(substr(videos2$published_at, 1,10), format="%Y-%m-%d"))
  data_count2 <- videos2 %>% group_by(data_date) %>% tally()
  # data_count2 <- rbind(data_count2, data_count2[length(data_count2$n),])
  # data_count2 <- rbind(data_count2, data_count2[length(data_count2$n),])
  # data_count <- cbind(data_count, n_video = data_count2$n)
  # 
  last_day <- as.Date(substr(max(comments3$published_at),1,10))
  first_day <- as.Date(substr(min(comments3$published_at),1,10))
  
  p <- ggplot() +
    geom_line(data=data_count[data_count$data_date > "2020-01-10",], 
              aes(x=data_date, y=n/3, group = 1, colour="User"), size = 1.2, linetype ="solid") + 
    geom_line(data = data_count2[data_count2$data_date > "2020-01-10",], 
              aes(x=data_date, y=n, group = 1, colour="Video"), size = 1.2, linetype ="solid") + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    scale_x_date(breaks = seq(as.Date(first_day), as.Date(last_day), by="3 days"))+
    scale_y_continuous()  +
    scale_colour_manual("", 
                        breaks = c("User", "Video"),
                        values = c("navyblue","coral2")) +
    ylab("Number of ") + xlab("Days") +
    theme(legend.position="none")
  #p
  
  plot_list[[i]] <- p
  
  #mi creo i named num vector del cluster e lo salvo su una lista 
  vec <- as.vector(data_count$n, mode = "numeric")
  names(vec) <- data_count$data_date
  #print(vec)
  
  vec_list[[i]] <- vec
  
}


# rm(df, clfg_sizes, clouv_sizes, first_day, last_day, i, 
#    c_louvain, cl_fg, com_in_cluster, data_count, p, g_video_c)

require(ggpubr)

pdf("cumulative_of_4biggest_cl_totale.pdf")
ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
          labels = c("1", "2", "3", "4"),
          ncol=2, nrow=2)
dev.off()



#### R0 unico #### 
library(R0)
pdf("R0_4biggestCluster_totale.pdf")
par(mfrow = c(2,2))

  # e 1
  GTs1 <- generation.time("empirical", c(2.45, 1.38))
  vec_list[[1]] <- vec_list[[1]]
  size <- length(vec_list[[1]][25:55])
  R0 =  est.R0.ML(vec_list[[1]][25:55], GTs1, end = size) #R0
  print(R0$R) #1.002603
  plotfit(R0)
  
  # e 2
  GTs1 <- generation.time("empirical", c(2.45, 1.38))
  size <- length(vec_list[[2]])
  R0 =  est.R0.ML(vec_list[[2]], GTs1, end = size) #R0
  print(R0$R) # 0.9996495
  plotfit(R0)

  # e 3
  GTs1 <- generation.time("empirical", c(2.45, 1.38))
  size <- length(vec_list[[3]])
  R0 =  est.R0.ML(vec_list[[3]], GTs1, end = size) #R0
  print(R0$R) # 0.9998838
  plotfit(R0)
  
  # e 4
  GTs1 <- generation.time("empirical", c(2.45, 1.38))
  size <- length(vec_list[[4]])
  R0 =  est.R0.ML(vec_list[[4]], GTs1, end = size) #R0
  print(R0$R) # 0.9998975
  plotfit(R0)
  
  
dev.off()

#### fine plot

#### R0-SB parziale ####
#save(vec_list, file="vec_list.RData")

load("tbl/vec_list.RData")


library(R0)

# e 1
min(vec_list[[1]])
vec_list[[1]] <- vec_list[[1]]/100
GTs1 <- generation.time("empirical", c(2.45, 1.38))
SB <- est.R0.SB(vec_list[[1]][25:55], GTs1)  #R0 parziale per intervalli
plotfit(SB, xscale="w")
print(SB$R)

# e 2
vec_list[[2]] <- vec_list[[2]]/100
vec_list[[2]][8] <- vec_list[[2]][8]+0.50
GTs1 <- generation.time("empirical", c(2.45, 1.38))
SB <- est.R0.SB(vec_list[[2]][1:55], GTs1)  #R0 parziale per intervalli
plotfit(SB, xscale="w")
print(SB$R)

# e 3
vec_list[[3]] <- (vec_list[[3]]/100)+1
GTs1 <- generation.time("empirical", c(2.45, 1.38))
SB =  est.R0.SB(vec_list[[3]], GTs1) #R0
plotfit(SB, xscale="w")
print(SB$R)

# e 4
vec_list[[4]] <- vec_list[[4]]/100
vec_list[[4]][1] <-vec_list[[4]][1]+1
vec_list[[4]][5] <-vec_list[[4]][5]+1
vec_list[[4]][8] <-vec_list[[4]][8]+1
GTs1 <- generation.time("empirical", c(2.45, 1.38))
SB =  est.R0.SB(vec_list[[4]][1:55], GTs1) #R0
plotfit(SB, xscale="w")
print(SB$R)


dev.off()


