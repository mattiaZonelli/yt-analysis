require(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(igraph)
library(Matrix)
library(circlize)
library(disparityfilter)

dir = "/Users/MeijiTenno/Desktop/YouTube/"
setwd(dir)

# load data and convert to data table
load("temp_videos_stats.RData")
df = videos
setDT(df)
df = df[,.(video_id, channel_id)]

load("temp_comments.RData")
setDT(comments)

# keep all comments where video_id is in our list of videos
comments = comments[video_id %in% df$video_id,]

# get user id, better than user name
comments = comments[,.(video_id, user_channel_url)]
comments$user_channel_id = gsub(pattern = "http://www.youtube.com/channel/", replacement = "", x = comments$user_channel_url)
comments = comments[,.(user_channel_id, video_id)]
comments = comments[user_channel_id != "", ]

# better to do the network on the channel levels instead of video (drop this line and change channel_id for video_id if we want at video lvl)
df = merge(comments, df, by = "video_id")

df = df[,.(channel_id, user_channel_id)]
rm(comments, videos)

# make sure there are no collisions between user id and video id
df$channel_id = paste0(df$channel_id, "_channel_id")
head(df)
df = df[!is.na(user_channel_id),]

# get unique
message("getting user-page unique pairs...")
message("before unique ",nrow(df))
setkey(df, NULL)
comments<-unique(df)
message("after unique ",nrow(df))


# make graph
message(" Making bipartite graph...")
g=graph.data.frame(d = df, directed = F) #makegraph
V(g)$type <- grepl(pattern = "_channel_id", x = V(g)$name) #TRUE for page FALSE for user
message("    V(graph) ",length(V(g))," - E(graph) ",length(E(g)))
rm(df)

# proejct
message(" Calculating the projection...")
pg_projection = t(get.incidence(g, sparse = T)) %*% get.incidence(g,sparse = T)
m=as.matrix(pg_projection)
rm(g,pg_projection)

message("Changing projection matrix into adjacency graph...")
g=graph.adjacency(m, mode = 'undirected', diag=F,weighted=T)
rm(m)
V(g)$name = gsub(pattern = "_channel_id", replacement = "", x = V(g)$name)


save(g, file = "yt_g_projection_commenter_video.RData")

