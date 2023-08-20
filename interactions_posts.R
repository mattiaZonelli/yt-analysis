# per svuotare
{
  library(varhandle)
  rm.all.but(c("dir"))
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
dir = "/Users/MeijiTenno/"
setwd(dir)
load("Documents/Scuola/UNIVE/3^anno/YouTube Analisys/tbl/temp_videos_stats.RData")

vid2 <- videos
vid2[is.na(vid2)] <- 0
rm(videos)
# View interactions - number of posts
vid2$view_count[vid2$view_count > 1000] <- round(vid2$view_count[vid2$view_count > 1000], -1) 
sqldf("select count(distinct(view_count)) from vid2;") #10382
view_post <- vid2 %>% group_by(view_count) %>% tally()
#View(view_post)
names(view_post)[2] <- "view_posts"

# Like interactions - number of posts
sqldf("select count(distinct(like_count)) from vid2;") #10382
like_post <- vid2 %>% group_by(like_count) %>% tally()
#View(like_post)
names(like_post)[2] <- "like_posts"

# DISike interactions - number of posts
sqldf("select count(distinct(dislike_count)) from vid2;") #1013
dislike_post <- vid2 %>% group_by(dislike_count) %>% tally()
#View(dislike_post)
names(dislike_post)[2] <- "dislike_posts"

# Comments interactions - number of posts
sqldf("select count(distinct(comment_count)) from vid2;") #2081
comment_post <- vid2 %>% group_by(comment_count) %>% tally()
#View(comment_post)
names(comment_post)[2] <- "comment_posts"

# Full
pdf("full_interactions-posts.pdf")
ggplot() + 
  geom_point(data=view_post, aes(x=view_count, y = view_posts,group=1, colour="View"), size=2) +
  geom_point(data=like_post, aes(x=like_count, y = like_posts, colour="Like"), size=2) +
  geom_point(data=dislike_post, aes(x=dislike_count, y = dislike_posts, colour="Dislike"), size=2) +
  geom_point(data=comment_post, aes(x=comment_count, y = comment_posts, colour="Comment"), size=2) +
  scale_x_continuous(breaks=c(1,10,1000,10000,10000000),
                     trans="log10") +
  scale_y_continuous(trans="log10") +
  scale_colour_manual("", 
                      breaks = c("Comment", "Dislike", "Like", "View"),
                      values = c("sienna2", "lightskyblue1", "olivedrab2", "slateblue4")) +
  xlab("Number of interactions") + ylab("Number of posts") +
  theme(legend.position = c(0.9,0.9))
dev.off()


# Fino a 1000
pdf("1-1000_interactions_posts.pdf")
ggplot() + 
  geom_point(data=view_post, aes(x=view_count, y = view_posts, colour="View"), size=3) +
  geom_point(data=like_post, aes(x=like_count, y = like_posts, colour="Like"), size=2) +
  geom_point(data=dislike_post, aes(x=dislike_count, y = dislike_posts, colour="Dislike"), size=2) +
  geom_point(data=comment_post, aes(x=comment_count, y = comment_posts, colour="Comment"), size=2) +
  scale_x_continuous(trans="log10", limits = c(1, 1000)) +
  scale_y_continuous(trans="log10") +
  scale_colour_manual("", 
                      breaks = c("Comment", "Dislike", "Like", "View"),
                      values = c("sienna2", "lightskyblue1", "olivedrab2", "slateblue4")) +
  xlab("Number of interactions") + ylab("Number of posts") +
  theme(legend.position = c(0.9,0.9))
dev.off()

# Da 1000 a 10^7
pdf("1e03-1e05_interactions_posts.pdf")
ggplot() + 
  geom_point(data=view_post, aes(x=view_count, y = view_posts, colour="View"), size=2) +
  geom_point(data=like_post, aes(x=like_count, y = like_posts, colour="Like"), size=2) +
  geom_point(data=dislike_post, aes(x=dislike_count, y = dislike_posts, colour="Dislike"), size=2) +
  geom_point(data=comment_post, aes(x=comment_count, y = comment_posts, colour="Comment"), size=2) +
  scale_x_continuous(trans="log10", limits = c(1000, 10000)) +
  scale_y_continuous(trans="log10") +
  scale_colour_manual("", 
                      breaks = c("Comment", "Dislike", "Like", "View"),
                      values = c("sienna2", "lightskyblue1", "olivedrab2", "slateblue4")) +
  xlab("Number of interactions") + ylab("Number of posts") +
  theme(legend.position = c(0.9,0.9))
dev.off()
