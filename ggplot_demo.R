library(ggplot2)

setwd('F:\\R\\R case\\visualization')
# load data
book_info <- read.csv('book_douban.csv',  header=F)
colnames = c('name', 'ratings', 'comment_num', 'pub', 'url')
book_info_df <- as.data.frame(book_info, col.names=colnames)
plot_data <- data.frame(as.vector(unlist(book_info_df[2])), 
                        as.vector(unlist(book_info_df[3])))
colnames(plot_data) <- c('ratings', 'comments')

# data preprocess
plot_data$ratings[plot_data$ratings >= 9.0] <- 'great'
plot_data$ratings[plot_data$ratings >= 8.0 & plot_data$ratings < 9.0 ] <- 'good'
plot_data$ratings[plot_data$ratings >= 7.0 & plot_data$ratings < 8.0 ] <- 'average'
plot_data$ratings[plot_data$ratings >= 6.0 & plot_data$ratings < 7.0 ] <- 'low'
plot_data$ratings[plot_data$ratings < 6.0 ] <- 'poor'
plot_data$ratings <- factor(plot_data$ratings, 
                            levels = c('high', 'good', 'average', 'low', 'poor'))
# visulization

# 各档评分的多少
ggplot(plot_data,aes(x=ratings, ))+
  geom_bar()
# 可以看出，average档（7.0—8.0）的评分比较多，其次是good（8.0-9.0）
# 特别好high（>9.0）和特别差（<6.0）都比较少,近似于正态分布

# 评论人数与评分的关系