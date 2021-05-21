
#### ANALYSING #METOO ###


library(stringr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(ggplot2)
library(reshape2)

wd <- '~/Dropbox/lse/gi499/diss/quant'
setwd(wd)


# general functions and palette
char_func <- function(x){as.numeric(as.character(x))}
fac_func <- function(x){as.numeric(as.factor(x))}
info_func <- function(x) {(cbind('nodes'=vcount(x), 'edges'=ecount(x)))}

pal <- c('#E8F1F2', '#5F4E8E', '#2D3047','#6d6d6d', '#000000', '#0e0e93', '#7A9E7E', '#1C3F23', '#870000', '#FFFFFF')

# prepare all necessary files
# text and metadata

txts <- list.files('june_19')
metadata.df <- data.frame(read.csv('metadata.csv'))
metadata.df$date <- paste(metadata.df$date, '00', sep = '')

# csvs
setwd(paste(wd, '/outputs', sep=''))

# pageview.df <- data.frame(read.csv('pv.csv'))
# edit.df <- data.frame(read.csv('edit.csv'))
# byte.df <- data.frame(read.csv('byte.csv'))
all.df <- data.frame(read.csv('all.csv'), row.names = NULL)


# all.df <- data.frame(cbind('page' = as.character(pageview.df$page), 
                       # 'timestamp' = pageview.df$timestamp, 
                       # 'views' = pageview.df$views, 
                       # 'edits_all' = edit.df$all,
                       # 'edits_user' = edit.df$user,
                       # 'edits_bot' = edit.df$bot, 
                       # 'bytes' = byte.df$bytes, 
                       # 'range' = v_all))


# table 1
summary_func <- function(x,y){
  output <- NULL 
  for (i in 1:nlevels(as.factor(y))){
    key <- levels(as.factor(y))[i]
    sum <- length(which(y == key))
    temp.df <- data.frame(cbind(key, sum))
    output <- data.frame(rbind(temp.df, output))
  }
  return(output)
}

race <- summary_func(metadata.df, metadata.df$race)
title <- summary_func(metadata.df, metadata.df$title)
no.reports <- summary_func(metadata.df, metadata.df$no.report)
violence <- summary_func(metadata.df, metadata.df$violence)
industry <- summary_func(metadata.df, metadata.df$industry)

table_1 <- data.frame(rbind(race,
                            title,
                            no.reports,
                            violence,
                            industry))

### participation ###

all.df$edits_user <- as.numeric(as.character(all.df$edits_user))
all.df$views <- as.numeric(as.character(all.df$views))
all.df$bytes <- as.numeric(as.character(all.df$bytes))

all_hw.df <- all.df[all.df$page != 'Harvey_Weinstein',]

# data prep & actual analysis

p_func <- function(df,x){
  df <- df[x != 0,]
  x <- x[x != 0]
  mn_b <- mean(x[df$range < 0])
  mn_a <- mean(x[df$range >= 0])
  md_b <- median(x[df$range < 0])
  md_a <- median(x[df$range >= 0])
  s_b <- sum(x[df$range < 0])
  s_a <- sum(x[df$range >= 0])
  df <- data.frame(cbind('mean_b' = mn_b,
                         'mean_a' = mn_a,
                         'median_b' = md_b, 
                         'median_a' = md_a, 
                         'sum_b' = s_b, 
                         'sum_a' = s_a))
  return(df)
}

p_func(all.df, all.df$views)
p_func(all.df, all.df$edits_all)
p_func(all.df, all.df$bytes)


summary(all.df$views[all.df$range < 0])
summary(all.df$views[all.df$range >= 0])


summary(all.df$edits_all[all.df$range < 0])
summary(all.df$edits_all[all.df$range >= 0])

m <- which.max(all.df$edits_all[all.df$range == -1])
all.df[all.df$range == -1,][m,]

summary(all_hw.df$bytes[all_hw.df$range < 0])
summary(all_hw.df$bytes[all_hw.df$range >= 0])

sum(all_hw.df$bytes[all_hw.df$range < 0])
sum(all_hw.df$bytes[all_hw.df$range == 2])

plot(cumsum(all_hw.df$bytes))


summary(all_hw.df$edits_all)

shapiro.test(all.df$views[all.df$range < 0])
wilcox.test(all.df$edits_user[all.df$range < 0], all.df$edits_user[all.df$range >= 0])
wilcox.test(all_hw.df$bytes[all.df$range < 0], all_hw.df$bytes[all.df$range >= 0])
wilcox.test(all_z.df$edits_all[all_z.df$range < 0], all_z.df$edits_all[all_z.df$range >= 0])

### LOOK AT THESE GRAPHS
v_pal <- c(rep(pal[6], 76))
e_pal <- c(rep(pal[7], 76))
b_pal <- c(rep(pal[8], 76))

p_melted.df <- melt(all.df, c('bytes', 'page', 'views', 'edits_user'), c('range'))

p_melted.df$edits_user <- as.numeric(as.character(p_melted.df$edits_user))
p_melted.df$views <- as.numeric(as.character(p_melted.df$views))
p_melted.df$bytes <- as.numeric(as.character(p_melted.df$bytes))

# views 
plot_1 <- ggplot(p_melted.df, aes(value, group = page)) + 
  geom_line(aes(y = views, color = page)) + 
  ggtitle('Page Views - Individual Pages') +
  labs(x = 'Days Before / After Report', y = '') +
  theme_bw() +
  scale_color_manual(values = v_pal) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = "dashed")

# edits
plot_2 <- ggplot(p_melted.df, aes(value, group = page)) + 
  geom_line(aes(y = edits_user, color = page)) + 
  ggtitle('Edits (users) - Individual Pages') +
  labs(x = 'Days Before / After Report', y = 'Page Views') +
  theme_bw() +
  scale_color_manual(values = e_pal) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = "dashed")

# bytes
phw_melted.df <- p_melted.df[p_melted.df$page != 'Harvey_Weinstein',]

# without harvey w
plot_3 <- ggplot(phw_melted.df, aes(value, group = page)) + 
  geom_line(aes(y = bytes, color = page)) + 
  ggtitle('Byte Changes - Individual Pages') +
  labs(x = 'Days Before / After Report', y = '') +
  theme_bw() +
  scale_color_manual(values = b_pal) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
    text = element_text(size = 11, family = "Times New Roman"),
    axis.title = element_text(face="bold"),
    axis.text.x=element_text(size = 11), 
    legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = "dashed")

cb_agg2.df <-  aggregate(as.numeric(as.character(all.df$bytes)), 
                     list(all.df$range), 
                     FUN = sum)

colnames(cb_agg2.df) <- c('range', 'cs_bytes')

plot_3b <- ggplot(cb_agg2.df, aes(range, group = 1)) + 
  geom_line(aes(y = cumsum(cs_bytes))) + 
  ggtitle('Byte Changes - Individual Pages') +
  labs(x = 'Days Before / After Report', y = '') +
  theme_bw() +
  scale_color_manual(values = b_pal) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = "dashed")

cb_aa.df <-  aggregate(as.numeric(as.character(all.df$bytes[all.df$page == 'Dan_Harmon'])), 
                         list(all.df$range[all.df$page == 'Dan_Harmon']), 
                         FUN = sum)
colnames(cb_aa.df) <- c('range', 'bytes')

plot_3d <- ggplot(cb_aa.df, aes(range, group = 2)) + 
  geom_line(aes(y = bytes, color = b_pal[1])) + 
  ggtitle('Byte Changes - Dan Harmon') +
  labs(x = 'Days Before / After Report', y = '') +
  theme_bw() +
  scale_color_manual(values = b_pal[1]) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = "dashed")

cumsum(all_hw.df$bytes)

# with harvey weinstein 
plot_4 <- ggplot(p_melted.df, aes(value, group = page)) + 
  geom_line(aes(y = bytes, color = page)) + 
  ggtitle('Byte Changes - Individual Pages') +
  labs(x = 'Days Before / After Report', y = '') +
  theme_bw() +
  scale_color_manual(values = b_pal) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = "dashed")


# all in agg

v_agg <-  aggregate(as.numeric(as.character(p_melted.df$views)), 
                    list(p_melted.df$value), 
                    FUN = sum)
e_agg <- aggregate(as.numeric(as.character(p_melted.df$edits_user)), 
                   list(p_melted.df$value), 
                   FUN = sum)
b_agg <- aggregate(as.numeric(as.character(p_melted.df$bytes)), 
                   list(p_melted.df$value), 
                   FUN = sum)
p_agg.df <- data.frame(cbind('range' = v_agg$Group.1, 
                             'views' = (v_agg$x)/10, 
                             'edits' = e_agg$x*10, 
                             'bytes' = b_agg$x))

p_melted_agg.df <- melt(p_agg.df, 'range',  c('bytes', 'views', 'edits'))

plot_4 <- ggplot(p_melted_agg.df, aes(range, group = variable)) +
  geom_line(aes(y = value, colour = variable)) + 
  theme_bw() + 
  ggtitle('Views, Edits & Bytes Changes: Aggregate Pages') +
  labs(x = 'Days Before / After Report', y = '') +
  theme_bw() +
  scale_color_manual(values = pal[6:8]) + 
  theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
        text = element_text(size = 11, family = "Times New Roman"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  geom_vline(xintercept = 0, linetype = "dashed")

length(which(all.df$bytes[all.df$range > 0] > 0))
length(which(all.df$bytes[all.df$range > 0] > 0))
length(which(all.df$bytes < 0))

sum(all.df$edits_bot[all.df$range < 0], na.rm = TRUE)/sum(all.df$edits_bot[all.df$range >= 0], na.rm = TRUE)
sum(all.df$edits_user[all.df$range < 0], na.rm = TRUE)/sum(all.df$edits_user[all.df$range >= 0], na.rm = TRUE)

# INDIVIDUAL BYTE CHANGES OVER TIME
plot_func <- function(x){
  cb_aa.df <-  aggregate(as.numeric(as.character(all.df$bytes[all.df$page == x])), 
                         list(all.df$range[all.df$page == x]), 
                         FUN = sum)
  colnames(cb_aa.df) <- c('range', 'bytes')
  
  output <- ggplot(cb_aa.df, aes(range, group = 2)) + 
    geom_line(aes(y = bytes, color = b_pal[1])) + 
    # geom_line(aes(y = views, color = v_pal[1])) + 
    # geom_line(aes(y = edits, color = e_pal[1])) + 
    ggtitle(x) +
    labs(x = 'Days Before / After Report', y = '') +
    theme_bw() +
    scale_color_manual(values = b_pal[1]) + 
    theme(plot.title = element_text(size = 13, family = "Times New Roman", face = "bold"),
          text = element_text(size = 11, family = "Times New Roman"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11), 
          legend.position = 'none') +
    geom_vline(xintercept = 0, linetype = "dashed")
  return(output)
}

plot_func('Bob_Weinstein') # dismissed
plot_func('Jeff_Franklin')
plot_func('Ryan_Seacrest') # dismissed
plot_func('Leonard_Lopate')
plot_func('Nick_Carter_(musician)') #dismissed 
plot_func('Paul_Marciano') # investigation
plot_func('Stan_Lee') # investigation, dismissed
plot_func('Karl_Templer')


#### Calculating Expertise

# what language is in the text 
# make a dictionary of terms
dict <- c('accusation', 
          'accused', 
          'allegation', 
          'controversy', 
          'alleged', 
          'sexual violence', 
          'sexual harassment', 
          'sexual assault', 
          'sexual misconduct',
          'metoo', 
          'scandal', 
          'times up', 
          'gendered violence')

# run through the texts and aggregate counts of terms in the dictionary 
dict.df <- NULL
setwd(paste(wd, '/june_19', sep =''))
for (i in 1:length(txts)){
  name <- gsub('.txt', '', txts[i])
  txt <- paste(scan(txts[i], what = 'character'), sep = '', collapse = ' ')
  txt <- tolower(txt)
  temp.df <- NULL
  for (i in 1:length(dict)){
    key <- dict[i]
    value <- str_count(txt, key)
    kv.df <- data.frame(cbind(key, value, name))
    temp.df <- data.frame(rbind(kv.df, temp.df))
  }
  dict.df <- data.frame(rbind(temp.df, dict.df))
}

# data frame of dictionary results with zeros removed 
dict_nonz.df <- data.frame(dict.df[dict.df$value != 0,])

# overall rates of language use in the current pages based on the dictionary 
# make a function jfc
info_func <- function(x,y){
  output <- NULL 
  for (i in 1:nlevels(y)){
    key <- levels(y)[i]
    sub <- x[y == key,]
    sum <- nrow(sub)
    temp.df <- data.frame(cbind(key, sum))
    output <- data.frame(rbind(temp.df, output))
  }
  return(output)
}

# outputs 
dict_keys.df <- info_func(dict_nonz.df, dict_nonz.df$key)
dict_names.df <- info_func(dict_nonz.df, dict_nonz.df$name)

output1 <- NULL 
for (i in 1:nlevels(dict_nonz.df$key)){
  key <- levels(dict_nonz.df$key)[i]
  sub <- dict_nonz.df[dict_nonz.df$key == key,]
  sum <- length(unique(sub$name))
  temp.df <- data.frame(cbind(key, sum))
  output1 <- data.frame(rbind(temp.df, output1))
}

output2 <- NULL 
for (i in 1:nlevels(dict_nonz.df$key)){
  key <- levels(dict_nonz.df$key)[i]
  sub <- dict_nonz.df[dict_nonz.df$key == key,]
  sum <- sum(char_func(sub$value))
  temp.df <- data.frame(cbind(key, sum))
  output2 <- data.frame(rbind(temp.df, output2))
}

table_5 <- output2[order(char_func(output2$sum), decreasing = TRUE),]
write.csv(table_5, 'table_5.csv')

length(which(dict_names.df == 0))
dict_names.df[84]

# metadata.df$related_terms <- dict_names.df$sum
# write.csv(metadata.df, file = 'metadata_4.csv')

# this pulls the sections for each of the men based on the text of their current profile
# not possible to subset based on the kind of section (main, sub) because of lack of consistency in syntax, so i've collapsed all into one 
sections.df <- NULL
setwd(paste(wd, '/june_19', sep =''))
for (i in 1:length(txts)){
  name <- gsub('.txt', '', txts[i])
  txt <- paste(scan(txts[i], what = 'character'), sep = '', collapse = ' ')
  txt <- tolower(txt)
  # remove ambiguous subsectioning using gsub
  txt <- gsub(pattern = '====', replacement = '==', txt)
  txt <- gsub(pattern = '===', replacement = '==', txt)
  # this pulls all the matches in a txt that occur between the == and ==, which indicates a section
  # uses regex and stringr package, then unlists to get only results, with == removed
  s <- (str_match_all(txt, "== (.*?) =="))
  temp.df <- data.frame(name, 'sections' = s[[1]][,2])
  sections.df <- data.frame(rbind(sections.df, temp.df))
}

# what sections appear multiple times?
unique(sections.df$sections[duplicated(sections.df$sections)])
words <- labels(summary(sections.df$sections))[1:53]
freq <- as.numeric(summary(sections.df$sections))[1:53]



# make a wordcloud <3 
pal <- c('#E8F1F2', '#5F4E8E', '#2D3047','#6d6d6d', '#0e0e93', '#7A9E7E', '#1C3F23', '#870000')
wordcloud(words = words, freq = freq, random.order=FALSE, max.words = 25, scale=c(4,.5), colors = pal[2:8], fixed.asp = TRUE)

          
### ARCHIVE ###


# Impact of References / Race / Violence / Number of Reporters

# References
sum(metadata.df$refs_start)
sum(metadata.df$refs_end)

summary((metadata.df$refs_end)-(metadata.df$refs_start))

sum(metadata.df$refs_start[metadata.df$race != 'white'])
sum(metadata.df$refs_end[metadata.df$race != 'white'])

sum(metadata.df$refs_end[metadata.df$race == 'white'])-sum(metadata.df$refs_start[metadata.df$race == 'white'])

# Violence
summary(metadata.df$refs_end[metadata.df$violence == 'a']-metadata.df$refs_start[metadata.df$violence == 'a'])
summary(metadata.df$refs_end[metadata.df$violence == 'h']-metadata.df$refs_start[metadata.df$violence == 'h'])

# Race
dif_b <- (metadata.df$refs_end[metadata.df$race != 'white']-metadata.df$refs_start[metadata.df$race != 'white'])
dif_w <- (metadata.df$refs_end[metadata.df$race == 'white']-metadata.df$refs_start[metadata.df$race == 'white'])
summary(dif_b)
summary(dif_w)
wilcox.test(metadata.df$refs_end, metadata.df$refs_now)

hist(metadata.df$refs_start)
hist(metadata.df$refs_now)

# Number of Reporters
summary(metadata.df$refs_end[metadata.df$no.reporters == 1]-metadata.df$refs_start[metadata.df$no.reporters == 1])
summary(metadata.df$refs_end[which(metadata.df$no.reporters > 1)]-metadata.df$refs_start[which(metadata.df$no.reporters > 1)])

metadata.df$page[which.max((metadata.df$refs_end)-(metadata.df$refs_start))]
metadata.df$page[which.min((metadata.df$refs_end)-(metadata.df$refs_start))]

summary(all.df$range[all.df$edits_bot != 0])
summary(all.df$range[all.df$edits_user != 0])

summary(all.df$bytes[all.df$edits_user != 0])

length(which(all.df$bytes[all.df$edits_bot != 0] == 0))/
  (length(which(all.df$bytes[all.df$edits_bot != 0] == 0))+length(which(all.df$bytes[all.df$edits_bot != 0] != 0)))

length(which(all.df$bytes[all.df$edits_user != 0] == 0))/(length(which(all.df$bytes[all.df$edits_user != 0] != 0))+length(which(all.df$bytes[all.df$edits_user != 0] == 0)))

pp <- c(26, 1, 20, 3,18,6,30,18,30,0,1,1,8,5,0,6,0,1,1,0,30,25,1,0)



