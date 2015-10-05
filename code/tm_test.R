library(wordcloud)
library(ggthemes)
library(ggplot2)
library(Rgraphviz)
library(tm)
library(DT)
library(dplyr)

options(stringsAsFactors = F)

docs <- Corpus(DirSource(directory = 'txt'), readerControl = list(language="lat"))


source('code/functions/tm_functions.R')
source('code/functions/tm_term_functions.R')

junk <- c('will','we',
          'fi consulting', 'fi team')


txt <- sapply(content(docs), function(x){
  x[1] %>% paste(sep = '\n')
})

title <- sapply(content(docs), function(x){
  x[[2]][['id']]
})


news <- data_frame(title=title ,txt = txt)

save(docs, news, file = 'cache/news.RData')



# print wordcloud:
df_cloud <- get_unigrams_df(txt, junk, max.rows = 300)
set.seed(995)
png('output/wordcloud.png',width = 900, height = 850, res = 72*3)
wordcloud(df_cloud$word, df_cloud$freq, scale=c(1.5,0.5), max.words = 200,
          random.order=FALSE, rot.per=.35,  
          use.r.layout=FALSE,colors=brewer.pal(8, 'Dark2'))
dev.off()
shell.exec('output\\wordcloud.png')




# Term Analysis
#####################################################################

term <- 'reporting'
term_tm <- get_term_tm(news, term, junk, clust.method = 'complete', min.freq = 1000)

ggplot(data = term_tm$df, aes(freq, corr)) +
  theme_few() +
  geom_point(color = 'blue') +
  labs(title=paste0("Terms associated with the word '", term,"'"), 
       x='Frequency',y='Correlation') +
  geom_text(aes(label=ifelse(freq>=100 | (corr>=0.20),
                             word,'')),hjust=-0.1,vjust=-0.1)


plot(term_tm$tdm, 
     terms = term_tm$dict %>% head(10), 
     corThreshold = term_tm$df$corr[2], 
     y='dot', 
     attrs=list(node=list(fontsize=42, 
                          fillcolor='lightsteelblue')))


plot(term_tm$clust, main=NULL)
rect.hclust(term_tm$clust, k=7, border="red")


# cluster heatmap
library(d3heatmap)
d3heatmap(term_tm$mat, scale = "row", colors = 'Spectral', k_col = 10, k_row = 10, 
          hclustfun = function(x) hclust(x, method = 'ward.D'))

# docs table
term.news.tbl <- 
  term_tm$news %>% 
  select(-txt)

datatable(term.news.tbl,
          class = 'row-border stripe compact order-column hover',
          rownames = T, escape = F)


# Bigrams
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm.bi <- 
  docs %>% 
  tm_map(removePunctuation) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords('english')) %>% 
  tm_map(removeWords, junk) %>% 
  tm_map(stripWhitespace) %>% 
  TermDocumentMatrix(control = list(tokenize = BigramTokenizer)) 
dim(tdm.bi)

df.bi <- 
  data.frame(word = rownames(tdm.bi),
             freq = rowSums(as.matrix(tdm.bi))) %>%
  arrange(-freq)

set.seed(821)
png('wordcloud_bi.png',width = 900, height = 850, res = 72*3)
wordcloud(df.bi$word, df.bi$freq, scale=c(1.5,0.5),max.words = 300,
          random.order=FALSE, rot.per=0.0,  
          use.r.layout=FALSE,colors=brewer.pal(8, 'Dark2'))
dev.off()
shell.exec('wordcloud_bi.png')




# trigrams
TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdm.tri <- 
  docs %>% 
  tm_map(removePunctuation) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords('english')) %>% 
  tm_map(removeWords, junk) %>% 
  tm_map(stripWhitespace) %>% 
  TermDocumentMatrix(control = list(tokenize = TrigramTokenizer))
dim(tdm.tri)

df.tri <- data.frame(word = rownames(tdm.tri),
                     freq = rowSums(as.matrix(tdm.tri))) %>%
  arrange(-freq)

set.seed(832)
png('wordcloud_tri.png',width = 900, height = 850, res = 72*3)
wordcloud(df.tri$word, df.tri$freq, scale=c(0.65,0.15),
          max.words = 250,
          random.order=FALSE, rot.per=0.0,  
          use.r.layout=FALSE,colors=brewer.pal(8, 'Dark2'))
dev.off()
shell.exec('wordcloud_tri.png')

