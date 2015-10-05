library(tm)
library(SnowballC)
library(Rstem)
library(lubridate)
library(dplyr)

# return a tataframe of Unigrams from CHar vector
######################################################################
get_unigrams_df <- function(txt, junk=NULL, max.rows=NA){
    
    #corpus
    corp <- get_corpus(txt, junk)
    # term-document matrix
    tdm <- TermDocumentMatrix(corp)
    # freq df
    df <- get_freq_df(tdm, max.rows = max.rows)
    
    return(df)
}


# return Corpus from char vector
##########################################################################
get_corpus <- function(txt, junk=NA){
    library(tm)
    library(dplyr)
    
    doc <- VectorSource(txt)
    corp <- Corpus(doc) %>% 
        tm_map(removePunctuation) %>% 
        tm_map(content_transformer(tolower)) %>% 
        tm_map(removeNumbers) %>% 
        tm_map(removePunctuation) %>% 
        tm_map(removeWords, stopwords('english')) %>% 
        tm_map(removeWords, junk) %>% 
        tm_map(stripWhitespace)
    return(corp)
}


# Return frequency data frame from TDM
##########################################################################
get_freq_df <- function(tdm, stem=TRUE, max.rows=NA){
    
    df <- data.frame(word = rownames(tdm),
                     freq = rowSums(as.matrix(tdm))) 
    if(stem) 
        df <- df %>% 
            mutate(stem = Rstem::wordStem(word, language = "english")) %>% 
            group_by(stem) %>%
            summarise(word = first(word,order_by = -freq),
                      freq = sum(freq)) %>% 
            ungroup() %>% 
            select(-stem) 
    
    df <- arrange(df, -freq)
    
    if(!is.na(max.rows)){
        df <- df %>% 
            filter(row_number()<=max.rows)
    }
    
    return(df)
}




# Return TDM where each document is entire quarter
#####################################################################
get_term_period_matrix <- function(news, junk=NA, sparse=NA, stem=T){
    
    # prep dataframe
    df <- data_frame(
        qtr = lubridate::quarter(news$date, with_year = T),
        txt = paste(news$title, news$txt))

    # char vector by quarter
    qtr.txt <- tapply(df$txt, 
                      df$qtr, 
                      function(x) paste(x, collapse=' '))
    
    # tdm
    qtr.mat <- get_corpus(qtr.txt, junk = junk) %>% 
        TermDocumentMatrix()
    
    # remove sparse terms
    if(!is.na(sparse))
        qtr.mat <- removeSparseTerms(x = qtr.mat, sparse = sparse)
    
    qtr.mat <- as.matrix(qtr.mat)
    
    colnames(qtr.mat) <- names(qtr.txt)
    
    # stem
    if(stem) qtr.mat <- stem_tdm(mat = qtr.mat)
    
    return(qtr.mat)
}




# Stem TDM
#####################################################################
stem_tdm <- function(mat){
    
    word = row.names(mat)
    freq = rowSums(mat)
    stem = Rstem::wordStem(word, language = "english")
    
    # stem to most freq word cross-walk
    dict <- data_frame(word, freq) %>% 
        mutate(stem = Rstem::wordStem(word, language = "english")) %>% 
        group_by(stem) %>% 
        summarise(word = first(word,order_by = -freq)) %>% 
        ungroup()
    
    # Aggregate all columns by stem
    df <- mat %>% 
        as.data.frame() %>% 
        mutate(stem = stem) %>% 
        group_by(stem) %>%
        summarise_each(funs(sum)) %>% 
        ungroup() %>% 
        # add most frequent word for each stem
        left_join(dict, by='stem')
    
    # convert back to matrix
    row.names(df) <- df$word
    mat <- select(df, -stem,-word) %>% as.matrix()
    
    return(mat)
}
