


get_term_tm <- function(news, term, junk=NA, min.freq=35,clust.method='complete'){
    
    # subset and clean news
    news <- news %>% 
        filter(grepl(term, txt, ignore.case = T)) %>% 
        mutate(txt = gsub("'s ", " ", txt, ignore.case = T))
    
    # TDM
    txt <- news$txt
    corpus <- get_corpus(txt, junk = junk)
    tdm <- TermDocumentMatrix(corpus)
    
    
    assoc <- findAssocs(tdm, term, corlimit = 0.10) %>% 
        .[[1]] %>% 
        data_frame(word = names(.), corr=.)

    df <- get_freq_df(tdm, stem = F) %>% 
        right_join(assoc, by='word') %>% 
        filter(freq>=min.freq) %>% 
        group_by(corr) %>% 
        mutate(q = ecdf(freq)(freq)) %>% 
        ungroup() %>% 
        filter(q>=0.8) %>% 
        select(-q)
    
    dict <- c(term, df$word)
    
    mat <- corpus %>% 
        TermDocumentMatrix(list(dictionary=dict)) %>% 
        as.matrix()
    
    # Clustering
    mat.scale <- mat %>% t() %>% scale() %>% t()
    dist <- dist(mat.scale)
    clust <- hclust(dist, method = clust.method)
    
    return(list(
        news = news,
        tdm = tdm,
        dict = dict,
        df = df,
        mat = mat,
        clust = clust))
}