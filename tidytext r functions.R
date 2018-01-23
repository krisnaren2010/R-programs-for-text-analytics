if (!require(igraph)) {install.packages("igraph")}
if (!require(tidytext)) {install.packages("tidytext")}
if (!require(data.table)) {install.packages("data.table")}
if (!require(stringr)) {install.packages("stringr")}
if (!require(tm)) {install.packages("tm")}
#if (!require(RWeka)) {install.packages("RWeka")}
if (!require(wordcloud)) {install.packages("wordcloud")}
if (!require(ggplot2)) {install.packages("ggplot2")}
if (!require(dplyr)) {install.packages("dplyr")}
library(tidytext)
library(data.table)
library(stringr)
library(tm)
#library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)
library(igraph)
library(dplyr)


################# Function 1 - TExt Cleaning



text_cleaner <- function(corpus,
                       user_stopwords) ####User Defines Stop words
{ 
  require(tidytext)
  library(tidytext)
  require(tm)
  library(dplyr)
  library(tm)
  stop_wrds     <-   unique(c(user_stopwords , stop_words$word))
  text          <-   corpus
  text          <-   gsub("<.*?>", " ", text)              # regex for removing HTML tags
  text          <-   tolower(text) 
  text          <-   gsub("^\\s+|\\s+$", "", text)          # remove leading and trailing white space
  text          <-   removeWords(text,stop_wrds)
  text          <-   stripWhitespace(text)
  text          <-   gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)  #removing all extra spaces
  textdf        <-   data_frame(document = 1:length(text) , text = text) 
  return(textdf)
} 


#Function 2a :Gives DTM matrix usign Term Frequency
dtm_builder_tf <- function(x_defined){
  text_defined<-x_defined
  corpus_words<-text_defined %>% unnest_tokens(word,text) %>% count(document,word,sort = TRUE)%>% ungroup()
  total_words<-corpus_words%>%group_by(document)%>%summarize(total=sum(n))
  corpus_words<-left_join(corpus_words,total_words)
  dtm_tf <-corpus_words %>% cast_dtm(document , word, n)
  
  return(dtm_tf)
}
#Function 2b :Gives DTM matrix usign Term Frequency
dtm_builder_tf_idf <- function(x_defined){
  
  require(tidytext)
  library(tidytext)
  
  text_df<-x_defined
  corpus_words<-text_defined %>% unnest_tokens(word,text) %>% count(document,word,sort = TRUE)%>% ungroup()
  total_words<-corpus_words%>%group_by(document)%>%summarize(total=sum(n))
  corpus_words<-left_join(corpus_words,total_words)
  corpus_words<-corpus_words%>% bind_tf_idf(word,document,n)
  dtm_tf_idf <-corpus_words %>% cast_dtm(document , word, idf)
  
  return(dtm_tf_idf)
}
##Word Cloud Function
  Plot_Wordc = function(dtm){    # plot wordcloud func opens. 
    library(wordcloud)
    dtm = as.matrix(dtm)  
    dtm_colsum = apply(dtm, 2, sum)  
    
    min1 = min(120, length(dtm_colsum))  # no more than 100 terms in wordcloud  
    words = colnames(dtm)[1:min1]  
    freq = 10 * dtm_colsum/mean(dtm_colsum)  # rescaling for better viewing
    wordcloud(words,  # wordcloud func begins
              freq,           
              scale = c(8, 0.3),  # can change this to adjust font scale
              colors=1:15)        # randomly choose between 15 colors
  } # func ends
  
  ################  Bar Graphs Fuction #########
  
  ######### ggplot
  Plot_BarChart = function(dtm){
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    wf <- data.frame(word=names(freq), freq=freq)
    # Plot Histogram
    a1 <- subset(wf, freq>20)    %>%
      ggplot(aes(word, freq)) +
      geom_bar(stat="identity", fill="darkred", colour="darkgreen") +
      theme(axis.text.x=element_text(angle=45, hjust=1))
    plot(a1, 
         layout = layout.kamada.kawai)
    
  } 
  
  
  ################ COG
  
  Plot_distill_cog = function(dtm, # input DTM  
                              title, # title for the graph
                              s,    # no. of central nodes
                              k1){  # max no. of connections  
    s<-5
    k1<-10
    
    dtm1 = as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
    adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
    diag(adj.mat) = 0     # no self-references. So diag is 0.
    a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
    adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]]) 
    mat1 = adj.mat
    
    library(igraph)
    a = colSums(mat1) # collect colsums into a vector obj a
    b = order(-a)     # nice syntax for ordering vector in decr order  
    
    mat2 = mat1[b, b]     # order both rows and columns along vector b
    
    diag(mat2) =  0
    
    ## +++ go row by row and find top k adjacencies +++ ##
    
    wc = NULL
    
    for (i1 in 1:s){ 
      thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
      mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
      mat2[i1, mat2[i1,] > 0 ] = 1
      word = names(mat2[i1, mat2[i1,] > 0])
      mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
      wc = c(wc,word)
    } # i1 loop ends
    
    
    mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
    ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
    mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
    graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
    graph = simplify(graph) 
    V(graph)$color[1:s] = "green"
    V(graph)$color[(s+1):length(V(graph))] = "pink"
    
    graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
    
    plot(graph, 
         layout = layout.kamada.kawai, 
         main = title)
    
   # Plot_distill_cog  func ends
  }
  total_plot <- function(x){  
    dtm<-x
    print(dtm%>%Plot_Wordc() )
    print(dtm%>%Plot_BarChart())
    print(dtm%>%Plot_distill_cog(title,s,k1))
    }
# x <-readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
# 
# y <- text_cleaner(x,c("nokia","phone")) %>% dtm_builder_tf()%>% total_plot()
# z <- text_cleaner(x,c("nokia","phone")) %>% dtm_builder_tf_idf()%>%total_plot()
# system.time(text_cleaner(x,c("nokia","phone")) %>% dtm_builder_tf()%>% total_plot())
# system.time(text_cleaner(x,c("nokia","phone")) %>% dtm_builder_tf_idf()%>%total_plot())
