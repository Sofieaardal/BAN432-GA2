# BAN 432
# Group Assignment 2
# Group 14

# Load libraires 
require(tm)
require(topicmodels)
require(wordcloud)
require(udpipe)
require(dplyr)
library(stringr)
library(tokenizers) 
# Load data
load("congressional_records.RData")
tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

sample_data <- sample_n(crec, 1000)


process_document <- function(doc_text) {
  doc_text %>%
    removePunctuation() %>%
    removeNumbers() %>%
    removeWords(stopwords("en")) %>%
    tolower() %>%
    stripWhitespace() %>%
    str_replace_all("\\b\\w{1,2}\\b|\\b\\w{21,}\\b", "") %>%
    stripWhitespace() -> cleaned_text
  
  annotations <- udpipe_annotate(tagger, x=cleaned_text)
  data_frame <- as.data.frame(annotations)
  filtered_data <- data_frame %>%
    filter(upos %in% c("NOUN", "VERB")) %>%
    pull(lemma) %>%
    paste(collapse = " ")
  
 return(filtered_data)
}

processed_docs <- lapply(sample_data$strText, process_document)
# Preproccessing of data
corpus <- Corpus(VectorSource(processed_docs))
#corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
dtm <- DocumentTermMatrix(corpus,
                          control = list( 
                            removePunctuation = T,
                            stopwords = T,
                            stemming = F,
                            removeNumbers = T,
                            wordLengths = c(4, 20),
                            bounds = list(global = c(10,50))))


dtm <- dtm[row_sums(dtm) > 10,]
dim(dtm)

# 2. Model Estimation
num_topics <- 75 # Decide on the number
topic <- LDA(dtm,  # document term matrix
             k = num_topics, # specifify number of topics
             method = "Gibbs",
             control = list(
               seed = 1234, # eases replication
               burnin = 100,  # how often sampled before estimation recorded
               iter = 1000,  # number of iterations
               keep = 1,    # saves additional data per iteration (such as logLik)
               save = F,     # saves logLiklihood of all iterations
               verbose = 10  # report progress
             ))

# Display top 10 terms per topic
apply(topic@beta, 1, function(x) head(topic@terms[order(x, decreasing = T)],10))

