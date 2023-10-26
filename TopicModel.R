# BAN 432
# Group Assignment 2
# Group 14

# Load libraires 
require(tm)
require(topicmodels)
require(wordcloud)
require(udpipe)
require(dplyr)
require(slam)
# Load data
load("congressional_records.RData")
tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# Function to clean the documents
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
# Preproccessing of data

#processed_docs <- lapply(crec$strText, process_document)
#save(processed_docs, file = "ProcessedData.RData")
load("ProcessedData.Rdata")

#Creating a DTM
corpus <- Corpus(VectorSource(processed_docs))
dtm <- DocumentTermMatrix(corpus,
                          control = list(
                            wordLengths = c(4, 20),
                            bounds = list(global = c(100,5000))))


dtm <- dtm[row_sums(dtm) > 10,]
dim(dtm)

# 2. Model Estimation
num_topics <- 50 # Decide on the number
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

