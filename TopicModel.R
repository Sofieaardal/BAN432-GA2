# BAN 432
# Group Assignment 2
# Group 14

# Load libraires 
library(tm)
library(topicmodels)
library(dplyr)

# Load data
load("congressional_records.RData")


# Preproccessing of data

corpus <- Corpus(VectorSource(crec))
dtm <- DocumentTermMatrix(x = corpus, 
                          control = list(
                            removePunctuation = T, 
                            tolower= T, 
                            stopwords = T, 
                            removeNumbers = T,
                            bounds = list(global = c(10, 100))
                          )
                        )
dtm <- removeSparseTerms(dtm, 0.995) # Adjust this threshold

# 2. Model Estimation
num_topics <- 10 # Decide on the number
lda_model <- LDA(dtm, k = num_topics)

# Display top 10 terms per topic
terms(lda_model, 10)