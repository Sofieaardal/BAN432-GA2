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

#Task 2: Answering relevant questions in detail

#Question 1: 1. What are the dimensions of your final document-term-matrix (how many documents and how many terms does it contain)?
#From displaying dim(dtm) we know that the document has 44038 documents and 3288 terms

#Question 2: What is the number of topics you use?
#In our soloution we are using 50 topics, a number we chose because each topic seem to be quite coherent and focused with no need to add more topics.
# we do not have repetitive topics, so no need to reduce number either.

#Question 3: What topics are more prevalent in the early part of the sample (1995 & 1996), which topics are more prevalent in the late part?

# Extracting year info
years <- as.numeric(format(crec$date, "%Y"))

# Indices of the rows kept in dtm to align new dtm 
kept_indices <- which(row_sums(as.matrix(dtm)) > 10)

# Filter 'years' to only include these rows
aligned_years <- years[kept_indices]

# Indices of documents from 1995 and 1996
early_indices_aligned <- which(aligned_years %in% c(1995, 1996))

# Filter the DTM to keep only documents from early years (e.g., 1995 and 1996)
dtm_early <- dtm[early_indices_aligned, ]

# Run LDA model on the early-period DTM
topic_early = LDA(dtm_early,
                  k = num_topics,
                  method = "Gibbs", 
                  control = list(
                    seed = 1234,
                    burnin = 100,
                    iter = 1000, 
                    keep = 1, 
                    save = F, 
                    verbose = 10))

# Filter the DTM to keep only documents from late years (e.g., 2021 and 2022)
late_indices_aligned = which(aligned_years %in% c(2021, 2022))
dtm_late = dtm[late_indices_aligned, ]

# Run LDA model on the late-period DTM
topic_late = LDA(dtm_late,
                 k = num_topics,
                 method = "Gibbs",
                 control = list(
                   seed = 1234,
                   burnin = 100, 
                   iter = 1000,
                   keep = 1,
                   save = F, 
                   verbose = 10))

# Display top 10 terms per topic
apply(topic_early@beta, 1, function(x) head(topic_early@terms[order(x, decreasing = T)],10))

# Display top 10 terms per topic
apply(topic_late@beta, 1, function(x) head(topic_late@terms[order(x, decreasing = T)],10))

# Calculate term frequencies for early and late periods
tf_early = colSums(as.matrix(dtm_early))
tf_late = colSums(as.matrix(dtm_late))

# Sort term frequencies in descending order
tf_early_sorted <- sort(tf_early, decreasing = TRUE)
tf_late_sorted <- sort(tf_late, decreasing = TRUE)

# Filter top 40 terms for early and late periods
terms_top_40_early <- names(tf_early_sorted)[1:40]
terms_top_40_late <- names(tf_late_sorted)[1:40]

# Generate word cloud for early period
wordcloud(words = terms_top_40_early,
          freq = tf_early_sorted[1:40],
          random.order = F,
          scale = c(4, 1))

# Generate word cloud for late period
wordcloud(words = terms_top_40_late,
          freq = tf_late_sorted[1:40],
          random.order = F,
          scale = c(4, 1))

# Calculate term frequencies for early and late periods
#tf_early = colSums(as.matrix(dtm[early_indices_aligned, ]))
#tf_late = colSums(as.matrix(dtm[late_indices_aligned, ]))

# Create a data frame to hold term and frequencies
#tf_df = data.frame(Term = names(tf_early), Early = as.numeric(tf_early), Late = as.numeric(tf_late))

# Calculate the change in term frequency from early to late
#tf_df$Change = tf_df$Late - tf_df$Early

# Sort terms by change in frequency
#tf_df_sorted = tf_df[order(tf_df$Change), ]

# Filter terms that have a change below a certain threshold
#threshold = -10  
#tf_df_filtered = tf_df_sorted[tf_df_sorted$Change < threshold, ]


#As a result of these calculations we see that the most commonly mentioned topics  in the early sample were
#industry & technology, insurance policies, water and environment, war on drugs and american history.
#in the late sample the most common topics seemed to be social issues, terrorism, healthcare, national defense and education.
#Additionaly we see the biggest decrease in usage of words like welfare, medicare, conference, appropriation and education.

#4. What topics are mostly associated with one political party? Do this analysis for both the early and the late sample separately.

# Create a data frame with both party and year information
party_year_data <- data.frame(Party = crec$party[kept_indices ], Year = aligned_years)

# Subsetting the DTM based on both party and year
early_party_year_indices <- which(party_year_data$Year %in% c(1995, 1996))
dtm_early_political <- dtm[early_party_year_indices, ]

# For the late sample
late_party_year_indices <- which(party_year_data$Year %in% c(2021, 2022))
dtm_late_political <- dtm[late_party_year_indices, ]

# Run LDA model on the early-period DTM with political parties
topic_parties_early = LDA(dtm_early_political,
                  k = num_topics,
                  method = "Gibbs", 
                  control = list(
                    seed = 1234,
                    burnin = 100,
                    iter = 1000, 
                    keep = 1, 
                    save = F, 
                    verbose = 10))


# Run LDA model on the late-period DTM with political parties
topic_parties_late = LDA(dtm_late_political,
                  k = num_topics,
                  method = "Gibbs", 
                  control = list(
                    seed = 1234,
                    burnin = 100,
                    iter = 1000, 
                    keep = 1, 
                    save = F, 
                    verbose = 10))

# Function to get top 10 terms
get_top_terms <- function(dtm_subset) {
  term_freqs <- colSums(as.matrix(dtm_subset))
  sorted_terms <- sort(term_freqs, decreasing = TRUE)
  top_terms <- names(sorted_terms)[1:10]
  return(top_terms)
}

# Get top 10 terms for each subset
top_terms_early_dem <- get_top_terms(dtm[early_dem_indices, ])
top_terms_early_rep <- get_top_terms(dtm[early_rep_indices, ])
top_terms_late_dem <- get_top_terms(dtm[late_dem_indices, ])
top_terms_late_rep <- get_top_terms(dtm[late_rep_indices, ])

# Print or visualize them
print(paste("Top terms for Early Democrats: ", paste(top_terms_early_dem, collapse = ", ")))
print(paste("Top terms for Early Republicans: ", paste(top_terms_early_rep, collapse = ", ")))
print(paste("Top terms for Late Democrats: ", paste(top_terms_late_dem, collapse = ", ")))
print(paste("Top terms for Late Republicans: ", paste(top_terms_late_rep, collapse = ", ")))

#5. Bonus: What topics were mostly associated with one party early in the sample, but with the opposite party late in the sample?
