topicmodel <- function(data, code, n_topics = 2, n_terms = 10,
                       custom_stopwords = NULL, sparse_threshold = 0.99) {
  stopifnot("excerpt" %in% names(data))
  stopifnot(code %in% names(data))

  # filter excerpts
  excerpts <- data[data[[code]] == TRUE, "excerpt", drop = TRUE]
  if (length(excerpts) == 0) stop("No excerpts found for code: ", code)

  # combine tidytext stopwords + user-supplied
  all_stopwords <- unique(c(tidytext::stop_words$word, custom_stopwords))

  # create corpus
  corpus <- tm::VCorpus(tm::VectorSource(excerpts))

  # build DTM with stopword removal
  dtm <- tm::DocumentTermMatrix(
    corpus,
    control = list(
      removePunctuation = TRUE,
      stopwords = all_stopwords,
      removeNumbers = TRUE,
      wordLengths = c(3, Inf)
    )
  )

  # remove sparse terms
  dtm <- tm::removeSparseTerms(dtm, sparse_threshold)

  if (ncol(dtm) == 0) {
    stop("No terms left in the DocumentTermMatrix. Try fewer stopwords or adjust sparse_threshold.")
  }

  # run LDA
  lda_model <- topicmodels::LDA(dtm, k = n_topics, control = list(seed = 1234))

  if (length(lda_model@terms) == 0) {
    stop("LDA model has no terms. Check preprocessing.")
  }

  # extract top terms
  terms_mat <- topicmodels::terms(lda_model, n_terms)

  # reshape to wide format
  top_terms <- as.data.frame(terms_mat, stringsAsFactors = FALSE)
  top_terms <- tibble::tibble(rank = seq_len(n_terms), top_terms)
  names(top_terms) <- c("rank", paste0("topic_", seq_len(n_topics)))

  # posterior probabilities
  post <- topicmodels::posterior(lda_model)
  posterior_probs <- as.data.frame(post[["topics"]])
  names(posterior_probs) <- paste0("topic_", seq_len(n_topics))

  posterior <- tibble::tibble(
    excerpt = excerpts,
    !!!posterior_probs
  )

  return(list(top_terms = top_terms, posterior = posterior))
}

# Testing

# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
clean_data <- clean_data(filepath, preferred_coders)
excerpts <- clean_data$data
codebook <- clean_data$codebook

tm_emerging_leader <- topicmodel(excerpts,
                                 "c_emerging_leader",
                                 n_topics = 2,
                                 n_terms = 25,
                                 custom_stopwords = c("directing", "change", "theyre", "yeah"))
