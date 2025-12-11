wordcloud <- function(data, code, max_words = 100, custom_stopwords = NULL) {
  stopifnot("excerpt" %in% names(data))
  stopifnot(code %in% names(data))

  # filter excerpts for selected code
  excerpts <- data[data[[code]] == TRUE, "excerpt", drop = TRUE]
  if (length(excerpts) == 0) {
    stop("No excerpts found for code: ", code)
  }

  # build stopword list
  all_stopwords <- tidytext::stop_words$word
  if (!is.null(custom_stopwords)) {
    all_stopwords <- unique(c(all_stopwords, custom_stopwords))
  }

  # tokenize, remove stop words and punctuation
  tokens <- tibble::tibble(text = excerpts) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::filter(!word %in% all_stopwords) %>%
    dplyr::filter(!grepl("^[[:punct:]]+$", word)) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::slice_head(n = max_words)

  if (nrow(tokens) == 0) {
    stop("No valid words found for code: ", code)
  }

  # make word cloud
  wordcloud2::wordcloud2(tokens)
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

# Generate word cloud
knowledge_awareness_wordcloud <- wordcloud(excerpts, "c_knowledge_awareness",
          max_words = 100,
          custom_stopwords = c("racall", "stuff", "everyone's"))
