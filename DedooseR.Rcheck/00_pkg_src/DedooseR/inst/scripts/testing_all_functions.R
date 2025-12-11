# Step 1: Test the function with others

# Load libraries
library(DedooseR)
library(tidyverse)
library(readxl)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
df <- clean_data(filepath,
           preferred_coders,
           rename_vars = list(memo_destigmatization = "...274"),
           relabel_vars = list(title = "Memo: Destigmatization"),
           output_type = "dta")
data <- df$data
codebook <- df$codebook

# Merge codes
excerpts_merged <- recode_themes(data,
                               merges = list(
                                 c_belonging_connectedness = c(
                                   "c_sense_of_belonging", "c_sense_of_belonging_others", "c_sense_of_belonging_self",
                                   "c_sense_of_connectedness", "c_sense_of_connectedness_family",
                                   "c_sense_of_connectedness_peers", "c_sense_of_connectedness_school_community",
                                   "c_sense_of_connectedness_staff"
                                 ),
                                 c_suicide_comfort = c("c__suicide_comfort_directing_change", "c__suicide_comfort_general")
                               ),
                               relabel_vars = list(
                                 c_belonging_connectedness = "Sense of Belonging & Connectedness",
                                 c_suicide_comfort = "Suicide Comfort Conversing"
                               ))

data_merged <- excerpts_merged$data
codebook_merged <- excerpts_merged$codebook

# View excerpts
view_excerpts(data_merged)


# Create code summary
create_code_summary <- create_code_summary(data_merged,
                                           table_min_count = 40,
                                           plot = TRUE,
                                           use_labels = TRUE,
                                           codebook = codebook_merged, 
                                          output_type = "tibble")
create_code_summary$table
create_code_summary$plot

# Set saturation
out <- set_saturation(create_code_summary$table,
                      table_min_count = 10,
                      plot = TRUE,
                      plot_metric = "count") # Plot both metrics on the same graph
out$plot

# Compare saturation

# Define thresholds
thresholds_list <- list(
  Set1 = list(code_count = 20, prop_media_title = 0.2),
  Set2 = list(code_count = 40, prop_media_title = 0.4)
)

# Apply thresholds
comp_saturation <- compare_saturation(create_code_summary$table, thresholds_list,
                                      plot = TRUE,
                                      plot_metric = "both")

# Generate word cloud
wordcloud(data_merged, "c_knowledge_awareness",
         max_words = 100,
         custom_stopwords = c("recall", "stuff", "everyone's"))

# Create matrix
cooccur_matrix <- cooccur(data_merged, min_bold = 20, output = "tibble", scale = "prop")
cooccur_matrix$matrix
cooccur_matrix$plot


# Topic model
topicmodel(excerpts, "c_emerging_leader", n_topics = 2, n_terms = 25,
           custom_stopwords = c("directing", "change", "theyre", "yeah"))

## ARCHIVED: Quality indicator check
df_qual_summary <- quality_indicators(
  excerpts = excerpts,
  preferred_coders = preferred_coders,
  qual_indicators = c("Priority excerpt", "Heterogeniety"))


# Step 2: Push to package
# Restart session

## Create shell for function script
library(usethis)

# REPLACE with function name
use_r("recode_themes.R")

# Update roxygen + Paste function

# Refresh Box 

## Build > Check
devtools::check()

# Refresh Box folder

## Git
  # Stage
  # Commit
  # Push
  # On Positron run in terminal
# git add --all        # stage
# git status           # see changes
# git commit -m "fixed a warning"
# git push             # push to remote

# Update News.MD doc with changes=

# Update documentation - run below in console
devtools::load_all()
devtools::document()
# If you want to clear your current function so no conflict exists, run:
rm(list = c("summarize_codes"))

# OPTIONAL: Update version - swap patch with major, minor, or patch, depending on what it is
usethis::use_version("patch")

# Update DESCRIPTION and or NEWS with any changes

# Update README with example usage

# Create package - in console
devtools::build(vignettes = TRUE)

# Move package file into a folder!

# Run devtools::install() to rebuild & install your local package.
devtools::install(build_vignettes = TRUE)

# Run below to rebuild site
pkgdown::build_site()

# Load library, Reinstall package and Restart R
  # Remove package: remove.packages("DedooseR")
  # Install new tar file: SWAP VERSION
    # install.packages('/Users/abishankar/Library/CloudStorage/Box-Box/DedooseR (package files)/DedooseR_0.0.6.tar.gz', repos = NULL, type = "source")

# Step 3: Create function tests
# use_test(function name)
# copy description
# ask chat to write 5 tests
# load libraries: devtools, testthat, DedooseR
# to check test coverage for all functions in package:
  # covr::package_coverage(type = "tests")

# Vignettes
# create file 
