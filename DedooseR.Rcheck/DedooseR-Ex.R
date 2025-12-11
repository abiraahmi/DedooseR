pkgname <- "DedooseR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('DedooseR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("clean_data")
### * clean_data

flush(stderr()); flush(stdout())

### Name: clean_data
### Title: Clean and prepare qualitative excerpts for export
### Aliases: clean_data

### ** Examples

## Not run: 
##D result <- clean_data(
##D   excerpts = excerpts_raw,
##D   preferred_coders = c("CoderA", "CoderB"),
##D   rename_vars = list(new_name = "old_name"),
##D   relabel_vars = list(old_name = "new variable label"),
##D   output_path = "cleaned_excerpts.dta",
##D   output_type = "dta"
##D )
##D 
##D # Access cleaned data and codebook
##D head(result$data)
##D head(result$codebook)
## End(Not run)




cleanEx()
nameEx("compare_saturation")
### * compare_saturation

flush(stderr()); flush(stdout())

### Name: compare_saturation
### Title: Compare Code Saturation Across Threshold Sets
### Aliases: compare_saturation

### ** Examples

# Example data: excerpts with coded logical columns
set.seed(123)
excerpts <- data.frame(
  media_title = rep(paste0("Interview_", 1:5), each = 3),
  code_A = sample(c(TRUE, FALSE), 15, replace = TRUE),
  code_B = sample(c(TRUE, FALSE), 15, replace = TRUE),
  code_C = sample(c(TRUE, FALSE), 15, replace = TRUE)
)

# Create a code summary table (from your package function)
code_summary <- create_code_summary(excerpts, output_type = "tibble")

# Define two saturation threshold sets
thresholds_list <- list(
  Liberal = list(code_count = 3, prop_media_title = 0.2),
  Strict  = list(code_count = 5, prop_media_title = 0.5)
)

# Compare saturation (table only)
compare_saturation(code_summary, thresholds_list)

# Compare and plot using proportions
res <- compare_saturation(code_summary, thresholds_list, plot = TRUE, plot_metric = "prop")
res$plot

# Compare and plot both metrics with dual y-axes
res2 <- compare_saturation(code_summary, thresholds_list, plot = TRUE, plot_metric = "both")
res2$plot




cleanEx()
nameEx("cooccur")
### * cooccur

flush(stderr()); flush(stdout())

### Name: cooccur
### Title: Create a Code Co-occurrence Matrix and Network Plot
### Aliases: cooccur

### ** Examples

# Example 1: Basic co-occurrence matrix from excerpts
df <- data.frame(
  media_title = c("Doc1", "Doc2", "Doc3"),
  c_hope = c(1, 0, 1),
  c_family = c(1, 1, 0),
  c_school = c(0, 1, 1)
)

result <- cooccur(
  excerpts = df,
  scale = "count",
  output = "tibble",
  plot = TRUE
)

result$matrix  # Co-occurrence matrix
result$plot    # Network plot

# Example 2: Use descriptive labels from a codebook and proportions in the table
codebook <- data.frame(
  variable = c("c_hope", "c_family", "c_school"),
  label = c("Hope & Optimism", "Family Connectedness", "School Belonging")
)

labeled_result <- cooccur(
  excerpts = df,
  use_labels = TRUE,
  codebook = codebook,
  scale = "prop",
  output = "kable",
  plot = TRUE
)

labeled_result$matrix
labeled_result$plot




cleanEx()
nameEx("create_code_summary")
### * create_code_summary

flush(stderr()); flush(stdout())

### Name: create_code_summary
### Title: Create a Summary Table and Plot of Code Frequencies
### Aliases: create_code_summary

### ** Examples

# Example 1: Basic usage without a codebook
df <- data.frame(
  media_title = c("Doc1", "Doc2", "Doc3", "Doc4"),
  code_a = c(TRUE, FALSE, TRUE, TRUE),
  code_b = c(FALSE, TRUE, TRUE, FALSE)
)

create_code_summary(df, plot = TRUE)

# Example 2: Using a codebook for readable labels
codebook <- data.frame(
  variable = c("code_a", "code_b"),
  label = c("Community Engagement", "Policy Support")
)

create_code_summary(
  df,
  use_labels = TRUE,
  codebook = codebook,
  plot = TRUE,
  plot_metric = "both"
)

# Example 3: Excluding a code and outputting as datatable
create_code_summary(
  df,
  exclude = "code_b",
  output_type = "datatable"
)




cleanEx()
nameEx("recode_themes")
### * recode_themes

flush(stderr()); flush(stdout())

### Name: recode_themes
### Title: Recode logical code variables and optionally relabel them
### Aliases: recode_themes

### ** Examples

# Example dataset
df <- data.frame(
c_support = c(TRUE, FALSE, TRUE),
c_assist = c(FALSE, TRUE, TRUE),
c_anxiety = c(TRUE, FALSE, FALSE),
c_pressure = c(FALSE, TRUE, FALSE)
)

# Define recodes
recode_plan <- list(
c_help = c("c_support", "c_assist"),
c_stress = c("c_anxiety", "c_pressure")
)

# Run recode_themes() with new labels
result <- recode_themes(
data = df,
recodes = recode_plan,
relabel_vars = list(
c_help = "Mentions of helping or supporting others",
c_stress = "Mentions of stress or pressure"
)
)

# Extract recoded data and codebook
data_recode <- result$data_recode
codebook_recode <- result$codebook_recode

# View recoded dataset
head(data_recode)

# View codebook
head(codebook_recode)




cleanEx()
nameEx("set_saturation")
### * set_saturation

flush(stderr()); flush(stdout())

### Name: set_saturation
### Title: Compute and Visualize Code Saturation
### Aliases: set_saturation

### ** Examples

# Example dataset
code_counts <- tibble::tibble(
  code = c("Belonging", "Resilience", "Stress", "Hope"),
  count = c(15, 10, 8, 5),
  n_media_titles = c(8, 6, 5, 3)
)

# Basic usage (returns a tibble)
set_saturation(code_counts)

# Apply count and proportion filters, return a kable table
set_saturation(
  code_counts,
  total_media_titles = 10,
  table_min_count = 5,
  table_min_prop = 0.3,
  output_type = "kable"
)

# Generate a plot of proportions
res <- set_saturation(
  code_counts,
  total_media_titles = 10,
  plot = TRUE,
  plot_metric = "prop"
)
res$plot

# Plot both count and proportion using dual y-axes
res <- set_saturation(
  code_counts,
  total_media_titles = 10,
  plot = TRUE,
  plot_metric = "both",
  fill_color = "darkgreen"
)
res$plot




cleanEx()
nameEx("view_excerpts")
### * view_excerpts

flush(stderr()); flush(stdout())

### Name: view_excerpts
### Title: View Qualitative Excerpts by Code
### Aliases: view_excerpts

### ** Examples

library(dplyr)

df <- tibble::tibble(
  excerpt = c(
    "I felt supported by my peers.",
    "Teachers really listened to us.",
    "I learned a lot about myself."
  ),
  c_support = c(TRUE, TRUE, FALSE),
  c_growth = c(FALSE, FALSE, TRUE)
)
attr(df$c_support, "label") <- "Peer/Teacher Support"
attr(df$c_growth, "label") <- "Personal Growth"

# View excerpts interactively
if (interactive()) view_excerpts(df)




cleanEx()
nameEx("wordcloud")
### * wordcloud

flush(stderr()); flush(stdout())

### Name: wordcloud
### Title: Generate a word cloud for excerpts by code
### Aliases: wordcloud

### ** Examples

library(dplyr)
df <- tibble::tibble(
  excerpt = c(
    "I felt connected to peers and friends.",
    "We should normalize conversations about mental health.",
    "My teachers helped me belong at school.",
    "I am comfortable talking about suicide prevention."
  ),
  c_belonging = c(TRUE, FALSE, TRUE, FALSE),
  c_destigmatization = c(FALSE, TRUE, FALSE, FALSE)
)

# Word cloud for belonging excerpts
wordcloud(df, "c_belonging")

# With custom stop words
wordcloud(df, "c_belonging", custom_stopwords = c("connected", "school"))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
