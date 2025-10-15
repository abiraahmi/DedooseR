
# DedooseR 

<!-- badges: start -->
<!-- badges: end -->

**DedooseR** is an R package that connects with 
[Dedoose](https://www.dedoose.com/) to support the analysis of qualitative data.
It was built to help researchers streamline workflows, explore qualitative data flexibly,
and conduct qualitative coding and analysis with rigor. 

## Key Features

DedooseR currently allows you to:

- Clean data exported from Dedoose and populate a codebook
- Create code summaries with code counts and proportions across transcripts 
while defining min frequencies and proportions to table and plot!
- Set a saturation criteria and plot it
- Plot and play with saturation thresholds
- Produce and plot code co-occurrence heatmaps  
- Create network maps between codes
- Generate word clouds

### Coming Soon
- Run topic models to understand the distribution of topics and top terms within them for selected codes
- Creating reliability tests and calculating reliability per coder (Cohen's kappa) and between coders 
(Fleiss' kappa)

## Why This Package?

Ongoing challenges in qualitative research include defining what constitutes 
high-quality data and demonstrating transparency in how saturation is 
reached (Small & Calarco, 2022). Informed by guidelines for high-quality 
qualitative research (Hennink & Kaiser, 2022), DedooseR allows you to
better understand your data with quality tags in Dedoose like:

- The **concreteness** of excerpts (priority code)
- The **heterogeneity** within codes  (heterogeneity code)

By tagging these indicators in Dedoose and exploring them in R, 
this allows for gain greater confidence in both the **depth** and **diversity** 
of datasets.

## Installation

You can install the development version of DedooseR from 
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("abiraahmi/DedooseR")
```

## Example

This is a basic example which shows you how you may the package:

``` r
library(DedooseR)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
clean_data <- clean_data(filepath,
           preferred_coders,
           rename_vars = list(memo_destigmatization = "...274"),
           relabel_vars = list(title = "Memo: Destigmatization"))
excerpts <- clean_data$data
codebook <- clean_data$codebook

# Recode themes
excerpts_recoded <- recode(data,
                               recodes = list(
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

data_recode <- excerpts_recoded$data_recode
codebook_recode <- excerpts_recoded$codebook_recode

# Create code summary
create_code_summary <- create_code_summary(data_recode,
                                    table_min_count = 40,
                                    table_min_prop = 0.25,
                                    plot = TRUE,
                                    plot_metric = "prop")
                           
# View excerpts in filterable table
view_excerpts(excerpts)                           

# Set saturation 
out <- set_saturation(create_code_summary, 
table_min_count = 40,
table_min_n_media_titles = 0.25,
plot = TRUE, 
plot_metric = "both") # Plot both metrics on the same graph
out$plot


# Compare saturation

# Define thresholds list
thresholds_list <- list(
  "Liberal" = list(code_count = 40, prop_media_title = 0.2),
  "Strict"  = list(code_count = 60, prop_media_title = 0.6)
)

# Compare against thresholds
saturation_comparison <- compare_saturation(code_summary, thresholds_list,
                                            plot = TRUE,
                                            plot_metric = "both")
# View table
saturation_comparison$results

# View plot
saturation_comparison$plot

# Generate word cloud
wordcloud(excerpts, "c_knowledge_awareness", 
         max_words = 100,
         custom_stopwords = c("racall", "stuff", "everyone's"))

# Create matrix, set threshold and plot
cooccur_01 <- cooccur(data_recode,
              matrix_threshold = 15)
cooccur_01$matrix
cooccur_01$plot

# COMING SOON: Topic models
topicmodel(excerpts, "c_emerging_leader", n_topics = 2, n_terms = 25,
           custom_stopwords = c("na", "bruh", "theyre", "yeah"))

```

## References
Hennink, M., & Kaiser, B. N. (2022). Sample sizes for saturation in qualitative 
research: A
systematic review of empirical tests. Social science & medicine, 292, 114523.

Small, M. L., & Calarco, J. M. (2022). Qualitative Literacy: A Guide to 
Evaluating
Ethnographic and Interview Research (1st ed.). University of California Press. 
https://doi.org/10.2307/j.ctv2vr9c4x 

