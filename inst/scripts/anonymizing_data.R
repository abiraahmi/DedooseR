# Load packages
library(tidyverse)
library(stringr)
library(readxl)
library(DedooseR)
library(tidyr)
library(purrr)

# Load test data
df <- read_xlsx("inst/raw_data/test_data.xlsx")

preferred_coders <- c("v", "r", "l", "s")

# Clean data
clean_data <- clean_data(df, 
                  preferred_coders) 
data <- clean_data$data
codebook <- clean_data$codebook

# Keep only 5 parent codes and their child codes
test_data <- data |> 
  select(media_title, excerpt_creator, excerpt, resource_creator, resource_date,
  codes_applied_combined, 
  c_school_climate, c_school_climate_other_mh_initiatives,
  c_gateway_to_support, c_gateway_to_support_becoming_a_gateway,
  c_gateway_to_support_being_seen_as_a_gateway,
  c_knowledge_awareness, c_vigilance,
  c_self_expression, c_self_expression_identities, c_self_expression_interests_personality_emotions, c_self_expression_interests_personality_emotions, c_self_expression_other, 
  c_salience, c_salience_personal_development, c_salience_relevance)

# Merge some codes into other for ease of understanding
merges_list = list(
  c_school_climate = c("c_school_climate", "c_school_climate_other_mh_initiatives"),
  c_self_expression = c("c_self_expression", "c_self_expression_interests_personality_emotions"),
  c_salience = c("c_salience", "c_salience_personal_development", "c_salience_relevance")
)
merge <- merge_codes(test_data, 
                    merges = merges_list,
                    relabel_vars = list(
                      c_school_climate = "school climate",
                      c_self_expression = "self-expression",
                      c_salience = "salience of mental health"
                    ))
merged_data <- merge$data_merged
merged_codebook <- merge$codebook_merged

c_cols <- grep("^c_", names(merged_data), value = TRUE)

# Drop rows where all code columns = FALSE
merged_data <- merged_data |>
  dplyr::filter(rowSums(dplyr::across(dplyr::all_of(c_cols), ~ .x %in% TRUE), na.rm = TRUE) > 0)

# Revert to raw data variable names so we can use as a demo dataset
# Read raw data to verify code names
colnames(df)

raw_data <- merged_data |> 
rename(
    `Media Title` = media_title,
    `Excerpt Creator` = excerpt_creator,
    `Excerpt Copy` = excerpt,
    `Resource Creator` = resource_creator,
    `Resource Date` = resource_date,
    `Codes Applied Combined` = codes_applied_combined,
    `Code: School Climate Applied` = c_school_climate,
    `Code: Gateway to Support Applied` = c_gateway_to_support,
    `Code: Gateway to Support\\Becoming a Gateway Applied` = c_gateway_to_support_becoming_a_gateway,
    `Code: Gateway to Support\\Being Seen as a Gateway Applied` = c_gateway_to_support_being_seen_as_a_gateway,
    `Code: Knowledge/Awareness Applied` = c_knowledge_awareness,
    `Code: Salience Applied` = c_salience,
    `Code: Self-expression Applied` = c_self_expression,
    `Code: Self-expression Applied` = c_self_expression,
    `Code: Self-expression - Identities Applied` = c_self_expression_identities,
    `Code: Self-expression - Other Applied` = c_self_expression_other,
    `Code: Vigilance Applied` = c_vigilance
  )

colnames(raw_data)

# Swap resource creator name
raw_data[["Resource Creator"]] <- "janedoe"

# Replace Media Titles with generic transcipt IDs
set.seed(20250229)  # for reproducability

unique_titles <- unique(raw_data[["Media Title"]])
n_titles <- length(unique_titles)

fake_dates <- as.Date("2025-01-01") + sample.int(365, n_titles, replace = TRUE)

anon_ids <- sprintf(
  "Transcript_%02d_Participant_%02d_%s",
  seq_len(n_titles),
  seq_len(n_titles),
  format(fake_dates, "%Y-%m-%d")
)
names(anon_ids) <- unique_titles

raw_data[["Media Title"]] <- unname(anon_ids[raw_data[["Media Title"]]])
raw_data[["Resource Date"]] <- unname(format(fake_dates[match(raw_data[["Media Title"]], anon_ids)], "%Y-%m-%d"))

# Add range and weight columns to mimic Dedoose output
applied_cols <- grep("Applied$", names(raw_data), value = TRUE)

for (col in applied_cols) {
  range_name <- sub("Applied$", "Range", col)
  weight_name <- sub("Applied$", "Weight", col)

  raw_data[[range_name]] <- NA
  raw_data[[weight_name]] <- NA
}

# Arrange it like Dedoose
first_cols <- c(
  "Media Title",
  "Excerpt Creator",
  "Excerpt Copy",
  "Resource Creator",
  "Resource Date",
  "Codes Applied Combined"
)

code_cols <- sort(names(raw_data)[grepl("^Code:", names(raw_data))])

raw_data <- raw_data |>
  dplyr::select(
    dplyr::all_of(first_cols),
    dplyr::all_of(code_cols),
    dplyr::everything()
  )

# Ensure each column is stored as the correct data type
raw_data <- raw_data |> 
mutate(
  `Media Title` = as.character(`Media Title`),
  `Excerpt Creator` = as.character(`Excerpt Creator`),
  `Excerpt Copy` = as.character(`Excerpt Copy`),
  `Resource Creator` = as.character(`Resource Creator`),
  `Resource Date` = as.character(`Resource Date`),
  `Codes Applied Combined` = as.character(`Codes Applied Combined`),
  `Code: School Climate Applied` = as.logical(`Code: School Climate Applied`),
  `Code: Gateway to Support Applied` = as.logical(`Code: Gateway to Support Applied`),
  `Code: Gateway to Support\\Becoming a Gateway Applied` = as.logical(`Code: Gateway to Support\\Becoming a Gateway Applied`),
  `Code: Gateway to Support\\Being Seen as a Gateway Applied` = as.logical(`Code: Gateway to Support\\Being Seen as a Gateway Applied`),
  `Code: Knowledge/Awareness Applied` = as.logical(`Code: Knowledge/Awareness Applied`),
  `Code: Vigilance Applied` = as.logical(`Code: Vigilance Applied`)
)

# Save as xlsx
library(writexl)
write_xlsx(raw_data, "inst/raw_data/demo_data.xlsx")

# Hand over to interns to manually read through the excerpts and make changes to the excerpts

# Try other functions with demo dataset ------

library(DedooseR)
library(readxl)
library(tidyverse)

demo_data <- read_xlsx("inst/raw_data/demo_data.xlsx")

# Clean
preferred_coders <- c("l", "r", "s", "v")
df <- clean_data(demo_data, preferred_coders)

data <- df$data
codebook <- df$codebook

# Merge codes
merges = list(
  c_gateway_to_support = c("c_gateway_to_support", "c_gateway_to_support_becoming_a_gateway", "c_gateway_to_support_being_seen_as_a_gateway"),
  c_knowledge_awareness = c("c_knowledge_awareness", "c_vigilance"),
  c_self_expression = c("c_self_expression", "c_self_expression_identities", "c_self_expression_other"))

merged_data <- merge_codes(data, merges,
relabel_vars = list(
  c_knowledge_awareness = "knowledge & awareness of mental health",
  c_gateway_to_support = "becoming or being seen as a gateway to mental health support",
  c_self_expression = "self-expressin of identities, interests, and other"
))
data_merged <- merged_data$data_merged
codebook_merged <-merged_data$codebook

# Create code summary
code_summary <- create_code_summary(
  data_merged, 
  table_min_count = 5, 
  table_min_prop = NULL,
  plot = TRUE,
  plot_min_count = 5, 
  output_type = "tibble",
  plot_metric = "count",
  use_labels = TRUE,
  codebook = codebook_merged
)

code_summary_table <- code_summary$table
code_summary_plot <- code_summary$plot

# Set saturation
set_saturation(
  code_summary_table,
  table_min_count = 20,
  table_min_prop = 0.5,
  plot = TRUE, 
  plot_metric = "both"
)

# Compare saturation
thresholds_list <- list(
  Liberal = list(code_count = 4, prop_media_title = 0.25),
  Strict  = list(code_count = 9, prop_media_title = 0.5)
)

compare_saturation(code_summary_table, 
  thresholds_list,
    output_type = "kable", 
  plot = TRUE,
  plot_metric = "both")

# Cooccurence
cooccur(
  data_merged, 
  min_bold = 0, 
  scale = "count",
  output = "tibble",
  plot = TRUE,
  edge_min = 4, 
  use_labels = TRUE, 
  codebook = codebook_merged
)

# View excerpts
view_excerpts(data_merged)

# Word cloud
wordcloud(data_merged, "c_self_expression", max_words = 100)
