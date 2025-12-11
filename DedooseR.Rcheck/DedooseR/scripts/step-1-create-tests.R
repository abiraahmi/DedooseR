# Step 1: Create tests

# Load packages
library(tidyverse)
library(yardstick)
library(dplyr)
library(purrr)
library(irr)

# STEP 1: Identify excerpts for testing
# Pull long excerpts from qual tracker
long_excerpts <- readRDS('/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/IRR testing/long-excerpts/long_excerpts_flagged.rds')

# Set up test data

double_examples <- long_excerpts %>% 
  filter(Priority == "Yes") %>% 
  distinct() %>% 
  mutate(total_number_code_apply = n(), .by = c( Code)) %>% 
  #filter(id_ex == 2) %>% 
  arrange(total_number_code_apply,Code ) %>% 
  mutate(times_exe_used = n(), .by = `Excerpt Copy`) %>% 
  mutate(`Parent Code` = sub("\\\\.*", "", Code)) %>% 
  rename("Original Code" = "Code") %>%
  select(`Parent Code`,`Original Code`, `Excerpt Copy`, Priority, total_number_code_apply, times_exe_used, everything()) %>% 
  #filter(id_es == 1 & ncode ==3)
  view()

all_codes <- double_examples %>% 
  count(`Original Code` )

final_examples <- double_examples %>% 
  filter(times_exe_used == 1) %>% 
  count(`Original Code` )

double_examples %>% 
  anti_join(final_examples) %>% 
  arrange(`Excerpt Copy`, times_exe_used) %>% 
  mutate(times_exe_used1 = n(), .by = `Excerpt Copy`) %>% 
  select(`Parent Code`,`Original Code`, `Excerpt Copy`, Priority, total_number_code_apply, times_exe_used, everything()) %>% 
  view()

# Count number of examples per code
 double_examples %>% count(`Original Code` ) %>% print(n=100)

# Identify which excerpts to keep 
test <- double_examples %>% 
  filter(`Parent Code` %in% c( # Add codes you want to test below
    "Changing the narrative",
    "Destigmatization",
    "Emerging leader",
    "Emotional impact of films",
    "Empowerment",
    "Helping others",
    "Hope/Inspiration",
    "Identity",
    "Joy",
    "Knowledge/Awareness",
    "Long-term impact",
    "Opening conversations",
    "Salience",
    "School climate",
    "Self expression",
    "Student: Help-seeking for Others",
    "Sense of Belonging",
    "Sense of Connectedness",
    "Shift in MH/S norms",
    "Staff: Help-seeking",
    "Student: Help-seeking for Others",
    "Vigilance"
  )) %>% 
  mutate(distinct_ex = n(), .by = c(`Parent Code`,`Excerpt Copy` )) %>% 
  filter(distinct_ex == 1) %>% 
  mutate(total_used = row_number(), .by = c( `Parent Code` )) %>% 
  mutate(keep = case_when(
    total_used == 1   ~ "keep", 
    total_used == 3  ~ "keep", 
    total_used == 5 ~ "keep", 
    .default = "no"
  )) %>% 
  filter(keep == "keep") %>% 
  mutate(id = paste0(str_sub(`Parent Code`, 1, 3), "_", str_count(`Excerpt Copy`, "\\w+"))) 


# Create key for test + test files
key_data <- test %>% 
  select(`Parent Code`, `Excerpt Copy` ) %>%
  mutate(id = row_number()) %>%
  rename("Excerpt" = `Excerpt Copy`)

code_counts <- key_data %>%
  count(`Parent Code`, sort = TRUE)

test_data <- test %>% 
  select(id, `Excerpt Copy` )  %>%
  rename("Excerpt" = `Excerpt Copy`) %>%
  mutate(id = row_number())


# Save
saveRDS(test_data, "/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/reliability_app/test.rds")

saveRDS(key_data, "/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/reliability_app/key.rds")

test_data <- readRDS("/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/reliability_app/test.rds")

key_data <- readRDS("/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/reliability_app/key.rds")

# If there are codes you want to swap, fix it manually above

# # Manual fixes were throwing errors, so below we are pulling those rows in from test1 and key 1 and dropping the initial 54 selections
# final_test_data <-read_csv("test1.csv") %>%
#   mutate(id = row_number()) %>%
#   rename("Excerpt" = `Excerpt Copy`)
# 
# test <- bind_rows(test_data, final_test_data) %>%
#   slice(-(1:54))
# 
# final_key_data <-read_csv("key1.csv") %>%
#   rename("Excerpt" = `Excerpt Copy`) %>%
#   rename("id" = `...1`)
# 
# key <- bind_rows(key_data, final_key_data) %>%
#   slice(-(1:54))
# 
# saveRDS(test, "/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/reliability_app/test1.rds")
# 
# saveRDS(key, "/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/reliability_app/key1.rds")


## Now, you can run app.R pulling in the cleaned up test and key

