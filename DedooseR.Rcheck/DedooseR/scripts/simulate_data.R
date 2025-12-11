# Simulate qualitative data for DedooseR package
# Creates mock data with specified number of participants and codes

library(tibble)
library(dplyr)

#' Simulate qualitative data with participants and codes
#' 
#' @param n_participants Number of participants (default: 15)
#' @param n_codes Number of codes (default: 10) 
#' @param n_excerpts_per_participant Average number of excerpts per participant (default: 5)
#' @param seed Random seed for reproducibility (default: 123)
#' @return A tibble mimicking Dedoose export structure
simulate_qual_data <- function(n_participants = 15, 
                               n_codes = 10, 
                               n_excerpts_per_participant = 5,
                               seed = 123) {
  
  set.seed(seed)
  
  # Define participant types and coders
  participant_types <- c("Interview", "Focus Group")
  coders <- c("Aliyah", "Rohan", "Maya", "Jordan", "Alex")
  
  # Define 10 simple emotion and experience codes
  codes <- c(
    "Happy",
    "Sad", 
    "Excited",
    "Worried",
    "Confident",
    "Frustrated", 
    "Grateful",
    "Confused",
    "Proud",
    "Hopeful"
  )
  
  # Create participant names and session types
  participant_names <- paste0("Participant_", sprintf("%02d", 1:n_participants))
  
  # Generate variable number of excerpts per participant (3-8 range)
  excerpts_per_participant <- sample(3:8, n_participants, replace = TRUE)
  total_excerpts <- sum(excerpts_per_participant)
  
  # Create base data structure
  participant_data <- tibble(
    participant_id = rep(participant_names, excerpts_per_participant),
    session_type = sample(participant_types, total_excerpts, replace = TRUE, prob = c(0.7, 0.3))
  ) %>%
    mutate(
      media_title = paste(session_type, participant_id, sep = ": "),
      excerpt_range = paste0("Excerpt ", sprintf("%02d", sample(10:99, total_excerpts)), 
                            "-", sprintf("%02d", sample(10:99, total_excerpts))),
      excerpt_creator = sample(coders, total_excerpts, replace = TRUE),
      excerpt_date = as.Date("2023-01-01") + sample(0:365, total_excerpts, replace = TRUE),
      resource_creator = sample(c("Staff Team", "Evaluation Lead", "Research Assistant"), 
                               total_excerpts, replace = TRUE),
      resource_date = excerpt_date - sample(1:30, total_excerpts, replace = TRUE)
    )
  
  # Generate realistic excerpt text
  excerpt_templates <- c(
    "I felt really happy when I accomplished that goal.",
    "It made me sad to see my friend struggling like that.",
    "I was so excited about the opportunity to participate.",
    "I'm worried about what might happen next semester.",
    "I feel confident in my abilities now.",
    "The whole situation left me feeling frustrated.",
    "I'm grateful for all the support I received.",
    "I was confused about what was expected of me.",
    "I felt proud of how far I've come.",
    "I'm hopeful that things will get better.",
    "The experience brought me joy and happiness.",
    "There were moments when I felt overwhelmed and sad.",
    "My excitement about the project kept me motivated.",
    "I couldn't shake the worried feeling in my stomach.",
    "Building confidence has been a gradual process."
  )
  
  participant_data$excerpt_copy <- sample(excerpt_templates, total_excerpts, replace = TRUE)
  
  # Create code application patterns with realistic variability
  # Some codes are more common than others
  code_probabilities <- c(0.4, 0.25, 0.35, 0.3, 0.45, 0.2, 0.15, 0.25, 0.4, 0.3)
  
  # Generate code applications for each excerpt
  for (i in seq_along(codes)) {
    code_name <- codes[i]
    prob <- code_probabilities[i]
    
    # Create column names following Dedoose format
    applied_col <- paste0("Code: ", code_name, " Applied")
    range_col <- paste0("Code: ", code_name, " Range") 
    weight_col <- paste0("Code: ", code_name, " Weight")
    
    # Generate applications with some clustering by participant
    # (some participants more likely to have certain codes)
    applications <- logical(total_excerpts)
    
    for (p in unique(participant_data$participant_id)) {
      p_indices <- which(participant_data$participant_id == p)
      # Each participant has different propensity for each code
      p_prob <- prob * runif(1, 0.5, 1.5) # Vary by participant
      p_prob <- pmax(0.05, pmin(0.8, p_prob)) # Keep within reasonable bounds
      
      applications[p_indices] <- rbinom(length(p_indices), 1, p_prob) == 1
    }
    
    participant_data[[applied_col]] <- ifelse(applications, "True", "False")
    participant_data[[range_col]] <- NA_character_
    participant_data[[weight_col]] <- ifelse(applications,
                                           sample(1:3, sum(applications), replace = TRUE),
                                           NA_real_)
  }
  
  # Create combined codes column
  applied_codes <- participant_data %>%
    select(ends_with(" Applied")) %>%
    mutate(across(everything(), ~ . == "True")) %>%
    mutate(row_id = row_number()) %>%
    tidyr::pivot_longer(-row_id, names_to = "code", values_to = "applied") %>%
    filter(applied) %>%
    mutate(code = stringr::str_remove(code, "^Code: "),
           code = stringr::str_remove(code, " Applied$")) %>%
    group_by(row_id) %>%
    summarise(codes_applied_combined = paste(code, collapse = "; "), .groups = "drop")
  
  # Add combined codes back to main data
  participant_data <- participant_data %>%
    mutate(row_id = row_number()) %>%
    left_join(applied_codes, by = "row_id") %>%
    mutate(codes_applied_combined = ifelse(is.na(codes_applied_combined), "", codes_applied_combined)) %>%
    select(-row_id)
  
  # Reorder columns to match Dedoose format
  final_data <- participant_data %>%
    select(
      `Media Title` = media_title,
      `Excerpt Range` = excerpt_range, 
      `Excerpt Creator` = excerpt_creator,
      `Excerpt Date` = excerpt_date,
      `Excerpt Copy` = excerpt_copy,
      `Resource Creator` = resource_creator,
      `Resource Date` = resource_date,
      `Codes Applied Combined` = codes_applied_combined,
      everything()
    ) %>%
    select(-participant_id, -session_type)
  
  return(final_data)
}

# Generate the simulated data
demo_excerpts <- simulate_qual_data(
  n_participants = 15,
  n_codes = 10, 
  n_excerpts_per_participant = 5,
  seed = 123
)

demo_excerpts |> view()

# Display summary
cat("Generated", nrow(demo_excerpts), "excerpts from", 
    length(unique(demo_excerpts$`Media Title`)), "participants\n")

# Show code frequency distribution
code_cols <- grep("Applied$", names(demo_excerpts), value = TRUE)
code_freq <- demo_excerpts %>%
  select(all_of(code_cols)) %>%
  summarise(across(everything(), ~ sum(. == "True"))) %>%
  tidyr::pivot_longer(everything(), names_to = "Code", values_to = "Frequency") %>%
  mutate(Code = stringr::str_remove(Code, "^Code: "),
         Code = stringr::str_remove(Code, " Applied$")) %>%
  arrange(desc(Frequency))

print(code_freq)
