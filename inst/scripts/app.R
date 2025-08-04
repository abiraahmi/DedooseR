# Step 2: Run app

library(shiny)
library(dplyr)
library(readr)

# Load and prepare data
test_data <- readRDS("test1.rds") %>%
  mutate(id = row_number())

key_data <- readRDS("key1.rds") %>%
  mutate(id = row_number())

combined_data <- left_join(test_data, key_data, by = "id") %>%
  select(id, Excerpt.x, `Parent Code`) %>%
  rename(Excerpt = Excerpt.x) %>%
  mutate(Excerpt = str_remove_all(Excerpt, "[\"\'?]")) %>% 
  as.data.frame()

# Find the right columns
excerpt_columns <- names(combined_data)[grepl("excerpt|Excerpt", names(combined_data), ignore.case = TRUE)]
parent_code_columns <- names(combined_data)[grepl("parent|code", names(combined_data), ignore.case = TRUE)]

if ("Parent Code" %in% names(combined_data)) {
  parent_col <- "Parent Code"
} else if ("Parent.Code" %in% names(combined_data)) {
  parent_col <- "Parent.Code"
} else {
  parent_col <- parent_code_columns[1]
}

if (length(excerpt_columns) > 0) {
  excerpt_col <- excerpt_columns[1]
} else {
  excerpt_col <- setdiff(names(combined_data), c("id", parent_col))[1]
}

# Create final dataset
final_data <- combined_data %>%
  select(id, all_of(excerpt_col), all_of(parent_col)) %>%
  rename(Excerpt = all_of(excerpt_col), parent_code = all_of(parent_col)) %>%
  filter(!is.na(Excerpt) & !is.na(parent_code) & Excerpt != "" & parent_code != "") %>%
  mutate(
    code_given = NA_character_,
    name = NA_character_
  ) %>%
  as.data.frame()

code_choices <- unique(final_data$parent_code)
code_choices <- sort(code_choices[!is.na(code_choices) & code_choices != ""])

# UI
ui <- fluidPage(
  titlePanel("Qualitative Coding Interface"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("coder_name", "Enter your name:", placeholder = "Your name"),
      br(),
      
      conditionalPanel(
        condition = "input.coder_name != ''",
        
        
        h4("Navigation"),
        numericInput("excerpt_number", "Go to excerpt #:", 
                     value = 1, min = 1, max = nrow(final_data), step = 1),
        br(),
        
        actionButton("prev_excerpt", "← Previous", class = "btn-secondary"),
        actionButton("next_excerpt", "Next →", class = "btn-secondary"),
        br(), br(),
        
        downloadButton("download_results", "Download Results", class = "btn-primary")
      )
    ),
    
    mainPanel(
      width = 9,
      conditionalPanel(
        condition = "input.coder_name == ''",
        div(
          style = "text-align: center; margin-top: 100px;",
          h3("Welcome to the Qualitative Coding Interface"),
          p("Please enter your name in the sidebar to begin coding."),
          p(paste("Total excerpts to code:", nrow(final_data)))
        )
      ),
      
      conditionalPanel(
        condition = "input.coder_name != ''",
        fluidRow(
          column(12,
                 div(id = "coding_interface",
                     uiOutput("current_excerpt_ui")
                 )
          )
        ),
        br(),
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store the data
  values <- reactiveValues(
    data = final_data,
    current_excerpt = 1
  )
  
  # Update current excerpt when navigation is used
  observeEvent(input$excerpt_number, {
    if (!is.na(input$excerpt_number) && 
        input$excerpt_number >= 1 && 
        input$excerpt_number <= nrow(values$data)) {
      values$current_excerpt <- input$excerpt_number
    }
  })
  
  observeEvent(input$prev_excerpt, {
    if (values$current_excerpt > 1) {
      values$current_excerpt <- values$current_excerpt - 1
      updateNumericInput(session, "excerpt_number", value = values$current_excerpt)
    }
  })
  
  observeEvent(input$next_excerpt, {
    if (values$current_excerpt < nrow(values$data)) {
      values$current_excerpt <- values$current_excerpt + 1
      updateNumericInput(session, "excerpt_number", value = values$current_excerpt)
    }
  })
  
  # Current excerpt UI
  output$current_excerpt_ui <- renderUI({
    req(input$coder_name != "")
    req(values$current_excerpt <= nrow(values$data))
    
    current_row <- values$data[values$current_excerpt, ]
    
    div(
      style = "border: 2px solid #007bff; border-radius: 10px; padding: 20px; background-color: #f8f9fa;",
      
      fluidRow(
        column(8,
               h4(paste("Excerpt", values$current_excerpt, "of", nrow(values$data)))
        ),
      ),
      
      hr(),
      
      div(
        style = "background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;",
        h5("Text Excerpt:"),
        p(current_row$Excerpt, style = "font-size: 16px; line-height: 1.5;")
      ),
      
      br(),
      
      fluidRow(
        column(6,
               selectInput(
                 "current_code",
                 "Select your code:",
                 choices = c("Select a code..." = "", code_choices),
                 selected = ifelse(is.na(current_row$code_given), "", current_row$code_given),
                 width = "100%"
               )
        ),
        column(6,
               div(style = "margin-top: 25px;",
                   actionButton("clear_current", "Clear", class = "btn-warning")
               )
        )
      ),
      
      conditionalPanel(
        condition = "input.current_code != ''",
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #d4edda; border-radius: 5px;",
          tags$strong("Selected: "), textOutput("selected_code_display", inline = TRUE)
        )
      )
    )
  })
  
  # Auto-save when code is selected
  observeEvent(input$current_code, {
    req(input$coder_name != "")
    req(input$current_code != "")
    req(values$current_excerpt <= nrow(values$data))
    
    values$data[values$current_excerpt, "code_given"] <- input$current_code
    values$data[values$current_excerpt, "name"] <- input$coder_name
    
    showNotification("Code saved automatically!", type = "message")
  })
  
  # Clear current coding
  observeEvent(input$clear_current, {
    # Clear the code from the data
    values$data[values$current_excerpt, "code_given"] <- NA_character_
    
    # Update the UI
    updateSelectInput(session, "current_code", selected = "")
  })
  
  # Display selected code
  output$selected_code_display <- renderText({
    input$current_code
  })
  
  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("coding_results_", input$coder_name, "_", Sys.Date(), ".rds")
    },
    content = function(file) {
      # Get the current state of the data
      current_data <- values$data
      
      # Save the complete dataset including all coded entries
      saveRDS(current_data, file)
    }
  )
}

# Run the app
shinyApp(ui, server)

## BUG: when I try to swap the files out for the excerpts I want to pull in, it crashes :/ 

# # Load data
# test_data <- readRDS("test.rds") %>%
#   mutate(id = row_number())
# 
# key_data <- readRDS("key.rds") %>%
#   mutate(id = row_number())
# 
# combined_data <- left_join(test_data, key_data, by = "id") %>%
#   select(id, Excerpt.x, `Parent Code`) %>%
#   rename(Excerpt = Excerpt.x) %>%
#   as.data.frame()
# 
# code_choices <- unique(combined_data$`Parent Code`)
# 
# # UI
# ui <- fluidPage(
#   tags$head(
#     tags$style(HTML("
#       .form-group {
#         width: 100%;
#       }
#       label.control-label {
#         white-space: normal;
#         width: 100%;
#         display: block;
#         font-weight: normal;
#       }
#     "))
#   ),
#   titlePanel("Coder Reliability Check"),
#   textInput("coder_name", "Step 1: Enter your name:"),
#   p("Step 2:"),
#   uiOutput("coding_ui"),
#   strong(
#     tags$ul(
#       tags$li("For each excerpt below, select the code you feel most confident about. There are no double codes!"),
#       tags$li("When you are done, use the button at the end of this app to download your responses."),
#       tags$li("Save the CSV file in the `responses` folder in our Box Drive!")
#     )
#   ),
#   
#   hr(),
#   downloadButton("download_all", "Download Responses")
# )
# 
# # Server
# server <- function(input, output, session) {
#   
#   
#   # Create selectInput UI for each excerpt
#   output$coding_ui <- renderUI({
#     req(nrow(combined_data) > 0)
#     lapply(1:nrow(combined_data), function(i) {
#       selectInput(
#         inputId = paste0("code_", i),
#         label = paste0("Excerpt ", i, ": ", combined_data$Excerpt[i]),
#         choices = code_choices,
#         selected = NULL,
#         width = "100%"
#       )
#     })
#   })
#   
#   # Reactive expression to collect current responses dynamically
#   current_responses <- reactive({
#   req(input$coder_name)
#   submitted_codes <- vapply(1:nrow(combined_data), function(i) {
#     val <- input[[paste0("code_", i)]]
#     if (is.null(val)) "" else val
#   }, character(1))
#   
#   data.frame(
#     name = input$coder_name,
#     excerpt_id = 1:nrow(combined_data),
#     excerpt = combined_data$Excerpt,
#     code_given = submitted_codes,
#     correct_code = combined_data$`Parent Code`,
#     is_correct = submitted_codes == combined_data$`Parent Code`,
#     stringsAsFactors = FALSE
#   )
# })
#   
#   # Show all responses
#   output$results <- renderTable({
#     # Only show if all codes are selected (no NA or NULL)
#     df <- current_responses()
#     if (any(is.na(df$code_given) | df$code_given == "")) {
#       return(NULL)
#     } else {
#       df
#     }
#   })
#   
#   # Summary output
#   output$summary <- renderPrint({
#     df <- current_responses()
#     if (any(is.na(df$code_given) | df$code_given == "")) {
#       cat("Please complete all code selections to see summary.")
#     } else {
#       df %>%
#         group_by(name) %>%
#         summarise(
#           total = n(),
#           correct = sum(is_correct),
#           incorrect = total - correct
#         ) %>%
#         print()
#     }
#   })
#   
#   # Download handler
#   output$download_all <- downloadHandler(
#     filename = function() {
#       paste0("coder_responses_", Sys.Date(), ".rds")
#     },
#     content = function(file) {
#       saveRDS(current_responses(), file)  # no row.names argument here
#     }
#   )
# }
# 
# shinyApp(ui, server)


  # Debugging - works!
  
  # library(shiny)
  # library(dplyr)
  # library(readr)
  # 
  # # Load data with detailed logging
  # cat("Loading data...\n")
  # test_data <- readRDS("test1.rds") %>%
  #   mutate(id = row_number())
  # cat("test_data loaded. Columns:", paste(names(test_data), collapse = ", "), "\n")
  # cat("test_data first few rows:\n")
  # print(head(test_data, 2))
  # 
  # key_data <- readRDS("key1.rds") %>%
  #   mutate(id = row_number())
  # cat("key_data loaded. Columns:", paste(names(key_data), collapse = ", "), "\n")
  # cat("key_data first few rows:\n")
  # print(head(key_data, 2))
  # 
  # combined_data <- left_join(test_data, key_data, by = "id") %>%
  #     select(id, Excerpt.x, `Parent Code`) %>%
  #     rename(Excerpt = Excerpt.x) %>%
  #     as.data.frame()
  # cat("After join. Columns:", paste(names(combined_data), collapse = ", "), "\n")
  # cat("Combined data dimensions:", dim(combined_data), "\n")
  # 
  # # Show what we're working with
  # excerpt_columns <- names(combined_data)[grepl("excerpt|Excerpt", names(combined_data), ignore.case = TRUE)]
  # cat("Potential excerpt columns:", paste(excerpt_columns, collapse = ", "), "\n")
  # 
  # parent_code_columns <- names(combined_data)[grepl("parent|code", names(combined_data), ignore.case = TRUE)]
  # cat("Potential parent code columns:", paste(parent_code_columns, collapse = ", "), "\n")
  # 
  # # Try to find the right columns
  # if ("Parent Code" %in% names(combined_data)) {
  #   parent_col <- "Parent Code"
  # } else if ("Parent.Code" %in% names(combined_data)) {
  #   parent_col <- "Parent.Code"
  # } else {
  #   parent_col <- parent_code_columns[1]
  # }
  # 
  # if (length(excerpt_columns) > 0) {
  #   excerpt_col <- excerpt_columns[1]
  # } else {
  #   # Find any column that's not id or parent code
  #   excerpt_col <- setdiff(names(combined_data), c("id", parent_col))[1]
  # }
  # 
  # cat("Using excerpt column:", excerpt_col, "\n")
  # cat("Using parent code column:", parent_col, "\n")
  # 
  # # Create final dataset
  # if (!is.null(excerpt_col) && !is.null(parent_col) && 
  #     excerpt_col %in% names(combined_data) && parent_col %in% names(combined_data)) {
  #   
  #   final_data <- combined_data %>%
  #     select(id, all_of(excerpt_col), all_of(parent_col)) %>%
  #     rename(Excerpt = all_of(excerpt_col), ParentCode = all_of(parent_col)) %>%
  #     filter(!is.na(Excerpt) & !is.na(ParentCode) & Excerpt != "" & ParentCode != "") %>%
  #     as.data.frame()
  #   
  #   cat("Final data created with", nrow(final_data), "rows\n")
  #   code_choices <- unique(final_data$ParentCode)
  #   code_choices <- sort(code_choices[!is.na(code_choices) & code_choices != ""])
  #   cat("Code choices:", paste(code_choices, collapse = ", "), "\n")
  # } else {
  #   cat("ERROR: Could not identify excerpt and parent code columns\n")
  #   final_data <- data.frame()
  #   code_choices <- c()
  # }
  # 
  # # UI with lots of debug info
  # ui <- fluidPage(
  #   titlePanel("Debug: Data Structure Analysis"),
  #   
  #   h3("Data Loading Results:"),
  #   verbatimTextOutput("data_info"),
  #   
  #   h3("Sample Data:"),
  #   tableOutput("sample_table"),
  #   
  #   hr(),
  #   
  #   if (nrow(final_data) > 0) {
  #     div(
  #       h3("Coding Interface Preview:"),
  #       textInput("coder_name", "Enter your name:"),
  #       conditionalPanel(
  #         condition = "input.coder_name != ''",
  #         h4("First 3 excerpts:"),
  #         uiOutput("sample_coding")
  #       )
  #     )
  #   } else {
  #     div(
  #       h3(style = "color: red;", "ERROR: No valid data found"),
  #       p("Check the console output for details about what went wrong.")
  #     )
  #   }
  # )
  # 
  # server <- function(input, output, session) {
  #   
  #   output$data_info <- renderText({
  #     paste(
  #       "Test data dimensions:", paste(dim(test_data), collapse = " x "),
  #       "\nTest data columns:", paste(names(test_data), collapse = ", "),
  #       "\n\nKey data dimensions:", paste(dim(key_data), collapse = " x "),
  #       "\nKey data columns:", paste(names(key_data), collapse = ", "),
  #       "\n\nCombined data dimensions:", paste(dim(combined_data), collapse = " x "),
  #       "\nCombined data columns:", paste(names(combined_data), collapse = ", "),
  #       "\n\nFinal data dimensions:", paste(dim(final_data), collapse = " x "),
  #       "\nNumber of unique codes:", length(code_choices),
  #       "\nCodes:", paste(head(code_choices, 10), collapse = ", ")
  #     )
  #   })
  #   
  #   output$sample_table <- renderTable({
  #     if (nrow(final_data) > 0) {
  #       head(final_data, 3)
  #     } else {
  #       data.frame(Message = "No data to display")
  #     }
  #   })
  #   
  #   output$sample_coding <- renderUI({
  #     req(input$coder_name != "")
  #     req(nrow(final_data) > 0)
  #     
  #     n_show <- min(3, nrow(final_data))
  #     lapply(1:n_show, function(i) {
  #       div(style = "border: 1px solid #ccc; padding: 10px; margin: 10px 0;",
  #           h5(paste("Excerpt", i, ":")),
  #           p(final_data$Excerpt[i]),
  #           selectInput(
  #             paste0("code_", i),
  #             "Select code:",
  #             choices = c("Select..." = "", code_choices),
  #             width = "300px"
  #           )
  #       )
  #     })
  #   })
  # }
  # 
  # shinyApp(ui, server)
