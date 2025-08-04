#' Plot Code Co-occurrence Heatmap
#'
#' @param file_path String path to Excel file containing co-occurrence matrix.
#' @param sheet Integer or string specifying sheet number or name (default: 1).
#' @param min_frequency Numeric; minimum frequency threshold to display in heatmap (default: 5).
#' @param code_labels Optional named vector or data.frame for mapping codes to readable labels.
#'   - If named vector: names are original codes, values are labels.
#'   - If data.frame: must have columns 'Code' and 'Label'.
#' @return A ggplot2 heatmap object visualizing code co-occurrences above the threshold.
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c theme theme_minimal element_text labs coord_flip
#' @importFrom reshape2 melt
#' @importFrom dplyr filter select mutate
#' @importFrom readxl read_excel
#' @export
#'
coccur <- function(file_path,
                                           sheet = 1,
                                           min_frequency = 5,
                                           code_labels = NULL) {
  library(readxl)
  library(reshape2)
  library(dplyr)
  library(ggplot2)

  # Read data
  data <- read_excel(file_path, sheet = sheet)

  # Clean duplicated column names (e.g. Religion...89) by making unique
  colnames(data) <- make.unique(colnames(data))

  # Extract row names from first column
  row_names <- data[[1]]
  matrix_data <- as.matrix(data[, -1])
  rownames(matrix_data) <- row_names

  # Remove unwanted rows and columns
  unwanted <- c("Totals", "Priority excerpt", "Heterogeniety")
  matrix_data <- matrix_data[
    !rownames(matrix_data) %in% unwanted,
    !colnames(matrix_data) %in% unwanted
  ]

  # Melt matrix into long format
  melted_data <- melt(matrix_data)
  colnames(melted_data) <- c("Code1", "Code2", "Frequency")

  # Filter by frequency threshold
  melted_data <- melted_data %>% filter(Frequency > min_frequency)

  # If code_labels provided, replace code names with labels
  if (!is.null(code_labels)) {
    if (is.vector(code_labels) && !is.data.frame(code_labels)) {
      # Named vector
      melted_data$Code1 <- ifelse(melted_data$Code1 %in% names(code_labels),
                                  code_labels[melted_data$Code1],
                                  melted_data$Code1)
      melted_data$Code2 <- ifelse(melted_data$Code2 %in% names(code_labels),
                                  code_labels[melted_data$Code2],
                                  melted_data$Code2)
    } else if (is.data.frame(code_labels) && all(c("Code", "Label") %in% colnames(code_labels))) {
      # Data frame mapping
      melted_data <- melted_data %>%
        left_join(code_labels, by = c("Code1" = "Code")) %>%
        mutate(Code1 = ifelse(!is.na(Label), Label, Code1)) %>%
        select(-Label) %>%
        left_join(code_labels, by = c("Code2" = "Code")) %>%
        mutate(Code2 = ifelse(!is.na(Label), Label, Code2)) %>%
        select(-Label)
    } else {
      warning("`code_labels` must be a named vector or a data frame with columns 'Code' and 'Label'.")
    }
  }

  # Create heatmap plot
  p <- ggplot(melted_data, aes(x = reorder(Code2, Frequency), y = Code1, fill = Frequency)) +
    geom_tile(color = "white", linewidth = 0.1) +  # use linewidth instead of deprecated size
    scale_fill_viridis_c(name = "Co-occurrence\nFrequency",
                         option = "plasma",
                         trans = "sqrt") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_blank(),
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = "right"
    ) +
    labs(title = paste0("Code Co-occurrence Heatmap (>", min_frequency, ")"))

  print(p)
  invisible(p)
}


