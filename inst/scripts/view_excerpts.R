view_excerpts <- function(data) {
  stopifnot(requireNamespace("DT", quietly = TRUE))
  stopifnot(requireNamespace("tidyr", quietly = TRUE))
  stopifnot("excerpt" %in% names(data))

  # detect code columns
  code_cols <- grep("^c_", names(data), value = TRUE)
  if (length(code_cols) == 0) stop("No code columns found (must start with 'c_').")

  # name → label lookup
  label_lookup <- purrr::map_chr(code_cols, function(x) {
    lbl <- attr(data[[x]], "label")
    if (is.null(lbl) || lbl == "") x else lbl
  })
  names(label_lookup) <- code_cols

  # reshape to long: one row per excerpt per code (TRUE only)
  long_data <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(code_cols),
    names_to = "code",
    values_to = "applied"
  ) %>%
    dplyr::filter(.data$applied == TRUE) %>%
    dplyr::mutate(code = label_lookup[as.character(.data$code)] |> unname()) %>%
    dplyr::select(code, excerpt)

  # interactive datatable
  DT::datatable(
    long_data,
    escape = FALSE,
    filter = "top",
    options = list(
      pageLength = 10,
      autoWidth = FALSE,
      columnDefs = list(
        list(width = '150px', targets = 0),
        list(width = '600px', targets = 1)
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  this.api().columns([0]).every(function() {",
        "    var column = this;",
        "    var select = $('<select><option value=\"\"></option></select>')",
        "      .css('margin-top', '5px')",
        "      .appendTo($(column.header()))",
        "      .on('change', function() {",
        "        var val = $.fn.dataTable.util.escapeRegex($(this).val());",
        "        column.search(val ? '^' + val + '$' : '', true, false).draw();",
        "      });",
        "    column.data().unique().sort().each(function(d, j) {",
        "      select.append('<option value=\"' + d + '\">' + d + '</option>');",
        "    });",
        "  });",
        "}"
      ),
      headerCallback = DT::JS('function(thead, data, start, end, display) {
        $(thead).find("th").css("background-color", "#330662").css("color", "white");
      }')
    ),
    rownames = FALSE,
    colnames = c("Code", "Excerpt")
  ) %>%
    DT::formatStyle(
      'code',
      whiteSpace = 'nowrap'
    ) %>%
    DT::formatStyle(
      'excerpt',
      whiteSpace = 'normal',
      wordBreak = 'break-word'
    )
}

# Test

# Load libraries
library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
clean_data <- clean_data(filepath,
                         preferred_coders,
                         rename_vars = list(memo_destigmatization = "...274"),
                         relabel_vars = list(title = "Memo: Destigmatization"))
data <- clean_data$data
codebook <- clean_data$codebook

# Merge codes
merge_codes <- merge_codes(data,
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
                             c_belonging_connectedness = "sense of belonging & connectedness",
                             c_suicide_comfort = "suicide comfort conversing"
                           ))

data_merged <- merge_codes$data
codebook_merged <- merge_codes$codebook

# View excerpts
view_excerpts(data_merged)
