#' Interactive excerpts table
#'
#' Creates an interactive datatable where users can filter excerpts by code.
#' Automatically detects all code columns starting with "c_".
#'
#' @param data A data.frame or tibble with one column called `excerpt`
#'   and multiple code columns starting with `"c_"`.
#'
#' @return A DT datatable widget.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(DT)
#'
#' df <- tibble::tibble(
#'   excerpt = c(
#'     "I felt connected to peers.",
#'     "We should normalize conversations about MH.",
#'     "My teachers helped me belong.",
#'     "I am comfortable talking about suicide."
#'   ),
#'   c_belonging = c(TRUE, FALSE, TRUE, FALSE),
#'   c_destigmatization = c(FALSE, TRUE, FALSE, FALSE),
#'   c_suicide_comfort = c(FALSE, FALSE, FALSE, TRUE)
#' )
#'
#' view_excerpts_table(df)
#'
#' @export
view_excerpts_table <- function(data) {
  stopifnot(requireNamespace("DT", quietly = TRUE))
  stopifnot(requireNamespace("tidyr", quietly = TRUE))
  stopifnot("excerpt" %in% names(data))

  # detect code columns
  code_cols <- grep("^c_", names(data), value = TRUE)
  if (length(code_cols) == 0) stop("No code columns found (must start with 'c_').")

  # reshape to long: one row per excerpt per code (TRUE only)
  long_data <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(code_cols),
    names_to = "code",
    values_to = "applied"
  ) %>%
    dplyr::filter(applied == TRUE) %>%
    dplyr::select(code, excerpt)

  # interactive datatable
  DT::datatable(
    long_data,
    escape = FALSE,
    filter = "top",   # only filter on "code"
    options = list(
      pageLength = 10,
      autoWidth = FALSE,
      columnDefs = list(
        list(width = '150px', targets = 0),   # code
        list(width = '600px', targets = 1)    # excerpt
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  this.api().columns([0]).every(function() {",  # filter only code column
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
