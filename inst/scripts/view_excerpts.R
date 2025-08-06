view_excerpts <- function(excerpts) {
  # Load required libraries
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("DT", quietly = TRUE)
  requireNamespace("htmltools", quietly = TRUE)

  # Identify code columns (logical columns)
  code_cols <- names(excerpts)[vapply(excerpts, is.logical, logical(1))]

  # Pivot to long format: one row per Excerpt × Code
  long_excerpts <- excerpts %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(code_cols),
      names_to = "Code",
      values_to = "Applied"
    ) %>%
    dplyr::filter(Applied) %>%
    dplyr::select(Code, `Excerpt Copy` = `Excerpt`, Priority = `Priority excerpt`, Heterogeneity = `Heterogeneity?`)

  # Render datatable
  DT::datatable(
    long_excerpts,
    escape = FALSE,
    filter = "top",
    options = list(
      pageLength = 10,
      autoWidth = FALSE,
      columnDefs = list(
        list(width = '50px', targets = 0),   # Code
        list(width = '200px', targets = 1),  # Excerpt
        list(width = '100px', targets = 2),  # Priority
        list(width = '100px', targets = 3)   # Heterogeneity
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  this.api().columns().every(function() {",
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
    colnames = c("Code", "Excerpt", "P?", "H?")
  ) %>%
    DT::formatStyle(
      'Code',
      whiteSpace = 'nowrap'
    ) %>%
    DT::formatStyle(
      'Excerpt Copy',
      whiteSpace = 'normal',
      wordBreak = 'break-word'
    ) %>%
    DT::formatStyle(
      'Priority',
      whiteSpace = 'nowrap'
    ) %>%
    DT::formatStyle(
      'Heterogeneity',
      whiteSpace = 'nowrap'
    )
}
