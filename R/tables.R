#' Prettify a datatable by using Semantic UI
#'
#' Extends DT::datatable and automatically formats dates and numbers
#' while preserving sorting and order of columns.
#'
#' More about Semantic UI \url{https://semantic-ui.com/}
#' can be found here.
#'
#' @import dplyr
#' @import rlang
#'
#' @param data Any data frame.
#' @param class Table class that default to Semantic UI CSS.
#' @param ... Any other DT::datatable parameters.
#'
#' @return Interactive HTML table (DT::datatable).
#' @export
pretty_dt <- function(data, ...) {
  date_cols <- purrr::map_lgl(data, lubridate::is.Date) %>%
    which() %>% unname()
  num_cols <- purrr::map_lgl(data, is.numeric) %>%
    which() %>%unname()
  mod_cols <- c(date_cols, num_cols)

  js_values <- tibble(
    eq = mod_cols - 1,
    data = (ncol(data) + 1):(ncol(data) + length(mod_cols)),
    row = glue::glue("$('td:eq({eq})', row).html(data[{data}]);")
  )

  js <- c(
    "function(row, data) {",
    "console.log(data);",
    pull(js_values, row),
    "}"
  )
  cols_to_hide <- pull(js_values, data)

  options <- list(
    rowCallback = DT::JS(js),
    columnDefs = list(
      list(visible = FALSE, targets = c(0, cols_to_hide))
    )
  )

  date_cols_df <- purrr::map_dfc(date_cols, function(col) {
    d <- data[[col]]
    new_name <- paste0(names(data[, col]), "_mod")

    tibble(
      !!new_name := format(d, format = "%B %Y")
    )
  })

  num_cols_df <- purrr::map_dfc(num_cols, function(col) {
    d <- data[[col]]
    new_name <- paste0(names(data[, col]), "_mod")

    tibble(
      !!new_name := prettyNum(d, " ")
    )
  })

  data <- bind_cols(data, date_cols_df, num_cols_df)
  ret <- DT::datatable(
    data,
    class = "ui celled table",
    options = options,
    ...
  )
  return(ret)
}
