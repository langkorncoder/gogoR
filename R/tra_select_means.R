#' Selects and formats data from a tibble
#'
#' This function selects columns from a tibble and formats the data by adding year and month columns.
#'
#' @param input A tibble containing data to be selected and formatted.
#' @param output_file A character string specifying the name of the output file. Default is "means_bundesland/means_bundesland.csv".
#' @param selects A character vector specifying the names of the columns to be selected. Default is NULL.
#' @param date_format A character vector specifying the format of the date column. Default is c("dmy", "mdy", "ymd", "dBy", "mdBy", "ymdHMS").
#' @param date_col A character string specifying the name of the date column. Default is "datum".
#' @param to_csv A logical value indicating whether to write the output to a CSV file. Default is FALSE.
#'
#' @return A list containing the selected and formatted data and its structure.
#'
#' @examples
#' tra_select_means(input = data, selects = c("col1", "col2"), date_col = "date", to_csv = TRUE)
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate
#' @importFrom lubridate parse_date_time year month
#' @importFrom readr write_csv
#' @importFrom rlang sym
#' @export
tra_select_means <- function(input, output_file = "means_bundesland/means_bundesland.csv", selects = NULL, date_format = c("dmy", "mdy", "ymd", "dBy", "mdBy", "ymdHMS"), date_col = "datum", to_csv = FALSE) {

  means_select <- tibble::as_tibble(input)

  if (!is.null(selects)) {
    means_select <- means_select %>%
      dplyr::select(all_of(selects)) %>%
      dplyr::mutate(year = year(lubridate::parse_date_time(!!sym(date_col), orders = date_format)),
                    month = month(lubridate::parse_date_time(!!sym(date_col), orders = date_format)))
    # check if the new columns occured by performing structured
    structure <- function(newdf) {
      vars <- colnames(newdf)
      position <- seq_along(vars)
      class <- sapply(newdf, class)
      nrows <- nrow(newdf)
      n_unique <- sapply(newdf, function(x) length(unique(x)))
      n_observations <- sapply(newdf, function(x) sum(!is.na(x)))
      rate <- n_observations/nrows
      example <- sapply(newdf, function(x) paste0(x[1], ", ", x[5]))
      df_structured <- data.frame(variable = vars, position = position, class = class, nrows = nrows, n_unique = n_unique, n_obs = n_observations, rate = rate, example_rows = example)
    }
    means_structure <- structure(means_select)

    if (to_csv) {
      readr::write_csv(means_select, file = output_file)
    }

    df <- return(list(means_select = means_select, means_structure = means_structure))
  }
}

