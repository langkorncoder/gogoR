#' Provides information about the structure of a dataframe
#'
#' This function provides how the dataframe of your interest is structured. Names of column, their position, class, nrows, unique values and examples. Also gives possibility to export dataframe as .csv file.
#'
#' @param newdf The dataframe of interest
#' @param to_csv Logical; whether to write the dataframe to a .csv file
#' @param output_file The name of the output file
#'
#' @return A dataframe with information about the structure of the input dataframe
#'
#' @export
structured <- function(newdf = NULL, to_csv = FALSE, output_file = "structured.csv") {
  if(is.null(newdf)) {
    stop("Please provide a dataframe")
  }
  vars <- colnames(newdf)
  position <- seq_along(vars)
  class <- sapply(newdf, class)
  nrows <- nrow(newdf)
  n_unique <- sapply(newdf, function(x) length(unique(x)))
  n_observations <- sapply(newdf, function(x) sum(!is.na(x)))
  rate <- n_observations/nrows
  example <- sapply(newdf, function(x) paste0(x[1], ", ", x[5]))
  df_structured <- data.frame(variable = vars, position = position, class = class, nrows = nrows, n_unique = n_unique, n_obs = n_observations, rate = rate, example_rows = example)
  if(to_csv == TRUE) {
    readr::write_csv(df_structured, file = output_file)
  }
  return(df_structured)
}
