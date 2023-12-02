#' Perform Shapiro-Wilk test on all numeric variables in a data frame
#'
#' This function performs the Shapiro-Wilk test of normality on all numeric
#' variables in a given data frame. It returns a data frame with the test
#' statistic and p-value for each variable. It also allows the user to save the
#' results to a CSV file, and to drop some columns from the data frame before
#' testing.
#'
#' @param input A data frame or a tibble.
#' @param to_csv A logical value indicating whether to save the results to a CSV
#'   file or not. Default is FALSE.
#' @param drop_cols A character vector of column names to be dropped from the
#'   data frame before testing. Default is NULL.
#' @param output_file A character string specifying the name of the output file
#'   if to_csv is TRUE. Default is "output.csv".
#' @importFrom tibble as_tibble
#' @importFrom dplyr across select
#' @importFrom purrr map map_dfr compact
#' @importFrom readr write_csv
#' @return A data frame with three columns: data, W, and p_value. The data
#'   column contains the names of the numeric variables in the input data frame.
#'   The W column contains the Shapiro-Wilk test statistic for each variable.
#'   The p_value column contains the p-value for each variable.
#'
#' @examples
#' # Use the mtcars data set as an example
#' data(mtcars)
#'
#' # Run the des_shapiro function on the mtcars data set
#' des_shapiro(mtcars)
#'
#' # Save the results to a file named "mtcars_shapiro.csv"
#' des_shapiro(mtcars, to_csv = TRUE, output_file = "mtcars_shapiro.csv")
#'
#' # Drop the vs and am columns before testing
#' des_shapiro(mtcars, drop_cols = c("vs", "am"))
#'
#' @export
des_shapiro <- function(input, to_csv = FALSE, drop_cols = NULL,  output_file = "output.csv") {
  data <- tibble::as_tibble(input)

  if(!is.null(drop_cols)) {
    data <-  data %>%
      dplyr::select(- any_of(drop_cols))
  }

  numeric_columns <- sapply(data, is.numeric)
  column_numbers <- which(numeric_columns)

  shapiro_results <- purrr::map(data[, column_numbers], ~ {
    x <- na.omit(.x)
    if (length(unique(x)) > 1 && sd(x) != 0) shapiro.test(x)
  })

  shapiro_results <- purrr::compact(shapiro_results)

  shapiro_results_df <- purrr::map_dfr(shapiro_results, ~ data.frame(W = .x$statistic, p_value = .x$p.value))

  shapiro_results_df$Variable <- names(shapiro_results)

  rownames(shapiro_results_df) <- NULL

  shapiro_results_df <- shapiro_results_df %>%
    dplyr::select(Variable, everything())

  # Add significance column
  shapiro_results_df$significance <- dplyr::case_when(
    shapiro_results_df$p_value < 0.001 ~ "***",
    shapiro_results_df$p_value < 0.01 ~ "**",
    shapiro_results_df$p_value < 0.05 ~ "*",
    TRUE ~ ""
  )

  if (to_csv) {
    readr::write_csv(shapiro_results_df, output_file)
  }

  return(shapiro_results_df)
}

