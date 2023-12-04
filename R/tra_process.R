#' Process data and calculate means
#'
#' This function converts the data frame to a tibble, optionally drops and renames columns, groups the data by specified columns and calculates the mean of numeric columns, and optionally writes the processed data to a CSV file.
#'
#' @param input_data A data frame to be processed.
#' @param output_file A character string specifying the path and name of the output CSV file. Default is "meandata/means.csv".
#' @param cols_drop A character vector of column names to be dropped from the input data. Default is NULL.
#' @param cols_rename A named character vector of new names for the columns to be renamed. For example, c(old_name = "new_name"). Default is NULL.
#' @param cols_group A character vector of column names to be used for grouping the data. Default is NULL.
#' @param to_csv A logical value indicating whether to write the processed data to a CSV file. Default is FALSE.
#' @return A tibble of the processed data with means of numeric columns and first values of character columns.
#' @importFrom tibble as_tibble
#' @importFrom readr write_csv
#' @importFrom dplyr glimpse select rename group_by add_tally
#' @export
tra_process <- function(input_data, output_file = "meandata/means.csv",  cols_drop = NULL, cols_rename = NULL, cols_group = NULL, to_csv = FALSE) {

   # Convert the data frame to a tibble
  data <- tibble::as_tibble(input_data)

  # Check the structure
  dplyr::glimpse(data)


  # If needed, delete a specific column
  if (!is.null(cols_drop)) {
    data <- data %>%
      dplyr::select(-all_of(cols_drop))
  }

    # If needed, rename specific columns
  if (!is.null(cols_rename)) {
    data <- dplyr::rename(data, !!cols_rename)
    }


   # Group the data frame by specified columns and calculate the mean of numeric columns
  if (!is.null(cols_group)) {
    means_data <- data %>%
      dplyr::group_by(across(all_of(cols_group))) %>%
      dplyr::add_tally() %>%
      dplyr::summarize(across(where(is.numeric), mean, na.rm = TRUE),
                       across(where(is.character), ~first(.)),.groups="drop")
    dplyr::glimpse(means_data)

  }

      else if (is.null(cols_group)) {print("Missing group parameter 'cols_group'. Set grouping variables from which the rows shall get meaned, otherwise it is not possible to create any summarized rows.")}


    if(to_csv) {
    # Write the processed data frame to a CSV file
    readr::write_csv(means_data, file = output_file)
  }

  dplyr::glimpse(means_data)

  df <- return(means_data)
}
