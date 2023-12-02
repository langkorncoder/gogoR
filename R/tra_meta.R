#' Transform data and generate metadata
#'
#' This function takes a data frame or a tibble as input and performs some transformations and metadata generation based on the specified columns to group by. It returns a list of three data frames: one with the group indices, one with the group variables, and one with the first rows of each group. Optionally, it can also write the data frames to CSV files in a specified directory.
#' It can be used to describe the grouped data frames when you are presenting a grouped transformation.
#' @param input A data frame or a tibble to be transformed and analyzed.
#' @param cols_group A character vector of column names that shall be grouped by.
#' @param index_output A character string of the file path where the group indices data frame will be written to, if to_csv is TRUE. Default is "metadata/group_indices_df.csv".
#' @param vars_output A character string of the file path where the group variables data frame will be written to, if to_csv is TRUE. Default is "metadata/group_vars_df".
#' @param firstrows_output A character string of the file path where the first rows data frame will be written to, if to_csv is TRUE. Default is "metadata/first_rows_df.csv".
#' @param to_csv A logical value indicating whether to write the data frames to CSV files or not. Default is FALSE.
#' @return A list of three data frames: group_indices_df, group_vars_df, and first_rows_df.
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr group_by mutate slice_head
#' @importFrom readr write_csv
#' @export

tra_meta <- function(input, cols_group = NULL, index_output = "metadata/group_indices_df.csv", vars_output = "metadata/group_vars_df", firstrows_output = "metadata/first_rows_df.csv", to_csv = FALSE ) {

  data_tibble <- tibble::as_tibble(input)

  if(is.null(cols_group)){stop("Error :// Set cols_group to determine the columns that shall be grouped.")}
  else {

  grouped_meta <- data_tibble %>%
   dplyr::group_by(across(all_of(cols_group)))

  # Create a data frame of group indices
  group_indices_df <- grouped_meta %>%
   dplyr::mutate(group_index = cur_group_id())

  # Create a data frame of group variables
  group_vars_df <- tibble::tibble(group_vars = grouped_meta %>% group_vars())

  # Get the first rows of each group
  first_rows_df <- grouped_meta %>%
    dplyr::slice_head(n = 1)
  }

  if(to_csv) {
  # Write the data frames to CSV files
  readr::write_csv(group_indices_df, index_output)
  readr::write_csv(group_vars_df, vars_output)
  readr::write_csv(first_rows_df, firstrows_output)
  }

  return(list(group_indices_df = group_indices_df, group_vars_df = group_vars_df, first_rows_df = first_rows_df))
}
