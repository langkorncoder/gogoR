#' Rename Rows in Data Frame
#'
#' Rename rows in a data frame based on specified patterns and replacements.
#'
#' @param input The input data frame.
#' @param type The type of row renaming. Must be set to either 'all' or 'specific'.
#' This determines where to rename rows. Check help(row_rename) for additional explanations.
#' @param origin_col The name of the column that contains the original values.
#' This argument is required when type is set to 'specific'.
#' @param new_col The name of the column where the renamed values should be stored.
#' This argument is required when type is set to 'specific' and overwrite is set to FALSE.
#' @param from A character vector of patterns to be replaced.
#' @param to A character vector of replacement strings.
#' @param overwrite Should the original column be overwritten with the renamed values?
#' This argument is only applicable when type is set to 'specific' and new_col is not provided.
#' If TRUE, the original column will be overwritten with the renamed values. If FALSE, a new column
#' will be created with the renamed values.
#' @param tibble Should the input data frame be converted to a tibble?
#' Defaults to TRUE.
#' @param output_file The file path where the renamed data frame should be saved as a CSV file.
#' Defaults to "row_rename/renamed_input.csv".
#' @param to_csv Should the renamed data frame be saved as a CSV file?
#' Defaults to FALSE.
#'
#' @importFrom dplyr mutate across everything if_else
#' @importFrom tibble as_tibble
#' @importFrom readr write_csv
#' @importFrom stringr str_replace_all
#' @export
row_changer <- function(input, type = NULL, origin_col = NULL, new_col = NULL, from= NULL, to = NULL, overwrite = FALSE, tibble = TRUE, output_file = "row_rename/renamed_input.csv", to_csv = FALSE) {

  # Define error messages
  error_message_type <- "Error: Type must be set to either 'all' or 'specific', determining where to rename rows. Check help(row_rename) for additional explanations."
  error_message_all <- "Error: 'from' and 'to' arguments must be set. Otherwise the function won't work. Check help(row_rename) for additional explanations."
  error_message_specific_origin_cols <- "Error: Be sure to change default argument 'origin_col'. Check help(row_rename) for additional explanations."
  error_message_specific_new_cols <- "Error: Be sure to change default arguments 'origin_col' and 'new_col'. Check help(row_rename) for additional explanations."
  error_message_fromto <- "Error: 'from' and 'to' arguments must be set. Otherwise the function won't work. Check help(row_rename) for additional explanations."

  # Check if type is NULL
  if(is.null(type)) {
    stop(error_message_type)
  }

  # Convert input to tibble if tibble is TRUE
  if (tibble) {
    data <- tibble::as_tibble(input)
  } else {
    data <- input
  }

  # Switch operation based on type
  switch(type,
         "all" = {
           # Check if 'from' or 'to' is NULL
           if(is.null(from) || is.null(to)) {
             stop(error_message_fromto)
           }
           # Loop through 'from' vector and replace all instances in data

           for (i in seq_along(from)) {
             data <- data %>%
               dplyr::mutate(across(everything(), ~ stringr::str_replace_all(., from[i], to[i])))
           }
           origin_new_cols <- data %>%
             dplyr::distinct(pick(origin_col)) %>%
             dplyr::pull(origin_col)
         }
         ,
         "specific" = {
           # Check if 'from' or 'to' is NULL
           if(is.null(from) || is.null(to)) {
             stop(error_message_fromto)
           }
           # If overwrite is TRUE, replace values in origin_col
           if(overwrite) {
             if(is.null(origin_col)) {
               stop(error_message_specific_origin_cols)
             }
             data <- data %>%
               dplyr::mutate(across(matches(origin_col), ~ if_else(. == from, to, .)))
           } else {
             # If overwrite is FALSE, create new column with replaced values
             if(is.null(origin_col) || is.null(new_col)) {
               stop(error_message_specific_new_cols)
             }

             # Use mutate() instead of rename_with() to create a new column
             data <- data %>%
               dplyr::mutate({{new_col}} := stringr::str_replace_all(!!sym(origin_col), from, to))

             origin_new_cols <- data %>%
               dplyr::distinct(pick(origin_col, new_col)) %>%
               dplyr::pull(origin_col, name = new_col)
           }
         })

  # Print the structure of the data
  dplyr::glimpse(data)

  # If to_csv is TRUE, write data to CSV file
  if(to_csv) {
    readr::write_csv(data, file = output_file)
  }

  # Return data or list of data and origin_new_cols based on type
  return(list(data = data, origin_new_cols = origin_new_cols)) }


