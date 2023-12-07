#' @title Import environmental data
#' This function imports environmental data and creates new columns containing the name of the state and the env_id, which is a paste object of the state, the bdf, the year, and the month.
#'
#' @param input A tibble containing the environmental data.
#' @param output_file The path to the output file. Default is "import/env_import_{state_name}.csv".
#' @param str_output The path to the output file for the structured data frame. Default is “str.csv".
#' @param state_name The name of the state. Default is "Bundesland".
#' @param to_csv A logical value indicating whether to create a CSV file. Default is FALSE.
#' @param sprintf_format The format to be used in the sprintf function. Default is "%s-%s_%04d-%02d_env". This parameter allows you to specify the format of the `env_id` string. The format should be a string that contains placeholders for the `state`, `bdf`, `year`, and `month` variables. For example, if all variables are character strings, you could use "%s-%s_%s-%s_env". If `year` and `month` are numeric, and `state` and `bdf` are character strings, you could use "%s-%s_%04d-%02d_env".
#' @return A list containing two objects: env_import and str_env_import.
#' @importFrom dplyr mutate select everything
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' \dontrun{
#' # Load required packages
#' library(tibble)
#'
#' # Create a sample input dataset
#' input <- tibble::tibble(
#'   bdf = c("bdf1", "bdf2", "bdf3"),
#'   year = c(2021, 2022, 2023),
#'   month = c(12, 1, 2),
#'   var1 = c(1.1, 2.2, 3.3),
#'   var2 = c(4.4, 5.5, 6.6)
#' )
#'
#' # Example 1: Default sprintf format
#' result1 <- gogor::tra_env2(input)
#' print(result1)
#'
#' # Example 2: Custom sprintf format with all variables as character strings
#' sprintf_format2 <- "%s-%s_%s-%s_env"
#' result2 <- gogor::tra_env2(input, sprintf_format = sprintf_format2)
#' print(result2)
#'
#' # Example 3: Custom sprintf format with year and month as numeric
#' sprintf_format3 <- "%s-%s_%04d-%02d_env"
#' result3 <- gogor::tra_env2(input, sprintf_format = sprintf_format3)
#' print(result3)
#'
#' # Example 4: Write the result to a CSV file
#' output_file4 <- "env_import.csv"
#' str_output4 <- "str_env_import.csv"
#' result4 <- gogor::tra_env2(input, output_file = output_file4, str_output = str_output4, to_csv = TRUE)
#' print(result4)
#' }
#' @export
tra_env2 <- function(input, output_file = glue::glue("import/env_import_{state_name}.csv"), str_output = "str.csv", state_name = "Bundesland", to_csv = FALSE, sprintf_format = "%s-%s_%04d-%02d_env") {
  # Function body...

  env <- tibble::as_tibble(input)

  # Create new columns containing the name of the state and the env_id, which is a paste object of the state, the bdf, the year, and the month
  env_import <- env %>%
    dplyr::mutate(state = state_name,
                  env_id = sprintf(sprintf_format, state, bdf, year, month)) %>%
    dplyr::select(bdf, year, month, everything())

  # Check if the new columns occurred by performing structured
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

  str_env_import <- structure(env_import)

  if (to_csv) {
    readr::write_csv(str_env_import, file = str_output)
  }
  # If to_csv = TRUE, R directly creates a CSV file in the specified output_path
  if (to_csv) {
    readr::write_csv(env_import, file = output_file)
  }

  return(list(env_import = env_import, str_env_import = str_env_import))
}
