#' Import environmental data
#' This function imports environmental data and creates new columns containing the name of the state and the env_id which is a paste object of the state, the bdf, the year and the month.

#' @param input A tibble containing the environmental data.
#' @param output_file The path to the output file. Default is "import/env_import_{state_name}.csv".
#' @param str_output The path to the output file for the structured data frame. Default is “str.csv".
#' @param state_name The name of the state. Default is "Bundesland".
#' @param to_csv A logical value indicating whether to create a csv file. Default is FALSE.
#' @return A list containing two objects: env_import and str_env_import.
#' @importFrom dplyr mutate select everything
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom tibble as_tibble
#' @export
tra_env <- function(input, output_file = glue::glue("import/env_import_{state_name}.csv"), str_output = "str.csv", state_name = "Bundesland", to_csv = FALSE) {

  env = tibble::as_tibble(input)

  # Create new columns containing the name of the state and the env_id which is a paste object of the state, the bdf, the year and the month
  env_import = env %>%
    dplyr::mutate(state = state_name,
                  env_id = sprintf("%s-%s_%04d-%02d_env", state, bdf, year, month)) %>%
    dplyr::select(bdf, year, month, everything())

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

  str_env_import = structure(env_import)

  if(to_csv) {
    readr::write_csv(str_env_import, file = str_output)
  }
  # if to_csv = TRUE R directly creates a csv file in the specified output_path
  if(to_csv){
    readr::write_csv(env_import, file = output_file)
  }

  return(list(env_import = env_import, str_env_import = str_env_import))
}

