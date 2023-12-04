##########
#' Create custom histograms based on user inputs
#'
#' This function creates histograms for one or more numeric columns in a data frame,
#' using the ggplot2 package. The user can specify the column(s) to plot, the color
#' variable, the facet variable, and other options. The function also creates a new
#' column with a unique identifier for each histogram, which can be used for further
#' analysis or saved to a CSV file.
#'
#' @param input A data frame containing the data to plot.
#' @param type A character string indicating the type of histograms to create.
#'   Possible values are "all", "specific", or "id_only". Default is "specific".
#' @param hist_column A character string giving the name of the column in the input
#'   data frame that contains the values to plot. Required if type is "specific" or
#'   "id_only".
#' @param value_column A character string giving the name of the column in the input
#'   data frame that contains the values to plot. Required if type is "all".
#' @param id_name A named character vector giving the new name for the column that
#'   contains the histogram identifier. Default is c(method_id = "hist_id").
#' @param save_plots A logical value indicating whether to save the histograms as
#'   .jpg files in a folder named "histograms". Default is FALSE.
#' @param to_csv A logical value indicating whether to save the processed data frame
#'   to a CSV file. Default is FALSE.
#' @param output_file A character string giving the name of the CSV file to save the
#'   data to. Default is "data.csv".
#' @param color_var A character string giving the name of the column in the input
#'   data frame that contains the variable to use for coloring the histograms.
#'   Optional.
#' @param color_values A named character vector giving the custom color values to use
#'   for the color_var. Optional.
#' @param facet_var A character string giving the name of the column in the input
#'   data frame that contains the variable to use for faceting the histograms.
#'   Optional.
#' @return A list containing the processed data frame and the histograms.
#' @import ggplot2
#' @importFrom tibble as_tibble
#' @importFrom readr write_csv
#' @importFrom dplyr rename
#' @export
# Define a function that creates custom histograms based on user inputs
des_hist <- function(input, type = "specific",  hist_column = NULL, value_column = NULL, id_name = c(method_id = "hist_id"), save_plots = FALSE, to_csv = FALSE, output_file = "data.csv", color_var = NULL, color_values = NULL, facet_var = NULL) {

  # Convert the input to a tibble object
  data <- tibble::as_tibble(input)

  # Create an error message for invalid type argument
  error_message_type <- "Error :/ Did you specify 'type' argument? You have to set type argument to 'all', 'specific' or 'none'. Check help(des_hist) for more information :)"

  # Create an error message for missing hist_column argument
  error_message_hist_column <- "Error :/ You have to specify the 'hist_column' argument to create unique 'hist_id' that can be plotted afterwards or can be saved in a new column."

  # Create a warning message for default id_name argument
  warning_message_id_name <- "Warning! Are you sure you dont want to rename the id column? Otherwise it is named 'hist_id'"

  # Check if the type argument is NULL
  if (is.null(type)) {
    # Stop the function and show the error message
    stop(error_message_type)
  }

  # Use switch to handle different values of type argument
  switch(type,
         # If type is "all", create histograms for all numeric columns
         all = {
           # Create an empty list to store the histograms
           histograms <- list()

           # Loop over the names of the columns
           for (k in names(data)) {
             # Check if the k-th column is numeric
             if (is.numeric(data[[k]])) {
               # Create a histogram for the k-th column
               # Create a histogram for the k-th column
               if (is.null(color_var)) {
                 histograms[[k]] <- data %>%
                   ggplot2::ggplot(aes(x = .data[[k]])) +
                   ggplot2::geom_density() +
                   ggplot2::theme_minimal()

               } else {
                 histograms[[k]] <- data %>%
                   ggplot2::ggplot(aes(x = .data[[k]], color = .data[[color_var]])) +
                   ggplot2::geom_density() +
                   ggplot2::theme_minimal()
               }

               # Apply facet_wrap if facet_var is not NULL
               if (!is.null(facet_var)) {
                 histograms[[k]] <- histograms[[k]] + ggplot2::facet_wrap(as.formula(paste("~", facet_var)))
               }

               # Apply custom color values if color_values is not NULL
               if (!is.null(color_values)) {
                 histograms[[k]] <- histograms[[k]] + ggplot2::scale_color_manual(values = color_values)
               }

               # Save the histogram in the list
               histograms[[k]] <- histograms
               # This line is not needed

             }
           }

           # If the user chooses to save the plots, save them as .jpg files
           if (save_plots) {
             # Create a folder to store the plots
             dir.create("histograms")
             for (k in names(data)) {
               # Create a file name for the histogram
               filename <- paste("histograms/histogram_column", k, ".jpg", sep = "")

               # Save the histogram as a .jpg file
               ggplot2::ggsave(filename, histograms[[k]])
             }
           }
         },
         # If type is "specific", create histograms for a specific column
         specific = {

           # Check if the hist_column argument is NULL
           if(is.null(hist_column)) {
             # Stop the function and show the error message
             stop(error_message_hist_column)
           }

           # Create a new column named "hist_id" containing a number for each unique value in the hist_column
           data$hist_id <- as.integer(factor(data[[hist_column]], levels = unique(data[[hist_column]])))

           # Create a histogram for the value_column
           if (is.null(color_var)){

             histograms <- data %>%
               ggplot2::ggplot(aes(x = .data[[value_column]])) +
               ggplot2::geom_density() +
               ggplot2::facet_wrap(~ hist_id, scales = "free") +
               ggplot2::theme_minimal()}

           else {
             histograms <- ggplot2::ggplot(data, aes(x = .data[[value_column]], color = .data[[color_var]])) +
               ggplot2::geom_density() +
               ggplot2::facet_wrap(~ hist_id, scales = "free") +
               ggplot2::theme_minimal()}

           # Apply custom color values if color_values is not NULL
           if (!is.null(color_values)) {
             histograms <- histograms + ggplot2::scale_color_manual(values = color_values)
           }

           # If the id_name argument is NULL, show the warning message
           if (is.null(id_name)) {
             print(warning_message_id_name)
           } else {
             # Rename the hist_id column according to the id_name argument
             data <- dplyr::rename(data, !!id_name)
           }

           # If the user chooses to save the plots, save them as .jpg files
           if (save_plots) {
             # Create a folder to store the plots
             dir.create("histograms")
             # Create a file name for the histogram
             filename <- paste("histograms/histogram_column", data$hist_id , ".jpg", sep = "")

             # Save the histogram as a .jpg file
             ggplot2::ggsave(filename, histograms[[k]])
           }
         },
         # If type is "id_only", create a new column with hist_id and do not create any plots
         id_only = {
           # Check if the hist_column argument is NULL
           if(is.null(hist_column)) {
             # Stop the function and show the error message
             stop(error_message_hist_column)
           }

           # Create a new column named "hist_id" containing a number for each unique value in the hist_column
           data$hist_id <- as.integer(factor(data[[hist_column]], levels = unique(data[[hist_column]])))

           # If the id_name argument is NULL, show the warning message
           if (is.null(id_name)) {
             print(warning_message_id_name)
           } else {
             # Rename the hist_id column according to the id_name argument
             data <- dplyr::rename(data, !!id_name)
           }

           # If the user chooses to save the data, write it to a CSV file
           if(to_csv) {
             # Write the processed data frame to a CSV file
             readr::write_csv(data, file = output_file)
           }
         }
  )

  # Use the dplyr package to check the structure of the data
  dplyr::glimpse(data)

  # Return the output as a list containing the data and the histograms
  return(list(data = data, histograms = histograms))
}



