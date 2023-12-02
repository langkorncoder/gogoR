#' Create boxplots for numeric columns in a data frame
#'
#' This function creates boxplots for all numeric columns in a data frame. The boxplots can be colored by a specified variable and faceted by another variable.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_var A character string specifying the variable to be plotted on the x-axis. Default is "origin".
#' @param color_var A character string specifying the variable to be used for coloring the boxplots. Default is NULL.
#' @param title_prefix A character string specifying the prefix for the plot titles. Default is "Boxplot of".
#' @param theme_options A named list of ggplot2 theme options to be applied to the plots. Default is an empty list.
#' @param facet_var A character string specifying the variable to be used for faceting the boxplots. Default is NULL.
#' @param color_values A vector of color values to be used for coloring the boxplots. Default is NULL.
#' @param save_plots A logical value indicating whether the plots should be saved as .jpg files. Default is FALSE.
#' @return A list of ggplot2 objects containing the boxplots.
#'
#' @import ggplot2
#' @importFrom ggpubr ggboxplot
#' @importFrom ggplot2 theme element_text element_rect scale_y_continuous facet_wrap
#' @export
des_boxplots <- function(data, x_var = "origin", color_var = NULL, title_prefix = "Boxplot of", theme_options = list(), facet_var = NULL, color_values = NULL, save_plots = FALSE) {
  # Create an empty list to store the boxplots
  boxplots <- list()

  # Loop over the names of the columns
  for (k in names(data)) {
    # Check if the k-th column is numeric
    if (is.numeric(data[[k]])) {
      # Create a boxplot for the k-th column
      if (is.null(color_var)) {
        bxp <- ggpubr::ggboxplot(
          data, x = x_var, y = k,
          title = paste(title_prefix, k),
          bxp.errorbar = TRUE
        )
      } else {
        bxp <- ggpubr::ggboxplot(
          data, x = x_var, y = k,
          color = color_var,
          title = paste(title_prefix, k),
          bxp.errorbar = TRUE
        )
      }

      # Customize the theme
      bxp <- bxp + ggplot2::theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        legend.text = element_text(size = 6)  # Set the legend text size to 8
      ) + ggplot2::scale_y_continuous(minor_breaks = waiver())

      # Apply the user-specified theme options
      for (option in names(theme_options)) {
        bxp <- bxp + theme_options[[option]]
      }

      # Apply facet_wrap if facet_var is not NULL
      if (!is.null(facet_var)) {
        bxp <- bxp + ggplot2::facet_wrap(as.formula(paste("~", facet_var)))
      }

      # Apply custom color values if color_values is not NULL
      if (!is.null(color_values)) {
        bxp <- bxp + ggplot2::scale_color_manual(values = color_values)
      }

      # Save the boxplot in the list
      boxplots[[k]] <- bxp
    }
  }

  # If the user chooses to save the plots, save them as .jpg files
  if (save_plots) {
    # Create a folder to store the plots
    dir.create("boxplots")
    for (k in names(data)) {
      # Check if the k-th column is numeric
      if (is.numeric(data[[k]])) {
        # Create a file name for the boxplot
        filename <- paste("boxplots/boxplot_spalte", k, ".jpg", sep = "")

        # Save the boxplot as a .jpg file
        ggplot2::ggsave(filename, boxplots[[k]])
      }
    }
  }

  # Return the list of boxplots
  return(boxplots)
}
