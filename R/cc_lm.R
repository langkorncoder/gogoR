#' @title cc_lm - Funktion zur Vergleichsanalyse von linearen Modellen
#' @description Diese Funktion führt eine Vergleichsanalyse von linearen Modellen für eine gegebene Zielvariable und eine Liste von Effekten durch.
#' @param target Zielvariable
#' @param effects Liste der Effekte
#' @param data Datenrahmen
#' @param test Teststatistik
#' @return Eine Liste, die ANOVA-Tabelle, die model_list und die summary_list enthält
#' @importFrom stats lm
#' @import stats
#' @examples
#' # Erstellen Sie eine Liste von Effekten
#' effects <- list("cyl", "disp")
#' cc_lm(target = "mpg", effects = effects, data = mtcars, test = "Chisq")
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{anova}}
#' @export
cc_lm <- function(target, effects, data, test = "Chisq") {
  # Überprüfen Sie, ob die Effekte eine Liste sind
  if (!is.list(effects)) {
    stop("effects must be a list of effects, see help_fct('create_compare_lm'")
  }
  # Erstellen Sie eine Liste von Modellen mit lapply
  model_list <- lapply(effects, function(x) lm(as.formula(paste(target, "~", x)), data = data, REML = TRUE))
  # Vergleichen Sie die Modelle mit do.call und anova
  anova_table <- do.call(stats::anova, c(model_list, test = test))
  # Erstellen Sie eine Liste von Summaries mit lapply
  summary_list <- lapply(model_list, summary)
  # Geben Sie die ANOVA-Tabelle, die model_list und die summary_list zurück
  results <- return(list(anova_table = anova_table, model_list = model_list, summary_list = summary_list))
}
