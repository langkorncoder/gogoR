#' @title cc_glmer - Funktion zur Erstellung von Modellen und Vergleich von Modellen
#' @description Diese Funktion erstellt Modelle und vergleicht sie für eine gegebene Zielvariable und eine Liste von Effekten.
#' @param target Zielvariable
#' @param effects Liste der Effekte
#' @param data Datenrahmen
#' @param test Teststatistik
#' @return Eine Liste mit ANOVA-Tabelle, Modellliste und Zusammenfassungsliste
#' @importFrom lme4 glmer
#' @import stats
#' @seealso \code{\link[lme4]{glmer}}, \code{\link[stats]{anova}}
#' @export
cc_glmer <- function(target, effects, data, test = "Chisq") {
  # Überprüfen Sie, ob die Effekte eine Liste sind
  if (!is.list(effects)) {
    stop("effects must be a list of effects, see help_fct('create_compare_lmer'")
  }
  # Erstellen Sie eine Liste von Modellen mit lapply
  model_list <- lapply(effects, function(x) lme4::glmer(as.formula(paste(target, "~", x)), data = data))
  # Vergleichen Sie die Modelle mit do.call und anova
  anova_table <- do.call(anova, c(model_list, test = test))
  # Erstellen Sie eine Liste von Summaries mit lapply
  summary_list <- lapply(model_list, summary)
  # Geben Sie die ANOVA-Tabelle, die model_list und die summary_list zurück
  results <- return(list(anova_table = anova_table, model_list = model_list, summary_list = summary_list))
}
