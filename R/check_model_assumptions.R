#' Überprüft die Modellannahmen
#'
#' Diese Funktion überprüft die Modellannahmen für das gegebene Modell und gibt eine Liste mit den Ergebnissen zurück.
#'
#' @param model Ein Modellobjekt, das überprüft werden soll.
#'
#' @return Eine Liste mit den Ergebnissen der Normalitätstests für die Residuen, dem Breusch-Pagan-Test für die Heteroskedastizität der Residuen und dem Variationsinflationfaktor für die Multikollinearität der Prädiktoren.
#'
#' @import performance
#' @importFrom lmtest bptest
#' @export
check_model_assumptions <- function(model) {
  plot(model)
  performance::check_model(model)

  resid <- residuals(model)

  # Berechne die Normalitätstests für die Residuen
  shapiro_result <- performance::check_normality(model)

  # Berechne den Breusch-Pagan-Test für die Heteroskedastizität der Residuen
  bptest_result <- lmtest::bptest(model)

  # Berechne den Variationsinflationfaktor für die Multikollinearität der Prädiktoren
  vif_result <- performance::check_collinearity(model)

  assumption_results <- return(list(shapiro_result = shapiro_result, bptest_result = bptest_result, vif_result = vif_result))

}
