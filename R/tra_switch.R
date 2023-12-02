#' @title Switch
#' @param data A data frame.
#' @param type A character string. 'long' means it will be switched from many columns to two new columns, one contains the values and one contains the original column names, this is only necessary if the columns that shall be melted contain any pattern in their name, e.g. starts with x & ends with a number; 'long_melt' means that the names of the columns that shall be melted do not contain any pattern, they can be named totally different, here it is necessary to set the id columns (see ids); 'short' is the opposite of long, so it 'demelts' the data, sometimes this is useful e.g. for linear models;
#' @param ids A numeric vector. It is the id's to colindizes when 'type = long_melt', determines which columns will stay (see type), default is set to NULL.
#' @param variable.name A character string. It is the name of the new column that contains the column names of the melted variables, default is set to 'time'.
#' @param value.var A character string. It is the name of the new column that contains the values of the melted variables, default is set to 'value'.
#' @param melts_long A character string. It is the pattern of the column names that shall be melted (see type = long), default is set to 'x\\d+'.
#' @param time_pattern A character string. It is the pattern of the time information in the column names that match the melts_long pattern, default is set to 'x\\d+'.
#' @param time_prefix A character string. It is the prefix of the time information in the column names that match the melts_long pattern, default is set to 'x'.
#' @param time A numeric vector. It describes the vector of the time values that correspond to the time information in the column names that match the melts_long pattern, default is set to NULL.
#' @param set_numeric If TRUE time column will be set into numeric, which is often necessary for creating plots
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom reshape2 dcast
#' @examples
#' data(mtcars)
#' result <- tra_switch(data = mtcars, type = "long_melt", variable.name = "variable", value.var = "value", ids = 1:3)

#' @export
# Define the tra_switch function with additional parameters
tra_switch <- function(data, type = NULL, variable.name = "time", value.var = "value", melts_long = "x\\d+", set_numeric = TRUE, ids= NULL, time_pattern = "x\\d+", time_prefix = "x", time = NULL) {
  # Define the common part of the error message
  error_message_tra_switch <- "Error :/ You probably forgot to set the type argument!"

  # Prüfen, ob type angegeben wurde
  if (is.null(type)) {
    # Eine Fehlermeldung ausgeben und abbrechen
    stop(error_message_tra_switch)
  } # Prüfen, ob der Datensatz die Zeitpunkte als eigene Spalten hat
  switch(type,
         long = {
           if (any(grepl(melts_long, names(data)))) {
             # Die Zeitpunkte als eigene Spalten zusammenfassen
             data <- reshape2::melt(data, id.vars = names(data)[!grepl(melts_long, names(data))], variable.name = variable.name, value.name = value.var)
           }
           # Die Spalte mit den Zeitpunkten in einen numerischen Wert umwandeln
           if (set_numeric) {
             data[[variable.name]] <- as.numeric(sub(time_pattern, "", data[[variable.name]]))
           }
         },
         long_melt = {
           data <- reshape2::melt(data, id.vars = ids, value.name = value.var)
         },
         short = {
           # Eine Spalte mit Zeitpunkt und eine mit Wert haben
           data <- reshape2::dcast(data, ids ~ paste0(variable.name, "_", time_prefix, time), value.var = value.var)
         },
         stop(error_message_tra_switch)
  )

  # Den umgewandelten Datensatz zurückgeben
  df <- return(data)
}

