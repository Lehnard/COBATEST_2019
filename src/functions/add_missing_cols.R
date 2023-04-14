# # LINES TO TEST:
# disagg_dt <- disagg_aidsfondet
# colmap_dt <- col_maps            # EXCEL maping file previously loaded.
# colmap_ref_column <- "COBATEST_Variable_name"

add_missing_cols <- function(disagg_dt, colmap_dt, colmap_ref_column) {
  ## VERSION:
  #     18-06-2021
  #
  ## DESCRIPTION:
  #     Afegeix les columnes que falten al dataset de dades agregades d'acord amb
  #     la columna triada de referència en l'arxiu EXCEL de mapeig de nomes de columnes. 
  #
  ## PARAMETRES: 
  #   - disagg_dt: dataframe a unificar el nom de les seves variables amb les del diccionari de dades (colmap_dt).
  #   - colmap_dt: dataframe diccionari on es mapegen les variables dels datasets per nom.
  #   - colmap_ref_column: En el dataframe diccionari (colmap_dt), el nom de la columna amb els noms de variables estàndards. 
  
  # Columnes que no es troben a disagg_dt. 
  mis_cols <- colmap_dt[!get(colmap_ref_column) %in% colnames(disagg_dt), get(colmap_ref_column)] 
  if (length(mis_cols) != 0) disagg_dt[, (mis_cols) := as.character(NA)]  # Afegides amb NAs. 
  return(disagg_dt)}