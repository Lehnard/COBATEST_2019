standarize_colnames <- function(disagg_dt, colmap_dt, colmap_from_column, colmap_to_column) {
  ## VERSION:
  #     18-06-2021
  #
  ## DESCRIPTION:
  #     Estandaritza els noms de columnes dels diferents datasets de dades desagregades utilitzant un fitxer Excel (.xlsx) 
  #     de mapeig. Aquest Excel té  una columna amb els noms de referència de les variables de COBATEST i en 
  #     la resta de columnes els noms corresponents de les variables dels datasets de dades desagregades.   
  #
  ## PARAMETRES: 
  #   - disagg_dt: dataframe a unificar el nom de les seves variables amb les del diccionari de dades (colmap_dt).
  #   - colmap_dt: dataframe diccionari on es mapegen les variables dels datasets per nom.
  #   - colmap_from_column:  En el dataframe diccionari (colmap_dt), el nom de la columna amb els noms de variables del dt. 
  #   - colmap_to_column: En el dataframe diccionari (colmap_dt), el nom de la columna amb els noms de variables estàndards.
  
  #   0. Detectem i guardem les noves variables generades durant el preprocessament anterior.
  dt_prerpoc_new_varnames <- colnames(disagg_dt)[!colnames(disagg_dt) %in% colmap_dt[, get(colmap_from_column)] &
                                                   colnames(disagg_dt) %in% colmap_dt[, get(colmap_to_column)]]
  new_cols <- disagg_dt[, ..dt_prerpoc_new_varnames]
  #   1. Seleccionem variables de DT que es corresponguin amb les del Diccionari. 
  dt_COBA_names <- colmap_dt[!is.na(get(colmap_from_column)), get(colmap_from_column)]
  disagg_dt <- disagg_dt[, ..dt_COBA_names]
  #   2. Reanomenem les variables restants de DT en funcio del nom de referencia al Diccionari.
  ref_COBA_names <- colmap_dt[!is.na(get(colmap_from_column)), get(colmap_to_column)]
  setnames(x = disagg_dt, old = colnames(disagg_dt), new = ref_COBA_names)
  #   3. Afegim les noves variables generades durant el preprocessament.
  disagg_dt <- cbind(disagg_dt, new_cols)
  return(disagg_dt)}


