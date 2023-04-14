library(stringr)


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




get_agg_data_table <- function(raw_agg_dt, data_rows_idx, data_cols_idx, categories_row_idx, header_row_idx= NA, title_position_idx= c(NA,NA)) {
  
  ## VERSION:
  #     26-06-2021
  #
  ## DESCRIPTION:
  #     Les dades que envien els centres COBATEST tool poden tenir origen en un formulari EXCEL que han d'omplir amb
  #     dades agregades (no individualitzades). En aquest cas l'EXCEL conté tota la informació que hem d'extreure i 
  #     parsejar per a poder-la treballar millor. Aquesta funció extreu la taula delimitada pels índexs de files i 
  #     els índex de columnes. 
  #
  #     Aquesta funció parseja l'EXCEL en la seva estructura definida desde l'any 2019 i qualsevol canvi en l'estructura
  #     pot portar lloc a parsejos errònis. Cada cop que canviï l'EXCEL es fa necessari revisar aquesta funció de lectura. 
  #
  ## PARAMETRES: 
  #   - raw_agg_dt: data.table generat de la lectura del fitxer cru de dades agregades que envien els centres i que 
  #                 que suposa el EXCEL complimentat per aquests.
  #   - data_rows_idx: Valors enters de les files que ocupen la taula. P.ex:  5:9 o c(5,6,7,8,9) o c(3,1,4) 
  #   - data_cols_idx: Valors enters de les columnes que ocupen la taula. 
  #   - header_row_idx: Valor enter de la fila que contenen els headers: all, males, females, etc. 
  #   - title_position_idx: Valors enters (x, y) de positicó de la cel·la que conté el títol de la taula. 
  
  # Capturem les dades de la taula 
  table <- raw_agg_dt[data_rows_idx, ..data_cols_idx]
  
  # Afegim el header de la taula. 
  if(!is.na(header_row_idx)) {colnames(table) <- unlist(raw_agg_dt[header_row_idx, ..data_cols_idx])}
  
  # Afegim columna de categories.
  table[, Category := raw_agg_dt[categories_row_idx, 1]]
  setcolorder(x = table, neworder = c(which(colnames(table) == "Category"), which(colnames(table) != "Category")))
  
  
  # Afegim el títol de la taula com a columna adjunta
  if(!all(is.na(title_position_idx))) {table[, title := unlist(raw_agg_dt[title_position_idx[1], title_position_idx[2], with= FALSE])] }
  
  return(table)}



# # LINES TO TEST:
# raw_agg_dt <- raw_agg_data[["vih"]][["abraco"]]   # raw EXCEL aggregated data file readed.
# center <- 67
# center_name <- "Abraço"


parseig_raw_aggr_data <- function(raw_agg_dt, center, center_name) {
  
  ## VERSION:
  #     25-06-2021
  #
  ## DESCRIPTION:
  #     Les dades que envien els centres COBATEST tool poden tenir origen en un formulari EXCEL que han d'omplir amb
  #     dades agregades (no individualitzades). En aquest cas l'EXCEL conté tota la informació que hem d'extreure i 
  #     parsejar per a poder-la treballar millor. Aquesta funció extreu totes les taules del EXCEL complimentat.
  #
  #     Aquesta funció parseja l'EXCEL en la seva estructura definida desde l'any 2019 i qualsevol canvi en l'estructura
  #     pot portar lloc a parsejos errònis. Cada cop que canviï l'EXCEL es fa necessari revisar aquesta funció de lectura. 
  #
  ## PARAMETRES: 
  #   - raw_agg_dt: data.table generat de la lectura del fitxer cru de dades agregades que envien els centres i que 
  #                 que suposa el EXCEL complimentat per aquests.
  
  
  # Get CBCVT_1 table:
  table_01 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = 8:12,
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = 8:12,
                                 header_row_idx = 7,
                                 title_position_idx = c(6,1))
  
  # Get CBCVT_2 table:
  table_02 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = c(17,21,25,29,33),
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = c(15,19,23,27,31),
                                 header_row_idx = 14,
                                 title_position_idx = c(13,1))
  
  # Get CBCVT_3 table:
  table_03 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = c(39,43,47,51,55),
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = c(37,41,45,49,53),
                                 header_row_idx = 36,
                                 title_position_idx = c(35,1))
  
  # Get CBCVT_4 table:
  table_04 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = c(61,65,69,73,77),
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = c(59,63,67,71,75),
                                 header_row_idx = 58,
                                 title_position_idx = c(57,1))
  
  # Get CBCVT_5 table:
  table_05 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = c(83,87,91,95,99),
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = c(81,85,89,93,97),
                                 header_row_idx = 80,
                                 title_position_idx = c(79,1))
  
  # Get CBCVT_6 table:  (OJU!   El CBVCT_6 del "Report Cobatest final 2018.pdf" NO correspòn al CBVCT_6 de "Aggregated data excel.xlsx")  
  table_06 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = c(105,109,113,117,121),
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = c(103,107,111,115,119),
                                 header_row_idx = 102,
                                 title_position_idx = c(101,1))
  
  # Get CBCVT_7 table:  (OJU!   El CBVCT_6 del "Report Cobatest final 2018.pdf" correspòn al CBVCT_7 de "Aggregated data excel.xlsx")  
  table_07 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = c(127,131,135,139,143),
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = c(125,129,133,137,141),
                                 header_row_idx = 124,
                                 title_position_idx = c(123,1))
  
  # Get CBCVT_8 table: (OJU!   El CBVCT_7 del "Report Cobatest final 2018.pdf" correspòn al CBVCT_8 de "Aggregated data excel.xlsx")  
  table_08 <- get_agg_data_table(raw_agg_dt = raw_agg_dt, 
                                 data_rows_idx = c(149,153,157,161,165),
                                 data_cols_idx = 2:7, 
                                 categories_row_idx = c(147,151,155,159,163),
                                 header_row_idx = 146,
                                 title_position_idx = c(145,1))
  
  
  # Ajuntem taules
  table <- Reduce(function(...) rbind(..., fill= TRUE), mget(ls(pattern = "table_")))
  
  
  # Afegim codi de centre i nom de centre perquè després pugui fer-se un rowbind de les dades de tots els centres.
  table[, center := center]
  table[, center_name := center_name]
  
  
  return(table) 
} 


# # LINES TO TEST:  
# disagg_dt <- disagg_cobatest

recategoritza <- function(disagg_dt) {
  ## VERSION:
  #     23-06-2021
  #
  ## DESCRIPTION:
  #     Les dades es recopilen amb més categories de les que demana COBATEST report. Es 
  #     fa una recategorització d'acord amb els criteris del fitxer: "./COBATEST_2020/data/COBATEST_TOOL_codebook.xlsx"  
  #
  ## PARAMETRES: 
  #   - disagg_dt: datatable amb schema "cobatest tool" que contingui un o varios centres de dades desagregades ajuntats
  #                en un sol dataset.

  # Recategorització de variables.
  
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "Gender")][order(Centre, Gender)]
  disagg_dt[!Gender %in% c("1","2","3"), Gender := NA]    
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "AgeGroup")][order(Centre, AgeGroup)]
  disagg_dt[!AgeGroup %in% c("1","2"), AgeGroup := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "MSM")][order(Centre, MSM)]
  disagg_dt[!MSM %in% c("1","2"), MSM := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SW")][order(Centre, SW)]
  disagg_dt[!SW %in% c("1","2"), SW := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "PWID")][order(Centre, PWID)]
  disagg_dt[!PWID %in% c("1","2"), PWID := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "Migrant")][order(Centre, Migrant)]
  disagg_dt[!Migrant %in% c("1","2"), Migrant := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "EverTested")][order(Centre, EverTested)]
  disagg_dt[!EverTested %in% c("1","2"), EverTested := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ResultLastHIV")][order(Centre, ResultLastHIV)]
  disagg_dt[!ResultLastHIV %in% c("1","2"), ResultLastHIV := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "TestedLastYear")][order(Centre, TestedLastYear)]
  disagg_dt[!TestedLastYear %in% c("1","2"), TestedLastYear := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "TestedLastYearSameCBVCT")][order(Centre, TestedLastYearSameCBVCT)]
  disagg_dt[!TestedLastYearSameCBVCT %in% c("1","2"), TestedLastYearSameCBVCT := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "PreTestCounselling")][order(Centre, PreTestCounselling)]
  disagg_dt[!PreTestCounselling %in% c("1","2"), PreTestCounselling := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ScreeningHIVTest")][order(Centre, ScreeningHIVTest)]
  disagg_dt[!ScreeningHIVTest %in% c("1","2"), ScreeningHIVTest := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ScreeningTestResult")][order(Centre, ScreeningTestResult)]
  disagg_dt[!ScreeningTestResult %in% c("1","2"), ScreeningTestResult := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ScreeningTestResultReceived")][order(Centre, ScreeningTestResultReceived)]
  disagg_dt[!ScreeningTestResultReceived %in% c("1","2"), ScreeningTestResultReceived := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ScreeningPostTestCounselling")][order(Centre, ScreeningPostTestCounselling)]
  disagg_dt[!ScreeningPostTestCounselling %in% c("1","2"), ScreeningPostTestCounselling := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ConfirmatoryHIVTest")][order(Centre, ConfirmatoryHIVTest)]
  disagg_dt[!ConfirmatoryHIVTest %in% c("1","2"), ConfirmatoryHIVTest := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ConfirmatoryHIVTestResult")][order(Centre, ConfirmatoryHIVTestResult)]
  disagg_dt[!ConfirmatoryHIVTestResult %in% c("1","2","3"), ConfirmatoryHIVTestResult := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "ConfirmatoryTestResultReceived")][order(Centre, ConfirmatoryTestResultReceived)]
  disagg_dt[!ConfirmatoryTestResultReceived %in% c("1","2"), ConfirmatoryTestResultReceived := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "LinkageToHealthCare")][order(Centre, LinkageToHealthCare)]
  disagg_dt[!LinkageToHealthCare %in% c("1","2"), LinkageToHealthCare := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "CD4Count")][order(Centre, CD4Count)]
  disagg_dt[CD4Count %in% c("9999"), CD4Count := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HIVTestUsed")][order(Centre, HIVTestUsed)]
  disagg_dt[!HIVTestUsed %in% c("1","2","3"), HIVTestUsed := NA]
  
  
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphEverDiagnosed")][order(Centre, SyphEverDiagnosed)]
  disagg_dt[!SyphEverDiagnosed %in% c("1","2"), SyphEverDiagnosed := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphEverTested")][order(Centre, SyphEverTested)]
  disagg_dt[!SyphEverTested %in% c("1","2"), SyphEverTested := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphTestedLastYear")][order(Centre, SyphTestedLastYear)]
  disagg_dt[!SyphTestedLastYear %in% c("1","2"), SyphTestedLastYear := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphTestedLastYearSameCBVCT")][order(Centre, SyphTestedLastYearSameCBVCT)]
  disagg_dt[!SyphTestedLastYearSameCBVCT %in% c("1","2"), SyphTestedLastYearSameCBVCT := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphScreeningTestResult")][order(Centre, SyphScreeningTestResult)]
  disagg_dt[!SyphScreeningTestResult %in% c("1","2"), SyphScreeningTestResult := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphConfirmatoryTest")][order(Centre, SyphConfirmatoryTest)]
  disagg_dt[!SyphConfirmatoryTest %in% c("1","2"), SyphConfirmatoryTest := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphConfirmatoryTestResult")][order(Centre, SyphConfirmatoryTestResult)]
  disagg_dt[!SyphConfirmatoryTestResult %in% c("1","2","3"), SyphConfirmatoryTestResult := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "SyphTestUsed")][order(Centre, SyphTestUsed)]
  disagg_dt[!SyphTestUsed %in% c("1","2"), SyphTestUsed := NA]
  
  
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVEverDiagnosed")][order(Centre, HCVEverDiagnosed)]
    disagg_dt[!HCVEverDiagnosed %in% c("1","2"), HCVEverDiagnosed := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVEverTested")][order(Centre, HCVEverTested)]
  disagg_dt[!HCVEverTested %in% c("1","2"), HCVEverTested := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVTestedLastYear")][order(Centre, HCVTestedLastYear)]
  disagg_dt[!HCVTestedLastYear %in% c("1","2"), HCVTestedLastYear := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVTestedLastYearSameCBVCT")][order(Centre, HCVTestedLastYearSameCBVCT)]
  disagg_dt[!HCVTestedLastYearSameCBVCT %in% c("1","2"), HCVTestedLastYearSameCBVCT := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVScreeningTestResult")][order(Centre, HCVScreeningTestResult)]
  disagg_dt[!HCVScreeningTestResult %in% c("1","2"), HCVScreeningTestResult := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVRNATest")][order(Centre, HCVRNATest)]
  disagg_dt[!HCVRNATest %in% c("1","2"), HCVRNATest := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVConfirmatoryTestResult")][order(Centre, HCVConfirmatoryTestResult)]
  disagg_dt[!HCVConfirmatoryTestResult %in% c("1","2","3"), HCVConfirmatoryTestResult := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVTestUsed")][order(Centre, HCVTestUsed)]
  disagg_dt[!HCVTestUsed %in% c("1","2","3"), HCVTestUsed := NA]
  # COMPROBACIO:  disagg_dt[, .N, by= c("Centre", "HCVScreeningTestResult")][order(Centre, HCVScreeningTestResult)]
  disagg_dt[!HCVScreeningTestResult %in% c("1","2"), HCVScreeningTestResult := NA]

  return(disagg_dt) 
}


# # LINES TO TEST:  
# disagg_dt <- disagg_exaequo
# colmap_dt <- col_maps            # EXCEL maping file previously loaded. 
# colmap_from_column <- "Exaequo (Permanence nurse 2020).xlsx"
# colmap_to_column <- "COBATEST_Variable_name"

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


