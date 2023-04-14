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