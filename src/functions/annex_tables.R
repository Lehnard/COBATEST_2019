# ## LINES TO TEST:
# disagg_vih_clean_data <- clean_vih_data_list$vih_clean_data
# agg_vih_data <- agg_cobatest$vih
# centers_data <- centers

generate_hiv_annex1_table <- function(disagg_vih_clean_data, agg_vih_data, centers_data) {
  ## VERSION:
  #     30-11-2022
  #
  ## DESCRIPTION:
  #     Script per generar les taules de l'annex 1 segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #   - disagg_vih_clean_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R"
  #                             un cop s'ha netejat amb l'scritp "cleaning_vih_data.R" i que es correspòn just amb 
  #                             el punt final del flowchart. 
  #   - agg_vih_data:           data.table generada a partir del codi "DM_02_COBATEST_AGG_DATA_ETL_2020.R". 
  #                             Aquest codi genera una llista "agg_cobatest" amb un compartiment "vih". Aquest
  #                             compartiment "vih" és el que cal agafar.
  #   - centers_data:           data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                             "Centre" (codi del centre cobatest, numèrica)
  #                             "CentreName" (Nom del centre, string)
  #                             "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )
  #
  ## CAUTION:
  #   Script fortament dependent de l'estructura de dades de disagg_vih_clean_data i agg_vih_data. 
  
  
  # COBATEST 2020
  # ------------- #
  

  ## CBVCT 2 ####
  #-------------#
  # CBVCT 2: Proportion of clients who reported to have been previously tested for HIV by centre with corresponding 
  # estimates of indicators taking into account missing information (EMV)
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct2_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados") & Centre != 71])
  cbvct2_agg <- copy(centers_data[Origen %in% c("datos_agregados") & Centre != 71])
  cbvct2_nodata <- copy(centers_data[Origen %in% c("No_data") & Centre != 71])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct2_disagg <- merge(x = cbvct2_disagg,
                          y = disagg_vih_clean_data[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct2_disagg <- merge(x = cbvct2_disagg,
                          y = disagg_vih_clean_data[EverTested == 1, .('N ever tested' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct2_disagg <- merge(x = cbvct2_disagg,
                          y = disagg_vih_clean_data[, .('Missing' = sum(is.na(EverTested))), by= .(Centre)],
                          by = "Centre", all = T)

    # Calculem els percentatges
    cbvct2_disagg[, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)]
    cbvct2_disagg[, EMV := round(`N ever tested`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}

  
  ## DADES AGREGADES
  cbvct2_agg <- merge(x= cbvct2_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HIV`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct2_agg <- merge(x= cbvct2_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 2') & Category == 'All', .(`N ever tested`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct2_agg[Origen == "datos_agregados", `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)]
  
  
  cbvct2 <- rbind(cbvct2_disagg, cbvct2_agg, cbvct2_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct2[, Missing := ifelse(is.na(`N ever tested`), yes = NA, no = Missing)]
  cbvct2[is.na(cbvct2)] <- "-"
  
  # Maquejem output. 
  
  
  rm(cbvct2_disagg, cbvct2_agg, cbvct2_nodata)
  
  
  ## CBVCT 3 ####
  #-------------#
  # CBVCT 3: Proportion of clients who reported to have been tested for HIV during preceding 12 months by 
  # centre with corresponding estimates of indicators taking into account missing information (EMV)
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct3_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct3_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct3_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct3_disagg <- merge(x = cbvct3_disagg,
                          y = disagg_vih_clean_data[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct3_disagg <- merge(x = cbvct3_disagg,
                          y = disagg_vih_clean_data[TestedLastYear == 1, .('N Tested Last Year' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct3_disagg <- merge(x = cbvct3_disagg,
                          y = disagg_vih_clean_data[, .('Missing' = sum(is.na(TestedLastYear))), by= .(Centre)],
                          by = "Centre", all = T)


    # Calculem els percentatges
    cbvct3_disagg[, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)]
    cbvct3_disagg[, EMV := round(`N Tested Last Year`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}
  
  ## DADES AGREGADES
  cbvct3_agg <- merge(x =cbvct3_agg , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HIV`= as.numeric(All)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  cbvct3_agg <- merge(x =cbvct3_agg , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 3') & Category == 'All', .(`N Tested Last Year`= as.numeric(All)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  cbvct3_agg[Origen == "datos_agregados", `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)]
  
  
  cbvct3 <- rbind(cbvct3_disagg, cbvct3_agg, cbvct3_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct3[, Missing := ifelse(is.na(`N Tested Last Year`), yes = NA, no = Missing)]
  cbvct3[is.na(cbvct3)] <- "-"
  
  rm(cbvct3_disagg, cbvct3_agg, cbvct3_nodata)
  
  
  ## CBVCT 4 ####
  #-------------#
  # CBVCT 4: Proportion of clients who reported to have been tested for HIV at the same CBVCT facility during 
  # preceding 12 months by centre with corresponding estimates of indicators taking into account missing information (EMV)
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct4_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct4_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct4_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct4_disagg <- merge(x = cbvct4_disagg,
                          y = disagg_vih_clean_data[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct4_disagg <- merge(x = cbvct4_disagg,
                          y = disagg_vih_clean_data[TestedLastYearSameCBVCT == 1, .('N Tested Last Year Same CBVCT' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct4_disagg <- merge(x = cbvct4_disagg,
                          y = disagg_vih_clean_data[, .('Missing' = sum(is.na(TestedLastYearSameCBVCT))), by= .(Centre)],
                          by = "Centre", all = T)


    # Calculem els percentatges
    cbvct4_disagg[, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)]
    cbvct4_disagg[, EMV := round(`N Tested Last Year Same CBVCT`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}

  
  ## DADES AGREGADES
  cbvct4_agg <- merge(x =cbvct4_agg , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HIV`= as.numeric(All)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  cbvct4_agg <- merge(x =cbvct4_agg , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 4') & Category == 'All', .(`N Tested Last Year Same CBVCT`= as.numeric(All)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  cbvct4_agg[Origen == "datos_agregados", `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)]
  
  
  cbvct4 <- rbind(cbvct4_disagg, cbvct4_agg, cbvct4_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct4[, Missing := ifelse(is.na(`N Tested Last Year Same CBVCT`), yes = NA, no = Missing)]
  cbvct4[is.na(cbvct4)] <- "-"
  
  rm(cbvct4_disagg, cbvct4_agg, cbvct4_nodata)
  
  
  ## CBVCT 5 ####
  #-------------#
  # CBVCT 5: Proportion of clients with reactive screening HIV test result by centre with corresponding estimates 
  # of indicators taking into account missing information (EMV)
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct5_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct5_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct5_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                          y = disagg_vih_clean_data[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                          y = disagg_vih_clean_data[ScreeningTestResult == 1, .('N HIV Reactive' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                          y = disagg_vih_clean_data[, .('Missing' = sum(is.na(ScreeningTestResult))), by= .(Centre)],
                          by = "Centre", all = T)
    
    
    # Calculem els percentatges
    cbvct5_disagg[, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)]
    cbvct5_disagg[, EMV := round(`N HIV Reactive`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}
  
  ## DADES AGREGADES
  cbvct5_agg <- merge(x =cbvct5_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HIV`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct5_agg <- merge(x =cbvct5_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 5') & Category == 'All', .(`N HIV Reactive`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct5_agg[Origen == "datos_agregados", `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)]
  
  
  cbvct5 <- rbind(cbvct5_disagg, cbvct5_agg, cbvct5_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct5[, Missing := ifelse(is.na(`N HIV Reactive`), yes = NA, no = Missing)]
  cbvct5[is.na(cbvct5)] <- "-"
  
  rm(cbvct5_disagg, cbvct5_agg, cbvct5_nodata)
  
  
  ## CBVCT 6 ####
  #-------------#
  # CBVCT 6: Proportion of clients with reactive screening HIV test result who were tested with confirmatory 
  # HIV test by centre with corresponding estimates of indicators taking into account missing information (EMV) 
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  centers_data[, .N, Origen]
  cbvct6_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct6_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct6_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct6_disagg <- merge(x = cbvct6_disagg, 
                          y = disagg_vih_clean_data[ScreeningTestResult == 1, .('Persons reactive HIV result' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct6_disagg <- merge(x = cbvct6_disagg, 
                          y = disagg_vih_clean_data[ScreeningTestResult == 1 & ConfirmatoryHIVTest == 1, .('N HIV Confirmatory' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct6_disagg <- merge(x = cbvct6_disagg, 
                          y = disagg_vih_clean_data[ScreeningTestResult == 1, .('Missing' = sum(is.na(ConfirmatoryHIVTest))), by= .(Centre)],
                          by = "Centre", all = T)
    
    
    # Calculem els percentatges
    cbvct6_disagg[, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)]
    cbvct6_disagg[, EMV := round(`N HIV Confirmatory`/(`Persons reactive HIV result`- Missing)*100, digits = 1)]}
  
  ## DADES AGREGADES
  cbvct6_agg <- merge(x =cbvct6_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 5') & Category == 'All', .(`Persons reactive HIV result`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct6_agg <- merge(x =cbvct6_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 6') & Category == 'All', .(`N HIV Confirmatory`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct6_agg[Origen == "datos_agregados", `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)]
  
  
  cbvct6 <- rbind(cbvct6_disagg, cbvct6_agg, cbvct6_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct6[, Missing := ifelse(is.na(`N HIV Confirmatory`), yes = NA, no = Missing)]
  cbvct6[is.na(cbvct6)] <- "-"
  
  rm(cbvct6_disagg, cbvct6_agg, cbvct6_nodata)
  
  
  ## CBVCT 7 ####
  #-------------#
  # CBVCT 7: Proportion of clients with positive confirmatory HIV test result by centre with corresponding 
  # estimates of indicators taking into account missing information (EMV)
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct7_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct7_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct7_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct7_disagg <- merge(x = cbvct7_disagg, 
                          y = disagg_vih_clean_data[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct7_disagg <- merge(x = cbvct7_disagg, 
                          y = disagg_vih_clean_data[ConfirmatoryHIVTestResult == 1, .('N HIV Confirm result' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct7_disagg <- merge(x = cbvct7_disagg, 
                          y = disagg_vih_clean_data[, .('Missing' = sum(is.na(ConfirmatoryHIVTestResult))), by= .(Centre)],
                          by = "Centre", all = T)
    
    
    # Calculem els percentatges
    cbvct7_disagg[, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)]
    cbvct7_disagg[, EMV := round(`N HIV Confirm result`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}
  
  ## DADES AGREGADES
  cbvct7_agg <- merge(x =cbvct7_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HIV`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct7_agg <- merge(x =cbvct7_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 7') & Category == 'All', .(`N HIV Confirm result`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct7_agg[Origen == "datos_agregados", `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)]
  
  
  cbvct7 <- rbind(cbvct7_disagg, cbvct7_agg, cbvct7_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct7[, Missing := ifelse(is.na(`N HIV Confirm result`), yes = NA, no = Missing)]
  cbvct7[is.na(cbvct7)] <- "-"
  
  rm(cbvct7_disagg, cbvct7_agg, cbvct7_nodata)
  
  ## CBVCT 8 ####
  #-------------#
  # CBVCT 8: Proportion of clients with false positive screening results.
  # A false positive was considered a reactive screening test result followed by a negative confirmatory test result. 
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct8_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct8_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct8_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct8_disagg <- merge(x = cbvct8_disagg, 
                          y = disagg_vih_clean_data[ScreeningHIVTest == 1, .(`Persons screened for HIV` = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct8_disagg <- merge(x = cbvct8_disagg, 
                          y = disagg_vih_clean_data[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .('N False positive HIV results' = .N), by= .(Centre)],
                          by = "Centre", all = T)
    cbvct8_disagg <- merge(x = cbvct8_disagg, 
                          y = disagg_vih_clean_data[ScreeningTestResult == 1, .('Missing' = sum(is.na(ConfirmatoryHIVTestResult))), by= .(Centre)],
                          by = "Centre", all = T)
    
    
    # Calculem els percentatges
    cbvct8_disagg[, `% False positive HIV results` := round(`N False positive HIV results`/`Persons screened for HIV`*100, digits = 1)]
    cbvct8_disagg[, EMV := round(`N False positive HIV results`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}
  
  ## DADES AGREGADES
  cbvct8_agg <- merge(x =cbvct8_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HIV`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct8_agg <- merge(x =cbvct8_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 8') & Category == 'All', .(`N False positive HIV results`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct8_agg[Origen == "datos_agregados", `% False positive HIV results` := round(`N False positive HIV results`/`Persons screened for HIV`*100, digits = 1)]
  
  
  cbvct8 <- rbind(cbvct8_disagg, cbvct8_agg, cbvct8_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct8[, Missing := ifelse(is.na(`N False positive HIV results`), yes = NA, no = Missing)]
  cbvct8[is.na(cbvct8)] <- "-"
  
  rm(cbvct8_disagg, cbvct8_agg, cbvct8_nodata)
  
  
  ## CBVCT 9 ####
  #-------------#
  # CBVCT 9:  Number of clients needed to test to find a positive HIV result
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct9_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "Datos_desagregados")])
  cbvct9_agg <- copy(centers_data[Origen %in% c("Datos_agregados")])
  cbvct9_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct9_disagg <- merge(x = cbvct9_disagg, 
                           y = disagg_vih_clean_data[ConfirmatoryHIVTestResult == 1, .('N HIV Confirm result' = .N), by= .(Centre)],
                           by = "Centre", all = T)
    cbvct9_disagg <- merge(x = cbvct9_disagg, 
                           y = disagg_vih_clean_data[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                           by = "Centre", all = T)
    cbvct9_disagg <- merge(x = cbvct9_disagg,   # Sense sentit. 
                           y = disagg_vih_clean_data[ScreeningHIVTest == 1, .('Missing' = sum(is.na(ScreeningHIVTest))), by= .(Centre)],
                           by = "Centre", all = T)
    
    
    # Calculem els percentatges
    cbvct9_disagg[, `clients tested to find positive` := round(`Persons screened for HIV`/`N HIV Confirm result`, digits = 1)]
    cbvct9_disagg[, EMV := NA]}
  
  ## DADES AGREGADES
  cbvct9_agg <- merge(x =cbvct9_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 7') & Category == 'All', .(`N HIV Confirm result`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct9_agg <- merge(x =cbvct9_agg , 
                      y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HIV`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct9_agg[Origen == "datos_agregados", `clients tested to find positive` := round(`Persons screened for HIV`/`N HIV Confirm result`, digits = 1)]
  
  
  cbvct9 <- rbind(cbvct9_disagg, cbvct9_agg, cbvct9_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct9[, Missing := ifelse(is.na(`Persons screened for HIV`), yes = NA, no = Missing)]
  cbvct9[is.na(cbvct9)] <- "-"
  
  rm(cbvct9_disagg, cbvct9_agg, cbvct9_nodata)
  
  
  ## FORMAT SORTIDA 
  # Reordenem cols.
  new_order <- c("Centre", "RealCentreName", "Persons screened for HIV", "N ever tested", "Missing", 
                 "% ever tested", "EMV", "Origen", "CentreName")
  setcolorder(cbvct2, neworder = new_order)
  new_order <- c("Centre", "RealCentreName", "Persons screened for HIV", "N Tested Last Year", "Missing", 
                 "% Tested Last Year", "EMV", "Origen", "CentreName")
  setcolorder(cbvct3, neworder = new_order)
  new_order <- c("Centre", "RealCentreName", "Persons screened for HIV", "N Tested Last Year Same CBVCT", "Missing", 
                 "% Tested Last Year Same CBVCT", "EMV", "Origen", "CentreName")
  setcolorder(cbvct4, neworder = new_order)
  new_order <- c("Centre", "RealCentreName", "Persons screened for HIV", "N HIV Reactive", "Missing", 
                 "% HIV Reactive", "EMV", "Origen", "CentreName")
  setcolorder(cbvct5, neworder = new_order)
  new_order <- c("Centre", "RealCentreName", "Persons reactive HIV result", "N HIV Confirmatory", "Missing", 
                 "% HIV Confirmatory", "EMV", "Origen",  "CentreName")
  setcolorder(cbvct6, neworder = new_order)
  new_order <- c("Centre", "RealCentreName", "Persons screened for HIV", "N HIV Confirm result", "Missing", 
                 "% HIV Confirm result", "EMV", "Origen", "CentreName")
  setcolorder(cbvct7, neworder = new_order)
  new_order <- c("Centre", "RealCentreName", "Persons screened for HIV", "N False positive HIV results", "Missing", 
                 "% False positive HIV results", "EMV", "Origen", "CentreName")
  setcolorder(cbvct8, neworder = new_order)
  new_order <- c("Centre", "RealCentreName", "N HIV Confirm result", "Persons screened for HIV", "Missing", 
                 "clients tested to find positive", "EMV", "Origen", "CentreName")
  setcolorder(cbvct9, neworder = new_order)
  
  
  # Sortida en estructura llista. 
  annex1_taules <- list()

  annex1_taules[["cbvct2"]] <- cbvct2
  annex1_taules[["cbvct3"]] <- cbvct3
  annex1_taules[["cbvct4"]] <- cbvct4
  annex1_taules[["cbvct5"]] <- cbvct5
  annex1_taules[["cbvct6"]] <- cbvct6
  annex1_taules[["cbvct7"]] <- cbvct7
  annex1_taules[["cbvct8"]] <- cbvct8
  annex1_taules[["cbvct9"]] <- cbvct9
  
  
  return(annex1_taules)
}


## LINES TO TEST:
# disagg_data <- disagg_cobatest
# agg_vih_data <- agg_cobatest$vih
# centers_data <- centers

generate_hiv_annex2_table <- function(disagg_data, agg_vih_data, centers_data) {
  ## VERSION:
  #     14-09-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula de l'annex 2 segons report 2018: "Report Cobatest final 2018.pdf" 
  #
  ## PARAMETRES: 
  #   - disagg_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R".
  #   - agg_vih_data: data.table generada a partir del codi "DM_02_COBATEST_AGG_DATA_ETL_2020.R". 
  #                   Aquest codi genera una llista "agg_cobatest" amb un compartiment "vih". Aquest
  #                   compartiment "vih" és el que cal agafar.
  #   - centers_data: data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                   "Centre" (codi del centre cobatest, numèrica)
  #                   "CentreName" (Nom del centre, string)
  #                   "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )
  #
  ## CAUTION:
  #   Script fortament dependent de l'estructura de dades de disagg_data i agg_vih_data. 
  
  # Centres amb dades desagregades
  annex2_disag <- copy(centers_data[Origen %in% c("Cobatest_tool","datos_desagregados")])
  
  # Afegim chequejos de compliment per a les dades DESAGREGADES.
  {
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(AgeGroup= all(is.na(AgeGroup))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(MSM= all(is.na(MSM))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(SW= all(is.na(SW))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(PWID= all(is.na(PWID))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(Migrant= all(is.na(Migrant))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT1= all(is.na(ScreeningHIVTest))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT2= (all(is.na(ScreeningHIVTest)) | all(is.na(EverTested)))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT3= (all(is.na(ScreeningHIVTest)) | all(is.na(TestedLastYear)))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT4= (all(is.na(ScreeningHIVTest)) | all(is.na(TestedLastYearSameCBVCT)))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT5= (all(is.na(ScreeningHIVTest)) | all(is.na(ScreeningTestResult)))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT6= (all(is.na(ScreeningTestResult)) | all(is.na(ConfirmatoryHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT7= (all(is.na(ScreeningHIVTest)) | all(is.na(ConfirmatoryHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT8= (all(is.na(ScreeningTestResult)) | all(is.na(ConfirmatoryHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
  annex2_disag <- merge(x =annex2_disag , y= disagg_data[, .(CBVCT9= (all(is.na(ConfirmatoryHIVTest)) | all(is.na(ScreeningHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
  }

  # Centres amb dades desagregades
  annex2_ag <- copy(centers_data[Origen == "datos_agregados"])
  
  # Afegim chequejos de compliment per a les dades AGREGADES.
  {annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == "All", .(AgeGroup= is.na(`<25 years old`) & is.na(`25+ years old`)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == "MSM", .(MSM= is.na(All) & is.na(Males) & is.na(Females) & is.na(Transgender)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == "SW", .(SW= is.na(All) & is.na(Males) & is.na(Females) & is.na(Transgender)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == "IDU", .(PWID= is.na(All) & is.na(Males) & is.na(Females) & is.na(Transgender)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == "Migrants", .(Migrant= is.na(All) & is.na(Males) & is.na(Females) & is.na(Transgender)), by= center], 
                  by.x = "Centre", by.y= "center", all.x= T)
  # annex2_ag <- merge(x =annex2_ag , 
  #                 y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == "All", .(All= is.na(All) & is.na(Males) & is.na(Females) & is.na(Transgender)), by= center], 
  #                 by.x = "Centre", by.y= "center", all.x= T)
  }
  

  # Chequejos per a comprobar que es podran calcular els indicadors. 
  # S'utilitza el criteri de que TOTS els numeradors de cada CBVCT X de l'excel vinguin buits. 
  # Programaticament:  Prenem el dataset de tots els numeradors de tots els indicadors i tots els centres, filtrem per indicador, 
  #                    i després utilitzem un by= per capturar els .SD (subdatasets dels groups) i columnes .SDcols. En aquest
  #                    .SD resultant: si ve tot buit llavors no tenim indicador. si ve parcialment omplert si. 
  {annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 1'), .(CBVCT1= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 2'), .(CBVCT2= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 3'), .(CBVCT3= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 4'), .(CBVCT4= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 5'), .(CBVCT5= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 6'), .(CBVCT6= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 7'), .(CBVCT7= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)
  annex2_ag <- merge(x =annex2_ag , 
                  y= agg_vih_data[grepl(x = title, pattern = 'CBVCT 8'), .(CBVCT8= all(is.na(.SD))), by= center, .SDcols= c("All","Males","Females","Transgender","<25 years old","25+ years old")], 
                  by.x = "Centre", by.y= "center", all.x= T)}
  
  annex2_ag[, CBVCT9 := ifelse(test = CBVCT7 == T | CBVCT1 == T , yes = T, no = F)] 
  
  # Fem rowbind de annex2 amb dades desagregades i els annex2 amb dades agregades
  annex2 <- rbind(annex2_disag, annex2_ag)
  
  
  # Filtrem centres que no han aportat dades. 
  annex2 <- annex2[complete.cases(AgeGroup, MSM, SW, PWID, Migrant, CBVCT1, CBVCT2, CBVCT3, CBVCT4, CBVCT5, CBVCT6, CBVCT7, CBVCT8)]
  
  return(annex2)
}


## LINES TO TEST
# disagg_vih_clean_data <- clean_vih_data_list$vih_clean_data
# agg_vih_data <- agg_cobatest$vih
# centers_data <- centers

generate_hiv_annex6_table <- function(disagg_vih_clean_data, agg_vih_data, centers_data) {
  ## VERSION:
  #     14-09-2022
  #
  ## DESCRIPTION:
  #     Script per generar les taules de l'annex 6 segons report 2018: "Report Cobatest final 2018.pdf". 
  #     Annex 6: People screened for HIV (N) and Reactive Tests (n, %) by sociodemographic 
  #     characteristics of tester and center in the cobatest network. 
  #
  ## PARAMETRES: 
  #   - disagg_vih_clean_data:  data.table generat a partir del codi "DM_01_COBATEST_disagg_vih_clean_data_ETL_2020.R"
  #                             un cop s'ha netejat amb l'scritp "cleaning_vih_data.R" i que es correspòn just amb 
  #                             el punt final del flowchart. 
  #   - agg_vih_data:           data.table generada a partir del codi "DM_02_COBATEST_AGG_DATA_ETL_2020.R". 
  #                             Aquest codi genera una llista "agg_cobatest" amb un compartiment "vih". Aquest
  #                             compartiment "vih" és el que cal agafar.
  #   - centers_data:           data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                             "Centre" (codi del centre cobatest, numèrica)
  #                             "CentreName" (Nom del centre, string)
  #                             "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )
  #
  ## CAUTION:
  #   Script fortament dependent de l'estructura de dades de disagg_vih_clean_data i agg_vih_data. 
  
  
  ## Funció per generar les taules per cada centre. 
  
  ## LINES TO TEST:
  # agg_data_numerators <- disagg_to_agg_by_center[Centre == 1] # Aquest dataset es genera posteriorment al codi. 
  
  generate_table <- function(agg_data_numerators) {
    
    # Per simplificar nomenclatura i alhora mantenir un nom de paràmetre intuitiu.
    agg_dt <- copy(agg_data_numerators)   
    
    # Reconstruim l'estructura de la taula. 
    table <- data.table(X1= c('Persons tested','Age Group',NA,'Gender',NA,NA,'Migrant','PWID','SW',NA,NA,
                              'MSM','Previous HIV test','Tested in last 12 months','Test last 12 months in this CBVCT',
                              'False positive','Confirmatory HIV test','Positive confirmatory HIV test'),
                        X2= c(NA,'<25','>=25','Male','Female','Transgender','Yes',NA,'MSW','FSW','TSW',NA,NA,NA,NA,NA,NA,NA))
    
    # Omplenem valors. N Total / N Reactive / %  Reactive
    # persons tested. ####  
    {persons_tested_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)]
    persons_tested_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)]
    table[X1 == 'Persons tested', Total := persons_tested_total]
    table[X1 == 'Persons tested', Reactive := persons_tested_reactive]
    table[X1 == 'Persons tested', `Reactive (%)` := round(persons_tested_reactive/persons_tested_total*100, digits = 1)]}
    
    # Age group < 25 anys. ####  
    {less_25y_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(`<25 years old`), na.rm = T)]
    less_25y_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(`<25 years old`), na.rm = T)]
    table[X2 == '<25', Total := less_25y_total]
    table[X2 == '<25', Reactive := less_25y_reactive]
    table[X2 == '<25', `Reactive (%)` := round(less_25y_reactive/less_25y_total*100, digits = 1)]}
    
    # Age group >= 25 anys. ####  
    {great_25y_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(`25+ years old`), na.rm = T)]
    great_25y_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(`25+ years old`), na.rm = T)]
    table[X2 == '>=25', Total := great_25y_total]
    table[X2 == '>=25', Reactive := great_25y_reactive]
    table[X2 == '>=25', `Reactive (%)` := round(great_25y_reactive/great_25y_total*100, digits = 1)]}
    
    # Gender Male. ####  
    {male_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(Males), na.rm = T)]
    male_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(Males), na.rm = T)]
    table[X2 == 'Male', Total := male_total]
    table[X2 == 'Male', Reactive := male_reactive]
    table[X2 == 'Male', `Reactive (%)` := round(male_reactive/male_total*100, digits = 1)]}
    
    # Gender Female  #### 
    {female_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(Females), na.rm = T)]
    female_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(Females), na.rm = T)]
    table[X2 == 'Female', Total := female_total]
    table[X2 == 'Female', Reactive := female_reactive]
    table[X2 == 'Female', `Reactive (%)` := round(female_reactive/female_total*100, digits = 1)]}
    
    # Gender Trans ####  
    {trans_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(Transgender), na.rm = T)]
    trans_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(Transgender), na.rm = T)]
    table[X2 == 'Transgender', Total := trans_total]
    table[X2 == 'Transgender', Reactive := trans_reactive]
    table[X2 == 'Transgender', `Reactive (%)` := round(trans_reactive/trans_total*100, digits = 1)]}
    
    # Migrants. ####  
    {migrant_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'Migrants', sum(as.numeric(All), na.rm = T)]
    migrant_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'Migrants', sum(as.numeric(All), na.rm = T)]
    table[X1 == 'Migrant', Total := migrant_total]
    table[X1 == 'Migrant', Reactive := migrant_reactive]
    table[X1 == 'Migrant', `Reactive (%)` := round(migrant_reactive/migrant_total*100, digits = 1)]}
    
    # PWID #### 
    { pwid_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'IDU', sum(as.numeric(All), na.rm = T)]
    pwid_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'IDU', sum(as.numeric(All), na.rm = T)]
    table[X1 == 'PWID', Total := pwid_total]
    table[X1 == 'PWID', Reactive := pwid_reactive]
    table[X1 == 'PWID', `Reactive (%)` := round(pwid_reactive/pwid_total*100, digits = 1)]}
    
    # SW - MALE ####   
    {msw_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'SW', sum(as.numeric(Males), na.rm = T)]
    msw_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'SW', sum(as.numeric(Males), na.rm = T)]
    table[X2 == 'MSW', Total := msw_total]
    table[X2 == 'MSW', Reactive := msw_reactive]
    table[X2 == 'MSW', `Reactive (%)` := round(msw_reactive/msw_total*100, digits = 1)]}
    
    # SW - FEMALE ####    
    {fsw_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'SW', sum(as.numeric(Females), na.rm = T)]
    fsw_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'SW', sum(as.numeric(Females), na.rm = T)]
    table[X2 == 'FSW', Total := fsw_total]
    table[X2 == 'FSW', Reactive := fsw_reactive]
    table[X2 == 'FSW', `Reactive (%)` := round(fsw_reactive/fsw_total*100, digits = 1)]}
    
    # SW - TRANS ####    
    {tsw_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'SW', sum(as.numeric(Transgender), na.rm = T)]
    tsw_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'SW', sum(as.numeric(Transgender), na.rm = T)]
    table[X2 == 'TSW', Total := tsw_total]
    table[X2 == 'TSW', Reactive := tsw_reactive]
    table[X2 == 'TSW', `Reactive (%)` := round(tsw_reactive/tsw_total*100, digits = 1)]}
    
    # MSM ####  
    { msm_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'MSM', sum(as.numeric(All), na.rm = T)]
    msm_reactive <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'MSM', sum(as.numeric(All), na.rm = T)]
    table[X1 == 'MSM', Total := msm_total]
    table[X1 == 'MSM', Reactive := msm_reactive]
    table[X1 == 'MSM', `Reactive (%)` := round(msm_reactive/msm_total*100, digits = 1)]}
    
    # Previous HIV test ####
    testedBefore_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 2', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
    table[X1 == "Previous HIV test", Total := testedBefore_total]
    
    # Tested in last 12 months. ####
    testedlastyear_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 3', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
    table[X1 == "Tested in last 12 months", Total := testedlastyear_total]
    
    # Tested in last 12 months in same facility. ####
    testedlastyear_sameFac_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 4', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
    table[X1 == "Test last 12 months in this CBVCT", Total := testedlastyear_sameFac_total]
    
    # False positives. ####
    falsepositive_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 8', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
    table[X1 == "False positive", Total := falsepositive_total]
    
    # Confirmatory HIV test. ####
    confirmatory_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 6', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)]  
    table[X1 == "Confirmatory HIV test", Total := confirmatory_total]
    
    # Positive confirmatory HIV test. ####
    confirmatory_positive_total <- agg_dt[title == unique(grep(x = agg_dt[, title], pattern = 'CBVCT 7', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
    table[X1 == "Positive confirmatory HIV test", Total := confirmatory_positive_total]
    
    return(table)
  }
  
  # ---------------------------------------------------------------------------------------- #
  
  ## Dades Desagregades VIH post neteja a format dades agregades per centre.
  #
  # Guia de procés:
  #   1.- Agafem el dataset de dades desagregades VIH ja net i el partim per Centre -->  .SD
  #   2.- A cada .SD (subdataset original pero per cadascun dels centres) li apliguem la funció "disagg_data_to_data_aggr()"
  #   3.- Fem un row bind de cada .SD per a obtenir el dataset final. 
  message("VIH Clean Disaggregated data to Agreggated data format by center... it may take some time. Please, wait.")
  disagg_to_agg_by_center <- disagg_vih_clean_data[, rbind(disagg_data_to_aggr_format(disagg_clean_vih_data = .SD, center= Centre, center_title= unique(RealCentreName))), by= .(Centre, RealCentreName)]
  
  #   4.- Afegim les dades agregades. 
  disagg_to_agg_by_center[, `:=`(Centre= NULL, RealCentreName= NULL)]
  agg_data <- rbind(disagg_to_agg_by_center, agg_vih_data)
  
  #   5.- Agafem el dataset de dades en format aggregades i el partim per Centre ---> .SD
  #   6.- A cada .SD (subdataset original pero per cadascun dels centres) li apliguem la funció "generate_table()"
  #   7.- Fem un row bind de cada .SD per a obtenir el dataset final. 
  annex_6 <- agg_data[, rbind(generate_table(.SD)), by= .(center, center_name)]
  
  # Arreglem NaN values 0/0 division
  annex_6[is.nan(`Reactive (%)`), `Reactive (%)` := NA]
  
  return (annex_6)
}

  

# ## LINES TO TEST:
# disagg_syph_clean_data <- clean_syph_data_list$syph_clean_data
# agg_syph_data <- agg_cobatest$syph
# centers_data <- centers

generate_syph_annex4_table <- function(disagg_syph_clean_data, agg_syph_data, centers_data) {
  ## VERSION:
  #     30-11-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula de l'annex 4 segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #   - disagg_syph_clean_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R"
  #                             un cop s'ha netejat amb l'scritp "cleaning_data.R" i que es correspòn just amb 
  #                             el punt final del flowchart. 
  #   - agg_syph_data:           data.table generada a partir del codi "DM_02_COBATEST_AGG_DATA_ETL_2020.R". 
  #                             Aquest codi genera una llista "agg_cobatest" amb un compartiment "syph". Aquest
  #                             compartiment "syph" és el que cal agafar.
  #   - centers_data:           data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                             "Centre" (codi del centre cobatest, numèrica)
  #                             "CentreName" (Nom del centre, string)
  #                             "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )
  #
  ## CAUTION:
  #   Script fortament dependent de l'estructura de dades de disagg_syph_clean_data i agg_syph_data. 
  
  
  ## CBVCT 5 ####
  #-------------#
  # CBVCT 5: Proportion of clients with reactive screening HIV test result by centre with corresponding estimates 
  # of indicators taking into account missing information (EMV)
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  cbvct5_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct5_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct5_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                           y = disagg_syph_clean_data[SyphScreeningTest == 1, .('Persons screened for Syphilis' = .N), by= .(Centre)],
                           by = "Centre", all = T)
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                           y = disagg_syph_clean_data[SyphScreeningTestResult == 1, .('N Syphilis Reactive' = .N), by= .(Centre)],
                           by = "Centre", all = T)
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                           y = disagg_syph_clean_data[, .('Missing' = sum(is.na(SyphScreeningTestResult))), by= .(Centre)],
                           by = "Centre", all = T)
    
    
    # Calculem els percentatges
    cbvct5_disagg[, `% Syphilis Reactive` := round(`N Syphilis Reactive`/`Persons screened for Syphilis`*100, digits = 1)]
    cbvct5_disagg[, EMV := round(`N Syphilis Reactive`/(`Persons screened for Syphilis`- Missing)*100, digits = 1)]}
  
  ## DADES AGREGADES
  cbvct5_agg <- merge(x =cbvct5_agg , 
                      y= agg_syph_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for Syphilis`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct5_agg <- merge(x =cbvct5_agg , 
                      y= agg_syph_data[grepl(x = title, pattern = 'CBVCT 5') & Category == 'All', .(`N Syphilis Reactive`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct5_agg[Origen == "datos_agregados", `% Syphilis Reactive` := round(`N Syphilis Reactive`/`Persons screened for Syphilis`*100, digits = 1)]
  
  
  cbvct5 <- rbind(cbvct5_disagg, cbvct5_agg, cbvct5_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct5[, Missing := ifelse(is.na(`N Syphilis Reactive`), yes = NA, no = Missing)]
  cbvct5[is.na(cbvct5)] <- "-"
  
  
  rm(cbvct5_disagg, cbvct5_agg, cbvct5_nodata)
  
  
  ## FORMAT SORTIDA 
  # Reordenem cols.
    new_order <- c("Centre", "RealCentreName", "Persons screened for Syphilis", "N Syphilis Reactive", "Missing", 
                 "% Syphilis Reactive", "EMV", "Origen", "CentreName")
  setcolorder(cbvct5, neworder = new_order)
  

  return(cbvct5)
}



# ## LINES TO TEST:
# disagg_hcv_clean_data <- clean_hcv_data_list$hcv_clean_data
# agg_hcv_data <- agg_cobatest$syph
# centers_data <- centers

generate_hcv_annex5_table <- function(disagg_hcv_clean_data, agg_hcv_data, centers_data) {
  ## VERSION:
  #     30-11-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula de l'annex 5 segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #   - disagg_hcv_clean_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R"
  #                             un cop s'ha netejat amb l'scritp "cleaning_data.R" i que es correspòn just amb 
  #                             el punt final del flowchart. 
  #   - agg_hcv_data:           data.table generada a partir del codi "DM_02_COBATEST_AGG_DATA_ETL_2020.R". 
  #                             Aquest codi genera una llista "agg_cobatest" amb un compartiment "hcv". Aquest
  #                             compartiment "hcv" és el que cal agafar.
  #   - centers_data:           data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                             "Centre" (codi del centre cobatest, numèrica)
  #                             "CentreName" (Nom del centre, string)
  #                             "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )
  #
  ## CAUTION:
  #   Script fortament dependent de l'estructura de dades de disagg_hcv_clean_data i agg_hcv_data. 
  

  ## CBVCT 5 ####
  #-------------#
  # CBVCT 5: Proportion of clients with reactive screening HIV test result by centre with corresponding estimates 
  # of indicators taking into account missing information (EMV)
  #
  # EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
  # data reported for this indicator).
  
  # Definim 3 blocs de tracte diferent per posteriorment ajuntar. 
  # centers_data[, .N, Origen]
  cbvct5_disagg <- copy(centers_data[Origen %in% c("Cobatest_tool", "datos_desagregados")])
  cbvct5_agg <- copy(centers_data[Origen %in% c("datos_agregados")])
  cbvct5_nodata <- copy(centers_data[Origen %in% c("No_data")])
  
  ## DADES DESAGREGADES
  {# Recuperem les N per centres.
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                           y = disagg_hcv_clean_data[HCVScreeningTest == 1, .('Persons screened for HCV' = .N), by= .(Centre)],
                           by = "Centre", all = T)
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                           y = disagg_hcv_clean_data[HCVScreeningTestResult == 1, .('N HCV Reactive' = .N), by= .(Centre)],
                           by = "Centre", all = T)
    cbvct5_disagg <- merge(x = cbvct5_disagg, 
                           y = disagg_hcv_clean_data[, .('Missing' = sum(is.na(HCVScreeningTestResult))), by= .(Centre)],
                           by = "Centre", all = T)
    
    
    # Calculem els percentatges
    cbvct5_disagg[, `% HCV Reactive` := round(`N HCV Reactive`/`Persons screened for HCV`*100, digits = 1)]
    cbvct5_disagg[, EMV := round(`N HCV Reactive`/(`Persons screened for HCV`- Missing)*100, digits = 1)]}
  
  ## DADES AGREGADES
  cbvct5_agg <- merge(x =cbvct5_agg , 
                      y= agg_hcv_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', .(`Persons screened for HCV`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct5_agg <- merge(x =cbvct5_agg , 
                      y= agg_hcv_data[grepl(x = title, pattern = 'CBVCT 5') & Category == 'All', .(`N HCV Reactive`= as.numeric(All)), by= center], 
                      by.x = "Centre", by.y= "center", all.x= T)
  cbvct5_agg[Origen == "datos_agregados", `% HCV Reactive` := round(`N HCV Reactive`/`Persons screened for HCV`*100, digits = 1)]
  
  
  cbvct5 <- rbind(cbvct5_disagg, cbvct5_agg, cbvct5_nodata, fill= T)[order(Centre)]
  
  # Correcció de Missings
  cbvct5[, Missing := ifelse(is.na(`N HCV Reactive`), yes = NA, no = Missing)]
  cbvct5[is.na(cbvct5)] <- "-"
  
  rm(cbvct5_disagg, cbvct5_agg, cbvct5_nodata)
  
  
  ## FORMAT SORTIDA 
  # Reordenem cols.
  new_order <- c("Centre", "RealCentreName", "Persons screened for HCV", "N HCV Reactive", "Missing", 
                 "% HCV Reactive", "EMV", "Origen",  "CentreName")
  setcolorder(cbvct5, neworder = new_order)
  
  
  return(cbvct5)
}