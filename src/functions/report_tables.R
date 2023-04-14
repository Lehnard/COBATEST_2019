# LINES TO TEST
# disagg_vih_clean <- disagg_vih
# agg_vih_data <- agg_cobatest$vih

generate_hiv_report_table_1 <- function(disagg_vih_clean, agg_vih_data) {
  
  ### VERSION:
  #     19-10-2021
  #
  ## DESCRIPTION:
  #     Script per generar la taula "summary of people screened for HIV in the Cobatest Network"
  #     segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #   - disagg_vih_clean: data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_vih_data.R". És el dataset net de dades desagragades VIH. 
  #
  #   - agg_vih_data:     dataset amb les dades de tots els centres que les envien en format dades agregades 
  #                       (el generem amb DM_02_COBATEST_AGG_DATA_ETL_2020.R)
  #

  # Dades desagregades VIH post neteja a format dades agregades. 
  disagg_to_aggr <- disagg_data_to_aggr_format(disagg_clean_vih_data = disagg_vih, center_title= 'Cobatest_vih_clean_data')

  # Fem un dataset total que contempli l'informació tant del total de centres amb dades desagregades com amb dades agregades. 
  coba_dt <-rbind(disagg_to_aggr, agg_vih_data)
  # COMPROBACIO:  coba_dt[, .N, by= .(center, center_name)]
  
  # Reconstruim l'estructura de la taula. 
  table_1 <- data.table(X1= c('Persons tested','Age Group',NA,'Gender',NA,NA,'Migrant','PWID','SW',NA,NA,
                              'MSM','Previous HIV test','Tested in last 12 months','Test last 12 months in this CBVCT',
                              'False positive','Confirmatory HIV test','Positive confirmatory HIV test'),
                        X2= c(NA,'<25','>=25','Male','Female','Transgender','Yes',NA,'MSW','FSW','TSW',NA,NA,NA,NA,NA,NA,NA))
  
  # Omplenem valors. N Total / N Reactive / %  Reactive
  # persons tested. ####  
  {persons_tested_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)]
    persons_tested_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)]
    table_1[X1 == 'Persons tested', Total := persons_tested_total]
    table_1[X1 == 'Persons tested', Reactive := persons_tested_reactive]
    table_1[X1 == 'Persons tested', `Reactive (%)` := round(persons_tested_reactive/persons_tested_total*100, digits = 1)]}
  
  # Age group < 25 anys. ####  
  {less_25y_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(`<25 years old`), na.rm = T)]
    less_25y_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(`<25 years old`), na.rm = T)]
    table_1[X2 == '<25', Total := less_25y_total]
    table_1[X2 == '<25', Reactive := less_25y_reactive]
    table_1[X2 == '<25', `Reactive (%)` := round(less_25y_reactive/less_25y_total*100, digits = 1)]}
  
  # Age group >= 25 anys. ####  
  {great_25y_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(`25+ years old`), na.rm = T)]
    great_25y_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(`25+ years old`), na.rm = T)]
    table_1[X2 == '>=25', Total := great_25y_total]
    table_1[X2 == '>=25', Reactive := great_25y_reactive]
    table_1[X2 == '>=25', `Reactive (%)` := round(great_25y_reactive/great_25y_total*100, digits = 1)]}
  
  # Gender Male. ####  
  {male_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(Males), na.rm = T)]
    male_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(Males), na.rm = T)]
    table_1[X2 == 'Male', Total := male_total]
    table_1[X2 == 'Male', Reactive := male_reactive]
    table_1[X2 == 'Male', `Reactive (%)` := round(male_reactive/male_total*100, digits = 1)]}
  
  # Gender Female  #### 
  {female_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(Females), na.rm = T)]
    female_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(Females), na.rm = T)]
    table_1[X2 == 'Female', Total := female_total]
    table_1[X2 == 'Female', Reactive := female_reactive]
    table_1[X2 == 'Female', `Reactive (%)` := round(female_reactive/female_total*100, digits = 1)]}
  
  # Gender Trans ####  
  {trans_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', sum(as.numeric(Transgender), na.rm = T)]
    trans_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', sum(as.numeric(Transgender), na.rm = T)]
    table_1[X2 == 'Transgender', Total := trans_total]
    table_1[X2 == 'Transgender', Reactive := trans_reactive]
    table_1[X2 == 'Transgender', `Reactive (%)` := round(trans_reactive/trans_total*100, digits = 1)]}
  
  # Migrants. ####  
  {migrant_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'Migrants', sum(as.numeric(All), na.rm = T)]
    migrant_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'Migrants', sum(as.numeric(All), na.rm = T)]
    table_1[X1 == 'Migrant', Total := migrant_total]
    table_1[X1 == 'Migrant', Reactive := migrant_reactive]
    table_1[X1 == 'Migrant', `Reactive (%)` := round(migrant_reactive/migrant_total*100, digits = 1)]}
  
  # PWID #### 
  { pwid_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'IDU', sum(as.numeric(All), na.rm = T)]
    pwid_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'IDU', sum(as.numeric(All), na.rm = T)]
    table_1[X1 == 'PWID', Total := pwid_total]
    table_1[X1 == 'PWID', Reactive := pwid_reactive]
    table_1[X1 == 'PWID', `Reactive (%)` := round(pwid_reactive/pwid_total*100, digits = 1)]}
  
  # SW - MALE ####   
  {msw_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'SW', sum(as.numeric(Males), na.rm = T)]
    msw_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'SW', sum(as.numeric(Males), na.rm = T)]
    table_1[X2 == 'MSW', Total := msw_total]
    table_1[X2 == 'MSW', Reactive := msw_reactive]
    table_1[X2 == 'MSW', `Reactive (%)` := round(msw_reactive/msw_total*100, digits = 1)]}
  
  # SW - FEMALE ####    
  {fsw_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'SW', sum(as.numeric(Females), na.rm = T)]
    fsw_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'SW', sum(as.numeric(Females), na.rm = T)]
    table_1[X2 == 'FSW', Total := fsw_total]
    table_1[X2 == 'FSW', Reactive := fsw_reactive]
    table_1[X2 == 'FSW', `Reactive (%)` := round(fsw_reactive/fsw_total*100, digits = 1)]}
  
  # SW - TRANS ####    
  {tsw_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'SW', sum(as.numeric(Transgender), na.rm = T)]
    tsw_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'SW', sum(as.numeric(Transgender), na.rm = T)]
    table_1[X2 == 'TSW', Total := tsw_total]
    table_1[X2 == 'TSW', Reactive := tsw_reactive]
    table_1[X2 == 'TSW', `Reactive (%)` := round(tsw_reactive/tsw_total*100, digits = 1)]}
  
  # MSM ####  
  { msm_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 1', value = T)) & Category == 'MSM', sum(as.numeric(All), na.rm = T)]
    msm_reactive <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 5', value = T)) & Category == 'MSM', sum(as.numeric(All), na.rm = T)]
    table_1[X1 == 'MSM', Total := msm_total]
    table_1[X1 == 'MSM', Reactive := msm_reactive]
    table_1[X1 == 'MSM', `Reactive (%)` := round(msm_reactive/msm_total*100, digits = 1)]}
  
  # Previous HIV test ####
  testedBefore_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 2', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
  table_1[X1 == "Previous HIV test", Total := testedBefore_total]
  
  # Tested in last 12 months. ####
  testedlastyear_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 3', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
  table_1[X1 == "Tested in last 12 months", Total := testedlastyear_total]
  
  # Tested in last 12 months in same facility. ####
  testedlastyear_sameFac_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 4', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
  table_1[X1 == "Test last 12 months in this CBVCT", Total := testedlastyear_sameFac_total]
  
  # False positives. ####
  falsepositive_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 8', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
  table_1[X1 == "False positive", Total := falsepositive_total]
  
  
  # Confirmatory HIV test. ####
  confirmatory_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 6', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)]  
  table_1[X1 == "Confirmatory HIV test", Total := confirmatory_total]
  
  
  # Positive confirmatory HIV test. ####
  confirmatory_positive_total <- coba_dt[title == unique(grep(x = coba_dt[, title], pattern = 'CBVCT 7', value = T)) & Category == 'All', sum(as.numeric(All), na.rm = T)] 
  table_1[X1 == "Positive confirmatory HIV test", Total := confirmatory_positive_total]
  
  return(table_1)
  
  
}
# # LINES TO TEST
# centers_data <- centers
# disagg_vih_clean <- disagg_vih
# agg_vih_data <- agg_vih

generate_hiv_report_table_2 <- function(disagg_vih_clean, agg_vih_data, centers_data) {
  
  ### VERSION:
  #     13-04-2023
  #
  ## DESCRIPTION:
  #     Script per generar la taula "Table 2 People screened for HIV, screening results and confirmatory test results - COBATEST Network (2014-18)"
  #     segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #
  #   - disagg_vih_clean: data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_vih_data.R". És el dataset net de dades desagragades VIH. 
  #
  #   - agg_vih_data:     dataset amb les dades de tots els centres que les envien en format dades agregades 
  #                       (el generem amb DM_02_COBATEST_AGG_DATA_ETL_2020.R)
  #
  #   - centers_data: data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                   "Centre" (codi del centre cobatest, numèrica)
  #                   "CentreName" (Nom del centre, string)
  #                   "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )   
  
  table_2 <- data.table(X1= c('Participating centres (n)',
                              'People tested (n)', 
                              'People with a reactive HIV screening test (%)',
                              'People with a reactive HIV screening test (n)', 
                              'People tested with a confirmatory test (%, as % of all reactive results)',
                              'People with positive confirmatory test result (%)',
                              '',
                              '* Average of all centres.', 
                              '** BeTested centers count separately.'))
  
  ## Adding other years data.
  data_2014 <- c("20", "8554", "1.6", "135", "63", "0.8", NA, NA, NA)           # Source: Report 2019 pdf
  data_2015 <- c("37", "95493", "1.7*", "825", "71.8*", "1.5*", NA, NA, NA)     # Source: Report 2019 pdf
  data_2016 <- c("38", "72916", "1.8*", "609", "80.1*", "2.1*", NA, NA, NA)     # Source: Report 2019 pdf
  # data_2017 <- c("38", "72916", "1.8*", "609", "80.1*", "2.1*", NA, NA, NA)     # Source: Report 2019 pdf
  data_2017 <- c("32", "51799", "1.7", "892", "91.8", "1.3", NA, NA, NA)        # Source: Script COBATEST_2017


  table_2[, `:=`("2014"= data_2014, 
                 "2015"= data_2015,
                 "2016"= data_2016, 
                 "2017"= data_2017)]
  
  
  ## Current year data.
  year <- "2018"
  
  # Centers N. Agafem tots els centres, amb els BeTested desagrupats. 
  # centers_data[Origen != "No_data"]
  n_centers <- centers_data[Origen != "No_data" & Centre != 71, .N]
  table_2[X1 == 'Participating centres (n)', (year) := as.character(n_centers)]
  
  # People N.
  disagg_people_tested <- disagg_vih_clean[ScreeningHIVTest == 1, .N]
  agg_people_tested <- agg_vih_data[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_tested <- disagg_people_tested + agg_people_tested
  table_2[X1 == 'People tested (n)', (year) := as.character(total_people_tested)]
  
  # Reactive N.
  disagg_people_reactive <- disagg_vih_clean[ScreeningTestResult == 1, .N]
  agg_people_reactive <- agg_vih_data[grepl(x = title, pattern = 'CBVCT 5') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_reactive <- disagg_people_reactive + agg_people_reactive
  table_2[X1 == 'People with a reactive HIV screening test (n)', (year) := as.character(total_people_reactive)]
  
  # Reactive %.
  perc_react <- round(total_people_reactive/total_people_tested*100, digits = 1)
  table_2[X1 == 'People with a reactive HIV screening test (%)', (year) := as.character(perc_react)]
  
  # Confirmatory sobre reactive tested %.
  disagg_people_confirmatory <-  disagg_vih_clean[ConfirmatoryHIVTest == 1, .N]
  agg_people_confirmatory <- agg_vih_data[grepl(x = title, pattern = 'CBVCT 6') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_confirmatory <- disagg_people_confirmatory + agg_people_confirmatory
  perc_confirmatory_over_reactive <- round(total_people_confirmatory/total_people_reactive*100, digits = 1)
  table_2[X1 == 'People tested with a confirmatory test (%, as % of all reactive results)', (year) := as.character(perc_confirmatory_over_reactive)]
  
  # Positive Confirmatory sobre people tested %.
  disagg_people_pos_confirmatory <- disagg_vih_clean[ConfirmatoryHIVTestResult == 1, .N]
  agg_people_pos_confirmatory <- agg_vih_data[grepl(x = title, pattern = 'CBVCT 7') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_pos_confirmatory <- disagg_people_pos_confirmatory + agg_people_pos_confirmatory
  perc_people_pos_confirmatory <- round(total_people_pos_confirmatory/total_people_tested*100, digits = 1)
  table_2[X1 == 'People with positive confirmatory test result (%)', (year) := as.character(perc_people_pos_confirmatory)]
  
  
  return(table_2)
}


# LINES TO TEST
# centers_data <- centers
# disagg_syph_clean <- disagg_syph
# agg_syph_data <- agg_syph

generate_syph_report_figure_7 <- function(disagg_syph_clean, agg_syph_data, centers_data) {
  
  ### VERSION:
  #     14-09-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula "Figure 7 Number of people screned for syphilis, proportion of reactive tests and number of 
  #     centres submitting data by year - COBATEST Network" segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #
  #   - disagg_syph_clean: data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_syph_data.R". És el dataset net de dades desagragades SYPH. 
  #
  #   - agg_syph_data:     dataset amb les dades de tots els centres que les envien en format dades agregades 
  #                        (el generem amb DM_02_COBATEST_AGG_DATA_ETL_2020.R)
  #
  #   - centers_data: data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                   "Centre" (codi del centre cobatest, numèrica)
  #                   "CentreName" (Nom del centre, string)
  #                   "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )   
  
  
  # Layout de la taula. 
  figure_7 <- data.table(X1= c('Participating centres (n)',
                              'People tested (n)', 
                              'People with a reactive Syphilis screening test (%)',
                              'People with a reactive Syphilis screening test (n)', 
                              '', 
                              '* BeTested Centers count separately'))
  
  ## Adding other years data.
  {data_2014 <- c(NA, NA, NA, NA, NA, NA)
  data_2015 <- c(NA, NA, NA, NA, NA, NA)
  data_2016 <- c(NA, NA, NA, NA, NA, NA)
  data_2017 <- c(NA, NA, NA, NA, NA, NA)
  
  

  figure_7[, `:=`("2014"= data_2014, 
                 "2015"= data_2015,
                 "2016"= data_2016, 
                 "2017"= data_2016)]}
  
  
  ## Current year data.
  year <- "2018"
  
  
  # Centres amb dades de Syphilis reportades.
  # COMPROBACIO:  Screening Tests a dades desagregades
  #               disagg_syph_clean[, .N, SyphScreeningTest]
  centres_syph_disagg <-  disagg_syph_clean[SyphScreeningTest == 1, unique(Centre)]
  disagg_syph_clean_reported <- disagg_syph_clean[Centre %in% centres_syph_disagg]
  
  centres_syph_agg <- agg_syph_data[title == unique(grep(x = agg_syph_data[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', .(center, !is.na(All))][V2 == T, center]
  agg_syph_data_reported <- agg_syph_data[center %in% centres_syph_agg]
  
  
  # Centers N.
  n_centers <- disagg_syph_clean_reported[, uniqueN(Centre)] + agg_syph_data_reported[, uniqueN(center)]
  figure_7[X1 == 'Participating centres (n)', (year) := as.character(n_centers)]
  
  # People N.
  disagg_people_tested <- disagg_syph_clean_reported[SyphScreeningTest == 1, .N]
  agg_people_tested <- agg_syph_data_reported[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_tested <- disagg_people_tested + agg_people_tested
  figure_7[X1 == 'People tested (n)', (year) := as.character(total_people_tested)]
  
  # Reactive N.
  disagg_people_reactive <- disagg_syph_clean_reported[SyphScreeningTestResult == 1, .N]
  agg_people_reactive <- agg_syph_data_reported[grepl(x = title, pattern = 'CBVCT 5') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_reactive <- disagg_people_reactive + agg_people_reactive
  figure_7[X1 == 'People with a reactive Syphilis screening test (n)', (year) := as.character(total_people_reactive)]
  
  # Reactive %.
  perc_react <- round(total_people_reactive/total_people_tested*100, digits = 1)
  figure_7[X1 == 'People with a reactive Syphilis screening test (%)', (year) := as.character(perc_react)]
  

  return(copy(figure_7))
}

# LINES TO TEST
# centers_data <- centers
# disagg_hcv_clean <- disagg_hcv
# agg_hcv_data <- agg_hcv

generate_hcv_report_figure_6 <- function(disagg_hcv_clean, agg_hcv_data, centers_data) {
  
  ### VERSION:
  #     14-09-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula "# Figure 6 Number of people screened for Hepatitis C, proportion of 
  #     reactive tests and number of centres submitting data by year - COBATEST Network" segons 
  #     report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #
  #   - disagg_hcv_clean: data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_hcv_data.R". És el dataset net de dades desagragades hcv. 
  #
  #   - agg_hcv_data:     dataset amb les dades de tots els centres que les envien en format dades agregades 
  #                        (el generem amb DM_02_COBATEST_AGG_DATA_ETL_2020.R)
  #
  #   - centers_data: data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                   "Centre" (codi del centre cobatest, numèrica)
  #                   "CentreName" (Nom del centre, string)
  #                   "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )   
  
  figure_6 <- data.table(X1= c('Participating centres (n)',
                               'People tested (n)', 
                               'People with a reactive HCV screening test (%)',
                               'People with a reactive HCV screening test (n)',
                               '',
                               '* BeTested Centers count separately.'))
  
  ## Adding other years data.
  {data_2014 <- c(NA, NA, NA, NA, NA, NA)
  data_2015 <- c(NA, NA, NA, NA, NA, NA)
  data_2016 <- c(NA, NA, NA, NA, NA, NA)
  data_2016 <- c(NA, NA, NA, NA, NA, NA)
  
  figure_6[, `:=`("2014"= data_2014, 
                  "2015"= data_2015,
                  "2016"= data_2016,
                  "2017"= data_2017)]}
  
  
  ## Current year data.
  year <- "2018"
  
  # Centres amb dades de HCV reportades.
  centres_hcv_disagg <-  disagg_hcv_clean[HCVScreeningTest == 1, unique(Centre)]
  disagg_hcv_clean_reported <- disagg_hcv_clean[Centre %in% centres_hcv_disagg]
  
  centres_hcv_agg <- agg_hcv_data[title == unique(grep(x = agg_hcv_data[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', .(center, !is.na(All))][V2 == T, center]
  agg_hcv_data_reported <- agg_hcv_data[center %in% centres_hcv_agg]
  
  
  # Centers N.
  n_centers <- disagg_hcv_clean_reported[, uniqueN(Centre)] + agg_hcv_data_reported[, uniqueN(center)]
  figure_6[X1 == 'Participating centres (n)', (year) := as.character(n_centers)]
  
  # People N.
  disagg_people_tested <- disagg_hcv_clean_reported[HCVScreeningTest == 1, .N]
  agg_people_tested <- agg_hcv_data_reported[grepl(x = title, pattern = 'CBVCT 1') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_tested <- disagg_people_tested + agg_people_tested
  figure_6[X1 == 'People tested (n)', (year) := as.character(total_people_tested)]
  
  # Reactive N.
  disagg_people_reactive <- disagg_hcv_clean_reported[HCVScreeningTestResult == 1, .N]
  agg_people_reactive <- agg_hcv_data_reported[grepl(x = title, pattern = 'CBVCT 5') & Category == 'All', sum(as.numeric(All), na.rm = T)]
  total_people_reactive <- disagg_people_reactive + agg_people_reactive
  figure_6[X1 == 'People with a reactive HCV screening test (n)', (year) := as.character(total_people_reactive)]
  
  # Reactive %.
  perc_react <- round(total_people_reactive/total_people_tested*100, digits = 1)
  figure_6[X1 == 'People with a reactive HCV screening test (%)', (year) := as.character(perc_react)]
  
  
  return(copy(figure_6))
}