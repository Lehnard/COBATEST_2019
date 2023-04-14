## LINES TO TEST:
# disagg_clean_vih_data <- disagg_vih
# center= '0'
# center_title= 'Cobatest'

disagg_data_to_aggr_format <- function(disagg_clean_vih_data, center= '0', center_title= 'Cobatest') {
 
  # Funció per passar el dataset de dades desagregades VIH net (tal com queda 
  # després de passar els passos del Flowchart) a format dels numeradors de
  # les dades agregades.
  
  ## VERSION:
  #     23-09-2021
  #
  ## DESCRIPTION:
  #   Funció per passar el dataset de dades desagregades VIH net (tal com queda 
  #   després de passar els passos del Flowchart) a format dels numeradors de
  #   les dades agregades. 
  #
  ## PARAMETRES: 
  #   - disagg_clean_vih_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_vih_data.R". És el dataset net de dades desagragades VIH.

  fill_aggr_cbvct_block <- function(cbvct_sub_dt, cbvct_title) {
    
    # Generate aggregated data cbvct block layout.   
    cbvct_layout <- data.table(Category= c('MSM','SW','IDU','Migrants','All'))
    
    # columna All
    cbvct_layout[Category == 'MSM', All := cbvct_sub_dt[MSM == 1, .N]]
    cbvct_layout[Category == 'SW', All := cbvct_sub_dt[SW == 1, .N]]
    cbvct_layout[Category == 'IDU', All :=  cbvct_sub_dt[PWID == 1, .N]]
    cbvct_layout[Category == 'Migrants', All := cbvct_sub_dt[Migrant == 1, .N]]
    cbvct_layout[Category == 'All', All := cbvct_sub_dt[ScreeningHIVTest == 1, .N]]
    
    # columna Males
    cbvct_layout[Category == 'MSM', Males := cbvct_sub_dt[MSM == 1 & Gender == 1, .N]]
    cbvct_layout[Category == 'SW', Males := cbvct_sub_dt[SW == 1 & Gender == 1, .N]]
    cbvct_layout[Category == 'IDU', Males :=  cbvct_sub_dt[PWID == 1 & Gender == 1, .N]]
    cbvct_layout[Category == 'Migrants', Males := cbvct_sub_dt[Migrant == 1 & Gender == 1, .N]]
    cbvct_layout[Category == 'All', Males := cbvct_sub_dt[ScreeningHIVTest == 1 & Gender == 1, .N]]
    
    # columna Females
    cbvct_layout[Category == 'MSM', Females := cbvct_sub_dt[MSM == 1 & Gender == 2, .N]]
    cbvct_layout[Category == 'SW', Females := cbvct_sub_dt[SW == 1 & Gender == 2, .N]]
    cbvct_layout[Category == 'IDU', Females :=  cbvct_sub_dt[PWID == 1 & Gender == 2, .N]]
    cbvct_layout[Category == 'Migrants', Females := cbvct_sub_dt[Migrant == 1 & Gender == 2, .N]]
    cbvct_layout[Category == 'All', Females := cbvct_sub_dt[ScreeningHIVTest == 1 & Gender == 2, .N]]
    
    # columna Transgender
    cbvct_layout[Category == 'MSM', Transgender := cbvct_sub_dt[MSM == 1 & Gender == 3, .N]]
    cbvct_layout[Category == 'SW', Transgender := cbvct_sub_dt[SW == 1 & Gender == 3, .N]]
    cbvct_layout[Category == 'IDU', Transgender :=  cbvct_sub_dt[PWID == 1 & Gender == 3, .N]]
    cbvct_layout[Category == 'Migrants', Transgender := cbvct_sub_dt[Migrant == 1 & Gender == 3, .N]]
    cbvct_layout[Category == 'All', Transgender := cbvct_sub_dt[ScreeningHIVTest == 1 & Gender == 3, .N]]
    
    # columna <25 years old
    cbvct_layout[Category == 'MSM', `<25 years old` := cbvct_sub_dt[MSM == 1 & AgeGroup == 1, .N]]
    cbvct_layout[Category == 'SW', `<25 years old` := cbvct_sub_dt[SW == 1 & AgeGroup == 1, .N]]
    cbvct_layout[Category == 'IDU', `<25 years old` :=  cbvct_sub_dt[PWID == 1 & AgeGroup == 1, .N]]
    cbvct_layout[Category == 'Migrants', `<25 years old` := cbvct_sub_dt[Migrant == 1 & AgeGroup == 1, .N]]
    cbvct_layout[Category == 'All', `<25 years old` := cbvct_sub_dt[ScreeningHIVTest == 1 & AgeGroup == 1, .N]]
    
    # columna 25+ years old
    cbvct_layout[Category == 'MSM', `25+ years old` := cbvct_sub_dt[MSM == 1 & AgeGroup == 2, .N]]
    cbvct_layout[Category == 'SW', `25+ years old` := cbvct_sub_dt[SW == 1 & AgeGroup == 2, .N]]
    cbvct_layout[Category == 'IDU', `25+ years old` :=  cbvct_sub_dt[PWID == 1 & AgeGroup == 2, .N]]
    cbvct_layout[Category == 'Migrants', `25+ years old` := cbvct_sub_dt[Migrant == 1 & AgeGroup == 2, .N]]
    cbvct_layout[Category == 'All', `25+ years old` := cbvct_sub_dt[ScreeningHIVTest == 1 & AgeGroup == 2, .N]]
    
    cbvct_layout[, `:=`(title= cbvct_title, center= center, center_name= center_title)]  
    
    return (copy(cbvct_layout))  
  }
  
  disagg_to_aggr <- rbind(
    # "CBVCT 1: Number of clients tested for HIV"
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data, cbvct_title = "CBVCT 1: Number of clients tested for HIV with a screening test"),
    # "CBVCT 2 numerator: Number of clients who reported to have been previously tested for HIV "
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data[EverTested == 1], cbvct_title = "CBVCT 2: Proportion of clients who reported to have been previously tested for HIV "),
    # "CBVCT 3 numerator: Number of clients who reported to have been tested for HIV during preceding 12 months "
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data[TestedLastYear == 1], cbvct_title = "CBVCT 3:Proportion of clients who reported to have been tested for HIV during preceding 12 months "),
    # "CBVCT 4 numerator: Number of clients who reported to have been tested for HIV at the same CBVCT facility during preceding 12 months"
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data[TestedLastYearSameCBVCT == 1], cbvct_title = "CBVCT 4:Proportion of clients who reported to have been tested for HIV at the same CBVCT facility during preceding 12 months"),
    # "CBVCT 5 numerator: Number of clients with reactive screening HIV test result"
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data[ScreeningTestResult == 1], cbvct_title = "CBVCT 5: Proportion of clients with reactive screening HIV test result "),
    # "CBVCT 6 numerator: Number of clients with reactive screening HIV test result who were tested with confirmatory HIV test"
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data[ScreeningTestResult == 1 & ConfirmatoryHIVTest == 1], cbvct_title = "CBVCT 6: Proportion of clients with reactive screening HIV test result who were tested with confirmatory HIV test"),
    # "CBVCT 7 numerator: Number of clients with reactive screening HIV test result who were tested with confirmatory HIV test"
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data[ConfirmatoryHIVTestResult == 1], cbvct_title = "CBVCT 7: Proportion of clients with positive confirmatory HIV test result"),
    # "CBVCT 8 numerator: Number of clients with false positive screening results"
    fill_aggr_cbvct_block(cbvct_sub_dt = disagg_clean_vih_data[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2], cbvct_title = "CBVCT 8: Proportion of clients with false positive screening results") )
  
  return (disagg_to_aggr)
}

