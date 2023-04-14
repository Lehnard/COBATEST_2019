## LINES TO TEST
# numeradors_file <- agg_vih_numeradors_total
# indicador_numerad_number <- 3
# indicator_denom_number <- 1

compute_indicador <- function(numeradors_file, indicador_numerad_number, indicator_denom_number) {
  
  # VERSION:    15-11-2021
  
  # Es gestionen la línia default per als indicadors CBVCT2 -- > CBVCT8 
  # Es tracten apart CBVCT1 i CBVCT9
  
  # Gestió de noms indicadors.
  indicadors <- numeradors_file[, unique(title)]
  choice_numerad <- paste0('CBVCT ', indicador_numerad_number)
  choice_denom <- paste0('CBVCT ', indicator_denom_number)
  choice_indicador_num <- grep(x = indicadors, pattern = choice_numerad, ignore.case = T, value = T)
  choice_indicador_denom <- grep(x = indicadors, pattern = choice_denom, ignore.case = T, value = T)
  
  # Gestió de labels Numeradors, Denominadors i Proporcions. 
  proportion_txt <- paste0("Proportion (%)", unlist(strsplit(x = choice_indicador_num, split = "Proportion"))[2])
  numerator_txt <- paste0("Numerator: number", unlist(strsplit(x = choice_indicador_num, split = "Proportion"))[2])
  denominator_txt <- ifelse(test = indicator_denom_number == 1, 
                            yes = paste0("Denominator: ", unlist(strsplit(x = choice_indicador_denom, split = "CBVCT 1: "))[2]), 
                            no = paste0("Denominator: number", unlist(strsplit(x = choice_indicador_denom, split = "Proportion"))[2]))
  
  # Table layout com apareix la taula en el report.
  indicador_dt <- data.table(label = c('title','blank_1',
                                       'MSM_title','MSM_prop','MSM_num','MSM_denom',
                                       'SW_title','SW_prop','SW_num','SW_denom',
                                       'IDU_title','IDU_prop','IDU_num','IDU_denom',
                                       'Migr_title','Migr_prop','Migr_num','Migr_denom',
                                       'All_title','All_prop','All_num','All_denom'), 
                             subtitles= c(choice_indicador_num, "",
                                          "MSM", proportion_txt, numerator_txt, denominator_txt,
                                          "SW", proportion_txt, numerator_txt, denominator_txt,
                                          "IDU", proportion_txt,numerator_txt,denominator_txt,
                                          "Migrants", proportion_txt,numerator_txt,denominator_txt,
                                          "All", proportion_txt, numerator_txt, denominator_txt))
  
  # Add Colnames.
  indicador_dt[label == 'blank_1', `:=`(All= 'All', Males= 'Males', Females= 'Females', 
                                        Transgender= 'Transgender', Agegroup_1= "<25 years old", 
                                        Agegroup_2= '25+ years old')]
  
  # Defining subtitlles 
  {indicador_dt[label == 'MSM_title', subtitles := 'MSM']
  indicador_dt[label == 'SW_title', subtitles := 'SW']
  indicador_dt[label == 'IDU_title', subtitles := 'IDU']
  indicador_dt[label == 'Migr_title', subtitles := 'Migrants']
  indicador_dt[label == 'All_title', subtitles := 'All']}
  
  # Afegim numeradors
  {indicador_dt[label == 'MSM_num', `:=`(All= numeradors_file[Category == 'MSM' & title == choice_indicador_num, All],
                                         Males= numeradors_file[Category == 'MSM' & title == choice_indicador_num, Males],
                                         Females= numeradors_file[Category == 'MSM' & title == choice_indicador_num, Females],
                                         Transgender= numeradors_file[Category == 'MSM' & title == choice_indicador_num, Transgender],
                                         Agegroup_1= numeradors_file[Category == 'MSM' & title == choice_indicador_num, `<25 years old`],
                                         Agegroup_2= numeradors_file[Category == 'MSM' & title == choice_indicador_num, `25+ years old`] )]    
    
    indicador_dt[label == 'SW_num', `:=`(All= numeradors_file[Category == 'SW' & title == choice_indicador_num, All],
                                         Males= numeradors_file[Category == 'SW' & title == choice_indicador_num, Males],
                                         Females= numeradors_file[Category == 'SW' & title == choice_indicador_num, Females],
                                         Transgender= numeradors_file[Category == 'SW' & title == choice_indicador_num, Transgender],
                                         Agegroup_1= numeradors_file[Category == 'SW' & title == choice_indicador_num, `<25 years old`],
                                         Agegroup_2= numeradors_file[Category == 'SW' & title == choice_indicador_num, `25+ years old`] )]
    
    indicador_dt[label == 'IDU_num', `:=`(All= numeradors_file[Category == 'IDU' & title == choice_indicador_num, All],
                                          Males= numeradors_file[Category == 'IDU' & title == choice_indicador_num, Males],
                                          Females= numeradors_file[Category == 'IDU' & title == choice_indicador_num, Females],
                                          Transgender= numeradors_file[Category == 'IDU' & title == choice_indicador_num, Transgender],
                                          Agegroup_1= numeradors_file[Category == 'IDU' & title == choice_indicador_num, `<25 years old`],
                                          Agegroup_2= numeradors_file[Category == 'IDU' & title == choice_indicador_num, `25+ years old`] )]
    
    indicador_dt[label == 'Migr_num', `:=`(All= numeradors_file[Category == 'Migrants' & title == choice_indicador_num, All],
                                           Males= numeradors_file[Category == 'Migrants' & title == choice_indicador_num, Males],
                                           Females= numeradors_file[Category == 'Migrants' & title == choice_indicador_num, Females],
                                           Transgender= numeradors_file[Category == 'Migrants' & title == choice_indicador_num, Transgender],
                                           Agegroup_1= numeradors_file[Category == 'Migrants' & title == choice_indicador_num, `<25 years old`],
                                           Agegroup_2= numeradors_file[Category == 'Migrants' & title == choice_indicador_num, `25+ years old`] )]
    
    indicador_dt[label == 'All_num', `:=`(All= numeradors_file[Category == 'All' & title == choice_indicador_num, All],
                                          Males= numeradors_file[Category == 'All' & title == choice_indicador_num, Males],
                                          Females= numeradors_file[Category == 'All' & title == choice_indicador_num, Females],
                                          Transgender= numeradors_file[Category == 'All' & title == choice_indicador_num, Transgender],
                                          Agegroup_1= numeradors_file[Category == 'All' & title == choice_indicador_num, `<25 years old`],
                                          Agegroup_2= numeradors_file[Category == 'All' & title == choice_indicador_num, `25+ years old`] )]}
  
  
  # Afegim denominadors
  {indicador_dt[label == 'MSM_denom', `:=`(All= numeradors_file[Category == 'MSM' & title == choice_indicador_denom, All],
                                           Males= numeradors_file[Category == 'MSM' & title == choice_indicador_denom, Males],
                                           Females= numeradors_file[Category == 'MSM' & title == choice_indicador_denom, Females],
                                           Transgender= numeradors_file[Category == 'MSM' & title == choice_indicador_denom, Transgender],
                                           Agegroup_1= numeradors_file[Category == 'MSM' & title == choice_indicador_denom, `<25 years old`],
                                           Agegroup_2= numeradors_file[Category == 'MSM' & title == choice_indicador_denom, `25+ years old`] )]    
    
    indicador_dt[label == 'SW_denom', `:=`(All= numeradors_file[Category == 'SW' & title == choice_indicador_denom, All],
                                           Males= numeradors_file[Category == 'SW' & title == choice_indicador_denom, Males],
                                           Females= numeradors_file[Category == 'SW' & title == choice_indicador_denom, Females],
                                           Transgender= numeradors_file[Category == 'SW' & title == choice_indicador_denom, Transgender],
                                           Agegroup_1= numeradors_file[Category == 'SW' & title == choice_indicador_denom, `<25 years old`],
                                           Agegroup_2= numeradors_file[Category == 'SW' & title == choice_indicador_denom, `25+ years old`] )]
    
    indicador_dt[label == 'IDU_denom', `:=`(All= numeradors_file[Category == 'IDU' & title == choice_indicador_denom, All],
                                            Males= numeradors_file[Category == 'IDU' & title == choice_indicador_denom, Males],
                                            Females= numeradors_file[Category == 'IDU' & title == choice_indicador_denom, Females],
                                            Transgender= numeradors_file[Category == 'IDU' & title == choice_indicador_denom, Transgender],
                                            Agegroup_1= numeradors_file[Category == 'IDU' & title == choice_indicador_denom, `<25 years old`],
                                            Agegroup_2= numeradors_file[Category == 'IDU' & title == choice_indicador_denom, `25+ years old`] )]
    
    indicador_dt[label == 'Migr_denom', `:=`(All= numeradors_file[Category == 'Migrants' & title == choice_indicador_denom, All],
                                             Males= numeradors_file[Category == 'Migrants' & title == choice_indicador_denom, Males],
                                             Females= numeradors_file[Category == 'Migrants' & title == choice_indicador_denom, Females],
                                             Transgender= numeradors_file[Category == 'Migrants' & title == choice_indicador_denom, Transgender],
                                             Agegroup_1= numeradors_file[Category == 'Migrants' & title == choice_indicador_denom, `<25 years old`],
                                             Agegroup_2= numeradors_file[Category == 'Migrants' & title == choice_indicador_denom, `25+ years old`] )]
    
    indicador_dt[label == 'All_denom', `:=`(All= numeradors_file[Category == 'All' & title == choice_indicador_denom, All],
                                            Males= numeradors_file[Category == 'All' & title == choice_indicador_denom, Males],
                                            Females= numeradors_file[Category == 'All' & title == choice_indicador_denom, Females],
                                            Transgender= numeradors_file[Category == 'All' & title == choice_indicador_denom, Transgender],
                                            Agegroup_1= numeradors_file[Category == 'All' & title == choice_indicador_denom, `<25 years old`],
                                            Agegroup_2= numeradors_file[Category == 'All' & title == choice_indicador_denom, `25+ years old`] )]}
  
  
  # Afegim indicadors - proportion
  {indicador_dt[label == 'MSM_prop', `:=`(All= round(as.numeric(indicador_dt[label == 'MSM_num', All])/as.numeric(indicador_dt[label == 'MSM_denom', All])*100, digits= 2),
                                          Males= round(as.numeric(indicador_dt[label == 'MSM_num', Males])/as.numeric(indicador_dt[label == 'MSM_denom', Males])*100, digits= 2),  
                                          Females= round(as.numeric(indicador_dt[label == 'MSM_num', Females])/as.numeric(indicador_dt[label == 'MSM_denom', Females])*100, digits= 2), 
                                          Transgender= round(as.numeric(indicador_dt[label == 'MSM_num', Transgender])/as.numeric(indicador_dt[label == 'MSM_denom', Transgender])*100, digits= 2),
                                          Agegroup_1= round(as.numeric(indicador_dt[label == 'MSM_num', Agegroup_1])/as.numeric(indicador_dt[label == 'MSM_denom', Agegroup_1])*100, digits= 2),
                                          Agegroup_2= round(as.numeric(indicador_dt[label == 'MSM_num', Agegroup_2])/as.numeric(indicador_dt[label == 'MSM_denom', Agegroup_2])*100, digits= 2)  )]    
    
    indicador_dt[label == 'SW_prop', `:=`(All= round(as.numeric(indicador_dt[label == 'SW_num', All])/as.numeric(indicador_dt[label == 'SW_denom', All])*100, digits= 2),
                                          Males= round(as.numeric(indicador_dt[label == 'SW_num', Males])/as.numeric(indicador_dt[label == 'SW_denom', Males])*100, digits= 2),  
                                          Females= round(as.numeric(indicador_dt[label == 'SW_num', Females])/as.numeric(indicador_dt[label == 'SW_denom', Females])*100, digits= 2), 
                                          Transgender= round(as.numeric(indicador_dt[label == 'SW_num', Transgender])/as.numeric(indicador_dt[label == 'SW_denom', Transgender])*100, digits= 2),
                                          Agegroup_1= round(as.numeric(indicador_dt[label == 'SW_num', Agegroup_1])/as.numeric(indicador_dt[label == 'SW_denom', Agegroup_1])*100, digits= 2),
                                          Agegroup_2= round(as.numeric(indicador_dt[label == 'SW_num', Agegroup_2])/as.numeric(indicador_dt[label == 'SW_denom', Agegroup_2])*100, digits= 2)  )]
    
    indicador_dt[label == 'IDU_prop', `:=`(All= round(as.numeric(indicador_dt[label == 'IDU_num', All])/as.numeric(indicador_dt[label == 'IDU_denom', All])*100, digits= 2),
                                           Males= round(as.numeric(indicador_dt[label == 'IDU_num', Males])/as.numeric(indicador_dt[label == 'IDU_denom', Males])*100, digits= 2),  
                                           Females= round(as.numeric(indicador_dt[label == 'IDU_num', Females])/as.numeric(indicador_dt[label == 'IDU_denom', Females])*100, digits= 2), 
                                           Transgender= round(as.numeric(indicador_dt[label == 'IDU_num', Transgender])/as.numeric(indicador_dt[label == 'IDU_denom', Transgender])*100, digits= 2),
                                           Agegroup_1= round(as.numeric(indicador_dt[label == 'IDU_num', Agegroup_1])/as.numeric(indicador_dt[label == 'IDU_denom', Agegroup_1])*100, digits= 2),
                                           Agegroup_2= round(as.numeric(indicador_dt[label == 'IDU_num', Agegroup_2])/as.numeric(indicador_dt[label == 'IDU_denom', Agegroup_2])*100, digits= 2)  )]
    
    indicador_dt[label == 'Migr_prop', `:=`(All= round(as.numeric(indicador_dt[label == 'Migr_num', All])/as.numeric(indicador_dt[label == 'Migr_denom', All])*100, digits= 2),
                                            Males= round(as.numeric(indicador_dt[label == 'Migr_num', Males])/as.numeric(indicador_dt[label == 'Migr_denom', Males])*100, digits= 2),  
                                            Females= round(as.numeric(indicador_dt[label == 'Migr_num', Females])/as.numeric(indicador_dt[label == 'Migr_denom', Females])*100, digits= 2), 
                                            Transgender= round(as.numeric(indicador_dt[label == 'Migr_num', Transgender])/as.numeric(indicador_dt[label == 'Migr_denom', Transgender])*100, digits= 2),
                                            Agegroup_1= round(as.numeric(indicador_dt[label == 'Migr_num', Agegroup_1])/as.numeric(indicador_dt[label == 'Migr_denom', Agegroup_1])*100, digits= 2),
                                            Agegroup_2= round(as.numeric(indicador_dt[label == 'Migr_num', Agegroup_2])/as.numeric(indicador_dt[label == 'Migr_denom', Agegroup_2])*100, digits= 2)  )]
    
    indicador_dt[label == 'All_prop', `:=`(All= round(as.numeric(indicador_dt[label == 'All_num', All])/as.numeric(indicador_dt[label == 'All_denom', All])*100, digits= 2),
                                           Males= round(as.numeric(indicador_dt[label == 'All_num', Males])/as.numeric(indicador_dt[label == 'All_denom', Males])*100, digits= 2),  
                                           Females= round(as.numeric(indicador_dt[label == 'All_num', Females])/as.numeric(indicador_dt[label == 'All_denom', Females])*100, digits= 2), 
                                           Transgender= round(as.numeric(indicador_dt[label == 'All_num', Transgender])/as.numeric(indicador_dt[label == 'All_denom', Transgender])*100, digits= 2),
                                           Agegroup_1= round(as.numeric(indicador_dt[label == 'All_num', Agegroup_1])/as.numeric(indicador_dt[label == 'All_denom', Agegroup_1])*100, digits= 2),
                                           Agegroup_2= round(as.numeric(indicador_dt[label == 'All_num', Agegroup_2])/as.numeric(indicador_dt[label == 'All_denom', Agegroup_2])*100, digits= 2)  )]}
  
  
  # Gestionem cbvct1 i cbvct9 que son una mica diferents. 
  if(indicador_numerad_number == 1 & indicator_denom_number == 1) {
    # CAS CBVCT 1: Seleccionem només els numeradors i calculem % per grups tenint en compte el total de cada grup. 
    indicador_dt <- indicador_dt[label %in% c('title', 'blank_1', grep(x = indicador_dt[, label], pattern = "_num|_prop", value = T))]
    
    select_prop_mask <- grepl(x = indicador_dt[, label], pattern = "_prop")
    select_num_mask <- grepl(x = indicador_dt[, label], pattern = "_num")
    indicador_dt[select_prop_mask, All := round(indicador_dt[select_num_mask, as.numeric(All)]/indicador_dt[label== 'All_num', as.numeric(All)] * 100, digit= 1)]
    indicador_dt[select_prop_mask, Males := round(indicador_dt[select_num_mask, as.numeric(Males)]/indicador_dt[label== 'All_num', as.numeric(Males)] * 100, digit= 1)]
    indicador_dt[select_prop_mask, Females := round(indicador_dt[select_num_mask, as.numeric(Females)]/indicador_dt[label== 'All_num', as.numeric(Females)] * 100, digit= 1)]
    indicador_dt[select_prop_mask, Transgender := round(indicador_dt[select_num_mask, as.numeric(Transgender)]/indicador_dt[label== 'All_num', as.numeric(Transgender)] * 100, digit= 1)]
    indicador_dt[select_prop_mask, Agegroup_1 := round(indicador_dt[select_num_mask, as.numeric(Agegroup_1)]/indicador_dt[label== 'All_num', as.numeric(Agegroup_1)] * 100, digit= 1)]
    indicador_dt[select_prop_mask, Agegroup_2 := round(indicador_dt[select_num_mask, as.numeric(Agegroup_2)]/indicador_dt[label== 'All_num', as.numeric(Agegroup_2)] * 100, digit= 1)]
    
    indicador_dt[label == 'MSM_num', subtitles := "MSM"]  
    indicador_dt[label == 'SW_num', subtitles := "SW"]  
    indicador_dt[label == 'IDU_num', subtitles := "IDU"]  
    indicador_dt[label == 'Migr_num', subtitles := "Migrants"]  
    indicador_dt[label == 'All_num', subtitles := "All"]
    
    indicador_dt[label == 'MSM_prop', subtitles := "Proportion (%)"]
    indicador_dt[label == 'SW_prop', subtitles := "Proportion (%)"]
    indicador_dt[label == 'IDU_prop', subtitles := "Proportion (%)"]
    indicador_dt[label == 'Migr_prop', subtitles := "Proportion (%)"]
    indicador_dt[label == 'All_prop', subtitles := "Proportion (%)"]
    
  } else if(indicador_numerad_number == 1 & indicator_denom_number == 7) {
    # CAS CBVCT 9
    cbvct9_labels <- indicador_dt[, .(label, subtitles)]
    
    {cbvct9_labels[label == 'title', subtitles := "CBVCT 9:  Number of clients needed to test to find a positive HIV result"]
    
    cbvct9_labels[label == 'MSM_prop', subtitles := "Number of clients needed to test to find a positive HIV result"]
    cbvct9_labels[label == 'SW_prop', subtitles := "Number of clients needed to test to find a positive HIV result"]
    cbvct9_labels[label == 'IDU_prop', subtitles := "Number of clients needed to test to find a positive HIV result"]
    cbvct9_labels[label == 'Migr_prop', subtitles := "Number of clients needed to test to find a positive HIV result"]
    cbvct9_labels[label == 'All_prop', subtitles := "Number of clients needed to test to find a positive HIV result"]
    
    cbvct9_labels[label == 'MSM_num', subtitles := "Numerator: number of clients tested for HIV with a screening test"]
    cbvct9_labels[label == 'SW_num', subtitles := "Numerator: number of clients tested for HIV with a screening test"]
    cbvct9_labels[label == 'IDU_num', subtitles := "Numerator: number of clients tested for HIV with a screening test"]
    cbvct9_labels[label == 'Migr_num', subtitles := "Numerator: number of clients tested for HIV with a screening test"]
    cbvct9_labels[label == 'All_num', subtitles := "Numerator: number of clients tested for HIV with a screening test"]
    
    cbvct9_labels[label == 'MSM_denom', subtitles := "Denominator: number of clients with positive confirmatory HIV test result "]
    cbvct9_labels[label == 'SW_denom', subtitles := "Denominator: number of clients with positive confirmatory HIV test result "]
    cbvct9_labels[label == 'IDU_denom', subtitles := "Denominator: number of clients with positive confirmatory HIV test result "]
    cbvct9_labels[label == 'Migr_denom', subtitles := "Denominator: number of clients with positive confirmatory HIV test result "]
    cbvct9_labels[label == 'All_denom', subtitles := "Denominator: number of clients with positive confirmatory HIV test result "]}
    
    indicador_dt[, subtitles := cbvct9_labels[, .(subtitles)]]
    
    # Cal treure en les proporcions la part automatizada num/denom * 100.   Aquí no fem percentatges sino num/denom. 
    # Dividim per 100. 
    indicador_dt[grepl(x = label, pattern = '_prop'), `:=`(All= round(as.numeric(All)/100, digits = 1), 
                                                            Males= round(as.numeric(Males)/100, digits = 1), 
                                                            Females= round(as.numeric(Females)/100, digits = 1), 
                                                            Transgender= round(as.numeric(Transgender)/100, digits = 1), 
                                                            Agegroup_1= round(as.numeric(Agegroup_1)/100, digits = 1), 
                                                            Agegroup_2= round(as.numeric(Agegroup_2)/100, digits = 1))]
  }
  
  # Neteja per presentar-lo com a l'EXCEL
  indicador_dt[, label := NULL]
  
  return (copy(indicador_dt))  
}