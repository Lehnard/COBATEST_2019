# LINES TO TEST
# disagg_vih_clean <- disagg_vih
# agg_vih_data <- agg_cobatest$vih
# centers_data <- centers

generate_hiv_figure_2 <- function(disagg_vih_clean, agg_vih_data, centers_data) {
  
  ### VERSION:
  #     13-09-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula "# Figure 2: HIV Screening (N) and Reactive Tests (%) 
  #     by centre in the COBATEST Network 2019." segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #   - disagg_vih_clean: data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_vih_data.R". És el dataset net de dades desagragades VIH. 
  #
  #   - agg_vih_data:     dataset amb les dades de tots els centres que les envien en format dades agregades 
  #                       (el generem amb DM_02_COBATEST_AGG_DATA_ETL_2020.R)
  #
  
  # centers[, .N, Origen]
  fig2_disagg <- copy(centers_data[Origen %in% c('Cobatest_tool', 'datos_desagregados')])
  fig2_agg <- copy(centers_data[Origen == c('datos_agregados')])
  fig2_nodata <- copy(centers_data[Origen == c('No_data')])
  
  # Dades desagregades ####
  # Afegim les variables necessaries per a la N de HIV Screening i per al calcul del % Reactive Tests. 
  fig2_disagg <- merge(x = fig2_disagg, y = disagg_vih_clean[, .(N_hiv_screened= .N), .(Centre, ScreeningHIVTest)], 
                by.x = 'Centre', by.y = 'Centre', all.x = T)
  fig2_disagg <- merge(x = fig2_disagg, y = disagg_vih_clean[ScreeningTestResult == 1, .(N_hiv_reactive= .N), .(Centre, ScreeningTestResult)], 
                       by.x = 'Centre', by.y = 'Centre', all.x = T)
  
  # Cal treure els centres belgues o aquells que hagin pogut entrar sense vih test. 
  # fig2_disagg <- fig2_disagg[!(is.na(N_hiv_screened) & is.na(N_hiv_reactive))]  # obsolet, al final no els traiem
  fig2_disagg[is.na(N_hiv_screened) & is.na(N_hiv_reactive), Origen := "No_data"]
  
  # Posem a 0 els NAs. Volen dir que no hi ha casos reactius. 
  fig2_disagg[Origen != 'No_data' & is.na(N_hiv_reactive), N_hiv_reactive := 0] 
  
  # Treiem columnes auxiliars 
  fig2_disagg[, `:=`(ScreeningHIVTest= NULL, ScreeningTestResult= NULL)]
  
  # Dades agregades ####
  fig2_agg <- merge(x = fig2_agg, y = agg_vih_data[title == unique(grep(x = agg_vih_data[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', .(center, N_hiv_screened= All)], 
                    by.x = 'Centre', by.y= 'center', all.x = T)
  fig2_agg <- merge(x = fig2_agg, y = agg_vih_data[title == unique(grep(x = agg_vih_data[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', .(center, N_hiv_reactive= All)], 
                    by.x = 'Centre', by.y= 'center', all.x = T)
  
  # Dades No Data ####
  fig2_nodata[, `:=`(N_hiv_screened = NA, N_hiv_reactive = NA)]
  
  # RBind ####
  fig2 <- rbind(fig2_disagg, fig2_agg, fig2_nodata)
  
  # Calcul % ####
  fig2[, Perc_hiv_reactive := round(as.numeric(N_hiv_reactive)/as.numeric(N_hiv_screened) * 100, digits = 1)]
  
  # Missings ####
  fig2[is.na(fig2)] <- "-"
  
  return(fig2)
  
}

# LINES TO TEST
# disagg_syph_clean <- disagg_syph
# agg_syph_data <- agg_cobatest$syph
# centers_data <- centers

generate_syph_figure_4 <- function(disagg_syph_clean, agg_syph_data, centers_data) {
  
  ### VERSION:
  #     14-09-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula "# Figure 4: Syphilis Screening (N) and Reactive Tests (%) 
  #     by centre in the COBATEST Network 2019." segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #   - disagg_syph_clean: data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_data.R". És el dataset net de dades desagragades SYPH. 
  #
  #   - agg_syph_data:     dataset amb les dades de tots els centres que les envien en format dades agregades 
  #                       (el generem amb DM_02_COBATEST_AGG_DATA_ETL_2020.R)
  #
  
  # centers_data[, .N, Origen]
  fig4_disagg <- copy(centers_data[Origen %in% c('Cobatest_tool', 'datos_desagregados')])
  fig4_agg <- copy(centers_data[Origen == c('datos_agregados')])
  fig4_nodata <- copy(centers_data[Origen == c('No_data')])
  
  # Dades desagregades ####

  # COMPROBACIO:   disagg_syph_clean[, .N, .(Centre, ScreeningSyphTest, SyphScreeningTestResult)][order(Centre)]

  # Afegim les variables necessaries per a la N de SYPH Screening i per al calcul del % Reactive Tests. 
  fig4_disagg <- merge(x = fig4_disagg, y = disagg_syph_clean[SyphScreeningTest == 1, .(N_syph_screened= .N), .(Centre)], 
                       by.x = 'Centre', by.y = 'Centre', all.x = T)
  fig4_disagg <- merge(x = fig4_disagg, y = disagg_syph_clean[SyphScreeningTestResult == 1, .(N_syph_reactive= .N), .(Centre)], 
                       by.x = 'Centre', by.y = 'Centre', all.x = T)
  
  # Cal treure els centres belgues o aquells que hagin pogut entrar sense syph test. 
  # fig4_disagg <- fig4_disagg[!(is.na(N_syph_screened) & is.na(N_syph_reactive))]  # obsolet, al final no els traiem
  fig4_disagg[is.na(N_syph_screened) & is.na(N_syph_reactive), Origen := "No_data"]
  
  # Posem a 0 els NAs. Volen dir que no hi ha casos reactius. 
  fig4_disagg[is.na(N_syph_reactive), N_syph_reactive := 0] 
  
  # Dades agregades ####
  fig4_agg <- merge(x = fig4_agg, y = agg_syph_data[title == unique(grep(x = agg_syph_data[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', .(center, N_syph_screened= All)], 
                    by.x = 'Centre', by.y= 'center', all.x = T)
  fig4_agg <- merge(x = fig4_agg, y = agg_syph_data[title == unique(grep(x = agg_syph_data[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', .(center, N_syph_reactive= All)], 
                    by.x = 'Centre', by.y= 'center', all.x = T)
  
  # Dades No Data ####
  fig4_nodata[, `:=`(N_syph_screened = NA, N_syph_reactive = NA)]
  
  # RBind ####
  fig4 <- rbind(fig4_disagg, fig4_agg, fig4_nodata)
  
  # Calcul % ####
  fig4[, Perc_syph_reactive := round(as.numeric(N_syph_reactive)/as.numeric(N_syph_screened) * 100, digits = 1)]
  
  # Sortida per a la Megi. 
  fig4 <- fig4[, .(Centre, RealCentreName, N_syph_screened, N_syph_reactive, Perc_syph_reactive, Origen)]
  fig4[Origen == 'No_data', N_syph_reactive := NA]
  
  # Missings ####
  fig4[is.na(fig4)] <- "-"
  
  
  return(fig4)
  
}

# LINES TO TEST
# disagg_hcv_clean <- disagg_hcv
# agg_hcv_data <- agg_cobatest$hcv
# centers_data <- centers

generate_hcv_figure_3 <- function(disagg_hcv_clean, agg_hcv_data, centers_data) {
  
  ### VERSION:
  #     14-09-2022
  #
  ## DESCRIPTION:
  #     Script per generar la taula "# Figure 3: HCV Screening (N) and Reactive Tests (%) 
  #     by centre in the COBATEST Network 2019." segons report 2018: "Report Cobatest final 2018.pdf". 
  #
  ## PARAMETRES: 
  #   - disagg_hcv_clean: data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R" i
  #                             "cleaning_data.R". És el dataset net de dades desagragades HCV. 
  #
  #   - agg_hcv_data:     dataset amb les dades de tots els centres que les envien en format dades agregades 
  #                       (el generem amb DM_02_COBATEST_AGG_DATA_ETL_2020.R)
  #
  
  # centers_data[, .N, Origen]
  fig3_disagg <- copy(centers_data[Origen %in% c('Cobatest_tool', 'datos_desagregados')])
  fig3_agg <- copy(centers_data[Origen == c('datos_agregados')])
  fig3_nodata <- copy(centers_data[Origen == c('No_data')])
  
  # Dades desagregades ####
  # Afegim les variables necessaries per a la N de HCV Screening i per al calcul del % Reactive Tests. 
  fig3_disagg <- merge(x = fig3_disagg, y = disagg_hcv_clean[HCVScreeningTest == 1, .(N_hcv_screened= .N), .(Centre)], 
                       by.x = 'Centre', by.y = 'Centre', all.x = T)
  fig3_disagg <- merge(x = fig3_disagg, y = disagg_hcv_clean[HCVScreeningTestResult == 1, .(N_hcv_reactive= .N), .(Centre)], 
                       by.x = 'Centre', by.y = 'Centre', all.x = T)
  
  # Cal treure els centres belgues o aquells que hagin pogut entrar sense hcv test. 
  fig3_disagg[is.na(N_hcv_screened) & is.na(N_hcv_reactive), Origen := "No_data"]
  
  # Posem a 0 els NAs. Volen dir que no hi ha casos reactius. 
  fig3_disagg[is.na(N_hcv_reactive), N_hcv_reactive := 0] 
  
  # Dades agregades ####
  fig3_agg <- merge(x = fig3_agg, y = agg_hcv_data[title == unique(grep(x = agg_hcv_data[, title], pattern = 'CBVCT 1', value = T)) & Category == 'All', .(center, N_hcv_screened= All)], 
                    by.x = 'Centre', by.y= 'center', all.x = T)
  fig3_agg <- merge(x = fig3_agg, y = agg_hcv_data[title == unique(grep(x = agg_hcv_data[, title], pattern = 'CBVCT 5', value = T)) & Category == 'All', .(center, N_hcv_reactive= All)], 
                    by.x = 'Centre', by.y= 'center', all.x = T)
  
  # Dades No Data ####
  fig3_nodata[, `:=`(N_hcv_screened = NA, N_hcv_reactive = NA)]
  
  # RBind ####
  fig3 <- rbind(fig3_disagg, fig3_agg, fig3_nodata)
  
  # Calcul % ####
  fig3[, Perc_hcv_reactive := round(as.numeric(N_hcv_reactive)/as.numeric(N_hcv_screened) * 100, digits = 1)]
  
  # Sortida per a la Megi. 
  fig3 <- fig3[, .(Centre, RealCentreName, N_hcv_screened, N_hcv_reactive, Perc_hcv_reactive, Origen)]
  fig3[Origen == 'No_data', N_hcv_reactive := NA]
  
  # Missings ####
  fig3[is.na(fig3)] <- "-"
  
  return(fig3)
  
}