rm(list= ls())
graphics.off()
cat("\014")


################################################################################################# #
#
# Versió:      13-04-2023
# 
# Descripció:   Script conductor per al càlcul d'indicadors pel report COBATEST 2018 adaptat 
#               de l'script del 2021.
#
################################################################################################# #

library(data.table)
library(openxlsx)


source("src/functions/cleaning_data.R")               # Script per netejar les dades desagregades dels que han estat testats.
source("src/functions/compute_indicador.R")           # Script per calcular els indicadors i mostra-los directament en el format excel de sortida.
source("src/functions/disagg_data_to_aggr_format.R")  # Script per passar dades desagregades netes a format numeradors dades agregades.
source("src/functions/make_flowchart_diagramm.R")     # Script per pintar el diagrama flowchart vih.
source("src/functions/report_figures.R")              # Script per generar les figures del report.
source("src/functions/report_tables.R")               # Script per generar les taules del report.
source("src/functions/annex_tables.R")                # Script per generar les taules de l'annex1 i annex2.

#------------------------------------------------------------------------------#
# 0.- PARAMETRES                                                            ####
#------------------------------------------------------------------------------#

## Paths   ####
# ~~~~~~~~~~~~~  
DISAGG_PROCESSED_DATA_FOLDERPATH <- "data/DisaggragatedData/disagg_processed_data/"
AGG_PROCESSED_DATA_FOLDERPATH    <- "data/AggregadatedData/agg_processed_data/"
AUXILIAR_DATA_FOLDER             <- "data/Auxiliar_data/"
OUTPUT_FOLDERPATH                <- "out/results/"

## Centres data   ####
# ~~~~~~~~~~~~~~~~~~~~
CENTERS_DATA_FILENAME    <- "Centros_v2018.xlsx"
CENTERS_DATA_SHEET_NAME  <- "COBA_2018"

## Save params   ####
# ~~~~~~~~~~~~~~~~~~~
TODAY <- gsub(pattern = "-", replacement = "", x = Sys.Date())


#------------------------------------------------------------------------------#
# 1.- DISAGGREGATED DATA - GENEARATE / LOAD                                 ####
#------------------------------------------------------------------------------#
# Aquest script carrega les dades dels centres de COBATEST TOOL i hi afegeix les dades enviades
# en format TOOL d'alguns altres centres (dates desagregades) per unificar-ho tot en un únic dataset.

# Load saved CSV results. To generate them run the  DM_01_COBATEST_DISAGG_DATA_ETL_2017.R  script with the "fwrite" uncommented.
list.files(DISAGG_PROCESSED_DATA_FOLDERPATH)
file <- "disagg_cobatest_2018_20230413.csv"
disagg_cobatest <- fread(paste0(DISAGG_PROCESSED_DATA_FOLDERPATH, file), encoding = 'UTF-8')


#------------------------------------------------------------------------------#
# 2.- AGGREGATED DATA - GENEARATE / LOAD                                    ####
#------------------------------------------------------------------------------#
# Aquest script carrega les dades dels centres que envien en format .XLSX tipus dades desagregades i hi 
# fa un parseig per a desar-les en una llista amb compartiments VIH, SYPH i HCV. 

# Load saved CSV results. To generate them run the  DM_02_COBATEST_AGG_DATA_ETL_2017.R  script with the "save" uncommented.
list.files(AGG_PROCESSED_DATA_FOLDERPATH)
file <- "agg_cobatest_2018_20230413.rds"
agg_cobatest <- readRDS(paste0(AGG_PROCESSED_DATA_FOLDERPATH, file))

# View(agg_cobatest$vih)

#------------------------------------------------------------------------------#
# 3.- AUXILIAR DATA - LOAD                                                  ####
#------------------------------------------------------------------------------#
# Es carrega un fitxer de mapeig de noms de centres amb els seus codis i el tipus de dades que son: cobatest tool, desagregades o agregades.
centers <- setDT(read.xlsx(paste0(AUXILIAR_DATA_FOLDER, CENTERS_DATA_FILENAME), sheet = CENTERS_DATA_SHEET_NAME))


# ___________________________________________________________________________####
# VIH                                                                        ####
# ************************************************************************** ####


#------------------------------------------------------------------------------#
# 4.- PROC VIH DISAGG DATA                                                  ####
#------------------------------------------------------------------------------#

# Mostrem el nombre de registres per centre BeTested. 
bt_centers <- centers[grepl(x = RealCentreName, pattern = "BeTested"), Centre]
disagg_cobatest[Centre %in% bt_centers, .N, RealCentreName]

# Procés de neteja de les dades desagregades:
#
#     1.- Processar el dataset raw de vih datos desagregados. 
#     2.- Obtenir el flowchart de vih datos desagregados. 
clean_vih_data_list <- cleaning_vih_data(disagg_data= disagg_cobatest, centers_data= centers)

disagg_vih <- clean_vih_data_list[["vih_clean_data"]]
# flowchart_vih <- clean_vih_data_list[["flowchart"]]     # Flowchart.


# # COMPROBACIO:    Quants cauen en la neteja per centre?
# neteja <- merge(x = disagg_cobatest[, .N, Centre][order(as.numeric(Centre))], 
#                 y = disagg_vih[, .(N_clean= .N), Centre][order(as.numeric(Centre))],
#                 by= 'Centre', all.x = T)
# neteja[, diff := N-N_clean]
# neteja



#------------------------------------------------------------------------------#
# 5.- HIV INDICADORS                                                        ####
#------------------------------------------------------------------------------#

## Transformem les dades desagregades en format dades agregades. 
# CHEQUEIG: Només dades vih que hagin passat el pre-processament de neteja del flowchart (script: "cleaning_vih_data.R") permesos.
ScreeningHIVTest_categories <- disagg_vih[, .N, ScreeningHIVTest][, ScreeningHIVTest]
if(any(ScreeningHIVTest_categories != 1 | is.na(ScreeningHIVTest_categories))) stop("At this point, cleaned vih disaggregated data still contains ScreeningHIVTest != 1")

disagg_to_aggr <- disagg_data_to_aggr_format(disagg_clean_vih_data = disagg_vih)

## Afegim disagg_data in agg_data format to agg_vih_data
agg_vih_numeradors <- rbind(disagg_to_aggr, agg_cobatest$vih)

# # COMPROBACIO:    com son els numeradors agregats per centre per un CBVCT X donat.
# agg_vih_numeradors[grepl(x = title, pattern = 'CBVCT 4'), .(Category, All, Males, Females, Transgender, `<25 years old`, `25+ years old`, center_name)]

## Generem aggreggated data numeradors Totals. 

## COMPROBACIO:   Warnings a as.numeric
# agg_vih_numeradors[is.na(as.numeric(All)), .N, All]
# agg_vih_numeradors[is.na(as.numeric(Males)), .N, Males]
# agg_vih_numeradors[is.na(as.numeric(Females)), .N, Females]  # Aquí n'hi ha d'haver. És normal. Veure plantilla Agregades Females-MSM
# agg_vih_numeradors[is.na(as.numeric(Transgender)), .N, Transgender]
# agg_vih_numeradors[is.na(as.numeric(`<25 years old`)), .N, `<25 years old`]
# agg_vih_numeradors[is.na(as.numeric(`25+ years old`)), .N, `25+ years old`]
#
# colSums(is.na(agg_vih_numeradors[, .(All,Males,Females,Transgender,`<25 years old`,`25+ years old`)]))

agg_vih_numeradors_total <- agg_vih_numeradors[, .(All= sum(as.numeric(All), na.rm = T),
                                                   Males= sum(as.numeric(Males), na.rm = T),
                                                   Females= sum(as.numeric(Females), na.rm = T),
                                                   Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                   `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T),
                                                   `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T) ),
                                               by= .(title, Category)]


## Calculem indicadors.
{# CBVCT 1:  Number of clients tested for HIV with a screening test.
vih_cbvct1 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 1, indicator_denom_number = 1) 
# CBVCT 2: Proportion of clients who reported to have been previously tested for HIV.
vih_cbvct2 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 2, indicator_denom_number = 1) 
# CBVCT 3: Proportion of clients who reported to have been tested for HIV during preceding 12 months.
vih_cbvct3 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 3, indicator_denom_number = 1) 
# CBVCT 4: Proportion of clients who reported to have been tested for HIV at the same CBVCT facility during preceding 12 months.
vih_cbvct4 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 4, indicator_denom_number = 1) 
# CBVCT 5: Proportion of clients with reactive screening HIV test result.
vih_cbvct5 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 5, indicator_denom_number = 1) 
# CBVCT 6: Proportion of clients with reactive screening HIV test result who were tested with confirmatory HIV test.
vih_cbvct6 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 6, indicator_denom_number = 5) 
# CBVCT 7: Proportion of clients with positive confirmatory HIV test result.
vih_cbvct7 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 7, indicator_denom_number = 1) 
# CBVCT 8: Proportion of clients with false positive screening results.
vih_cbvct8 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 8, indicator_denom_number = 1) 
# CBVCT 9:  Number of clients needed to test to find a positive HIV result
# CBVCT 9 is automatically calculated during data analysis since both numerator and denominator of this indicator are previously reported in CBVCT 1 and CBVCT 7. There is no need to input any data here.
vih_cbvct9 <- compute_indicador(numeradors_file = agg_vih_numeradors_total, indicador_numerad_number = 1, indicator_denom_number = 7) }
# Agrupem indicadors 
indicadors <- list('cbvct1'= vih_cbvct1, 
                   'cbvct2'= vih_cbvct2,
                   'cbvct3'= vih_cbvct3,
                   'cbvct4'= vih_cbvct4,
                   'cbvct5'= vih_cbvct5,
                   'cbvct6'= vih_cbvct6,
                   'cbvct7'= vih_cbvct7,
                   'cbvct8'= vih_cbvct8,
                   'cbvct9'= vih_cbvct9)


## Exportem a EXCEL.
# Lists elements are written to individual worksheets, using list names as sheet names if available
sheets <- list("HIV_CBVCT_1" = indicadors$cbvct1, 
               "HIV_CBVCT_2" = indicadors$cbvct2,
               "HIV_CBVCT_3" = indicadors$cbvct3,
               "HIV_CBVCT_4" = indicadors$cbvct4,
               "HIV_CBVCT_5" = indicadors$cbvct5,
               "HIV_CBVCT_6" = indicadors$cbvct6,
               "HIV_CBVCT_7" = indicadors$cbvct7,
               "HIV_CBVCT_8" = indicadors$cbvct8,
               "HIV_CBVCT_9" = indicadors$cbvct9)

## Save
# write.xlsx(x = sheets, file = paste0(OUTPUT_FOLDERPATH, "vih_indicadors_Report_2018_", TODAY, ".xlsx"), colNames= FALSE)

# Shortcut to folderpath.
# rstudioapi::selectFile(path = OUTPUT_FOLDERPATH)

# ws cleaning. 
rm(list = ls(pattern = 'vih_cbvct'))
rm(sheets, ScreeningHIVTest_categories, disagg_to_aggr, agg_vih_numeradors, agg_vih_numeradors_total)



#------------------------------------------------------------------------------#
# 6.- FIGURES                                                               ####
#------------------------------------------------------------------------------#

## 6.1.- FIGURA 1:  FLOWCHART  ####
# ------------------------------- #

# Carreguem el Flowchart VIH que hem fet durant l'execució de la funció:  "cleaning_vih_data()". 
disagg_vih_flowchart  <- clean_vih_data_list[["flowchart"]]

# Cal afegir la N de les dades agregades.
agg_vih_data_population <- agg_cobatest[["vih"]][grepl(x = title, pattern = "CBVCT 1") & Category == 'All', .(title, center, center_name, Category, All)]

vih_flowchart <- disagg_vih_flowchart
vih_flowchart["Aggregated data"] <- agg_vih_data_population[, sum(as.numeric(All), na.rm = T)]
vars_to_add_agg_counts <- c("Total data",                                                                      
                            "HIV Tests performed",                                                                             
                            "HIV Tests of people Aged >= 16",   
                            "HIV Tests of people one per day",
                            "People tested for HIV",                                                                           
                            "People tested for HIV screening test result available",                                           
                            "People tested with HIV screening test result available who are not previously diagnosed with HIV")


vih_flowchart[names(vih_flowchart) %in% vars_to_add_agg_counts] <-  vih_flowchart[names(vih_flowchart) %in% vars_to_add_agg_counts] + vih_flowchart["Aggregated data"]

# Make DiagrammeR flowchart. 
make_flowchart_diagramm(vih_flowchart)

rm(vars_to_add_agg_counts, disagg_vih_flowchart, agg_vih_data_population)


## 6.2.- FIGURA 2  ####
# ------------------- #
# Figure 2: HIV Screening (N) and Reactive Tests (%) by centre in the COBATEST Network 2019.
agg_vih <- agg_cobatest[['vih']]

figure_2 <- generate_hiv_figure_2(disagg_vih_clean = disagg_vih, agg_vih_data = agg_vih, centers_data = centers)

# GRAFICS DUAL Y. 
# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
# ## No s'acaben d'aliniar bé les labels X ticks
# library(ggplot2)
# ggplot(data = figure_2) +
#   geom_bar(aes(x= RealCentreName, y = as.numeric(N_hiv_screened)), stat = 'identity', fill= 'firebrick2') +
#   scale_y_continuous(expand = c(0.01,0)) +
#   ggtitle("Figure 2. HIV Screened (N) and Reactive Test (%) by Centre in the COBATEST Network 2020") +
#   ylab('HIV Screened (N)') +
#   theme(axis.text.x = element_text(angle = 90, hjust= 1, size = 12, face= 'bold'),
#         axis.title.x = element_blank(),
#         axis.line.x = element_blank(),
#         panel.grid = element_blank(),
#         panel.grid.major.y = element_line(color = 'black'),
#         panel.background = element_rect(colour = 'black', fill = 'gray90'))


figure_2 <- figure_2[order(as.numeric(Centre))]

# output
figure_2 <- figure_2[Origen != 'No_data', .(Centre, RealCentreName, N_hiv_screened, N_hiv_reactive, Perc_hiv_reactive, Origen)]

# Save
# write.xlsx(x = figure_2, file = paste0(OUTPUT_FOLDERPATH, "vih_figure_2_2018_", TODAY, ".xlsx"), colNames= T)

# Shortcut to folderpath.
# rstudioapi::selectFile(path = OUTPUT_FOLDERPATH)

rm(agg_vih)


#------------------------------------------------------------------------------#
# 7.- HIV REPORT TABLES                                                     ####
#------------------------------------------------------------------------------#

## 7.1.- TABLE 1  ####
# ------------------ #
# Table 1: Summary of people screened for HIV. Del report cobatest 2018 pdf. 
# Ajuntem totes les dades en format dades agregades per a fer els còmputs. 

# Dades agregades VIH
agg_vih <- agg_cobatest$vih
# COMPROBACIO:    agg_vih[, .N, by= .(center, center_name)]

# Generem Taula
table_1 <- generate_hiv_report_table_1(disagg_vih_clean = disagg_vih, agg_vih_data = agg_vih)


## 7.2.- TABLE 2  ####
# ------------------ #
#
# COMPTE, la funció que genera aquesta taula té una part manual crucial. Cal adaptar quan canviem d'any
#         sino els resultats poden no tenir sentit. 

table_2 <- generate_hiv_report_table_2(disagg_vih_clean = disagg_vih, agg_vih_data = agg_vih, centers_data= centers)


# Lists elements are written to individual worksheets, using list names as sheet names if available
sheets <- list("Table_1" = table_1, 
               "Table_2" = table_2)

# Save
# write.xlsx(x = sheets, file = paste0(OUTPUT_FOLDERPATH, "vih_tables_1_and_2_2018_", TODAY, ".xlsx"), colNames= T)

rm(agg_vih)

 
#------------------------------------------------------------------------------#
# 8.- HIV ANNEX TABLES                                                      ####
#------------------------------------------------------------------------------#


## 8.1.- ANNEX 1 TABLE ####
# ----------------------- #
# DESCRIPCIO:   Reproduim les taules de l'Annex 1 de "Report Cobatest final 2018.pdf"
#
#               Annex 1: Indicators by centre with corresponding estimates of 
#               indicators taking into account missing information (EMV)
#
#               Es generen taules amb estructura: 
#               CENTRE - Persons screened for HIV - CBVCTX_N - CBVCTX_% - CBVCTX_NAs - CBVCTX_EMV  


# Generació de la taula. 
annex1_vih <- generate_hiv_annex1_table(disagg_vih_clean_data = disagg_vih, agg_vih_data = agg_cobatest$vih, centers_data = centers)


# Save results
# Lists elements are written to individual worksheets, using list names as sheet names if available
sheets <- list("HIV_CBVCT_2_Annex1" = annex1_vih$cbvct2[Origen != 'No_data'],
               "HIV_CBVCT_3_Annex1" = annex1_vih$cbvct3[Origen != 'No_data'],
               "HIV_CBVCT_4_Annex1" = annex1_vih$cbvct4[Origen != 'No_data'],
               "HIV_CBVCT_5_Annex1" = annex1_vih$cbvct5[Origen != 'No_data'],
               "HIV_CBVCT_6_Annex1" = annex1_vih$cbvct6[Origen != 'No_data'],
               "HIV_CBVCT_7_Annex1" = annex1_vih$cbvct7[Origen != 'No_data'],
               "HIV_CBVCT_8_Annex1" = annex1_vih$cbvct8[Origen != 'No_data'],
               "HIV_CBVCT_9_Annex1" = annex1_vih$cbvct9[Origen != 'No_data'])

# write.xlsx(x = sheets, file = paste0(OUTPUT_FOLDERPATH, "vih_annex_1_table_2018_", TODAY, ".xlsx"), colNames= TRUE)

rm(sheets)


## 8.2.- ANNEX 2 TABLE ####
# ----------------------- #
# DESCRIPCIO:   Reproduim taula Annex 2 de "Report Cobatest final 2018.pdf"
#               En aquesta taula, les cel·les amb TRUE indiquen:
#
#               - Centres dades desagregades: Que el Centre no aporta les dades 
#                 necessàries per calcular l'indicador. 
#
#               - Centres dades agregades: Que el centre té tots els numeradors 
#                 per calcular indicadors (All, Male, Female, etc.) a missing. 


# Generació de la taula. 
annex2_vih <- generate_hiv_annex2_table(disagg_data = disagg_cobatest, agg_vih_data = agg_cobatest$vih, centers_data = centers)

# Save results
# write.xlsx(x = annex2_vih, file = paste0(OUTPUT_FOLDERPATH, "vih_annex_2_table_2018_", TODAY, ".xlsx"), colNames= TRUE)
rm(annex2_vih)



## 8.3.- ANNEX 3 TABLE ####
# ----------------------- #

# OJU!    Aquesta taula és la mateixa que Annex1 - CBVCT5


## 8.4.- ANNEX 6 TABLE ####
# ----------------------- #
# Annex 6: People screened for HIV (N) and Reactive Tests (n, %) by sociodemographic 
# characteristics of tester and center in the cobatest network. Segons COBATEST report 2018.

# OJU!!   Aquest procés pot durar un temps. 
if(FALSE) {
  annex6_vih <- generate_hiv_annex6_table(disagg_vih_clean_data = clean_vih_data_list$vih_clean_data, agg_vih_data = agg_cobatest$vih, centers_data = centers)
  
  # Save results
  # write.xlsx(x = annex6_vih, file = paste0(OUTPUT_FOLDERPATH, "vih_annex_6_tables_2018_", TODAY, ".xlsx"), colNames= TRUE)
}

# rm(annex6_vih)

# ___________________________________________________________________________####
# SYPHILIS                                                                   ####
# ************************************************************************** ####

#------------------------------------------------------------------------------#
# 9.- PROC SYPH DISAGG DATA                                                  ####
#------------------------------------------------------------------------------#

# Neteja de dades syphilis.
clean_syph_data_list <- cleaning_syph_data(disagg_data= disagg_cobatest, centers_data= centers)
disagg_syph <- clean_syph_data_list[["syph_clean_data"]]

# Carreguem el Flowchart VIH que hem fet durant l'execució de la funció:  "cleaning_syph_data()". 
disagg_syph_flowchart  <- clean_syph_data_list[["flowchart"]]
disagg_syph_flowchart


#------------------------------------------------------------------------------#
# 10.- FIGURES                                                               ####
#------------------------------------------------------------------------------#

## 10.1.- FIGURA 4  ####
# -------------------- #
# Figure 4: SYPH Screening (N) and Reactive Tests (%) by centre in the COBATEST Network.
agg_syph <- agg_cobatest[['syph']]

figure_4 <- generate_syph_figure_4(disagg_syph_clean = disagg_syph, agg_syph_data = agg_syph, centers_data = centers)

# GRAFICS DUAL Y. 
# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
## No s'acaben d'aliniar bé les labels X ticks
# library(ggplot2)
# ggplot(data = figure_4) +
#   geom_bar(aes(x= RealCentreName, y = as.numeric(N_syph_screened)), stat = 'identity', fill= 'firebrick2') +
#   scale_y_continuous(expand = c(0.01,0)) +
#   ggtitle("Figure 4. Syphilis Screened (N) and Reactive Test (%) by Centre in the COBATEST Network 2020") +
#   ylab('Syphilis Screened (N)') +
#   theme(axis.text.x = element_text(angle = 90, hjust= 1, vjust= 0, size = 12, face= 'bold'),
#         axis.title.x = element_blank(),
#         axis.line.x = element_blank(),
#         panel.grid = element_blank(),
#         panel.grid.major.y = element_line(color = 'black'),
#         panel.background = element_rect(colour = 'black', fill = NA))

# output
figure_4 <- figure_4[Origen != 'No_data', .(Centre, RealCentreName, N_syph_screened, N_syph_reactive, Perc_syph_reactive, Origen)]


# Save
# write.xlsx(x = figure_4, file = paste0(OUTPUT_FOLDERPATH, "syph_figure_4_2018_", TODAY, ".xlsx"), colNames= T)

rm(agg_syph)


## 10.2.- FIGURA 7  ####
# ------------------- #
# Figure 7 Number of people screned for syphilis, proportion of reactive tests and number of centres
# submitting data by year - COBATEST Network

# COMPTE, la funció que genera aquesta taula té una part manual crucial. Cal adaptar quan canviem d'any
#         sino els resultats poden no tenir sentit.

# Dades agregades SYPH
agg_syph <- agg_cobatest$syph
# COMPROBACIO:    agg_syph[, .N, by= .(center, center_name)]

# Generem Taula
figure_7 <- generate_syph_report_figure_7(disagg_syph_clean = disagg_syph, agg_syph_data = agg_syph, centers_data = centers)


# Save
# write.xlsx(x = figure_7, file = paste0(OUTPUT_FOLDERPATH, "syph_figure_7_2018_", TODAY, ".xlsx"), colNames= T)


rm(agg_syph)


# 11.- SYPH ANNEX TABLE  ####
# ------------------------- #
## ANNEX 4 TABLE  ####
# ------------------ #
# Annex 4: People screened for syphilis (N) and Reactive Tests (n, %) by centre in the cobatest network. 

# Dades agregades SYPH
agg_syph <- agg_cobatest$syph

# Generem taula.
annex_4 <- generate_syph_annex4_table(disagg_syph_clean_data = disagg_syph, agg_syph_data = agg_syph, centers_data = centers)

# output
annex_4 <- annex_4[Origen != 'No_data',]


# Save
# write.xlsx(x = annex_4, file = paste0(OUTPUT_FOLDERPATH, "syph_annex_4_2018_", TODAY, ".xlsx"), colNames= T)

rm(agg_syph)


# ___________________________________________________________________________####
# HCV                                                                        ####
# ************************************************************************** ####

#------------------------------------------------------------------------------#
# 12.- PROC HCV DISAGG DATA                                                  ####
#------------------------------------------------------------------------------#

# Neteja de dades HCV
clean_hcv_data_list <- cleaning_hcv_data(disagg_data= disagg_cobatest, centers_data= centers)
disagg_hcv <- clean_hcv_data_list[["hcv_clean_data"]]

# Carreguem el Flowchart HCV que hem fet durant l'execució de la funció:  "cleaning_data()". 
disagg_hcv_flowchart  <- clean_hcv_data_list[["flowchart"]]
disagg_hcv_flowchart


#------------------------------------------------------------------------------#
# 13.- FIGURES                                                               ####
#------------------------------------------------------------------------------#

## 13.1.- FIGURA 3 ####
# ------------------- #
# Figure 3: HCV Screening (N) and Reactive Tests (%) by centre in the COBATEST Network.
agg_hcv <- agg_cobatest[['hcv']]

figure_3 <- generate_hcv_figure_3(disagg_hcv_clean = disagg_hcv, agg_hcv_data = agg_hcv, centers_data = centers)

# GRAFICS DUAL Y. 
# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
## No s'acaben d'aliniar bé les labels X ticks
# library(ggplot2)
# ggplot(data = figure_3) +
#   geom_bar(aes(x= RealCentreName, y = as.numeric(N_hcv_screened)), stat = 'identity', fill= 'firebrick2') +
#   scale_y_continuous(expand = c(0.01,0)) +
#   ggtitle("Figure 4. HCV Screened (N) and Reactive Test (%) by Centre in the COBATEST Network 2020") +
#   ylab('HCV Screened (N)') +
#   theme(axis.text.x = element_text(angle = 90, hjust= 1, vjust= 0, size = 12, face= 'bold'),
#         axis.title.x = element_blank(),
#         axis.line.x = element_blank(),
#         panel.grid = element_blank(),
#         panel.grid.major.y = element_line(color = 'black'),
#         panel.background = element_rect(colour = 'black', fill = NA))


# Output 
figure_3 <- figure_3[Origen != "No_data"]

# Save
# write.xlsx(x = figure_3, file = paste0(OUTPUT_FOLDERPATH, "hcv_figure_3_2018_", TODAY, ".xlsx"), colNames= T)


rm(agg_hcv)


## 13.2.- FIGURA 6  ####
# ------------------- #
# Figure 6 Number of people screened for Hepatitis C, proportion of reactive tests and number of centres 
# submitting data by year - COBATEST Network

# COMPTE, la funció que genera aquesta taula té una part manual crucial. Cal adaptar quan canviem d'any
#         sino els resultats poden no tenir sentit.

# Dades agregades HCV
agg_hcv <- agg_cobatest$hcv
# COMPROBACIO:    agg_hcv[, .N, by= .(center, center_name)]

# Generem Taula
figure_6 <- generate_hcv_report_figure_6(disagg_hcv_clean = disagg_hcv, agg_hcv_data = agg_hcv, centers_data = centers)

# Save
# write.xlsx(x = figure_6, file = paste0(OUTPUT_FOLDERPATH, "hcv_figure_6_2018_", TODAY, ".xlsx"), colNames= T)

rm(agg_hcv)


# 14.- HCV ANNEX TABLE  ####
# ------------------------ #
## ANNEX 5 TABLE  ####
# ------------------ #
# Annex 5: People screened for HCV (N) and Reactive Tests (n, %) by centre in the cobatest network. 

# Dades agregades SYPH
agg_hcv <- agg_cobatest$hcv

# Generem taula.
annex_5 <- generate_hcv_annex5_table(disagg_hcv_clean_data = disagg_hcv, agg_hcv_data = agg_hcv, centers_data = centers)

# Output 
annex_5 <- annex_5[Origen != "No_data"]


# Save
# write.xlsx(x = annex_5, file = paste0(OUTPUT_FOLDERPATH, "hcv_annex_5_2018_", TODAY, ".xlsx"), colNames= T)


rm(agg_syph)
