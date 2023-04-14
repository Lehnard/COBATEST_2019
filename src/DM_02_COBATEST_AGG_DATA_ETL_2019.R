rm(list= ls())
graphics.off()
cat("\014")


#***********************************************************************************************
#
# Versió:       14-11-2023
# 
# Descripció:   Script per a la càrrega i parseig de les dades agregades COBATEST 2018. 
#
#***********************************************************************************************

library(data.table)
library(openxlsx) 
library(readxl)

source("src/functions/parseig_raw_aggr_data.R")

AGGREGATED_DATA_FOLDER <- "data/AggregadatedData/"                    # Watch files:   list.files(AGGREGATED_DATA_FOLDER)
AUXILIAR_DATA_FOLDER <- "data/Auxiliar_data/"                         # Watch files:   list.files(AUXILIAR_DATA_FOLDER)
OUTPUT_DATA_FOLDER <- "data/AggregadatedData/agg_processed_data/"     # Watch files:   list.files(OUTPUT_DATA_FOLDER)


#------------------------------------------------------------------------------#
# 1.- LOAD DATA                                                             ####
#------------------------------------------------------------------------------#

## 1.1.- Auxiliar data  ####
#--------------------------#
# Es carrega un fitxer de mapeig de noms de centres amb els seus codis i el tipus de dades que son: cobatest tool, desagregades o agregades.
center_maps <- setDT(read.xlsx(paste0(AUXILIAR_DATA_FOLDER, "Centros_v2018.xlsx"), sheet = "COBA_2018"))


## 1.2.- Aggregated data  ####
#----------------------------#
list.files(AGGREGATED_DATA_FOLDER)
{raw_agg_data <- list(vih= list(), syph= list(), hcv= list())

raw_agg_data[["vih"]][["dugaRainbow"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_Association_Rainbow.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["vih"]][["legebrita"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_Legebitra.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["vih"]][["polishNC"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_NAC_Poland.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["vih"]][["huhiv"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_HUHIVCAHIV.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["vih"]][["hera"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_HERA.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["vih"]][["cshsofia"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_chechpointSofia.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["vih"]][["czechAIDS"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_czech_aids_society.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["vih"]][["gdm"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_GDM.xlsx"), sheet= 'HIV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))

raw_agg_data[["syph"]][["dugaRainbow"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_Association_Rainbow.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["syph"]][["legebrita"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_Legebitra.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["syph"]][["polishNC"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_NAC_Poland.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["syph"]][["huhiv"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_HUHIVCAHIV.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["syph"]][["hera"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_HERA.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["syph"]][["cshsofia"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_chechpointSofia.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["syph"]][["czechAIDS"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_czech_aids_society.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["syph"]][["gdm"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_GDM.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))

raw_agg_data[["hcv"]][["dugaRainbow"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_Association_Rainbow.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["hcv"]][["legebrita"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_Legebitra.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["hcv"]][["polishNC"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_NAC_Poland.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["hcv"]][["huhiv"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_HUHIVCAHIV.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["hcv"]][["hera"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_HERA.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["hcv"]][["cshsofia"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_chechpointSofia.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["hcv"]][["czechAIDS"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_czech_aids_society.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
raw_agg_data[["hcv"]][["gdm"]] <- setDT(read.xlsx(paste0(AGGREGATED_DATA_FOLDER, "Aggregated_data_2018_v_2021_GDM.xlsx"), sheet= 'HCV Indicators', colNames = FALSE, na.strings= c("", "N/A", "NA")))
}



# View(raw_agg_data[["syph"]][["huhiv"]])
# View(raw_agg_data[["hcv"]][["genderdoM"]])


#------------------------------------------------------------------------------#
# 2.- PARSE DATA                                                            ####
#------------------------------------------------------------------------------#

## 2.1.- Parseig VIH  ####
# ---------------------- #
{
agg_vih_duga <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["dugaRainbow"]], center = 65, center_name = "DUGA & RAINBOW")
agg_vih_huhiv <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["huhiv"]], center = 66, center_name = "HUHIV")
agg_vih_legebrita <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["legebrita"]], center = 39, center_name = "Legebitra")
agg_vih_polish <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["polishNC"]], center = 63, center_name = "Polish National AIDS Center")
agg_vih_hera <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["hera"]], center = 76, center_name = "HERA Macedonia")
agg_vih_cshsof <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["cshsofia"]], center = 100, center_name = "CSH Sofia Bulgaria")
agg_vih_czech <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["czechAIDS"]], center = 61, center_name = "Czech AIDS Society")
agg_vih_gdm <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["vih"]][["gdm"]], center = 68, center_name = "Genderdoc-M Moldova")

# Row-bind tots els VIH datasets.
agg_vih_data <- Reduce(function(...) rbind(..., fill= TRUE), mget(ls(pattern = "agg_vih_")))


# JORDI (24-09-2021): El títol del CBVCT 1 està incomplet. L'arreglem.
# COMPROBACIO:   agg_data[['vih']][, unique(title)]
agg_vih_data[grepl(pattern = 'CBVCT 1:', x = agg_vih_data$title), title := 'CBVCT 1: Number of clients tested for HIV with a screening test']


### COMPROBACIO:  Revisió dels valors entrats. 
# 
# agg_vih_data[, .N, Category]                                                # OJU: All i All clients
# agg_vih_data[is.na(as.numeric(All)), .N, All]                               # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.   
# agg_vih_data[is.na(as.numeric(Males)), .N, Males]                           # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
# agg_vih_data[is.na(as.numeric(Females)), .N, Females]                       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
# agg_vih_data[is.na(as.numeric(Transgender)), .N, Transgender]               # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
# agg_vih_data[is.na(as.numeric(`<25 years old`)), .N, `<25 years old`]       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
# agg_vih_data[is.na(as.numeric(`25+ years old`)), .N, `25+ years old`]       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
# agg_vih_data[, .N, title]                                                      
# agg_vih_data[, .N, .(center, center_name)][order(as.numeric(center))]                                                      

# Uniformem categories.
agg_vih_data[Category == "All clients", Category := 'All']

rm(list= ls()[!ls() %in% c("agg_vih_data", "raw_agg_data","parseig_raw_aggr_data","get_agg_data_table","OUTPUT_DATA_FOLDER")] )
}

## 2.2.- Parseig Syphilis  ####
# --------------------------- #
{
  agg_syph_duga <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["dugaRainbow"]], center = 65, center_name = "DUGA & RAINBOW")
  agg_syph_huhiv <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["huhiv"]], center = 66, center_name = "HUHIV")
  agg_syph_legebrita <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["legebrita"]], center = 39, center_name = "Legebitra")
  agg_syph_polish <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["polishNC"]], center = 63, center_name = "Polish National AIDS Center")
  agg_syph_hera <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["hera"]], center = 76, center_name = "HERA Macedonia")
  agg_syph_cshsof <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["cshsofia"]], center = 100, center_name = "CSH Sofia Bulgaria")
  agg_syph_czech <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["czechAIDS"]], center = 61, center_name = "Czech AIDS Society")
  agg_syph_gdm <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["syph"]][["gdm"]], center = 68, center_name = "Genderdoc-M Moldova")
  
  # Row-bind tots els VIH datasets.
  agg_syph_data <- Reduce(function(...) rbind(..., fill= TRUE), mget(ls(pattern = "agg_syph_")))
  

  ### COMPROBACIO:  Revisió dels valors entrats. 
  # 
  # agg_syph_data[, .N, Category]                                                # OJU: All i All clients
  # agg_syph_data[is.na(as.numeric(All)), .N, All]                               # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.   
  # agg_syph_data[is.na(as.numeric(Males)), .N, Males]                           # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_syph_data[is.na(as.numeric(Females)), .N, Females]                       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_syph_data[is.na(as.numeric(Transgender)), .N, Transgender]               # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_syph_data[is.na(as.numeric(`<25 years old`)), .N, `<25 years old`]       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_syph_data[is.na(as.numeric(`25+ years old`)), .N, `25+ years old`]       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_syph_data[, .N, title]                                                      
  # agg_syph_data[, .N, .(center, center_name)][order(as.numeric(center))]                                                      
  
  # Uniformem categories.
  agg_syph_data[Category == "All clients", Category := 'All']
  agg_syph_data[agg_syph_data == 'NA'] <- NA
  
  rm(list= ls()[!ls() %in% c("agg_vih_data", "agg_syph_data", "raw_agg_data","parseig_raw_aggr_data","get_agg_data_table","OUTPUT_DATA_FOLDER")] )
}


## 2.3.- Parseig HCV  ####
# ---------------------- #

{
  agg_hcv_duga <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["dugaRainbow"]], center = 65, center_name = "DUGA & RAINBOW")
  agg_hcv_huhiv <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["huhiv"]], center = 66, center_name = "HUHIV")
  agg_hcv_legebrita <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["legebrita"]], center = 39, center_name = "Legebitra")
  agg_hcv_polish <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["polishNC"]], center = 63, center_name = "Polish National AIDS Center")
  agg_hcv_hera <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["hera"]], center = 76, center_name = "HERA Macedonia")
  agg_hcv_cshsof <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["cshsofia"]], center = 100, center_name = "CSH Sofia Bulgaria")
  agg_hcv_czech <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["czechAIDS"]], center = 61, center_name = "Czech AIDS Society")
  agg_hcv_gdm <- parseig_raw_aggr_data(raw_agg_dt = raw_agg_data[["hcv"]][["gdm"]], center = 68, center_name = "Genderdoc-M Moldova")
  
  
  # Row-bind tots els VIH datasets.
  agg_hcv_data <- Reduce(function(...) rbind(..., fill= TRUE), mget(ls(pattern = "agg_hcv_")))
  
  
  ### COMPROBACIO:  Revisió dels valors entrats. 
  # 
  # agg_hcv_data[, .N, Category]                                                # OJU: All i All clients
  # agg_hcv_data[is.na(as.numeric(All)), .N, All]                               # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.   
  # agg_hcv_data[is.na(as.numeric(Males)), .N, Males]                           # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_hcv_data[is.na(as.numeric(Females)), .N, Females]                       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_hcv_data[is.na(as.numeric(Transgender)), .N, Transgender]               # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_hcv_data[is.na(as.numeric(`<25 years old`)), .N, `<25 years old`]       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_hcv_data[is.na(as.numeric(`25+ years old`)), .N, `25+ years old`]       # OJU: Buscar valors no numèrics. Diferenciar NAs manuals vs NAs de tipus char.  
  # agg_hcv_data[, .N, title]                                                      
  # agg_hcv_data[, .N, .(center, center_name)][order(as.numeric(center))]                                                      
  
  # Uniformem categories.
  agg_hcv_data[Category == "All clients", Category := 'All']
  agg_hcv_data[agg_hcv_data == 'NA'] <- NA
  
  rm(list= ls()[!ls() %in% c("agg_vih_data", "agg_syph_data", "agg_hcv_data", "raw_agg_data","parseig_raw_aggr_data","get_agg_data_table","OUTPUT_DATA_FOLDER")] )
}


# COMPROBACIO:    Categories canviades o diferents a les plantilles excel. 
#
#                 Les correctes haurien de ser: MSM, SW, IDU, Migrants, All
#                 agg_data$vih[, .N, Category]
#                 agg_data$hcv[, .N, Category]
#                 agg_data$syph[, .N, Category]


#                 agg_data$vih[, .N, Category]
#                 agg_data$vih[Category == "All clients"]

if(nrow(agg_vih_data[, .N, Category][!Category %in% c('MSM', 'SW', 'IDU', 'Migrants', 'All'), ]) > 0) warning("Uniformitzeu categories de l'excel. Sino, el codi pot fallar ja que es filtra per elles.")
if(nrow(agg_syph_data[, .N, Category][!Category %in% c('MSM', 'SW', 'IDU', 'Migrants', 'All'), ]) > 0) warning("Uniformitzeu categories de l'excel. Sino, el codi pot fallar ja que es filtra per elles.")
if(nrow(agg_hcv_data[, .N, Category][!Category %in% c('MSM', 'SW', 'IDU', 'Migrants', 'All'), ]) > 0) warning("Uniformitzeu categories de l'excel. Sino, el codi pot fallar ja que es filtra per elles.")



# Passem a una única estructura.
agg_data <- list(vih= list(), syph= list(), hcv= list())
agg_data[["vih"]] <- agg_vih_data                                              
agg_data[["syph"]] <- agg_syph_data                                              
agg_data[["hcv"]] <- agg_hcv_data   


# ----------------------------------------------------------- #
# 3.- SAVE AGG DATA                                        ####
# ----------------------------------------------------------- #
TODAY <-format(Sys.Date(), "%Y%m%d")
# saveRDS(agg_data, paste0(OUTPUT_DATA_FOLDER, "agg_cobatest_2018_", TODAY,".rds"))
list.files(OUTPUT_DATA_FOLDER)

# Source file return.
rm(list= ls()[!ls() %in% "agg_data"] )
