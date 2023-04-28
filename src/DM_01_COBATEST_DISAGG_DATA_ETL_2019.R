rm(list= ls())
graphics.off()
cat("\014")


################################################################################################# #
#
# Versió:       13-04-2023
# 
# Descripció:   Script per la càrrega i el processament de les dades desagregades COBATEST. 
#
#               Readaptació del codi generat per als anys 2018-2021 per reproduir resultats. 
#
################################################################################################# #

library(data.table)
library(openxlsx) 
library(readxl)
library(haven)        # read_dta()  
library(ggplot2)
library(lubridate)    # parse_date_time2()


source("src/functions/standarize_colnames.R")
source("src/functions/add_missing_cols.R")
source("src/functions/describe.R")


DISAGGREGATED_DATA_FOLDER <- "data/DisaggragatedData/"      
AUXILIAR_DATA_FOLDER <- "data/Auxiliar_data/"                         
OUTPUT_DATA_FOLDER <-   "data/DisaggragatedData/disagg_processed_data/"

# Watch files:   list.files(DISAGGREGATED_DATA_FOLDER)
# Watch files:   list.files(AUXILIAR_DATA_FOLDER)

COLUMN_MAPINGS_FILENAME <- "COBATEST_2019_Column_Mappings.xlsx"
CENTERS_DATA_FILENAME <- "Centros_v2019.xlsx"
CENTERS_DATA_SHEET_NAME <- "COBA_2019"


TODAY <-format(Sys.Date(), "%Y%m%d")
YEAR <- 2019


# _________________________________________________________________________ ####
#------------------------------------------------------------------------------#
# 1.- LOAD DATA                                                             ####
# ************************************************************************* ####


## 1.1.- Auxiliar data  ####
#--------------------------#
# Es carrega un fitxer de mapeig de noms de columnes dels datasets desagregats amb els noms estàndards. 
col_maps <- setDT(read.xlsx(paste0(AUXILIAR_DATA_FOLDER, COLUMN_MAPINGS_FILENAME), sep.names= " "))

# Es carrega un fitxer de mapeig de noms de centres amb els seus codis i el tipus de dades que son: cobatest tool, desagregades o agregades.
center_maps <- setDT(read.xlsx(paste0(AUXILIAR_DATA_FOLDER, CENTERS_DATA_FILENAME), sheet = CENTERS_DATA_SHEET_NAME))


## 1.2.- Disaggregated data  ####
#-------------------------------#
list.files(DISAGGREGATED_DATA_FOLDER)
# Cargamos el dataset extraido de la base de datos Cobatest Tool y tambien cargamos los datasets que nos mandan en formato datos desagregados. 

disagg_cobatool <- setDT(read_dta(file = paste0(DISAGGREGATED_DATA_FOLDER, "COBATEST 2019 clean data for indicators_20230427.dta")))  ## 12964 obs. 48 vars.            
# remove labels, label and format attributes.
disagg_cobatool <- zap_formats(disagg_cobatool); disagg_cobatool <- zap_label(disagg_cobatool); disagg_cobatool <- zap_labels(disagg_cobatool)

colnames(disagg_cobatool)


# Other disaggregated here. 
disagg_czechAids <- fread(paste0(DISAGGREGATED_DATA_FOLDER, "2020-04-06_Czech AIDS help society.csv"), encoding = 'UTF-8') 
disagg_aidsfondet <- setDT(read.xlsx(paste0(DISAGGREGATED_DATA_FOLDER, "CBVCT Data COBATEST Aids Fondet_Centre06 20191.xlsx"), sep.names = " ", detectDates = TRUE ))
disagg_aidsHilfeWien <- setDT(read.xlsx(paste0(DISAGGREGATED_DATA_FOLDER, "COBA Auswertung_Jänner bis Dezember_2019_AidsHilfeWien.xlsx"),sheet = 2, sep.names = " ", detectDates = TRUE))
disagg_medicins <- fread(paste0(DISAGGREGATED_DATA_FOLDER, "COBATEST MdM 2019_medicinsdumunde.csv"), encoding = 'UTF-8') 
disagg_espacep <- setDT(read.xlsx(paste0(DISAGGREGATED_DATA_FOLDER, "COBATEST_EspacePBELGIUM_2019.xlsx"), sep.names = " ", detectDates = TRUE))
disagg_sidasol <- setDT(read.xlsx(paste0(DISAGGREGATED_DATA_FOLDER, "Données dépistage Liège 2019 Cobatest_Sidasol.xlsx"), sep.names = " ", detectDates = TRUE ))
disagg_violett <- setDT(read.xlsx(paste0(DISAGGREGATED_DATA_FOLDER, "Finaal_Cobatest_document_Violett_proc.xlsx"), sep.names = " ", detectDates = TRUE ))
disagg_exaquo <- setDT(read.xlsx(paste0(DISAGGREGATED_DATA_FOLDER, "Stats dépistage 2019_cobatest_exæquo (2).xlsx"), sep.names = " ", detectDates = TRUE ))
disagg_adhara <-  setDT(read_dta(paste0(DISAGGREGATED_DATA_FOLDER, "Adhara 2019 clean.dta"))) 
disagg_cjas <- setDT(read_dta(paste0(DISAGGREGATED_DATA_FOLDER, "CJAS 2019 clean per COBATEST.dta"))) 

## OJU!  
#   - s'ha canviat a ma el nom de variable de "Stats dépistage 2019_cobatest_exæquo (2).xlsx" : Date of "requesting the test" visit
#   - S'han tret les cometes " per evitar que R introdueixi "\" constantment. 


## COMPROBACIONS:   Qualitat cobatest tool
#
# cat(paste(colnames(disagg_cobatool), collapse = '\n'))
# disagg_cobatool[, .N, centre][order(centre)]
# summary(as.Date(disagg_cobatool$DateofVisit))
# disagg_cobatool[grepl(x = Id, pattern = 'test', ignore.case = T)]
# disagg_cobatool[grepl(x = CBVCTIdentifier, pattern = 'test', ignore.case = T)]


# LAURA: Els del CJAS que vénen al cobatest Tool s'ha de treure. Després els ajuntarem com a fitxer de dades desagregades.
# COMPROBACIO:   disagg_cobatool[centre == 8, .N]  # Numero de CJAS
disagg_cobatool <- disagg_cobatool[centre != 8, ]


# PROBES OPENTIC?
disagg_cobatool[grepl(x = CBVCTIdentifier, pattern = 'test|prova', ignore.case = T)]
disagg_cobatool <- disagg_cobatool[CBVCTIdentifier != 'PROVA OPENTIC']


## 1.3.- Process data          ####
#---------------------------------#




## 1.3.1.- Medicins du Munde          ####
#----------------------------------------#

# Tenim 3 dates de visita informades en funció del test que s'ha fet.  Comprobem-ho.
disagg_medicins[, .(Id, DateofVisit, DateofVisit_SYPH, DateofVisit_HCV, ScreeningHIVTest, SyphScreeningTestResult, HCVScreeningTestResult )]

# Unifiquem totes les visites en la variable DateofVisit
# colSums(is.na(disagg_medicins[, .(DateofVisit, DateofVisit_SYPH, DateofVisit_HCV)]))
disagg_medicins[DateofVisit == '', DateofVisit := as.character(NA)] 
disagg_medicins[DateofVisit_SYPH == '', DateofVisit_SYPH := as.character(NA)] 
disagg_medicins[DateofVisit_HCV == '', DateofVisit_HCV := as.character(NA)] 


# __< ! > WARNING < ! >______    ####
# Aquest centre és un clar exemple que mostra en les visites poden testar-se o de vih o de syph o de hcv o de combinacions d'elles. 
# Això fa que mantenir variables tipus ScreeningHIVTest = 1, 2  per syph i HCV sigui important alhora de comptar proporcions de 
# positius sobre testats.  Aqui els missings en screening test de syph i hcv son pq no s'han testat. Quan posem un 2 a screening (negatiu)
# per indicar que no s'han testat, estem agafant gent que no tocxa. 

disagg_medicins[is.na(DateofVisit) & !is.na(DateofVisit_SYPH), DateofVisit := DateofVisit_SYPH]
disagg_medicins[is.na(DateofVisit) & !is.na(DateofVisit_HCV), DateofVisit := DateofVisit_HCV]



# _________________________________________________________________________ ####
#------------------------------------------------------------------------------#
# 2.- GENERA DISAGG DATA                                                    ####
# ************************************************************************* ####

## 2.1.- Estandaritzar colnames  ####
#-----------------------------------#
# En aquesta part cal assegurar-se molt bé que els noms de les variables escrites a l'Excel siguin exactament els del dataset. 
list.files(DISAGGREGATED_DATA_FOLDER)
disagg_cobatool <- standarize_colnames(disagg_dt = disagg_cobatool, colmap_dt = col_maps, colmap_from_column = "COBATEST 2019 clean data for indicators_20230427.dta", colmap_to_column = "COBATEST_Variable_name")
disagg_czechAids <- standarize_colnames(disagg_dt = disagg_czechAids, colmap_dt = col_maps, colmap_from_column = "2020-04-06_Czech AIDS help society.csv", colmap_to_column = "COBATEST_Variable_name")
disagg_aidsfondet <- standarize_colnames(disagg_dt = disagg_aidsfondet, colmap_dt = col_maps, colmap_from_column = "CBVCT Data COBATEST Aids Fondet_Centre06 20191.xlsx", colmap_to_column = "COBATEST_Variable_name")
disagg_aidsHilfeWien <- standarize_colnames(disagg_dt = disagg_aidsHilfeWien, colmap_dt = col_maps, colmap_from_column = "COBA Auswertung_Jänner bis Dezember_2019_AidsHilfeWien.xlsx", colmap_to_column = "COBATEST_Variable_name")
disagg_medicins <- standarize_colnames(disagg_dt = disagg_medicins, colmap_dt = col_maps, colmap_from_column = "COBATEST MdM 2019_medicinsdumunde.csv", colmap_to_column = "COBATEST_Variable_name")
disagg_espacep <- standarize_colnames(disagg_dt = disagg_espacep, colmap_dt = col_maps, colmap_from_column = "COBATEST_EspacePBELGIUM_2019.xlsx", colmap_to_column = "COBATEST_Variable_name")
disagg_sidasol <- standarize_colnames(disagg_dt = disagg_sidasol, colmap_dt = col_maps, colmap_from_column = "Données dépistage Liège 2019 Cobatest_Sidasol.xlsx", colmap_to_column = "COBATEST_Variable_name")
disagg_violett <- standarize_colnames(disagg_dt = disagg_violett, colmap_dt = col_maps, colmap_from_column = "Finaal_Cobatest_document_Violett_proc.xlsx", colmap_to_column = "COBATEST_Variable_name")
disagg_exaquo <- standarize_colnames(disagg_dt = disagg_exaquo, colmap_dt = col_maps, colmap_from_column = "Stats dépistage 2019_cobatest_exæquo (2).xlsx", colmap_to_column = "COBATEST_Variable_name")
disagg_adhara <- standarize_colnames(disagg_dt = disagg_adhara, colmap_dt = col_maps, colmap_from_column = "Adhara 2019 clean.dta", colmap_to_column = "COBATEST_Variable_name")
disagg_cjas <- standarize_colnames(disagg_dt = disagg_cjas, colmap_dt = col_maps, colmap_from_column = "CJAS 2019 clean per COBATEST.dta", colmap_to_column = "COBATEST_Variable_name")


## 2.2.- Assignem codi centre  ####
#---------------------------------#
# Exploracio rapida centres cobatest tool
# disagg_cobatool[, .N, Centre]
# center_maps[Origen == 'Datos_desagregados']
disagg_czechAids[, Centre := 61]
disagg_aidsfondet[, Centre := 28]
disagg_aidsHilfeWien[, Centre := 62]
disagg_medicins[, Centre := 55]
disagg_espacep[, Centre := 56]
disagg_sidasol[, Centre := 57]
disagg_violett[, Centre := 58]
disagg_exaquo[, Centre := 59]
disagg_adhara[, Centre := 60]
disagg_cjas[, Centre := 8]


## 2.3.- Afegim missing cols      ####
# ---------------------------------- #
disagg_cobatool <- add_missing_cols(disagg_dt = disagg_cobatool, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_czechAids <- add_missing_cols(disagg_dt = disagg_czechAids, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_aidsfondet <- add_missing_cols(disagg_dt = disagg_aidsfondet, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_aidsHilfeWien <- add_missing_cols(disagg_dt = disagg_aidsHilfeWien, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_medicins <- add_missing_cols(disagg_dt = disagg_medicins, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_espacep <- add_missing_cols(disagg_dt = disagg_espacep, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_sidasol <- add_missing_cols(disagg_dt = disagg_sidasol, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_violett <- add_missing_cols(disagg_dt = disagg_violett, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_exaquo <- add_missing_cols(disagg_dt = disagg_exaquo, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_adhara <- add_missing_cols(disagg_dt = disagg_adhara, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_cjas <- add_missing_cols(disagg_dt = disagg_cjas, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")


## 2.4.- Order cols      ####
# ------------------------- #
new_order <- col_maps[, COBATEST_Variable_name]
disagg_cobatool <- disagg_cobatool[, ..new_order]
disagg_czechAids <- disagg_czechAids[, ..new_order]
disagg_aidsfondet <- disagg_aidsfondet[, ..new_order]
disagg_aidsHilfeWien <- disagg_aidsHilfeWien[, ..new_order]
disagg_medicins <- disagg_medicins[, ..new_order]
disagg_espacep <- disagg_espacep[, ..new_order]
disagg_sidasol <- disagg_sidasol[, ..new_order]
disagg_violett <- disagg_violett[, ..new_order]
disagg_exaquo <- disagg_exaquo[, ..new_order]
disagg_adhara <- disagg_adhara[, ..new_order]
disagg_cjas <- disagg_cjas[, ..new_order]
rm(new_order)



## 2.5.- Fix Date cols   ####
# ------------------------- #
# Necessitem que DateofVisit i DateofBirth estiguin en format Date per al codi. Les altres variables Date no s'utilitzen.

# COMPROBACIÓ:  Format de les dades tipus Date
#
#               # Per veure els diferents formats.
#               lapply(X = mget(ls(pattern= "disagg_")), FUN = function(x) summary(x$DateofVisit))
#
#               # Per veure els diferents formats.
#               lapply(X = mget(ls(pattern= "disagg_")), FUN = function(x) summary(x$DateOfBirth))
#
# No ha llegit en format Dates:
#
#   - DateofVisit:  disagg_czechAids, disagg_medicins, 2 NAs a disagg_exaquo
#   - DateOfBirth:  disagg_adhara (1060 NAs), disagg_aidsfondet (all NA), disagg_aidsHilfeWien (all NA), disagg_cjas (29 NAs), 
#                   disagg_cobatool (265 NAs), disagg_czechAid, disagg_exaquo, disagg_medicins, disagg_sidasol,  

# Es pot llegir amb read_xlsx de "readxl" enlloc de "openxlsx", però és més rígid i 
# implica altres problematiques. De moment settegem l'origen

# COMPROBACIO: Cap missing
# lapply(X = mget(ls(pattern= "disagg_")), FUN = function(x) sum(is.na(x$DateofVisit)))
# lapply(X = mget(ls(pattern= "disagg_")), FUN = function(x) sum(is.na(x$DateOfBirth)))

disagg_czechAids[, DateofVisit := as.Date(DateofVisit, format= '%d.%m.%Y')]
disagg_medicins[, DateofVisit := as.Date(DateofVisit, format= '%d/%m/%Y')]

disagg_czechAids[, DateOfBirth := as.Date(paste0(DateOfBirth, "-01-01"), format= '%Y-%m-%d')]
disagg_exaquo[, DateOfBirth := as.Date(DateOfBirth, format= '%d/%m/%Y')]
disagg_medicins[, DateOfBirth := as.Date(DateOfBirth, format= '%d/%m/%Y')]

# disagg_sidasol:  D'entrada no podrem extreure informació d'aquesta variable.
# disagg_sidasol[, .N, .(is_na_birthdate= is.na(DateOfBirth), AgeGroup)]



## 2.6.- All cols to chars   ####
# ----------------------------- #
# Fem aquest pas previ al Row bind pq no dongui problemes de tipus de dades. 
# Uniformem totes les variables a tipus Character.
invisible(
  mapply(FUN= function(x) x[, (colnames(x)) := lapply(.SD, function(y) as.character(y)), .SDcols= colnames(x)], 
         mget(ls(pattern = "disagg_")))
)

## 2.7.- Rowbind datasets   ####
# ---------------------------- #
# Row-bind tots els datasets.
disagg_cobatest <- Reduce(function(...) rbind(..., fill= TRUE), mget(ls(pattern = "disagg_")))


# Borra datasets "disagg_" parcials. 
rm(list = ls(pattern = "disagg_")[!ls(pattern = "disagg_") %in% c("disagg_cobatest")])


## Tots els "" a NA.
colSums(is.na(disagg_cobatest))
disagg_cobatest[disagg_cobatest == ""] <- NA



## 2.8.- Data Checks   ####
#-------------------------#

### 2.8.1- Describe   ####
#-------------------------#
# disagg_cobatest[, DateofVisit := as.Date(DateofVisit)]
# 
# 
# # Describe data summary
# colnames(disagg_cobatest)
# coba_id_vars <- c("Centre","CBVCTIdentifier","new_id","Id")                     # no posem "COBATEST_Id" pq ve tot a NAs.
# coba_categoric_vars <- c("Gender","AgeGroup","MSM","SW","PWID","Migrant",       # no posem "ResultLastHIV" pq ve tot a NAs.
#                          "EverTested","TestedLastYear","TestedLastYearSameCBVCT",
#                          "PreTestCounselling", "ScreeningHIVTest","ScreeningTestResult",
#                          "ScreeningTestResultReceived","ScreeningPostTestCounselling",
#                          "ConfirmatoryHIVTest","ConfirmatoryHIVTestResult",
#                          "ConfirmatoryTestResultReceived","LinkageToHealthCare",
#                          "HIVTestUsed","SyphEverDiagnosed","SyphEverTested",
#                          "SyphTestedLastYear","SyphTestedLastYearSameCBVCT",
#                          "SyphScreeningTestResult","SyphConfirmatoryTest",
#                          "SyphConfirmatoryTestResult","SyphTestUsed","HCVEverDiagnosed",
#                          "HCVEverTested","HCVTestedLastYear","HCVTestedLastYearSameCBVCT",
#                          "HCVScreeningTestResult","HCVRNATest")
# coba_numeric_vars <- c("AgeInYears","CD4Count")
# coba_date_vars <- c("DateofVisit")
# 
# # Summary
# describe_disagg_data <- describe(data_table = disagg_cobatest,
#                                  id_vars = coba_id_vars,
#                                  categoric_vars = coba_categoric_vars,
#                                  numeric_vars = coba_numeric_vars, 
#                                  date_vars = coba_date_vars,
#                                  n_top_cat = 10, n_top_id = 5)

# TODAY <-format(Sys.Date(), "%Y%m%d")
# write.xlsx(disagg_cobatest, paste0("out/describe_disagg_cobatest_", TODAY, ".xlsx"))


## 2.9- Fix Data    ####
#--------------------- #
# És important tenir qualitat en les variables implicades en el cálcul dels indicadors.
# Això és en aquelles variables que permeten generar el dataset agregat donat per l'excel
# de les dades desagregades:  Genere, grup de transmissió, grup d'edat +-25a, i les
# necessaries per a calcular CBVCT2, CBVCT3, ...

### 2.9.0- Ids     ####
# ------------------- #
# Intentem detectar ids més recurrents 

# 01107097400S --->  SDDMMYYYYABC             S:        Sexe
#                                             DDMMYYYY: Data
#                                             A:        Num. Germans grans
#                                             B:        Num. Germanes grans
#                                             C:        Inicial nom mare
#                           CVBCTIdentifier:  La que reporten els centres
#                           COBATEST_ID:      Automatica a partir de les dades formulari cobatest tool. 
#                           Id:               Variable treballada per la Laura en cas que sigui cobatest tool ddbb. 


## Hem detectat que Aids Fondet centre 28 cal revisar-li els Ids. 
#
# unique(disagg_cobatest[Centre == 28, .(Id, CBVCTIdentifier, new_id, COBATEST_Id)])
#
## Cal fer un identificador autonumèric.
disagg_cobatest[Centre == 28, Id := paste0('id_aidsfondet_', 10001:(10000+disagg_cobatest[Centre==28, .N]))]


## Comencem mirant quins centres no tenen les variables Id i CBVCTIdentifier informades i podem treure la info d'altres ids.

## COMPROBACIO:  Tenim Registres amb tots els ids a missing?
# disagg_cobatest[, .(N= sum(is.na(Id) & is.na(CBVCTIdentifier)), "T"= .N), Centre][order(-N)]

# colSums(is.na(disagg_cobatest[Centre == 60, .(Id, CBVCTIdentifier, new_id, COBATEST_Id)]))
# Adhara té els ids sembla que en la variable COBATEST_Id.
disagg_cobatest[Centre == 60, `:=`(Id= paste0('id_adhara_', COBATEST_Id), 
                                   CBVCTIdentifier= paste0('id_adhara_', COBATEST_Id))]
disagg_cobatest[Centre == 60 & is.na(COBATEST_Id), `:=`(Id= paste0('id_adhara_', 1:disagg_cobatest[Centre==60& is.na(COBATEST_Id), .N]), 
                                                        CBVCTIdentifier= paste0('id_adhara_', 1:disagg_cobatest[Centre==60& is.na(COBATEST_Id), .N]))]

# colSums(is.na(disagg_cobatest[Centre == 56, .(Id, CBVCTIdentifier, new_id, COBATEST_Id)]))
# EspaceP té els ids a new_id
disagg_cobatest[Centre == 56, `:=`(Id= paste0('id_espacep_', new_id), 
                                   CBVCTIdentifier= paste0('id_espacep_', new_id))]

# colSums(is.na(disagg_cobatest[Centre == 59, .(Id, CBVCTIdentifier, new_id, COBATEST_Id)]))
# No té ids, fem un autonumèric.
disagg_cobatest[Centre == 59, `:=`(Id= paste0('id_exaequo_', 1:disagg_cobatest[Centre==59, .N]), 
                                   CBVCTIdentifier= paste0('id_exaequo_', 1:disagg_cobatest[Centre==59, .N]))]

# colSums(is.na(disagg_cobatest[Centre == 8, .(Id, CBVCTIdentifier, new_id, COBATEST_Id)]))
# disagg_cobatest[Centre == 8 & is.na(Id), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id)]
disagg_cobatest[Centre == 8 & is.na(Id), Id := paste0('id_cjas_', 101:(100+disagg_cobatest[Centre==8 & is.na(Id), .N]))]


# colSums(is.na(disagg_cobatest[Centre == 57, .(Id, CBVCTIdentifier, new_id, COBATEST_Id)]))
# disagg_cobatest[Centre == 57 & is.na(Id), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id)]
# disagg_cobatest[Centre == 57 & is.na(Id) & is.na(CBVCTIdentifier), ]
disagg_cobatest[Centre == 57 & is.na(Id), Id := CBVCTIdentifier]
disagg_cobatest[Centre == 57 & is.na(Id), Id := paste0('id_sidasol_', 101:(100+disagg_cobatest[Centre==57 & is.na(Id), .N]))]


# disagg_cobatest[Centre == 47 & is.na(Id), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id)]
disagg_cobatest[Centre == 47 & is.na(CBVCTIdentifier) & !is.na(COBATEST_Id), CBVCTIdentifier := COBATEST_Id]
disagg_cobatest[Centre == 47 & is.na(Id), Id := CBVCTIdentifier]
disagg_cobatest[Centre == 47 & is.na(Id), Id := paste0('id_platef_', 101:(100+disagg_cobatest[Centre==47 & is.na(Id), .N]))]

# Cal desambiguar ids comunes de persones diferents
disagg_cobatest[Centre == 47, .N, Id][N > 1][order(-N)]
disagg_cobatest[Centre == 47 & Id == 'PPSIDA', Id := paste0('id_platef_', 201:(200+disagg_cobatest[Centre==47 & Id == 'PPSIDA', .N]))]
disagg_cobatest[Centre == 47 & Id == 'XXXXXXXXXXXXM', Id := paste0('id_platef_', 301:(300+disagg_cobatest[Centre==47 & Id == 'XXXXXXXXXXXXM', .N]))]

# disagg_cobatest[Centre == 46 & is.na(Id), ]
disagg_cobatest[Centre == 46 & is.na(Id), `:=`(Id= paste0('id_marolles_', 1:disagg_cobatest[Centre==46 & is.na(Id), .N]), 
                                               CBVCTIdentifier= paste0('id_marolles_', 1:disagg_cobatest[Centre==46 & is.na(Id), .N]))]

# Cal desambiguar ids comunes de persones diferents
disagg_cobatest[Centre == 46, .N, Id][order(-N)]
disagg_cobatest[Centre == 46 & Id == 'XXXXXXXXXXXX']

disagg_cobatest[Centre == 46 & Id == 'XXXXXXXXXXXX' & DateOfBirth == as.Date('1995-01-01'), Id := 'XXXXXXXXXXXX_JA1']
disagg_cobatest[Centre == 46 & Id == 'XXXXXXXXXXXX' & DateOfBirth == as.Date('1990-01-01'), Id := 'XXXXXXXXXXXX_JA2']
disagg_cobatest[Centre == 46 & Id == 'XXXXXXXXXXXX' & DateOfBirth == as.Date('1970-08-14'), Id := 'XXXXXXXXXXXX_JA3']
disagg_cobatest[Centre == 46 & Id == 'XXXXXXXXXXXX' & DateofVisit == as.Date('2019-05-08'), Id := 'XXXXXXXXXXXX_JA4']
disagg_cobatest[Centre == 46 & Id == 'XXXXXXXXXXXX' & DateofVisit == as.Date('2019-08-01'), Id := 'XXXXXXXXXXXX_JA5']
disagg_cobatest[Centre == 46 & Id == 'XXXXXXXXXXXX' & DateofVisit == as.Date('2019-04-25'), Id := 'XXXXXXXXXXXX_JA6']



# disagg_cobatest[Centre == 48 & is.na(Id), ]
disagg_cobatest[Centre == 48 & is.na(Id), `:=`(Id= paste0('id_sips_', 1:disagg_cobatest[Centre==48 & is.na(Id), .N]), 
                                               CBVCTIdentifier= paste0('id_sips_', 1:disagg_cobatest[Centre==48 & is.na(Id), .N]))]

# Ara mirem de completar Id := CBVCTIDentifier i viceversa.  Les variables CBVCTIdentifier i Id no poden tenir missings. 
# disagg_cobatest[is.na(Id) & !is.na(CBVCTIdentifier), .N, Centre]
disagg_cobatest[is.na(Id) & !is.na(CBVCTIdentifier), Id := CBVCTIdentifier]

# disagg_cobatest[!is.na(Id) & is.na(CBVCTIdentifier), .N, Centre]
disagg_cobatest[!is.na(Id) & is.na(CBVCTIdentifier), CBVCTIdentifier := Id]

if(any(disagg_cobatest[, .(N= sum(is.na(Id) & is.na(CBVCTIdentifier)), "T"= .N), Centre][, N] > 0)) warning("Hi ha Ids Missing.")


## Continuem mirant si hi ha ids iguals a través de centres diferents. Això pot influir en la imputació per Id locf.
# disagg_cobatest[, uniqueN(Centre), .(Id)][V1 > 1][order(-V1)]  ## 69 a revisar.
#
# El que farem serà gestionar-ho afegint Centre al locf.


## TOY example
# dt <- rbind(data.table(A= rep("a001", 10), B= c(rep("c1", 4), rep("c2", 2), rep("c1", 3), "c1")), 
#             data.table(A= rep("a002", 5), B= c(rep("c3", 4), "c1")))
# aux <- unique(dt[, .(A, B)])
# aux[, id_suffix := paste0(A, "_", 1:.N), by= .(A)]

disagg_cobatest[Id %in% disagg_cobatest[, uniqueN(Centre), .(Id)][V1 > 1, Id], order := 1:.N, by= .(Id, Centre)]


## COMPROBACIO: Podem arreglar Ids?
# dt <- disagg_cobatest[, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, Gender, DateOfBirth)]
# dt[, nchar := nchar(Id)]
# dt[, has_cobaid_Id := grepl(x = Id, pattern = "[0-9]{11}[A-Za-z]")]
# dt[, has_cobaid_CBVCTIdentifier := grepl(x = CBVCTIdentifier, pattern = "[0-9]{11}[A-Za-z]")]
# dt[, has_cobaid_COBATEST_Id := grepl(x = COBATEST_Id, pattern = "[0-9]{11}[A-Za-z]")]
# dt[, .N, .(has_cobaid_Id, has_cobaid_CBVCTIdentifier, has_cobaid_COBATEST_Id)][order(has_cobaid_Id, has_cobaid_CBVCTIdentifier, has_cobaid_COBATEST_Id)]
# dt[has_cobaid_Id == F & has_cobaid_CBVCTIdentifier == F & has_cobaid_COBATEST_Id == F, .N, nchar][order(nchar)]
#    has_cobaid_Id has_cobaid_CBVCTIdentifier has_cobaid_COBATEST_Id    N
# 1:         FALSE                      FALSE                  FALSE 5616
# 2:         FALSE                      FALSE                   TRUE  329      <----- No imputem desde COBATEST_Id, tret de casos molt particulars si no hi ha CBVCTIdentifier 
# 3:         FALSE                       TRUE                  FALSE  718      <----- imputem desde CBVCTIdentifier
# 4:          TRUE                      FALSE                   TRUE 3314      
# 5:          TRUE                       TRUE                  FALSE    4
# 6:          TRUE                       TRUE                   TRUE 3465



# Imputem. NO UTILITZAR COBATEST_Id per imputar massivament. Es construeix amb el formulari i ja porta la informació
#          que voldriem extreure posteriorment en les variables Gender i Dateofbirth. 
# disagg_cobatest[grepl(x = Id, pattern = "[0-9]{11}[A-Za-z]") == F & grepl(x = CBVCTIdentifier, pattern = "[0-9]{11}[A-Za-z]") == T, .(Id, CBVCTIdentifier)]
disagg_cobatest[grepl(x = Id, pattern = "[0-9]{11}[A-Za-z]") == F 
                & grepl(x = CBVCTIdentifier, pattern = "[0-9]{11}[A-Za-z]") == T, 
                Id := CBVCTIdentifier]


## COMPROBACIO:
#
# disagg_cobatest[grepl(x = Id, pattern= '^000'), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, Gender, DateOfBirth)]
disagg_cobatest[grepl(x = Id, pattern = '^00000') == T 
                & grepl(x = CBVCTIdentifier, pattern = "[0-9]{11}[A-Za-z]") == T, 
                Id := CBVCTIdentifier]


## Del centre 47 cal desambiguar els ids repetits, ja que en la imputació de missings per locf estarem alterant registres
disagg_cobatest[Centre == 47, .N, Id][N > 1][order(-N)]
disagg_cobatest[Centre == 47 & Id == 'XXXXXXXXXXXXF' & SW == 2, Id := "XXXXXXXXXXXXF_2"]
disagg_cobatest[Centre == 47 & Id == 'AAAAMMDDXXXXM' & MSM == 2, Id := "AAAAMMDDXXXXM_2"]


## Ha quedat algú sense Id?
if(disagg_cobatest[, sum(is.na(Id))] > 0 ) {stop("Hi ha registres sense Id.")}
# disagg_cobatest[, sum(is.na(Id)), Centre][V1 > 0]
# disagg_cobatest[is.na(Id), .(Centre, Id, CBVCTIdentifier, new_id, COBATEST_Id)]


### 2.9.1- Gender  ####
# ------------------- #
# El gender a cobatest ha de ser:   1 male, 2 female, 3 transgender 

## COMPROBACIO
# disagg_cobatest[, .N, Gender][order(Gender)]
# disagg_cobatest[is.na(Gender), .(Centre, CBVCTIdentifier, COBATEST_Id, Id, Gender)]
#
## COMPROBACIO   Repetidors informats
# disagg_cobatest[Id %in% disagg_cobatest[is.na(Gender), unique(Id)], uniqueN(Gender), Id][V1 > 1]
# disagg_cobatest[Id == '00304198600F']

# Imputem gènere. dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, Gender, na.last = T)
disagg_cobatest[!is.na(Id), Gender := nafill(x = as.numeric(Gender), type = 'locf'), by= .(Centre, Id)]

# Imputem gènere.
disagg_cobatest[!Gender %in% 1:3, Gender := as.numeric(NA)]




### 2.9.2.- AgeInYears  ####  
# ------------------------ #
# Aquesta variable serveix bàsicament per a descartar els menors de 16 anys. També ens pot servir per a imputar el AgeGroup. 

## COMPROBACIÓ:   Estat de la variable.
#
# print(disagg_cobatest[, .N, as.numeric(AgeInYears)][order(-N)], topn = 200)   
#                                                                                  
# disagg_cobatest[is.na(as.numeric(AgeInYears)), .(Id, AgeInYears)]
# disagg_cobatest[is.na(AgeInYears), .N]
# disagg_cobatest[, .N, by= .(age= floor(as.numeric(AgeInYears)))][order(age)]
#
#
# Tenim valors < 16 anys
# Tenim molts valors a 0
# Tenim valors a -1
# Tenim valors superiors a 100 anys
# Tenim Missings.


# Fem variable auxiliar AgeInYearsNum per a corregir la variable. 
disagg_cobatest[, AgeInYearsNum := as.numeric(AgeInYears)]
disagg_cobatest[, AgeInYearsNum := floor(AgeInYearsNum)]      # Treiem valors decimals

## COMPROBACIO   Repetidors informats
# disagg_cobatest[Id %in% disagg_cobatest[is.na(AgeInYearsNum), unique(Id)], uniqueN(AgeInYearsNum), .(Centre, Id)][V1 > 1] # 9
disagg_cobatest[Id == 'ZV9922']

# Imputem gènere. dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, AgeInYearsNum, na.last = T)
disagg_cobatest[!is.na(Id), AgeInYearsNum := nafill(x = as.numeric(AgeInYearsNum), type = 'locf'), by= .(Centre, Id)]

# Passem a date
disagg_cobatest[is.na(DateOfBirth), .N, .(Centre)]
disagg_cobatest[, DateOfBirth := as.Date(DateOfBirth)]

## COMPROBACIÓ:   Distribució d'edats
#
#  disagg_cobatest[, .N, AgeInYearsNum][order(AgeInYearsNum)]
#
## Tenim:    ("Clean data 2017 COBATEST for indicators.dta")
#
#  1.- Individus amb edat < 0.             ##  244 obs.
#  2.- Individus amb edat 0.               ##  131 obs.
#  3.- Alguns individus amb 1 - 16 anys.   ##   77 obs.
#  4.- Individus de més de 100 anys.       ##   14 obs. 
#  5.- Individus amb edat Missing.         ## 8567 obs.       

# (30-08-2022 -- LAURA F)   Si s'ha d'imputar a partir dels identificadors:  01107097400S --->  SDDMMYYYYABC
#                                                                                               S:        Sexe
#                                                                                               DDMMYYYY: Data
#                                                                                               A:        Num. Germans grans
#                                                                                               B:        Num. Germanes grans
#                                                                                               C:        Inicial nom mare
#                           CVBCTIdentifier:  La que reporten els centres
#                           COBATEST_ID:      Automatica a partir de les dades formulari cobatest tool. 
#                           Id:               Variable treballada per la Laura en cas que sigui cobatest tool ddbb. 
#
#                           Es recomana agafar la informació per jerarquia de confiança:  Id > CBVCTIdentifier > COBATEST_ID


### Edat NAs  ####
# -------------- #

## COMPROBACIO:    disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]    # 8566
#
# dt <- disagg_cobatest[is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id, DateOfBirth, AgeInYearsNum)]
# dt[, nchar_id := nchar(Id)]
# dt[, .N, nchar_id][order(nchar_id)]

# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= Id, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]


# Pas zero: 
# Probem d'imputar a partir de DateofBirth. 
# disagg_cobatest[is.na(AgeInYearsNum) & !is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]    # 8566
#
# NO podem.


# Primer: 
# Imputem ageYearsNum pels AgeYearsNum == NA amb CBVCTIdentifier o Id tipus SDDMMYYYYABC:   [0-9]{11}[A-Z].
#
# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]   
# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= Id, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]   
disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), DateOfBirth := as.Date(substr(x = CBVCTIdentifier, start = 2, stop = 9), format= "%d%m%Y")]  

# Algun que no s'hagi fet?
# disagg_cobatest[is.na(AgeInYearsNum)
#                 & (grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]')
#                 | grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]')), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]

disagg_cobatest[CBVCTIdentifier == "03111198800J", DateOfBirth := as.Date("1988-11-30")]



# Segon: 
# Imputem ageYearsNum pels Ids de menys de 6 caracters. 
#
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & nchar(Id) <= 6, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & nchar(Id) <= 6, .N, Centre][order(as.numeric(Centre))]
#
# sel_centre <- c(4,5,8,11,13,19,26,32,37,42,43)    ## Cap conté dates
# sel_centre <- 57                                  ## Cap conté dates
# sel_centre <- 62                                  ## Cap conté dates
# dt <- disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & nchar(Id) <= 6 & Centre %in% sel_centre, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
# 

## Tercer: 
# Imputem ageYearsNum pels Ids de forma "^[A-Z]{x}[0-9]{6}$" amb x = 1, 2, 3... caracters 
#
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1,5}[0-9]{6}$"), .(nchar= nchar(Id), Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{6}$"), .(nchar= nchar(Id), Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{6}$"),  DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 2, stop = 7), orders = 'dmy', cutoff_2000 = 20L))]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{2}[0-9]{6}$"),  DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 3, stop = 8), orders = 'ymd', cutoff_2000 = 20L))]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{3}[0-9]{6}$"),  DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 4, stop = 9), orders = 'ymd', cutoff_2000 = 20L))]


# Els que per la imputació anterior han donat error.
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1,5}[0-9]{6}$"), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum, AgeGroup)]


## Quart: 
# Imputem ageYearsNum pels Ids de forma "^[A-Z]{x}[0-9]{8}$" amb x = 1, 2, 3... caracters 
#
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1,5}[0-9]{8}$"), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{8}$"),  DateOfBirth := as.Date(substr(x = Id, start = 2, stop = 9), format= '%d%m%Y')]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{2}[0-9]{8}$"),  DateOfBirth := as.Date(substr(x = Id, start = 3, stop = 10), format= '%d%m%Y')]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{3}[0-9]{8}$"),  DateOfBirth := as.Date(substr(x = Id, start = 4, stop = 11), format= '%d%m%Y')]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{4}[0-9]{8}$"),  DateOfBirth := as.Date(substr(x = Id, start = 5, stop = 12), format= '%d%m%Y')]


## Cinquè:
# El centre 47 té uns Ids particulars ben formatats. 

# disagg_cobatest[Centre == 47, .N, is.na(DateOfBirth)]
# disagg_cobatest[Centre == 47 & is.na(DateOfBirth), .N, grepl(x = Id, pattern = "^[0-9]{8}")]
# disagg_cobatest[Centre == 49 & is.na(DateOfBirth), .N, grepl(x = Id, pattern = "^[0-9]{8}")]
disagg_cobatest[Centre %in% c(47,49) & is.na(DateOfBirth) &grepl(x = Id, pattern = "^[0-9]{8}"), DateOfBirth := as.Date(substr(x = Id, start = 1, stop = 8), format= '%Y%m%d')]

## COMPROBEM: 
# disagg_cobatest[Centre == 47, summary(DateOfBirth)]
disagg_cobatest[CBVCTIdentifier == '18750615MCACM', DateOfBirth := as.Date('1975-06-15')]  

## COMPROBEM: Quins han quedat buits
# disagg_cobatest[Centre == 47 & is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, DateOfBirth)]
disagg_cobatest[CBVCTIdentifier == '1970XXXXXXXXF', DateOfBirth := as.Date('1970-06-15')]  


## Sisè: 
# Imputem els restants a mà. Més que la data exacta, és important determinar l'any.
#
# dt <- disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & nchar(Id) > 6, .(Centre, ncharId= nchar(Id), Id, nchar(CBVCTIdentifier), CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
# dt[, .N, .(Centre, ncharId)]

disagg_cobatest[CBVCTIdentifier == '01021981', DateOfBirth := as.Date('1981-02-01')]  
disagg_cobatest[CBVCTIdentifier == '020529GC', DateOfBirth := as.Date('2002-05-29')]  
disagg_cobatest[CBVCTIdentifier == '900731FG', DateOfBirth := as.Date('1990-07-31')]  
disagg_cobatest[CBVCTIdentifier == '14081994', DateOfBirth := as.Date('1994-08-14')]  
disagg_cobatest[CBVCTIdentifier == '25091991', DateOfBirth := as.Date('1991-09-25')]  
disagg_cobatest[CBVCTIdentifier == '1B25012002', DateOfBirth := as.Date('2002-01-25')]  
disagg_cobatest[CBVCTIdentifier == '1AO19011979', DateOfBirth := as.Date('1979-01-19')]  
disagg_cobatest[CBVCTIdentifier == '1PN04011982', DateOfBirth := as.Date('1982-01-04')]  
disagg_cobatest[CBVCTIdentifier == '1DNS15091974', DateOfBirth := as.Date('1974-09-15')]  
disagg_cobatest[CBVCTIdentifier == '23041971TINO', DateOfBirth := as.Date('1971-04-23')]  
disagg_cobatest[CBVCTIdentifier == 'DMS 0 07101984', DateOfBirth := as.Date('1984-11-04')]  
disagg_cobatest[CBVCTIdentifier == '2 GORI 19981104', DateOfBirth := as.Date('1998-11-04')]  

## COMPROBACIO:  El centre 57
# dt <- disagg_cobatest[Centre == 57 & is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id, DateOfBirth, AgeInYears, AgeInYearsNum, AgeGroup)]



## Últim, revisió i correcció de dates fora de rang
# summary(disagg_cobatest$DateOfBirth)
# disagg_cobatest[DateOfBirth < as.Date("1920-01-01")]

# Anem a corregir uns que hem comprovat a les dades originals
disagg_cobatest[Centre == 61 & Id == 'MT44', DateOfBirth := as.Date('1973-01-01')]  
disagg_cobatest[Centre == 61 & Id == '58952', `:=`(DateOfBirth=  as.Date(NA), AgeInYearsNum = NA)]  
disagg_cobatest[Centre == 61 & Id == '59106', DateOfBirth := as.Date('1958-01-01')]  
disagg_cobatest[Centre == 61 & Id == 'MT1281', DateOfBirth := as.Date('1963-01-01')]  
disagg_cobatest[Centre == 61 & Id == 'MT721', `:=`(DateOfBirth=  as.Date(NA), AgeInYearsNum = NA)]  
disagg_cobatest[Centre == 61 & Id == 'MT1605', DateOfBirth := as.Date('1982-01-01')]  
disagg_cobatest[Centre == 61 & Id == 'MT1804', DateOfBirth := as.Date('1998-01-01')]  
disagg_cobatest[Centre == 61 & Id == 'BR14', `:=`(DateOfBirth=  as.Date("1994-01-01"), AgeInYearsNum = 2019-1994)]  


## COMPROBACIO: Quants podrem imputar:
disagg_cobatest[is.na(AgeInYearsNum) & !is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]    # 131


# Imputem Age in Years.
# disagg_cobatest[is.na(AgeInYearsNum) & !is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYears, AgeInYearsNum)]  ## 150 obs.
disagg_cobatest[is.na(AgeInYearsNum) & !is.na(DateOfBirth), AgeInYearsNum := YEAR - year(DateOfBirth)]

## Setè: Hi ha registres assignats a una única persona en que tingui escrita la data de naixement o l'edat en algun d'aquests?
# disagg_cobatest[is.na(AgeInYearsNum), .N, .(Centre, Id)][N > 1][order(-N)]
# disagg_cobatest[Id %in% disagg_cobatest[is.na(AgeInYearsNum), .N, .(Centre, Id)][N > 1][, Id], .(Centre, Id, CBVCTIdentifier, COBATEST_Id)]

## COMPROBACIO:  Qui ha quedat amb edat a missing?   
#
# disagg_cobatest[is.na(AgeInYearsNum), .(Id, CBVCTIdentifier, DateOfBirth, AgeInYears, AgeInYearsNum)]
disagg_cobatest[is.na(AgeInYearsNum), uniqueN(Id), by= Centre][order(-V1)]
#
# Centre 62   Aids Hilfe Wien   No informa i el id és autonumeric. No tenim aquesta informació. 
# Centre 60   Adhara            No informa totes les dates de naixement i no té grup edat ni edat. Id autonumèric i missings. 
# Centre 57   Sidasol           360 buides que no tenen id per extraure info. 
#
# 
# dt <- unique(disagg_cobatest[is.na(AgeInYearsNum) & !Centre %in% c(57, 60, 62), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYears, AgeInYearsNum)])



# ids <- disagg_cobatest[is.na(AgeInYearsNum), unique(Id)]
# disagg_cobatest[Id %in% ids, .N, Id]
# disagg_cobatest[Id %in% ids & !is.na(Id), .(Id, CBVCTIdentifier, DateOfBirth, AgeInYears)][order(Id, CBVCTIdentifier)]



### Edat <= 0   ####
# ---------------- #

## COMPROBACIO:   disagg_cobatest[AgeInYearsNum <= 0, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]   ## 355 obs. 
#
#
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum <= 0, .N, year(DateOfBirth)]  ## any naix: 9999(2), 2020(2), 2019(371), 2018(1)
#

# Pels de 9999 
# imputem 24 anys pq tenim info que son del grup 1
# disagg_cobatest[year(DateOfBirth) == 9999, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id, DateOfBirth, AgeGroup, AgeInYears, AgeInYearsNum)]
disagg_cobatest[year(DateOfBirth) == 9999, AgeInYearsNum := 24]

# Pels de 2020 
# disagg_cobatest[year(DateOfBirth) == 2020, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id, DateOfBirth, AgeGroup, AgeInYears, AgeInYearsNum)]
disagg_cobatest[Centre == 32 & Id == '11501202014E', `:=`(DateOfBirth=  as.Date("2001-02-15"), AgeInYearsNum = 2019-2001)]  
disagg_cobatest[Centre == 1 & Id == '021091994009', `:=`(DateOfBirth=  as.Date("1994-09-21"), AgeInYearsNum = 2019-1994)]  

# Pels de 2018 
# disagg_cobatest[year(DateOfBirth) == 2018, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id, DateOfBirth, AgeGroup, AgeInYears, AgeInYearsNum)]

# Pels de 2019
# dt <- disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id, DateOfBirth, AgeGroup, AgeInYears, AgeInYearsNum)]
# dt[, .N, Centre]  # la majoria son cjas (224).
disagg_cobatest[Centre == 8 & year(DateOfBirth) == 2019 & grepl(x = Id, pattern = '^[0-9]+'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 1, stop = 6), orders = 'ymd', cutoff_2000 = 20L))]
disagg_cobatest[Centre == 8 & year(DateOfBirth) == 2019 & grepl(x = Id, pattern = '^cjas[0-9]+', ignore.case = T), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 1, stop = 6), orders = 'ymd', cutoff_2000 = 20L))]

# imputem ageYearsNum pels AgeYearsNum <= 0 amb CBVCTIdentifier tipus SDDMMYYYYABC:   [0-9]{11}[A-Z].
# disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
# disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '[0-9]{12}[A-Z]'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), DateOfBirth := as.Date(substr(x = CBVCTIdentifier, start = 2, stop = 9), format= "%d%m%Y")]  

# imputem ageYearsNum pels AgeYearsNum <= 0 amb CBVCTIdentifier tipus ^[A-Z]{2,3}[0-9]{8}$
# disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2,3}[0-9]{8}$'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{3}[0-9]{8}$'), DateOfBirth := as.Date(substr(x = CBVCTIdentifier, start = 4, stop = 11), format= "%d%m%Y")]  

# imputem ageYearsNum pels AgeYearsNum <= 0 amb CBVCTIdentifier tipus ^[A-Z]{2,3}[0-9]{6}$
# disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2,3}[0-9]{6}$'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2}[0-9]{6}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 3, stop = 8), orders = 'ymd', cutoff_2000 = 20L))]  
disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{3}[0-9]{6}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 4, stop = 9), orders = 'ymd', cutoff_2000 = 20L))]  

# imputem ageYearsNum pels AgeYearsNum <= 0 amb CBVCTIdentifier tipus ^[0-9]{8}[A-Z]+$
# disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[0-9]{8}[A-Z]+$'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[0-9]{8}[A-Z]+$'), DateOfBirth := as.Date(substr(x = CBVCTIdentifier, start = 1, stop = 8), format= "%Y%m%d")]

# imputem ageYearsNum pels AgeYearsNum <= 0 amb COBATEST_ID tipus ^[0-9]{3}$   121-11-1991
# disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[0-9]{3}'), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum <= 0 & year(DateOfBirth) == 2019 & grepl(x= CBVCTIdentifier, pattern = '^[0-9]{3}'), DateOfBirth := as.Date(gsub(x = substr(x = COBATEST_Id, start = 2, stop = 11), pattern = "-", replacement = ""), format= "%Y%m%d")]


# Arreglem a mà. Vigilem que no hi hagi duplicats. 
# disagg_cobatest[Id == 'APO181093', ]
disagg_cobatest[Id == 'APO181093',  DateOfBirth := as.Date('1993-10-18', format= '%Y-%m-%d')]


# Imputem Age in Years.
disagg_cobatest[AgeInYearsNum <= 0, AgeInYearsNum := YEAR - year(DateOfBirth)]


## COMPROBACIO:   Ha quedat algun?   
#
# disagg_cobatest[AgeInYearsNum <= 0, .(Centre, Id, CBVCTIdentifier, new_id, COBATEST_Id, DateOfBirth, AgeInYearsNum)]    
#


### Edat [1, 15]  ####
# Nascuts entre 2004-2018
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16, .N, AgeInYearsNum]   ## 
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16, .(Centre, Id, CBVCTIdentifier, new_id, COBATEST_Id, DateOfBirth, AgeInYearsNum)][order(AgeInYearsNum)]
disagg_cobatest[Id == '02408199900M',  DateOfBirth := as.Date('1999-08-24', format= '%Y-%m-%d')]

# Imputem Age in Years.
# disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16, .(Centre, Id, CBVCTIdentifier, new_id, COBATEST_Id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16 & !is.na(DateOfBirth), AgeInYearsNum := YEAR - year(DateOfBirth)]


### Edat > 100  ####
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 100, .N, AgeInYearsNum][order(AgeInYearsNum)]                              
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 100, .(Centre, Id, CBVCTIdentifier, new_id, COBATEST_Id, DateOfBirth, AgeInYearsNum)]    
# disagg_cobatest[AgeInYearsNum == 999, .N, Centre]   ## Tots son del mateix centre i té pinta que són els missings
disagg_cobatest[AgeInYearsNum == 999, AgeInYearsNum := as.numeric(NA)]

disagg_cobatest[Id == '10110199001M',  DateOfBirth := as.Date('1990-10-01', format= '%Y-%m-%d')]
disagg_cobatest[Id == 'id_exaequo_130',  DateOfBirth := as.Date(NA, format= '%Y-%m-%d')]

# Imputem Age in Years.
disagg_cobatest[AgeInYearsNum > 100, AgeInYearsNum := YEAR - year(DateOfBirth)]


## VALIDACIO FINAL
# disagg_cobatest[, .N, AgeInYearsNum][order(AgeInYearsNum)]
# disagg_cobatest[is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, AgeInYearsNum)]
# disagg_cobatest[AgeInYearsNum < 0, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, AgeInYearsNum)]
# disagg_cobatest[AgeInYearsNum == 0, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, AgeInYearsNum)]
# disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, AgeInYearsNum)]
# disagg_cobatest[AgeInYearsNum > 90 , .(Centre, Id, CBVCTIdentifier, COBATEST_Id, AgeInYearsNum, AgeGroup, DateOfBirth)]



### 2.9.3.- AgeGroup  ####  
# ---------------------- #
# A cobatest, AgeGroup ha de venir definit com:  1: <25years old, 2: 25+ years old
disagg_cobatest[, .N, AgeGroup]

# Només ha de tenir 2 categories.
disagg_cobatest[!AgeGroup %in% 1:2, AgeGroup := NA]

# Podem imputar AgeGroup a partir de "DateOfBirth", "AgeInYears", "CBVCTIdentifier", "COBATEST_Id" o "Id".

## COMPROBACIO:   
# Abans hem treballat exhaustivament la variable AgeInYearsNum a partir dels identificadors de manera que podem utilitzar-la. 
#
# disagg_cobatest[, .N, .(AgeGroup, is_NA_age= is.na(AgeInYearsNum))][order(AgeGroup, is_NA_age)]
# disagg_cobatest[!is.na(AgeGroup) & is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, new_id, COBATEST_Id, AgeInYearsNum)]

# disagg_cobatest[AgeGroup == 1 & is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, AgeInYearsNum)]

# Podrem recuperar els AgeGroup == NA i amb AgeInYearsNum corregida de AgeInYears.
disagg_cobatest[is.na(AgeGroup) & !is.na(AgeInYearsNum), AgeGroup := fifelse(test = AgeInYearsNum >= 25, yes = '2', no = '1')]


## COMPROBACIO: Explorem més a fons els AgeGroup == NA i AgeInYearsNum == NA
# disagg_cobatest[is.na(AgeGroup) & is.na(AgeInYearsNum), .N, Centre]
# disagg_cobatest[is.na(AgeGroup) & is.na(AgeInYearsNum), .(AgeGroup, AgeInYearsNum, Centre, Id, CBVCTIdentifier, DateOfBirth)]


### 2.9.4.- MSM       ####  
# ---------------------- #
# MSM: 1 yes, 2 no

## COMPROBACIO:  Estat de la variable
# disagg_cobatest[, .N, MSM][order(as.numeric(MSM))]
# disagg_cobatest[!MSM %in% c('1','2'), .N, .(Centre, MSM)][order(as.numeric(Centre))]

# Recategoritzem. 
disagg_cobatest[!MSM %in% c('1', '2'), MSM := as.character(NA)]


## TOY EXAMPLE:   Imputació de Missings quan tenim ids repetits amb algun d'ells informat. 
# dt <- data.table(V1= c("A", "A", "A", "B", "B", "C", "D", "D", "D", "E", "E", "F", NA, NA, NA),
#                  V2= as.character(c(NA, 1, 1, 2, NA, NA, 2, NA, 2, 1, 1, NA, 1, NA, NA)), 
#                  V3= 1:15)
# set.seed(1234)
# dt <- dt[sample(x = 1:nrow(dt), size = nrow(dt))]
# dt
# 
# setorder(x = dt, V1, V2, na.last = T)
# dt
# 
# dt[!is.na(V1), V4 := nafill(x = as.numeric(V2), type = 'locf'), by= V1]
# dt


## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el MSM?
# disagg_cobatest[, .(N= .N, NAs= sum(is.na(MSM))), by= .(Centre, Id)][N > 1 & NAs > 0 & NAs < N]
# disagg_cobatest[Id == '00206196901M', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, MSM)]
# disagg_cobatest[Id == '01706199401M', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, MSM)]
# disagg_cobatest[Id == 'D000000', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, MSM)]

# Imputem MSM dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, MSM, na.last = T)
disagg_cobatest[!is.na(Id), MSM := nafill(x = as.numeric(MSM), type = 'locf'), by= .(Centre, Id)]



### 2.9.5.- SW        ####  
# ---------------------- #
# SW: 1 yes, 2 no

## COMPROBACIO: 
# disagg_cobatest[, .N, SW][order(as.numeric(SW))]

# Recategoritzem. 
disagg_cobatest[!SW %in% c('1', '2'), SW := as.character(NA)]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el SW?
# disagg_cobatest[, .(.N, sum(is.na(SW))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == 'brde06101992', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, SW)]
# disagg_cobatest[Id == '', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, SW)]
# disagg_cobatest[Id == 'xxxxxxxx', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, SW)]
# disagg_cobatest[Id == '00501199300C', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, SW)]

# Imputem SW dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, SW, na.last = T)
disagg_cobatest[!is.na(Id), SW := nafill(x = as.numeric(SW), type = 'locf'), by= .(Centre, Id)]



### 2.9.6.- PWID       ####  
# ---------------------- #
# PWID: 1 yes, 2 no

## COMPROBACIO: 
# disagg_cobatest[, .N, PWID]

# Recategoritzem. 
disagg_cobatest[!PWID %in% c('1', '2'), PWID := as.character(NA)]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el PWID?
# disagg_cobatest[, .(.N, sum(is.na(PWID))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '03101199610M', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, PWID)]


# Imputem PWID dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, PWID, na.last = T)
disagg_cobatest[!is.na(Id), PWID := nafill(x = as.numeric(PWID), type = 'locf'), by= .(Centre, Id)]


### 2.9.7.- Migrants       ####  
# --------------------------- #
# Migrant: 1 yes, 2 no

## COMPROBACIO: 
# disagg_cobatest[, .N, Migrant]

# Recategoritzem. 
disagg_cobatest[!Migrant %in% c('1', '2'), Migrant := as.character(NA)]


## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el Migrant?
# disagg_cobatest[, .(.N, sum(is.na(Migrant))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, Migrant)]
# disagg_cobatest[Id == '00912200110M', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, Migrant)]


# Imputem Migrant dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, Migrant, na.last = T)
disagg_cobatest[!is.na(Id), Migrant := nafill(x = as.numeric(Migrant), type = 'locf'), by= .(Centre, Id)]


### 2.9.8.- HIV Tested     ####  
# --------------------------- #
# ScreeningHIVTest: 1 yes, 2 no
# Variable que selecciona els testats. Totes les dades agregades van en funció dels testats.

## COMPROBACIO:
# disagg_cobatest[, .N, ScreeningHIVTest]
# disagg_cobatest[is.na(ScreeningHIVTest), .N, .(ScreeningHIVTest, ScreeningTestResult, ConfirmatoryHIVTest, ConfirmatoryHIVTestResult, HIVTestUsed)]



### 2.9.9.- HIV Results     ####  
# ---------------------------- #
# CBVCT 5: Proportion of clients with reactive screening HIV test result 
# ScreeningTestResult: 1 yes, 2 no

## COMPROBACIO:
# disagg_cobatest[, .N, ScreeningTestResult]
# disagg_cobatest[, .N, by= .(ScreeningHIVTest, ScreeningTestResult)][order(ScreeningHIVTest, ScreeningTestResult)]
# 
# Entren en el comput d'indicadors aquells que tenen Hiv test però després NA a results. Què ha passat?
# 
# disagg_cobatest[ScreeningHIVTest == '1' & is.na(ScreeningTestResult), .N, by= Centre][order(as.numeric(Centre))]

# Recategoritzem. 
disagg_cobatest[!ScreeningTestResult %in% c('1', '2'), ScreeningTestResult := as.character(NA)]

# Coherència.
disagg_cobatest[ScreeningHIVTest == 2, ScreeningTestResult := as.character(NA) ]


### 2.9.10.- HIV Ever Tested  ####  
# ----------------------------- #
# CBVCT 2: Proportion of clients who reported to have been previously tested for HIV 
# EverTested: 1 yes, 2 no

## COMPROBACIO:
# disagg_cobatest[, .N, .(EverTested)][order(EverTested)]
# View(disagg_cobatest[, .N, .(Centre, EverTested)])
# disagg_cobatest[Centre == 72, .N, by= EverTested]   # Valors 1 i 3 sense 2 a EverTested.  

## COMPROBACIO: Coherencia de dades
# disagg_cobatest[, .N, .(ScreeningHIVTest, EverTested)][order(ScreeningHIVTest, EverTested)]


## COMPROBACIO:   Coherencia de dades EverTested i TestedLastYear. Si es varen testar l'any passat, 
#                 s'assumeix que han estat testats alguna vegada. 
# disagg_cobatest[, .N, .(EverTested, TestedLastYear)][order(EverTested, TestedLastYear)]
disagg_cobatest[EverTested == "2" & TestedLastYear == "1", EverTested := '1']
disagg_cobatest[EverTested == "3" & TestedLastYear == "1", EverTested := '1']

## COMPROBACIO:   Coherencia de dades EverTested i TestedLastYear. Si es varen testar l'any passat, 
#                 s'assumeix que han estat testats alguna vegada. 
# disagg_cobatest[, .N, .(EverTested, TestedLastYearSameCBVCT)][order(EverTested, TestedLastYearSameCBVCT)]
disagg_cobatest[EverTested == "2" & TestedLastYearSameCBVCT == "1", EverTested := '1']
disagg_cobatest[EverTested == "3" & TestedLastYearSameCBVCT == "1", EverTested := '1']
disagg_cobatest[EverTested == "5" & TestedLastYearSameCBVCT == "1", EverTested := '1']

# Recategoritzem. 
disagg_cobatest[!EverTested %in% c('1', '2'), EverTested := as.character(NA)]


## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el EverTested?
# disagg_cobatest[, .(.N, sum(is.na(EverTested))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, EverTested)]
# disagg_cobatest[Id == 'J290190', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, EverTested)]
# disagg_cobatest[Id == '146702', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, EverTested)]

# Imputem EverTested dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, EverTested, na.last = T)
disagg_cobatest[!is.na(Id), EverTested := nafill(x = as.numeric(EverTested), type = 'locf'), by= .(Centre, Id)]


### 2.9.11.- HIV Tested Last Year  ####  
# ---------------------------------- #
# CBVCT 3:Proportion of clients who reported to have been tested for HIV during preceding 12 months 
# TestedLastYear: 1 yes, 2 no

## COMPROBACIO:
# disagg_cobatest[, .N, .(TestedLastYear)][order(TestedLastYear)]

# Es podrien imputar...
# disagg_cobatest[, .N, .(TestedLastYear, TestedLastYearSameCBVCT)][order(TestedLastYear, TestedLastYearSameCBVCT)]
disagg_cobatest[TestedLastYear == '2' & TestedLastYearSameCBVCT == '1', TestedLastYear := 1]
disagg_cobatest[TestedLastYear == '3' & TestedLastYearSameCBVCT == '1', TestedLastYear := 1]
disagg_cobatest[TestedLastYear == '5' & TestedLastYearSameCBVCT == '1', TestedLastYear := 1]

# Recategoritzem

# (2023-02-16 -- LAURA):  Tested Last Year es defineix en el codi de la Laura a partir de dues variables crues
#                         que s'obren quan es contesta EverTested = Si. En cas que EverTested = 2 aquestes variables 
#                         no poden calcularse i opentic afegeix un 0 per defecte en alguns casos. 
#                         Cal posar aquests 0 a 2, com que no s'han testat l'any passat
disagg_cobatest[TestedLastYear == 0, TestedLastYear := 2]
disagg_cobatest[!TestedLastYear %in% c('1', '2'), TestedLastYear := as.character(NA)]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el TestedLastYear?
# disagg_cobatest[, .(.N, sum(is.na(TestedLastYear))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '01702200010M', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, TestedLastYear)]
# disagg_cobatest[Id == 'XXXXXXXXXXXXM', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, TestedLastYear)]
# disagg_cobatest[Id == '', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, TestedLastYear)]

# Imputem TestedLastYear dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, TestedLastYear, na.last = T)
disagg_cobatest[!is.na(Id), TestedLastYear := nafill(x = as.numeric(TestedLastYear), type = 'locf'), by= .(Centre, Id)]


### 2.9.12.- HIV Tested Last Year sameCBVCT ####  
# -------------------------------------------- #
# CBVCT 4:Proportion of clients who reported to have been tested for HIV at the same CBVCT facility during preceding 12 months
# TestedLastYearSameCBVCT: 1 yes, 2 no

## COMPROBACIÓ:
# disagg_cobatest[, .N, .(TestedLastYearSameCBVCT)][order(TestedLastYearSameCBVCT)]

# Recategoritzem.

# (2023-02-16 -- LAURA):  TestedLastYearSameCBVCT es defineix en el codi de la Laura a partir de  quan es contesta EverTested = Si. 
#                         En cas que EverTested = 2 aquestes variables no pot calcularse i opentic afegeix un 0 per defecte en alguns casos. 
#                         Cal posar aquests 0 a 2, com que no s'han testat l'any passat
disagg_cobatest[TestedLastYearSameCBVCT == 0, TestedLastYearSameCBVCT := 2]
disagg_cobatest[!TestedLastYearSameCBVCT %in% c('1', '2'), TestedLastYearSameCBVCT := as.character(NA)]


## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el TestedLastYearSameCBVCT?
# disagg_cobatest[, .(.N, sum(is.na(TestedLastYearSameCBVCT))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '00406199500O', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, TestedLastYearSameCBVCT)]
# disagg_cobatest[Id == '0', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, TestedLastYearSameCBVCT)]
# disagg_cobatest[Id == '02003199500L', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, TestedLastYearSameCBVCT)]

# Imputem TestedLastYearSameCBVCT dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, TestedLastYearSameCBVCT, na.last = T)
disagg_cobatest[!is.na(Id), TestedLastYearSameCBVCT := nafill(x = as.numeric(TestedLastYearSameCBVCT), type = 'locf'), by= .(Centre, Id)]



### 2.9.13.- HIV Confirmatory Test  ####  
# ------------------------------------ #
# CBVCT 6: Proportion of clients with reactive screening HIV test result who were tested with confirmatory HIV test
# ConfirmatoryHIVTest: 1 yes, 2 no

## COMPROBACIÓ:
# disagg_cobatest[, .N, .(ConfirmatoryHIVTest)]

# imputem per completitud.
disagg_cobatest[, .N, .(ConfirmatoryHIVTest, ConfirmatoryHIVTestResult)][order(ConfirmatoryHIVTest, ConfirmatoryHIVTestResult)]
disagg_cobatest[is.na(ConfirmatoryHIVTest) & ConfirmatoryHIVTestResult == '1', ConfirmatoryHIVTest := '1']

# Recategoritzem.
disagg_cobatest[!ConfirmatoryHIVTest %in% c('1', '2'), ConfirmatoryHIVTest := as.character(NA)]


### 2.9.14.- HIV Confirmatory Test Result  ####  
# ------------------------------------------- #
# CBVCT 7: Proportion of clients with positive confirmatory HIV test result
# ConfirmatoryHIVTestResult: 1 yes, 2 no

## COMPROBACIÓ:
# disagg_cobatest[, .N, .(ConfirmatoryHIVTestResult)]
# disagg_cobatest[ConfirmatoryHIVTestResult == 8, .N, .(Centre)]

# Recategoritzem.
disagg_cobatest[!ConfirmatoryHIVTestResult %in% c('1', '2'), ConfirmatoryHIVTestResult := as.character(NA)]



## 2.10- Fix SYPH Data     ####
#---------------------------- #


### 2.10.1.- Syph Screening Test           ####  
# ------------------------------------------- #
syph_cols <- colnames(disagg_cobatest)[grepl(x = colnames(disagg_cobatest), pattern = "syph", ignore.case = T)]

# COMPROBACIO:   disagg_cobatest[, .N, SyphScreeningTest][order(SyphScreeningTest)]
#
# - Què vol dir SyphScreeningTest = 3. Explorem
#
#                disagg_cobatest[, .N, .(Centre, SyphScreeningTest)][order(as.numeric(Centre), SyphScreeningTest)]
#                 disagg_cobatest[SyphScreeningTest == 3, .N, syph_cols]
# 
# Després d'una inspecció del contingut i les Ns s'interpreta que va posarl-los com SyphScreeningTest = 2. I la resta a Missing.
disagg_cobatest[SyphScreeningTest == 3, (syph_cols) := lapply(X = .SD, FUN = function(x) x <- NA), .SDcols = syph_cols]

# Per esbrinar l'omplenament d'aquest subset de dades
n_syph_missing <- rowSums(is.na(disagg_cobatest[, ..syph_cols]))
disagg_cobatest[, n_syph_NA := n_syph_missing]
disagg_cobatest[, .N, n_syph_NA][order(n_syph_NA)]

## COMPROBACIO:  Tota la fila syphilis a NA.
# disagg_cobatest[n_syph_NA == length(syph_cols), .N, by= syph_cols]   # Tots Missing. ---> Posar SyphScreeningTest == 2
# disagg_cobatest[n_syph_NA == length(syph_cols), .N, by= Centre]   # Tots Missing. ---> Posar SyphScreeningTest == 2
disagg_cobatest[n_syph_NA == length(syph_cols), SyphScreeningTest := 2]


## COMPROBACIO:  Imputar syphscreeningTEst a partir d'alguna variable de resultats informada amb positiu. 
disagg_cobatest[, .N, .(SyphScreeningTest, SyphScreeningTestResult, SyphConfirmatoryTest, SyphConfirmatoryTestResult, SyphTestUsed)][order(SyphScreeningTest, SyphScreeningTestResult, SyphConfirmatoryTest, SyphConfirmatoryTestResult, SyphTestUsed)]


aquiiiiii
# old --------------------------------------------------------------------



## COMPROBACIO:  Altres omplenaments syph amb algun 1 en variables clau
disagg_cobatest[, .N, by= .(SyphScreeningTest, SyphScreeningTestResult, SyphConfirmatoryTest, SyphConfirmatoryTestResult)][order(SyphScreeningTest, SyphScreeningTestResult, SyphConfirmatoryTest, SyphConfirmatoryTestResult)]
#
# Dels missings a SyphscreeningTest, mirem les variables syph relacionades amb testatge, no resultats. 
# disagg_cobatest[is.na(SyphScreeningTest) & is.na(SyphScreeningTestResult) & is.na(SyphConfirmatoryTest) & is.na(SyphConfirmatoryTestResult), .N, .(SyphEverTested, SyphEverDiagnosed, SyphTestedLastYear, SyphTestedLastYearSameCBVCT)]
#
# S'arriba a la conclusió que pel fet de tenir informació sobre syphilis, si no hi ha cap resultat de tests, posarem que si s'ha fet el test per tal 
# d'obtenir la informació però entraran com a missings als resultats

disagg_cobatest[is.na(SyphScreeningTest) 
                & is.na(SyphScreeningTestResult) 
                & is.na(SyphConfirmatoryTest) 
                & is.na(SyphConfirmatoryTestResult) 
                & (SyphEverTested %in% 1:2 | SyphEverDiagnosed %in% 1:2 | SyphTestedLastYear %in% 1:2 | SyphTestedLastYearSameCBVCT %in% 1:2), 
                SyphScreeningTest := 1]
                
## Ens queda:  
disagg_cobatest[is.na(SyphScreeningTest), .N, syph_cols]

### 2.10.2.- Syph Ever Tested    ####  
# --------------------------------- #  
# CBVCT 2: Proportion of clients who reported to have been previously tested for Syphilis 
# Mirem aquells que siguin "previously tested" per Syphilis. 
# SyphEverTested: 1 yes, 2 no



## COMPROBACIÓ:
# disagg_cobatest[, .N, SyphEverTested]
# disagg_cobatest[SyphEverTested == '5', .N, Centre]

## COMPROBACIÓ:  Possible impuació I
# disagg_cobatest[, .N, .(SyphEverTested, SyphTestedLastYear)][order(SyphEverTested, SyphTestedLastYear)]
disagg_cobatest[SyphEverTested == '3' & SyphTestedLastYear == '1', SyphEverTested := 1]
disagg_cobatest[is.na(SyphEverTested) & SyphTestedLastYear == '1', SyphEverTested := 1]

## COMPROBACIÓ:  Possible impuació II
# disagg_cobatest[, .N, .(SyphEverTested, SyphTestedLastYearSameCBVCT)][order(SyphEverTested, SyphTestedLastYearSameCBVCT)]
disagg_cobatest[SyphEverTested == '3' & SyphTestedLastYearSameCBVCT == '1', SyphEverTested := 1]

## COMPROBACIÓ:  Possible impuació III
# disagg_cobatest[, .N, .(SyphEverTested, SyphEverDiagnosed)][order(SyphEverTested, SyphEverDiagnosed)]
disagg_cobatest[SyphEverTested == '3' & SyphEverDiagnosed == '1', SyphEverTested := 1]
disagg_cobatest[SyphEverTested == '5' & SyphEverDiagnosed == '1', SyphEverTested := 1]
disagg_cobatest[is.na(SyphEverTested) & SyphEverDiagnosed == '1', SyphEverTested := 1]

# Recategoritzem
disagg_cobatest[!SyphEverTested %in% c('1', '2'), SyphEverTested := as.character(NA)]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el SyphEverTested?
# disagg_cobatest[, .(.N, sum(is.na(SyphEverTested))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '13107198502D', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, SyphEverTested)]


# Imputem SyphEverTested dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, SyphEverTested, na.last = T)
disagg_cobatest[!is.na(Id), SyphEverTested := nafill(x = as.numeric(SyphEverTested), type = 'locf'), by= .(Centre, Id)]



### 2.10.3.- Syph Ever Diagnosed    ####  
# ------------------------------------ #
# CBVCT 3: Proportion of clients who reported to have been previously diagnosed with Syphilis 
# SyphEverDiagnosed: 1 yes, 2 no

## COMPROBACIÓ:
# disagg_cobatest[, .N, SyphEverDiagnosed]
# disagg_cobatest[SyphEverDiagnosed == '3', .N, Centre]

# Recategoritzem
disagg_cobatest[!SyphEverDiagnosed %in% c('1', '2'), SyphEverDiagnosed := as.character(NA)]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el SyphEverDiagnosed?
# disagg_cobatest[, .(.N, sum(is.na(SyphEverDiagnosed))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '12912200100M', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, SyphEverDiagnosed)]
# disagg_cobatest[Id == '02502197711M', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, SyphEverDiagnosed)]


# Imputem SyphEverDiagnosed dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, SyphEverDiagnosed, na.last = T)
disagg_cobatest[!is.na(Id), SyphEverDiagnosed := nafill(x = as.numeric(SyphEverDiagnosed), type = 'locf'), by= .(Centre, Id)]





### 2.10.4.- Syph Screening Test Result    ####  
# ------------------------------------------- #
# CBVCT 5: Proportion of clients with reactive screening Syphilis test result 
# SyphScreeningTestResult: 1 yes, 2 no

## COMPROBACIÓ:
# disagg_cobatest[, .N, SyphScreeningTestResult]

# Recategoritzem
disagg_cobatest[!SyphScreeningTestResult %in% c('1', '2'), SyphScreeningTestResult := as.character(NA)]





  
## 2.11- Fix HCV Data     ####
#---------------------------- #
colnames(disagg_cobatest)[grepl(pattern = 'hcv', x = colnames(disagg_cobatest), ignore.case = T)]

### 2.11.1.- HCV Ever Tested    ####  
# --------------------------------- # 
# CBVCT 2: Proportion of clients who reported to have been previously tested for HCV 
# Mirem aquells que siguin "previously tested" per HCV
# HCVEverTested: 1 yes, 2 no


## COMPROBACIÓ:
# disagg_cobatest[, .N, HCVEverTested]
# disagg_cobatest[HCVEverTested == '3', .N, Centre]
# disagg_cobatest[HCVEverTested == '5', .N, Centre]

## COMPROBACIÓ:  Possible impuació I
# disagg_cobatest[, .N, .(HCVEverTested, HCVTestedLastYear)][order(HCVEverTested, HCVTestedLastYear)]
disagg_cobatest[HCVEverTested == '3' & HCVTestedLastYear == '1', HCVEverTested := 1]
disagg_cobatest[HCVEverTested == '5' & HCVTestedLastYear == '1', HCVEverTested := 1]
disagg_cobatest[is.na(HCVEverTested) & HCVTestedLastYear == '1', HCVEverTested := 1]

## COMPROBACIÓ:  Possible impuació II
# disagg_cobatest[, .N, .(HCVEverTested, HCVTestedLastYearSameCBVCT)][order(HCVEverTested, HCVTestedLastYearSameCBVCT)]
disagg_cobatest[HCVEverTested == '3' & HCVTestedLastYearSameCBVCT == '1', HCVEverTested := 1]


## COMPROBACIÓ:  Possible impuació III
# disagg_cobatest[, .N, .(HCVEverTested, HCVEverDiagnosed)][order(HCVEverTested, HCVEverDiagnosed)]
disagg_cobatest[HCVEverTested == '5' & HCVEverDiagnosed == '1', HCVEverTested := 1]
disagg_cobatest[is.na(HCVEverTested) & HCVEverDiagnosed == '1', HCVEverTested := 1]

# Recategoritzem
disagg_cobatest[!HCVEverTested %in% c('1', '2'), HCVEverTested := as.character(NA)]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el HCVEverTested?
# disagg_cobatest[, .(.N, sum(is.na(HCVEverTested))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '00202198111P', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, HCVEverTested)]
# disagg_cobatest[Id == '', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, HCVEverTested)]


# Imputem HCVEverTested dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, HCVEverTested, na.last = T)
disagg_cobatest[!is.na(Id), HCVEverTested := nafill(x = as.numeric(HCVEverTested), type = 'locf'), by= .(Centre, Id)]


### 2.11.2.- HCV Ever Diagnosed    ####  
# ----------------------------------- # 
# CBVCT 3: Proportion of clients who reported to have been previously diagnosed with HCV 
# HCVEverDiagnosed: 1 yes, 2 no

## COMPROBACIÓ:
# disagg_cobatest[, .N, HCVEverDiagnosed]
# disagg_cobatest[HCVEverDiagnosed == '3', .N, Centre]
# disagg_cobatest[HCVEverDiagnosed == '5', .N, Centre]

# Recategoritzem
disagg_cobatest[!HCVEverDiagnosed %in% c('1', '2'), HCVEverDiagnosed := as.character(NA)]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el HCVEverDiagnosed?
# disagg_cobatest[, .(.N, sum(is.na(HCVEverDiagnosed))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == '01905198413J', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, HCVEverDiagnosed)]
# disagg_cobatest[Id == '01212195820L', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, HCVEverDiagnosed)]


# Imputem HCVEverDiagnosed dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, HCVEverDiagnosed, na.last = T)
disagg_cobatest[!is.na(Id), HCVEverDiagnosed := nafill(x = as.numeric(HCVEverDiagnosed), type = 'locf'), by= .(Centre, Id)]


### 2.11.3.- HCV Screening Test           ####  
# ------------------------------------------ #

## COMPROBACIÓ:  
# disagg_cobatest[, .N, HCVScreeningTest]
# disagg_cobatest[, .N, .(HCVScreeningTest, HCVScreeningTestResult)][order(HCVScreeningTest, HCVScreeningTestResult)]
# disagg_cobatest[is.na(HCVScreeningTest) & !is.na(HCVScreeningTestResult), .N, Centre]
# disagg_cobatest[is.na(HCVScreeningTest) & !is.na(HCVScreeningTestResult), .N, .(Centre, HCVScreeningTestResult)][order(as.numeric(Centre),HCVScreeningTestResult )]
disagg_cobatest[is.na(HCVScreeningTest) & HCVScreeningTestResult %in% c('1','2'), HCVScreeningTest := '1']

# Recategoritzem. 
disagg_cobatest[!HCVScreeningTest %in% c('1', '2'), HCVScreeningTest := as.character(NA)]


### 2.11.4.- HCV Screening Test Results   ####  
# ------------------------------------------ #
# CVBCT 5
# HCVScreeningTestResult: 1 yes, 2 no

## COMPROBACIÓ:
# disagg_cobatest[, .N, HCVScreeningTestResult]

# Recategoritzem
disagg_cobatest[!HCVScreeningTestResult %in% c('1', '2'), HCVScreeningTestResult := as.character(NA)]


## 2.12- Fix Other Data     ####
#----------------------------- #
# COMPROBACIO: disagg_cobatest[, .N, by= .(Centre, HCVTestUsed)][order(Centre, HCVTestUsed)]    # Valors "2, 3" que passem a "2" per ser poquets (LAURA 07/07/2021). 
disagg_cobatest[Centre == 59 & HCVTestUsed == "2, 3", HCVTestUsed := "2"] 


### 2.12.1.- Females - MSM    ####  
# ------------------------------ #

## COMPROBACIÓ:
# disagg_cobatest[Gender == 2 & MSM == 1, .N, Centre]
# disagg_cobatest[Centre == 72, .N, Gender]

# (LAURA -- 13-11-2021):  Acabo de ver que estos 3 centros no son del COBATEST tool, sinó que mandan los datos desagregados. 
#                         Así que no podemos saber dónde está el error.  
# 
#                         Tenemos que decidir qué nos creemos, el MSM o el Gender..., y todo depende de cómo se recoja la información. Si la variable MSM está calculada, entonces es más creíble la de Gender. 
#                         Si las dos se recogen directamente, entonces me fiaria mas de MSM...
# 
#                         Podemos preguntarles a ellos, pero puede que tarden mucho. 
# 
#                         Cómo lo véis?
#                         Nos quedamos con la de MSM, y en los casos donde el Gender es 2, cambiamos el Gender a 1?
#                          O preferís preguntarles a ellos?
#   
disagg_cobatest[Gender == '2' & MSM == '1', Gender := '1']

### 2.12.2.- Negative Tested when prior positive  ####  
# -------------------------------------------------- #
## Testats positius HIV anteriors que després dónen negatiu en un test posterior. 

## COMPROBACIO:
# disagg_cobatest[, .N, ResultLastHIV]

## COMPROBACIO:  Hi ha algún id repetit amb qui poder imputar el ResultLastHIV?
# disagg_cobatest[, .(.N, sum(is.na(ResultLastHIV))), by= Id][N > 1 & V2 > 0 & V2 < N]
# disagg_cobatest[Id == 'XX-XX-XXXX', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, ResultLastHIV)]
# disagg_cobatest[Id == '12404199022J', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, ResultLastHIV)]
# disagg_cobatest[Id == '02410199600I', .(Id, Centre, CBVCTIdentifier, AgeInYearsNum, ResultLastHIV)]


# Imputem ResultLastHIV dels repetits. 
setorder(x = disagg_cobatest, Centre, Id, ResultLastHIV, na.last = T)
disagg_cobatest[!is.na(Id), ResultLastHIV := nafill(x = as.numeric(ResultLastHIV), type = 'locf'), by= .(Centre, Id)]

## COMPROBACIO:  
# disagg_cobatest[, .N, .(ResultLastHIV, ScreeningTestResult)][order(ResultLastHIV, ScreeningTestResult)]
#
# Tenim ResultLAstHIV == 1 (Positiu) que en el nou test dona negatiu ScreeningTestResult == 2. 
disagg_cobatest[ResultLastHIV == '1' & ScreeningTestResult == '2', ScreeningTestResult := '1']


# _________________________________________________________________________ ####
#------------------------------------------------------------------------------#
# 3.- PROCESSAT DE DADES                                                    ####
# ************************************************************************* ####


## 3.1.- Noms de centres   ####
# --------------------------- #
# Afegim el nom del centre
disagg_cobatest <- merge(x = disagg_cobatest, y= center_maps[, .(Centre= as.character(Centre), CentreName, RealCentreName, Origen)], by.x= "Centre", by.y= "Centre", all.x= T)
# COMPROBACIO:    disagg_cobatest[, .N, by= c('Centre','CentreName','RealCentreName')][order(as.numeric(Centre))]


## 3.2.- Id_main variable   ####
# ---------------------------- #
# En aquest punt unifiquem en una única variable Id els diferents Ids que trobem en els datos agregados i els datos cobatool.
## OJU!!!     Crec que aquest pas es superflu, ja que hem mapejat els ids per mitjà de l'excel de colnames. De moment ho deixem perquè 
#             posteriorment el codi està utilitzant la variable Id_main. 

# center_maps[Origen == 'datos_desagregados', ]

## COMPROBACIÓ:  Ids a Cobatest tool
#
# disagg_cobatest[Origen == "Cobatest_tool", .N, by= .(no_NA_Id= !is.na(Id), no_NA_CBVCTIdentifier= !is.na(CBVCTIdentifier), no_NA_COBATEST_Id= !is.na(COBATEST_Id))]
#
# Utilitzem Id
disagg_cobatest[Centre %in% center_maps[Origen == "Cobatest_tool", Centre] , Id_main := Id]   # BBDD Cobatest Tool
disagg_cobatest[Centre == 69, Id_main := Id]                                                  # Fulcrum
 
if(disagg_cobatest[, sum(is.na(Id_main))] > 0) {
  warning("No pot haver-hi Id_main Missing. Revisa.")
  disagg_cobatest[is.na(Id_main), .N, .(Centre, CentreName)]
  }


## 3.3.- Tractament Belgues   ####
# ------------------------------ #
# Ajuntem alguns centres Belgues sota un únic centre:  BeTested ('Aimer Jeunes','Jette','Marolles','SIPS','Uccle','Watermael-Boitsfort')
# COMPROBACIO:  disagg_cobatest[Centre %in% c(45,46,48,49,50,51), .N, by= Centre]  # Centres Belgues.
# disagg_cobatest[Centre %in% c(45,46,48,49,50,51),  Centre := 71]
# disagg_cobatest <- disagg_cobatest[!Centre %in% c(45,46,48,49,50,51)]   ## NO els traiem però recordar que en alguns indicadors no han de contar!!

# (30-11-2022): No es fa cap tractament. Entren a la neteja separats per no afectar al flowchart. 
#               Després es treuen en funció de si la megi els vol agupats en la taula o no. 

## 3.4.- Reorder Columns    ####
# ---------------------------- #
# colnames(disagg_cobatest)
new_order <- c("Centre", "CentreName", "RealCentreName", "CBVCTIdentifier", "COBATEST_Id", 
               "new_id", "Id", "Id_main", "Gender", "AgeInYears", "AgeInYearsNum", "DateOfBirth", 
               "AgeGroup", "MSM", "SW", "PWID", "Migrant", "DateofVisit", "EverTested", 
               "ResultLastHIV", "TestedLastYear", "TestedLastYearSameCBVCT", "PreTestCounselling", 
               "ScreeningHIVTest", "ScreeningTestResult", "DateScreeningTestResult", 
               "ScreeningTestResultReceived", "ScreeningPostTestCounselling", "ConfirmatoryHIVTest", 
               "DateConfirmatoryTest", "ConfirmatoryHIVTestResult", "DateConfirmatoryTestResult", 
               "ConfirmatoryTestResultReceived", "LinkageToHealthCare", "Datedlinkage", "CD4Count", 
               "DateCD4Count", "HIVTestUsed", "SyphEverDiagnosed", "SyphEverTested", 
               "SyphTestedLastYear", "SyphTestedLastYearSameCBVCT", "SyphScreeningTest", 
               "SyphScreeningTestResult", "SyphConfirmatoryTest", "SyphConfirmatoryTestResult", 
               "SyphTestUsed", "HCVEverDiagnosed", "HCVEverTested", "HCVTestedLastYear", 
               "HCVTestedLastYearSameCBVCT", "HCVScreeningTest", "HCVScreeningTestResult", "HCVRNATest",                     
               "HCVConfirmatoryTestResult", "HCVTestUsed" )
setcolorder(x = disagg_cobatest, neworder = new_order)


## 3.5.- Reorder Rows    ####
# ------------------------- #
# disagg_cobatest[, sum(is.na(DateofVisit))]
setorder(x = disagg_cobatest, DateofVisit, na.last = T)


## 3.6.- Treiem possibles duplicats    ####
# --------------------------------------- #
# Si ens hem oblidat de treure tots els dulicats els treiem aquí. 
disagg_cobatest <- unique(disagg_cobatest)


# _________________________________________________________________________ ####
#------------------------------------------------------------------------------#
# 4.- SAVE DISAGG DATA                                                      ####
# ************************************************************************* ####

# fwrite(x = disagg_cobatest, file = paste0(OUTPUT_DATA_FOLDER, "disagg_cobatest_2018_", TODAY, ".csv"))
list.files(OUTPUT_DATA_FOLDER)

# Per borrar existents
# rstudioapi::selectFile(path = OUTPUT_DATA_FOLDER)


# Source file return.
rm(list= ls()[!ls() %in% "disagg_cobatest"] )

