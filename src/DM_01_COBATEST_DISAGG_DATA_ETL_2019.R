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

disagg_cobatool <- setDT(read_dta(file = paste0(DISAGGREGATED_DATA_FOLDER, "Clean data 2018 COBATEST for indicators.dta")))  ## 10906 obs. 49 vars.            
# remove labels, label and format attributes.
disagg_cobatool <- zap_formats(disagg_cobatool); disagg_cobatool <- zap_label(disagg_cobatool); disagg_cobatool <- zap_labels(disagg_cobatool)

# Other disaggregated here. 
disagg_fulcrum <- setDT(read.xlsx(paste0(DISAGGREGATED_DATA_FOLDER, "FULCRUM 2018 original_disaggregated.xlsx"), detectDates = T)) 


## COMPROBACIONS:   Qualitat cobatest tool
#
# cat(paste(colnames(disagg_cobatool), collapse = '\n'))
# disagg_cobatool[, .N, centre][order(centre)]
# summary(as.Date(disagg_cobatool$DateofVisit))
# disagg_cobatool[grepl(x = Id, pattern = 'test', ignore.case = T)]
# disagg_cobatool[grepl(x = CBVCTIdentifier, pattern = 'test', ignore.case = T)]


## (WARN -- 11-04-2023): Tenim centre 31 amb 1 obs., centre 37 amb 1 obs. i centre 42 amb 4 obs.  
# disagg_cobatool[centre == 31]
# disagg_cobatool[centre == 37]  ## test. Fora.
# disagg_cobatool[centre == 42]  ## tots son d'un mateix dia de visita.
#
## (10-03-2023 -- LAURA): •	pel centre Athens Checkpoint, que només té 1 test, s'ha d'eliminar
#                         •	Pel centre Checkpoint Milano, que només té 1 tests, s'ha d'eliminar, doncs es una prova de com funcionava la plataforma, van començar a entrar dades el 2019. 
#                         •	Pel centre Háttér Society, tenen dades entrades el 2017, 2018 i 2019, molt poques, en particular el 2018, amb només 4 proves entrades. Tenint en compte que no són errors i que és un dels països EECA, jo el deixaria. # •	Pel centre Háttér Society, tenen dades entrades el 2017, 2018 i 2019, molt poques, en particular el 2018, amb només 4 proves entrades. Tenint en compte que no són errors i que és un dels països EECA, jo el deixaria. # •	Pel centre Háttér Society, tenen dades entrades el 2017, 2018 i 2019, molt poques, en particular el 2018, amb només 4 proves entrades. Tenint en compte que no són errors i que és un dels països EECA, jo el deixaria. # •	Pel centre Háttér Society, tenen dades entrades el 2017, 2018 i 2019, molt poques, en particular el 2018, amb només 4 proves entrades. Tenint en compte que no són errors i que és un dels països EECA, jo el deixaria. 
disagg_cobatool <- disagg_cobatool[!centre %in% c(31,37)]


## 1.3.- Process Fulcrum data  ####
#---------------------------------#

## Duplicats
# disagg_fulcrum[duplicated(disagg_fulcrum), .N]   ## 67 obs. duplicades
disagg_fulcrum <- unique(disagg_fulcrum)

# (13-04-2023 - JORDI):  Es detecten duplicats per Id, Dateofvisit que es corresponen a a les dades de vih en un registre
#                        i a les de sífilis en l'altre. 
#
#                        disagg_fulcrum[, .N, .(clientidentifier, DateOfVisit)][N > 1][order(-N)]
#
# clientidentifier_ids <- disagg_fulcrum[, .N, .(clientidentifier, DateOfVisit)][N > 1, unique(clientidentifier)]
# disagg_fulcrum[clientidentifier %in% clientidentifier_ids 
#                & is.na(SyphEverTested), unique(clientidentifier) ]
#
## pex.
#
#   disagg_fulcrum[clientidentifier == '6591FDO13290']

# Imputem MSM dels repetits. 
setorder(x = disagg_fulcrum, clientidentifier, DateOfVisit)
sel_cols <- colnames(disagg_fulcrum)[grepl(x = colnames(disagg_fulcrum), pattern = '^Syph|^HCV')]

for (col in sel_cols) {
  disagg_fulcrum[, (col) := nafill(x = get(col), type = 'locf'), by= .(clientidentifier, DateOfVisit)]
  disagg_fulcrum[, (col) := nafill(x = get(col), type = 'nocb'), by= .(clientidentifier, DateOfVisit)]
}; rm(sel_cols)

setorder(x = disagg_fulcrum, DateOfVisit)
disagg_fulcrum <- unique(disagg_fulcrum)

# _________________________________________________________________________ ####
#------------------------------------------------------------------------------#
# 2.- GENERA DISAGG DATA                                                    ####
# ************************************************************************* ####

## 2.1.- Estandaritzar colnames  ####
#-----------------------------------#
# En aquesta part cal assegurar-se molt bé que els noms de les variables escrites a l'Excel siguin exactament els del dataset. 
disagg_cobatool <- standarize_colnames(disagg_dt = disagg_cobatool, colmap_dt = col_maps, colmap_from_column = "Clean_data_2021_COBATEST_for_indicators_V1_09062022.dta", colmap_to_column = "COBATEST_Variable_name")
disagg_fulcrum <- standarize_colnames(disagg_dt = disagg_fulcrum, colmap_dt = col_maps, colmap_from_column = "FULCRUM 2018 original_disaggregated.xlsx", colmap_to_column = "COBATEST_Variable_name")


## 2.2.- Assignem codi centre  ####
#---------------------------------#
# Exploracio rapida centres cobatest tool
disagg_cobatool[, .N, Centre]
disagg_fulcrum[, Centre := 69]


## 2.3.- Afegim missing cols      ####
# ---------------------------------- #
disagg_cobatool <- add_missing_cols(disagg_dt = disagg_cobatool, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")
disagg_fulcrum <- add_missing_cols(disagg_dt = disagg_fulcrum, colmap_dt = col_maps, colmap_ref_column = "COBATEST_Variable_name")


## 2.4.- Order cols      ####
# ------------------------- #
new_order <- col_maps[, COBATEST_Variable_name]
disagg_cobatool <- disagg_cobatool[, ..new_order]
disagg_fulcrum <- disagg_fulcrum[, ..new_order]
rm(new_order)



## 2.5.- Fix Date cols   ####
# ------------------------- #

# COMPROBACIÓ:  Format de les dades tipus Date
#
#               # Per veure els diferents formats.
#               lapply(X = mget(ls(pattern= "disagg_")), FUN = function(x) summary(x$DateofVisit))

# Es pot llegir amb read_xlsx de "readxl" enlloc de "openxlsx", però és més rígid i 
# implica altres problematiques. De moment settegem l'origen

# COMPROBACIO: Cap missing
# lapply(X = mget(ls(pattern= "disagg_")), FUN = function(x) sum(is.na(x$DateofVisit)))



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



## COMPROBACIO: Podem arreglar Ids?
dt <- disagg_cobatest[, .(Centre, Id, CBVCTIdentifier, COBATEST_Id, Gender, DateOfBirth)]
dt[, nchar := nchar(Id)]
dt[, has_cobaid_Id := grepl(x = Id, pattern = "[0-9]{11}[A-Za-z]")]
dt[, has_cobaid_CBVCTIdentifier := grepl(x = CBVCTIdentifier, pattern = "[0-9]{11}[A-Za-z]")]
dt[, has_cobaid_COBATEST_Id := grepl(x = COBATEST_Id, pattern = "[0-9]{11}[A-Za-z]")]
dt[, .N, .(has_cobaid_Id, has_cobaid_CBVCTIdentifier, has_cobaid_COBATEST_Id)][order(has_cobaid_Id, has_cobaid_CBVCTIdentifier, has_cobaid_COBATEST_Id)]
dt[has_cobaid_Id == F & has_cobaid_CBVCTIdentifier == F & has_cobaid_COBATEST_Id == F, .N, nchar][order(nchar)]
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
# disagg_cobatest[Id == '', .(Id, CBVCTIdentifier, COBATEST_Id)]
disagg_cobatest[Id == '', Id := COBATEST_Id]

## COMPROBACIO:
#
# disagg_cobatest[grepl(x = Id, pattern= '^000'), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, Gender, DateOfBirth)]
disagg_cobatest[grepl(x = Id, pattern = '^00000') == T 
                & grepl(x = CBVCTIdentifier, pattern = "[0-9]{11}[A-Za-z]") == T, 
                Id := CBVCTIdentifier]

# disagg_cobatest[CBVCTIdentifier == '18/301']
# disagg_cobatest[Id == '00000000012N']
disagg_cobatest[Id == '00000000012N', Id := '18/301']

# disagg_cobatest[CBVCTIdentifier == '25V01.1JAOS']
# disagg_cobatest[Id == '000000000000']
disagg_cobatest[Id == '00000000012N', Id := '25V01.1JAOS']


## COMPROBACIÓ:  Mateixos Ids en centres diferents.
# disagg_cobatest[, uniqueN(Centre), Id][V1 > 1][order(Id)]
disagg_cobatest[Id == '.00000000000']
disagg_cobatest <- disagg_cobatest[Id != '.00000000000']


### 2.9.1- Gender  ####
# ------------------- #
# El gender a cobatest ha de ser:   1 male, 2 female, 3 transgender 

## COMPROBACIO
# disagg_cobatest[, .N, Gender][order(Gender)]
# disagg_cobatest[is.na(Gender), .(Centre, CBVCTIdentifier, COBATEST_Id, Id, Gender)]
#
## COMPROBACIO   Repetidors informats
# disagg_cobatest[Id %in% disagg_cobatest[is.na(Gender), unique(Id)], .N, Id][N > 1]

# Imputem gènere.
disagg_cobatest[Id == '00503198410P', Gender := "1"]



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

# Passem a date
disagg_cobatest[, DateOfBirth := as.Date(DateOfBirth)]

## COMPROBACIÓ:   Distribució d'edats
#
#  disagg_cobatest[, .N, AgeInYearsNum][order(AgeInYearsNum)]
#
## Tenim:    ("Clean data 2017 COBATEST for indicators.dta")
#
#  1.- Individus amb edat < 0.             ##   61 obs.
#  2.- Individus amb edat 0.               ##  145 obs.
#  3.- Alguns individus amb 1 - 16 anys.   ##   29 obs.
#  4.- Individus de més de 100 anys.       ##    8 obs. 
#  5.- Individus amb edat Missing.         ##  934 obs.       

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

## COMPROBACIO:    disagg_cobatest[is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)] 
#
# dt <- disagg_cobatest[is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, COBATEST_Id, new_id, DateOfBirth, AgeInYearsNum)]
# dt[, nchar_id := nchar(Id)]
# dt[, .N, nchar_id][order(nchar_id)]
# 
# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= Id, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]


# Pas zero: 
# Probem d'imputar a partir de DateofBirth. 
#
# disagg_cobatest[, .N, .(isna_ageinyearsnum= is.na(AgeInYearsNum), isna_dateofbirth= is.na(DateOfBirth))] # volem imputar els TRUE-FALSE
# disagg_cobatest[is.na(AgeInYearsNum) & !is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeGroup, AgeInYearsNum)]



# Primer: 
# Imputem ageYearsNum pels AgeYearsNum == NA amb CBVCTIdentifier o Id tipus SDDMMYYYYABC:   [0-9]{11}[A-Z].
#
# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]   
# disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= Id, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]   
disagg_cobatest[is.na(AgeInYearsNum) & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), DateOfBirth := as.Date(substr(x = CBVCTIdentifier, start = 2, stop = 9), format= "%d%m%Y")]  

# Algun que no s'hagi fet?
disagg_cobatest[is.na(AgeInYearsNum) 
                & (grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]')
                | grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]')), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)] 

# Segon: 
# Imputem ageYearsNum pels Ids de menys de 6 caracters. 
#
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & nchar(Id) <= 6, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[Id == '250391', DateOfBirth := as.Date('1991-03-25')]
disagg_cobatest[Id == 'P30766', DateOfBirth := as.Date('1966-06-15')]  # Captem l'any
disagg_cobatest[Id == 'R19097', DateOfBirth := as.Date('1997-06-15')]  # Captem l'any

## Tercer: 
# Imputem ageYearsNum pels Ids de forma "^[A-Z]{x}[0-9]{6}$" amb x = 1, 2, 3... caracters 
#
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1,5}[0-9]{6}$"), .(nchar= nchar(Id), Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{6}$"), .(nchar= nchar(Id), Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{6}$"),  DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 2, stop = 7), orders = 'dmy', cutoff_2000 = 20L))]

# Els que per la imputació anterior han donat error.
# disagg_cobatest[grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{6}$"), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{6}$") & is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum, AgeGroup)]
disagg_cobatest[grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{6}$") & is.na(DateOfBirth),.N, Id]

disagg_cobatest[Id == 'A000056', DateOfBirth := as.Date('1956-06-15')]  # Captem l'any
disagg_cobatest[Id == 'X000086', DateOfBirth := as.Date('1986-06-15')]  # Captem l'any
disagg_cobatest[Id == 'B290282', DateOfBirth := as.Date('1982-02-28')]  # Corregim
disagg_cobatest[Id == 'M000000', DateOfBirth := as.Date('2000-06-15')]  # Captem l'any
disagg_cobatest[Id == 'D000095', DateOfBirth := as.Date('1995-06-15')]  # Captem l'any
disagg_cobatest[Id == 'P000075', DateOfBirth := as.Date('1975-06-15')]  # Captem l'any
disagg_cobatest[Id == 'T000000', DateOfBirth := as.Date('2000-06-15')]  # Captem l'any
disagg_cobatest[Id == 'X000000', DateOfBirth := as.Date('2000-06-15')]  # Captem l'any
disagg_cobatest[Id == 'X000086', DateOfBirth := as.Date('2000-06-15')]  # Captem l'any



## Quart: 
# Imputem ageYearsNum pels Ids de forma "^[A-Z]{x}[0-9]{8}$" amb x = 1, 2, 3... caracters 
#
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1,5}[0-9]{8}$"), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{8}$"),  DateOfBirth := as.Date(substr(x = Id, start = 2, stop = 9), format= '%d%m%Y')]
# disagg_cobatest[grepl(x = Id, pattern = "^[A-Z]{1}[0-9]{8}$"), .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]


## Cinquè: 
# Imputem els restants a mà. Més que la data exacta, és important determinar l'any.
#♦
# disagg_cobatest[is.na(AgeInYearsNum) & is.na(DateOfBirth) & nchar(Id) > 7, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
disagg_cobatest[CBVCTIdentifier == '13109198100R', DateOfBirth := as.Date('1981-09-30')]  
disagg_cobatest[CBVCTIdentifier == '02902197743N', DateOfBirth := as.Date('1977-02-28')]  
disagg_cobatest[CBVCTIdentifier == '13111199900A', DateOfBirth := as.Date('1999-11-30')]  
disagg_cobatest[CBVCTIdentifier == 'C1250994', DateOfBirth := as.Date('1994-09-01')]  
disagg_cobatest[CBVCTIdentifier == 'F0510890', DateOfBirth := as.Date('1990-08-01')]  
disagg_cobatest[CBVCTIdentifier == 'F0710954', DateOfBirth := as.Date('1954-09-01')]  
disagg_cobatest[CBVCTIdentifier == 'L1801084', DateOfBirth := as.Date('1984-10-01')]  
disagg_cobatest[CBVCTIdentifier == 'MJ190270', DateOfBirth := as.Date('1970-02-01')]  
disagg_cobatest[CBVCTIdentifier == 'XXXXXX88', DateOfBirth := as.Date('1988-01-01')]  


# Imputem Age in Years.
# disagg_cobatest[is.na(AgeInYearsNum) & !is.na(DateOfBirth), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYears, AgeInYearsNum)]
disagg_cobatest[is.na(AgeInYearsNum) & !is.na(DateOfBirth), AgeInYearsNum := YEAR - year(DateOfBirth)]

## Sisè: Hi ha registres assignats a una única persona en que tingui escrita la data de naixement o l'edat en algun d'aquests?
# disagg_cobatest[is.na(AgeInYearsNum), .N, .(Centre, Id)][N > 1][order(-N)]
# disagg_cobatest[Id %in% disagg_cobatest[is.na(AgeInYearsNum), .N, .(Centre, Id)][N > 1][, Id], .(Centre, Id, CBVCTIdentifier, COBATEST_Id)]

## COMPROBACIO:  Qui ha quedat amb edat a missing?   18 obs
#
# disagg_cobatest[is.na(AgeInYearsNum), .(Id, CBVCTIdentifier, DateOfBirth, AgeInYears, AgeInYearsNum)]
#
# ids <- disagg_cobatest[is.na(AgeInYearsNum), unique(Id)]
# disagg_cobatest[Id %in% ids, .N, Id]
# disagg_cobatest[Id %in% ids & !is.na(Id), .(Id, CBVCTIdentifier, DateOfBirth, AgeInYears)][order(Id, CBVCTIdentifier)]


### Edat < 0   ####
# --------------- #

## COMPROBACIO:   disagg_cobatest[AgeInYearsNum < 0, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]   ## 61 obs. 
#
# DateOfBirth son tots del 2021, Podem extraure la informació de Id o CBVCTIdentifier. Si utilitzem 
# els Id que tenen format "SDDMMYYYYABC" per extreure DDMMYYYY veiem que tenen YYYY = 2021. Podem utilitzar
# el mateix raonament però amb l'altre identificador: CBVCTIdentifier. 

# imputem ageYearsNum pels AgeYearsNum < 0 amb CBVCTIdentifier tipus SDDMMYYYYABC:   [0-9]{11}[A-Z].
# disagg_cobatest[AgeInYearsNum < 0 & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), .(Centre, Id, CBVCTIdentifier, DateOfBirth)]  
disagg_cobatest[AgeInYearsNum < 0 & grepl(x= CBVCTIdentifier, pattern = '[0-9]{11}[A-Z]'), DateOfBirth := as.Date(substr(x = CBVCTIdentifier, start = 2, stop = 9), format= "%d%m%Y")]  

# imputem ageYearsNum pels AgeYearsNum < 0 amb CBVCTIdentifier tipus ^[A-Z]{2,3}[0-9]{6}$
# disagg_cobatest[AgeInYearsNum < 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2,3}[0-9]{6}$'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum < 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2}[0-9]{6}$') & Centre == 19, DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 3, stop = 8), orders = 'ymd', cutoff_2000 = 20L))]  
disagg_cobatest[AgeInYearsNum < 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{3}[0-9]{6}$') & Centre == 19, DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 4, stop = 9), orders = 'ymd', cutoff_2000 = 20L))]  


# Arreglem a mà. Vigilem que no hi hagi duplicats. 
# disagg_cobatest[Id == 'APO181093', ]
disagg_cobatest[Id == 'APO181093',  DateOfBirth := as.Date('1993-10-18', format= '%Y-%m-%d')]


# Imputem Age in Years.
disagg_cobatest[AgeInYearsNum < 0, AgeInYearsNum := YEAR - year(DateOfBirth)]

## COMPROBACIO:   Ha quedat algun?   
#
# disagg_cobatest[AgeInYearsNum < 0, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]    
#


### Edat 0    ####
# -------------- #

## COMPROBACIO:   disagg_cobatest[AgeInYearsNum == 0, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]
#
#   dt <- disagg_cobatest[AgeInYearsNum == 0, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]  ## 161 obs.
#

# Primer: Imputem ageYearsNum pels AgeYearsNum == 0 amb Id tipus SDDMMYYYYABC:   [0-9]{11}[A-Z].
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= Id, pattern = '[0-9]{11}[A-Z]'), DateOfBirth := as.Date(substr(x = CBVCTIdentifier, start = 2, stop = 9), format= "%d%m%Y")]  


# Segon: 
# imputem ageYearsNum pels AgeYearsNum == 0 amb CBVCTIdentifier tipus ^[0-9]{6}[A-Z]{2,3}$
# disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[0-9]{6}[A-Z]{2,3}$'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[0-9]{6}[A-Z]{2}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = CBVCTIdentifier, start = 1, stop = 6), orders = 'ymd', cutoff_2000 = 20L))]  

# Tercer: 
# imputem ageYearsNum pels AgeYearsNum == 0 amb CBVCTIdentifier tipus ^[A-Z]{2,4}[0-9]{6}$ i amb CJAS davant
# disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2,4}[0-9]{6}$'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2}[0-9]{6}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 3, stop = 8), orders = 'ymd', cutoff_2000 = 20L))]  
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{3}[0-9]{6}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 4, stop = 9), orders = 'ymd', cutoff_2000 = 20L))]  
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{4}[0-9]{6}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 5, stop = 10), orders = 'ymd', cutoff_2000 = 20L))]  


# Quart: 
# imputem ageYearsNum pels AgeYearsNum == 0 amb CBVCTIdentifier tipus ^[A-Z]{2,4}[0-9]{8}$ i amb CJAS davant
# disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2,4}[0-9]{8}$'), .(Centre, Id, CBVCTIdentifier, DateOfBirth, AgeInYearsNum)]  
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{2}[0-9]{8}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 3, stop = 8), orders = 'ymd', cutoff_2000 = 20L))]  
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{3}[0-9]{8}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 4, stop = 9), orders = 'ymd', cutoff_2000 = 20L))]  
disagg_cobatest[AgeInYearsNum == 0 & grepl(x= CBVCTIdentifier, pattern = '^[A-Z]{4}[0-9]{8}$'), DateOfBirth := as.Date(parse_date_time2(x = substr(x = Id, start = 5, stop = 10), orders = 'ymd', cutoff_2000 = 20L))]  


# Arreglem a mà els altres patrons detectats 
disagg_cobatest[CBVCTIdentifier == 'JUMA116101999',  DateOfBirth := as.Date('1999-10-16', format= '%Y-%m-%d')]
disagg_cobatest[CBVCTIdentifier == 'JUMA215102000',  DateOfBirth := as.Date('2000-10-15', format= '%Y-%m-%d')]
disagg_cobatest[CBVCTIdentifier == '961126',  DateOfBirth := as.Date('1996-11-26', format= '%Y-%m-%d')]
disagg_cobatest[CBVCTIdentifier == 'CJAS890302L',  DateOfBirth := as.Date('1989-03-02', format= '%Y-%m-%d')]
disagg_cobatest[CBVCTIdentifier == 'CJAS960629LC',  DateOfBirth := as.Date('1996-06-29', format= '%Y-%m-%d')]
disagg_cobatest[CBVCTIdentifier == 'CJAS980129CL',  DateOfBirth := as.Date('1998-01-29', format= '%Y-%m-%d')]


# Imputem Age in Years.
disagg_cobatest[AgeInYearsNum == 0, AgeInYearsNum := YEAR - year(DateOfBirth)]

## VALIDACIO: 
# dt <- disagg_cobatest[AgeInYearsNum == 0, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]     ## 3 obs.
#
# Comprovem si aquests ids tenen altres registres
# dt[, .N, Id][N > 1]


### Edat [1, 15]  ####
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16, .N, AgeInYearsNum]
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)][order(AgeInYearsNum)]
disagg_cobatest[Id == '02406199201M',  DateOfBirth := as.Date('1992-06-24', format= '%Y-%m-%d')]
disagg_cobatest[Id == 'ALIN13041977',  DateOfBirth := as.Date('1977-04-13', format= '%Y-%m-%d')]
disagg_cobatest[Id == 'MAFA09051998',  DateOfBirth := as.Date('1998-05-09', format= '%Y-%m-%d')]
disagg_cobatest[Id == 'ROAN05061989',  DateOfBirth := as.Date('1989-06-05', format= '%Y-%m-%d')]

# Imputem Age in Years.
disagg_cobatest[AgeInYearsNum > 0 & AgeInYearsNum < 16, AgeInYearsNum := YEAR - year(DateOfBirth)]


### Edat > 100  ####
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 100, .N, AgeInYearsNum][order(AgeInYearsNum)]                              
## COMPROBACIO:   disagg_cobatest[AgeInYearsNum > 100, .(Centre, Id, CBVCTIdentifier, new_id, DateOfBirth, AgeInYearsNum)]    
disagg_cobatest[AgeInYearsNum == 999, .N, Centre]   ## Tots son del mateix centre i té pinta que són els missings
disagg_cobatest[AgeInYearsNum == 999, AgeInYearsNum := as.numeric(NA)]

disagg_cobatest[Id == '12106199110S',  DateOfBirth := as.Date('1991-06-21', format= '%Y-%m-%d')]
disagg_cobatest[Id == '13008198500B',  DateOfBirth := as.Date('1985-08-30', format= '%Y-%m-%d')]
disagg_cobatest[Id == '02612020001A',  DateOfBirth := as.Date('2000-12-26', format= '%Y-%m-%d')]
disagg_cobatest[Id == '10507019901M',  DateOfBirth := as.Date('1990-07-05', format= '%Y-%m-%d')]
disagg_cobatest[Id == '11903100601A',  DateOfBirth := as.Date('1996-03-19', format= '%Y-%m-%d')]
disagg_cobatest[Id == '12705020000V',  DateOfBirth := as.Date('2000-05-27', format= '%Y-%m-%d')]
disagg_cobatest[Id == '102091197700',  DateOfBirth := as.Date('1977-02-09', format= '%Y-%m-%d')]
disagg_cobatest[CBVCTIdentifier == '01301195810F',  DateOfBirth := as.Date('1958-01-13', format= '%Y-%m-%d')]
disagg_cobatest[CBVCTIdentifier == '11606199200R',  DateOfBirth := as.Date('1992-06-16', format= '%Y-%m-%d')]
disagg_cobatest[Id == '02011106140E',  DateOfBirth := as.Date('1961-11-20', format= '%Y-%m-%d')]
disagg_cobatest[Id == '02106099110S',  DateOfBirth := as.Date('1991-06-21', format= '%Y-%m-%d')]
disagg_cobatest[Id == '11303201810L',  DateOfBirth := as.Date('1982-08-29', format= '%Y-%m-%d')]

disagg_cobatest[AgeInYearsNum > 100 & is.na(DateOfBirth), AgeInYearsNum := as.numeric(NA)]

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

# Podem imputar AgeGroup a partir de "DateOfBirth", "AgeInYears", "CBVCTIdentifier", "COBATEST_Id" o "Id".

## COMPROBACIO:   
# Abans hem treballat exhaustivament la variable AgeInYearsNum a partir dels identificadors de manera que podem utilitzar-la. 
#
# disagg_cobatest[, .N, .(AgeGroup, No_NA_age= !is.na(AgeInYearsNum))]
# disagg_cobatest[is.na(AgeGroup) & !is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, AgeInYearsNum)]
# disagg_cobatest[AgeGroup == 1 & is.na(AgeInYearsNum), .(Centre, Id, CBVCTIdentifier, AgeInYearsNum)]

# Podrem recuperar els AgeGroup == NA i amb AgeInYearsNum corregida de AgeInYears.
disagg_cobatest[is.na(AgeGroup) & !is.na(AgeInYearsNum), AgeGroup := fifelse(test = AgeInYearsNum >= 25, yes = '2', no = '1')]


## COMPROBACIO: Explorem més a fons els AgeGroup == NA i AgeInYearsNum == NA
# disagg_cobatest[is.na(AgeGroup) & is.na(AgeInYearsNum), .N, Centre]
# disagg_cobatest[is.na(AgeGroup) & is.na(AgeInYearsNum), .(AgeGroup, AgeInYearsNum, Centre, Id, CBVCTIdentifier, DateOfBirth)]
# 
# Pels que queden transformables, no cal perdre més el temps.




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
# disagg_cobatest[, .(.N, sum(is.na(MSM))), by= Id][N > 1 & V2 > 0 & V2 < N]
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


### 2.9.10.- HIV Ever Tested  ####  
# ----------------------------- #
# CBVCT 2: Proportion of clients who reported to have been previously tested for HIV 
# EverTested: 1 yes, 2 no

## COMPROBACIO:
# disagg_cobatest[, .N, .(EverTested)][order(EverTested)]
# View(disagg_cobatest[, .N, .(Centre, EverTested)])
# disagg_cobatest[Centre == 72, .N, by= EverTested]   # Valors 1 i 3 sense 2 a EverTested.  

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
colnames(disagg_cobatest)[grepl(pattern = 'syph', x = colnames(disagg_cobatest), ignore.case = T)]


### 2.10.1.- Syph Ever Tested    ####  
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



### 2.10.2.- Syph Ever Diagnosed    ####  
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


### 2.10.3.- Syph Screening Test           ####  
# ------------------------------------------- #

## COMPROBACIÓ:  
# disagg_cobatest[, .N, SyphScreeningTest]
# disagg_cobatest[, .N, .(SyphScreeningTest, SyphScreeningTestResult)][order(SyphScreeningTest, SyphScreeningTestResult)]
# disagg_cobatest[is.na(SyphScreeningTest) & !is.na(SyphScreeningTestResult), .N, Centre]
# disagg_cobatest[is.na(SyphScreeningTest) & !is.na(SyphScreeningTestResult), .N, .(Centre, SyphScreeningTestResult)][order(as.numeric(Centre),SyphScreeningTestResult )]
disagg_cobatest[is.na(SyphScreeningTest) & !is.na(SyphScreeningTestResult), SyphScreeningTest := '1']

# Recategoritzem. 
disagg_cobatest[!SyphScreeningTest %in% c('1', '2'), SyphScreeningTest := as.character(NA)]


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

