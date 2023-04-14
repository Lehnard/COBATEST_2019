rm(list= ls())
dev.off()
cat("\014")

library(data.table)
library(openxlsx)     # read.xlsx() 
library(haven)        # read_dta()  


# ----------------------------------------------------------- #
## LOAD DATA                                               ####
# ----------------------------------------------------------- #
# Dades de l'extraccó BBDD Cobatest Tool pre-processades amb STATA.
bbdd_path <- "Data/COBATEST Data base extraction 2019/Arxiu final STATA per càlcul indicadors/20210128_COBATEST_2019_clean_data_for_indicators.dta"
desagg_00 <- setDT(read_dta(bbdd_path)); rm(bbdd_path)
# remove labels, label and format attributes.
desagg_00 <- zap_formats(desagg_00); desagg_00 <- zap_label(desagg_00); desagg_00 <- zap_labels(desagg_00)

# Dades desagregades enviades pels altres centres participants.
{file_path_1 <- "Data/Received Disaggregated data 2019/Disaggregated data received files 2019/"
file_path_2 <- "Data/Received Disaggregated data 2019/Disaggregated data received files 2019/Belgium/"
file_path_3 <- "Data/New Data 09 2019/Disaggregated data/"
desagg_01 <- fread(paste0(file_path_1, "2020-04-06_Czech AIDS help society.csv"), encoding = 'UTF-8') 
desagg_02 <- setDT(read.xlsx(paste0(file_path_1, "CBVCT Data COBATEST Aids Fondet_Centre06 20191.xlsx"), sep.names = " ", detectDates = TRUE ))
desagg_03 <- setDT(read.xlsx(paste0(file_path_1, "COBA Auswertung_Jänner bis Dezember_2019_AidsHilfeWien.xlsx"),sheet = 2, sep.names = " ", detectDates = TRUE))
desagg_04 <- fread(paste0(file_path_2, "COBATEST MdM 2019_medicinsdumunde.csv"), encoding = 'UTF-8') 
desagg_05 <- setDT(read.xlsx(paste0(file_path_2, "COBATEST_EspacePBELGIUM_2019.xlsx"), sep.names = " ", detectDates = TRUE))
desagg_06 <- setDT(read.xlsx(paste0(file_path_2, "Données dépistage Liège 2019 Cobatest_Sidasol.xlsx"), sep.names = " ", detectDates = TRUE ))
desagg_07 <- setDT(read.xlsx(paste0(file_path_2, "Finaal_Cobatest_document_Violett_proc.xlsx"), sep.names = " ", detectDates = TRUE ))
desagg_08 <- setDT(read.xlsx(paste0(file_path_2, "Stats dépistage 2019_cobatest_exæquo (2).xlsx"), sep.names = " ", detectDates = TRUE ))
desagg_09 <-  setDT(read_dta(paste0(file_path_1, "Adhara 2019 clean.dta"))) 
desagg_10 <- setDT(read_dta(paste0(file_path_3, "CJAS 2019 clean per COBATEST.dta"))) }; rm(file_path_1, file_path_2, file_path_3)

## OJU!  
#   - s'ha canviat a ma el nom de variable de "Stats dépistage 2019_cobatest_exæquo (2).xlsx" : Date of "requesting the test" visit
#   - S'han tret les cometes " per evitar que R introdueixi "\" constantment. 

# Diccionari de correspondencies (mapping) de les variables dels datasets dels centres amb el nom estandard de les variables. 
## OJU!  2019: Particularitat.
#   - Medicines du munde aporta 3 dates de visita en funcio de si es VIH, Sifilis o VHC. Per defecte el diccionari de 
#   - mapeig de variables aporta la Dateofvisit del VIH. Si es volen les altres s'hauran de recarregar.
{file_path <- "Data/Auxiliar_Data/"
col_maps <- setDT(read.xlsx(paste0(file_path, "COBATEST_Column_Mappings.xlsx"))); rm(file_path)}

# Diccionari de correspondencies (mapping) de codis dels centres amb el nom dels centres.
{file_path <- "Data/Auxiliar_Data/"
  center_maps <- setDT(read.xlsx(paste0(file_path, "Códigos centros.xlsx"), sheet = 1)); rm(file_path)}

# ----------------------------------------------------------- #
## DESAGG:___PRE-PROCESSAMENT                                ####
# ----------------------------------------------------------- #

# LAURA: Els del CJAS que vénen al cobatest Tool s'ha de treure. Després els ajuntarem com a fitxer desagregat.
# COMPROBACIO:   desagg_00[centre == 8, .N]  # Numero de CJAS
desagg_00 <- desagg_00[centre != 8, ]


# desagg_02 Pre-Proc. Informem variable "AgeGroup (not asked)" a partir de les dades a "AgeInYears".
{desagg_02[AgeInYears < 25, `AgeGroup (not asked)` := 1]
desagg_02[AgeInYears >= 25, `AgeGroup (not asked)` := 2]}


# desagg_03 Pre-Proc. Construim MSM, SW, PWID i Migrant a partir de KeyPopulation.
{ desagg_03[, MSM := ifelse(test = KeyPopulation == 1, yes = 1, no = 2)]
  desagg_03[, SW := ifelse(test = KeyPopulation == 2, yes = 1, no = 2)]
  desagg_03[, PWID := ifelse(test = KeyPopulation == 3, yes = 1, no = 2)]
  desagg_03[, Migrant := ifelse(test = KeyPopulation == 4, yes = 1, no = 2)]}

# Dateofvisit preproc.
{desagg_01[, DateofVisit := as.Date(DateofVisit, format= "%d.%m.%Y")]
desagg_04[, DateofVisit := as.Date(DateofVisit, format= "%d/%m/%Y")]}


# ----------------------------------------------------------- #
## DESAGG:___UNIFORM VARNAMES                                ####
# ----------------------------------------------------------- #
uniform_names <- function(dt, mapDT, mapDT_dtname, mapDT_RefName) {
  ## PARAMETRES: 
  #   - dt: dataframe a unificar el nom de les seves variables amb les del diccionari de dades.
  #   - mapDT: dataframe diccionari on es mapegen les variables dels datasets per nom.
  #   - mapDT_dtname:  En el dataframe diccionari, el nom de la columna amb els noms de variables del dt. 
  #   - mapDT_RefName: En el dataframe diccionari, el nom de la columna amb els noms de variables del diccionari.
  
  #   0. Detectem i guardem les noves variables generades durant el preprocessament anterior.
  dt_prerpoc_new_varnames <- colnames(dt)[!colnames(dt) %in% mapDT[, get(mapDT_dtname)] &
                                            colnames(dt) %in% mapDT[, get(mapDT_RefName)]]
  new_cols <- dt[, ..dt_prerpoc_new_varnames]
  #   1. Seleccionem variables de DT que es corresponguin amb les del Diccionari. 
  dt_COBA_names <- mapDT[!is.na(get(mapDT_dtname)), get(mapDT_dtname)]
  dt <- dt[, ..dt_COBA_names]
  #   2. Reanomenem les variables restants de DT en funcio del nom de referencia al Diccionari.
  ref_COBA_names <- mapDT[!is.na(get(mapDT_dtname)), get(mapDT_RefName)]
  setnames(x = dt, old = colnames(dt), new = ref_COBA_names)
  #   3. Afegim les noves variables generades durant el preprocessament.
  dt <- cbind(dt, new_cols)
  return(dt)}


{desagg_00 <- uniform_names(dt = desagg_00, mapDT = col_maps, mapDT_dtname = "DDBB_stata_final", mapDT_RefName = "COBATEST_Variable_name")
  desagg_01 <- uniform_names(dt = desagg_01, mapDT = col_maps, mapDT_dtname = "Czech_AIDS_help_society", mapDT_RefName = "COBATEST_Variable_name")
  desagg_02 <- uniform_names(dt = desagg_02, mapDT = col_maps, mapDT_dtname = "Aids_Fondet_Centre06", mapDT_RefName = "COBATEST_Variable_name")
  desagg_03 <- uniform_names(dt = desagg_03, mapDT = col_maps, mapDT_dtname = "AidsHilfeWien", mapDT_RefName = "COBATEST_Variable_name")
  desagg_04 <- uniform_names(dt = desagg_04, mapDT = col_maps, mapDT_dtname = "Medicinsdumunde", mapDT_RefName = "COBATEST_Variable_name")
  desagg_05 <- uniform_names(dt = desagg_05, mapDT = col_maps, mapDT_dtname = "Espace_P_Belgium", mapDT_RefName = "COBATEST_Variable_name")
  desagg_06 <- uniform_names(dt = desagg_06, mapDT = col_maps, mapDT_dtname = "Sida_Sol_Liege", mapDT_RefName = "COBATEST_Variable_name")
  desagg_07 <- uniform_names(dt = desagg_07, mapDT = col_maps, mapDT_dtname = "Violett", mapDT_RefName = "COBATEST_Variable_name")
  desagg_08 <- uniform_names(dt = desagg_08, mapDT = col_maps, mapDT_dtname = "Exaequo", mapDT_RefName = "COBATEST_Variable_name")
  desagg_09 <- uniform_names(dt = desagg_09, mapDT = col_maps, mapDT_dtname = "Adhara", mapDT_RefName = "COBATEST_Variable_name")
  desagg_10 <- uniform_names(dt = desagg_10, mapDT = col_maps, mapDT_dtname = "CJAS", mapDT_RefName = "COBATEST_Variable_name")
  rm(uniform_names)}


# Afegim els codis dels "Centres" aquí. El processat fort es fa en un punt més endavant. Sobre el dataset complet.
{desagg_01[, Centre := 61]   # Czech AIDS help society
  desagg_02[, Centre := 28]  # Aids Fondet
  desagg_03[, Centre := 62]  # Aids Hilfe Wien
  desagg_04[, Centre := 55]  # Medicins du munde
  desagg_05[, Centre := 56]  # EspaceP
  desagg_06[, Centre := 57]  # Sidasol
  desagg_07[, Centre := 58]  # Violett
  desagg_08[, Centre := 59]  # exaequo
  desagg_09[, Centre := 60]  # Adhara
  desagg_10[, Centre := 8]}  # CJAS


# ----------------------------------------------------------- #
## DESAGG:___AFEGIM MISSING VARS                           ####
# ----------------------------------------------------------- #
complete_missing_cols <- function(dt, mapDT, mapDT_RefName) {
  # Columnes que no es troben a dt. 
  mis_cols <- mapDT[!COBATEST_Variable_name %in% colnames(dt), COBATEST_Variable_name] 
  if (length(mis_cols) != 0) dt[, (mis_cols) := as.character(NA)]  # Afegides amb NAs. 
  return(dt)}


{desagg_00 <- complete_missing_cols(dt = desagg_00, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")                    
  desagg_01 <- complete_missing_cols(dt = desagg_01, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")                     
  desagg_02 <- complete_missing_cols(dt = desagg_02, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")               
  desagg_03 <- complete_missing_cols(dt = desagg_03, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name") 
  desagg_04 <- complete_missing_cols(dt = desagg_04, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")     
  desagg_05 <- complete_missing_cols(dt = desagg_05, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")       
  desagg_06 <- complete_missing_cols(dt = desagg_06, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")   
  desagg_07 <- complete_missing_cols(dt = desagg_07, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")     
  desagg_08 <- complete_missing_cols(dt = desagg_08, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")     
  desagg_09 <- complete_missing_cols(dt = desagg_09, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")   
  desagg_10 <- complete_missing_cols(dt = desagg_10, mapDT = col_maps, mapDT_RefName = "COBATEST_Variable_name")
  rm(complete_missing_cols)}


# ----------------------------------------------------------- #
## DESAGG:___REORDENAR VARS                                ####
# ----------------------------------------------------------- #
{new_order <- col_maps$COBATEST_Variable_name
desagg_00 <- desagg_00[, ..new_order]
desagg_01 <- desagg_01[, ..new_order]
desagg_02 <- desagg_02[, ..new_order]
desagg_03 <- desagg_03[, ..new_order]
desagg_04 <- desagg_04[, ..new_order]
desagg_05 <- desagg_05[, ..new_order]
desagg_06 <- desagg_06[, ..new_order]
desagg_07 <- desagg_07[, ..new_order]
desagg_08 <- desagg_08[, ..new_order]
desagg_09 <- desagg_09[, ..new_order]
desagg_10 <- desagg_10[, ..new_order]
rm(new_order)}


# ----------------------------------------------------------- #
## DESAGG:___ROW BIND DATASETS                             ####
# ----------------------------------------------------------- #
# Fem aquest pas previ al Row bind pq no dongui problemes de tipus de dades. 
# Uniformem totes les variables a tipus Character.
o <- mapply(FUN= function(x) x[, (colnames(x)) := lapply(.SD, function(y) as.character(y)), .SDcols= colnames(x)], 
              mget(ls(pattern = "desagg"))); rm(o)

# Row-bind tots els datasets.
cobatest_tool_final <- Reduce(function(...) rbind(..., fill= TRUE), mget(ls(pattern = "desagg")))


# ----------------------------------------------------------- #
## DESAGG:___RECATEGORITZACIO                              ####
# ----------------------------------------------------------- #
# Recategorització de variables.
{cobatest_tool_final[!Gender %in% c("1","2","3"), Gender := NA]
cobatest_tool_final[!AgeGroup %in% c("1","2"), AgeGroup := NA]
cobatest_tool_final[!MSM %in% c("1","2"), MSM := NA]
cobatest_tool_final[!SW %in% c("1","2"), SW := NA]
cobatest_tool_final[!PWID %in% c("1","2"), PWID := NA]
cobatest_tool_final[!Migrant %in% c("1","2"), Migrant := NA]
cobatest_tool_final[!EverTested %in% c("1","2"), EverTested := NA]
cobatest_tool_final[!ResultLastHIV %in% c("1","2"), ResultLastHIV := NA]
cobatest_tool_final[!TestedLastYear %in% c("1","2"), TestedLastYear := NA]
cobatest_tool_final[!TestedLastYearSameCBVCT %in% c("1","2"), TestedLastYearSameCBVCT := NA]
cobatest_tool_final[!PreTestCounselling %in% c("1","2"), PreTestCounselling := NA]
cobatest_tool_final[!ScreeningHIVTest %in% c("1","2"), ScreeningHIVTest := NA]
cobatest_tool_final[!ScreeningTestResult %in% c("1","2"), ScreeningTestResult := NA]
cobatest_tool_final[!ScreeningTestResultReceived %in% c("1","2"), ScreeningTestResultReceived := NA]
cobatest_tool_final[!ScreeningPostTestCounselling %in% c("1","2"), ScreeningPostTestCounselling := NA]
cobatest_tool_final[!ConfirmatoryHIVTest %in% c("1","2"), ConfirmatoryHIVTest := NA]
cobatest_tool_final[!ConfirmatoryHIVTestResult %in% c("1","2","3"), ConfirmatoryHIVTestResult := NA]
cobatest_tool_final[!ConfirmatoryTestResultReceived %in% c("1","2"), ConfirmatoryTestResultReceived := NA]
cobatest_tool_final[!LinkageToHealthCare %in% c("1","2"), LinkageToHealthCare := NA]
cobatest_tool_final[CD4Count %in% c("9999","?"), CD4Count := NA]
cobatest_tool_final[!HIVTestUsed %in% c("1","2","3"), HIVTestUsed := NA]
cobatest_tool_final[!SyphEverDiagnosed %in% c("1","2"), SyphEverDiagnosed := NA]
cobatest_tool_final[!SyphEverTested %in% c("1","2"), SyphEverTested := NA]
cobatest_tool_final[!SyphTestedLastYear %in% c("1","2"), SyphTestedLastYear := NA]
cobatest_tool_final[!SyphTestedLastYearSameCBVCT %in% c("1","2"), SyphTestedLastYearSameCBVCT := NA]
cobatest_tool_final[!SyphScreeningTestResult %in% c("1","2"), SyphScreeningTestResult := NA]
cobatest_tool_final[!SyphConfirmatoryTest %in% c("1","2"), SyphConfirmatoryTest := NA]
cobatest_tool_final[!SyphConfirmatoryTestResult %in% c("1","2","3"), SyphConfirmatoryTestResult := NA]
cobatest_tool_final[!SyphTestUsed %in% c("1","2"), SyphTestUsed := NA]
cobatest_tool_final[!HCVEverDiagnosed %in% c("1","2"), SyphTestUsed := NA]
cobatest_tool_final[!HCVEverTested %in% c("1","2"), HCVEverTested := NA]
cobatest_tool_final[!HCVTestedLastYear %in% c("1","2"), HCVTestedLastYear := NA]
cobatest_tool_final[!HCVTestedLastYearSameCBVCT %in% c("1","2"), HCVTestedLastYearSameCBVCT := NA]
cobatest_tool_final[!HCVScreeningTestResult %in% c("1","2"), HCVScreeningTestResult := NA]
cobatest_tool_final[!HCVRNATest %in% c("1","2"), HCVRNATest := NA]
cobatest_tool_final[!HCVConfirmatoryTestResult %in% c("1","2","3"), HCVConfirmatoryTestResult := NA]
cobatest_tool_final[!HCVTestUsed %in% c("1","2","3"), HCVTestUsed := NA]
cobatest_tool_final[!HCVScreeningTestResult %in% c("1","2"), HCVScreeningTestResult := NA]}


# ----------------------------------------------------------- #
## DESAGG:___PROCESSAT DE DADES                            ####
# ----------------------------------------------------------- #
# Declarem variables numeriques

## JORDI -- 28-01-2021
#  OJU!!
#   - Al fer canvi de tipus de character a numeric, S'introduiran NAs per coertion a la variable CD4Count.
#     Son els valors "" (12745) mes el valor "0,40" per tenir separador decimal ",". Com que no hi ha 
#     cap indicador que treballi amb CD4Count, de moment no apliquem cap tractament en aquest aspecte.
numeric_vars <- c('Centre','Gender','AgeInYears','AgeGroup','MSM','SW','PWID','Migrant','EverTested','ResultLastHIV',
                  'TestedLastYear','TestedLastYearSameCBVCT','PreTestCounselling','ScreeningHIVTest',"ScreeningTestResult",
                  "ScreeningTestResultReceived","ScreeningPostTestCounselling",
                  "ConfirmatoryHIVTest","ConfirmatoryHIVTestResult","ConfirmatoryTestResultReceived",
                  "LinkageToHealthCare","CD4Count","HIVTestUsed","SyphEverDiagnosed","SyphEverTested",
                  "SyphTestedLastYear","SyphTestedLastYearSameCBVCT","SyphScreeningTestResult",
                  "SyphConfirmatoryTest","SyphConfirmatoryTestResult","SyphTestUsed","HCVEverDiagnosed",
                  "HCVEverTested","HCVTestedLastYear","HCVTestedLastYearSameCBVCT","HCVScreeningTestResult",
                  "HCVRNATest","HCVConfirmatoryTestResult","HCVTestUsed")
cobatest_tool_final[, (numeric_vars) := lapply(.SD, function(x) as.numeric(x)), .SDcols= numeric_vars]


# Blanks, de variables tipus character, a NAs.
character_vars <- c('CBVCTIdentifier','COBATEST_Id','new_id','Id','DateOfBirth','DateofVisit',
                    "DateScreeningTestResult","DateConfirmatoryTest","DateConfirmatoryTestResult","Datedlinkage",
                    "DateCD4Count")
cobatest_tool_final[, (character_vars) := lapply(.SD, function(x) ifelse(x=='', NA, x)), .SDcols= character_vars]


# # MEGI 19-02-2021:  Extraccion centros Belgas: 45, 46, 47, 48, 49, 50, 51, 55, 56, 57, 58, 59
# belgas <- cobatest_tool_final[Centre %in% c(45, 46, 47, 48, 49, 50, 51, 55, 56, 57, 58, 59), ]
# belgas <- merge(x = belgas, y= center_maps[, .(Centre, CentreName)], by.x= "Centre", by.y= "Centre", all.x= T)
# new_cols <- c( "Centre","CentreName","CBVCTIdentifier","COBATEST_Id","new_id","Id","Gender","AgeInYears","DateOfBirth",
#                "AgeGroup","MSM","SW","PWID","Migrant","DateofVisit","EverTested","ResultLastHIV","TestedLastYear",
#                "TestedLastYearSameCBVCT","PreTestCounselling","ScreeningHIVTest","ScreeningTestResult",
#                "DateScreeningTestResult","ScreeningTestResultReceived","ScreeningPostTestCounselling",
#                "ConfirmatoryHIVTest","DateConfirmatoryTest","ConfirmatoryHIVTestResult","DateConfirmatoryTestResult",
#                "ConfirmatoryTestResultReceived","LinkageToHealthCare","Datedlinkage","CD4Count",
#                "DateCD4Count","HIVTestUsed","SyphEverDiagnosed","SyphEverTested","SyphTestedLastYear",
#                "SyphTestedLastYearSameCBVCT" ,"SyphScreeningTestResult","SyphConfirmatoryTest","SyphConfirmatoryTestResult","SyphTestUsed"                ,   "HCVEverDiagnosed"             ,  "HCVEverTested"                 ,
#                 "HCVTestedLastYear","HCVTestedLastYearSameCBVCT","HCVScreeningTestResult","HCVRNATest",
#                "HCVConfirmatoryTestResult","HCVTestUsed")
# setcolorder(x = belgas, neworder = new_cols)
# # COMPROBACIO:  belgas[, .N, by= .(Centre, CentreName)] # Registres per centre.
# fwrite(belgas, file = 'Outputs/datasets/belgas_cobatool.csv')


# Ajuntem alguns centres Belgues sota un únic centre:  BeTested ('Aimer Jeunes','Jette','Marolles','SIPS','Uccle','Watermael-Boitsfort')
cobatest_tool_final[Centre %in% c(45,46,48,49,50,51),  Centre := 71]

# Afegim el nom del centre
cobatest_tool_final <- merge(x = cobatest_tool_final, y= center_maps[, .(Centre, CentreName)], by.x= "Centre", by.y= "Centre", all.x= T)


# Triem de l'ID, dels 4 que hi ha, que cada centre utilitza per buscar duplicats.
{cobatest_tool_final[Centre %in% center_maps[Origen_2019 == "Cobatest_tool", Centre] , Id_main := Id]         # BBDD Cobatest Tool
  cobatest_tool_final[Centre == 61, Id_main := Id]          # Czech AIDS help society
  cobatest_tool_final[Centre == 28, Id_main := Id]          # Aids Fondet
  cobatest_tool_final[Centre == 62, Id_main := Id]          # Aids Hilfe Wien
  cobatest_tool_final[Centre == 55, Id_main := Id]          # Medicins du munde
  cobatest_tool_final[Centre == 56, Id_main := new_id]      # EspaceP
  cobatest_tool_final[Centre == 57, Id_main := Id]          # Sidasol
  cobatest_tool_final[Centre == 58, Id_main := Id]          # Violett
  cobatest_tool_final[Centre == 59, Id_main := Id]          # exaequo
  cobatest_tool_final[Centre == 60, Id_main := COBATEST_Id] # Adhara
  cobatest_tool_final[Centre == 8, Id_main := Id]}          # CJAS


rm(list= ls(pattern = "desagg"))
rm(numeric_vars, character_vars, col_maps)


# ----------------------------------------------------------- #
## DESAGG:___MISSINGS                                      ####
# ----------------------------------------------------------- #
Missings <- as.data.frame(lapply(FUN = function(x) round(colSums(is.na(x))/nrow(x)*100, digits = 1), X = mget(ls(pattern = "desagg"))))
Missings$Variables <- row.names(Missings)

date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = Missings, file = paste0(output_path,"missings_cobatest_tool_final_",date,".xlsx"))
rm(Missings, output_path, date)




# ________________________________________________________ ####
## HIV INDICATORS                                          ####
# ******************************************************** ####
# ----------------------------------------------------------- #
## VIH TESTS                                               ####
# ----------------------------------------------------------- #
# Filtrem dataset pels que han estat testats per VIH.
vih_tests <- copy(cobatest_tool_final[ScreeningHIVTest == 1])

# Processat de les edats.
# LAURA: Respecto a los menores de 16 años, he revisado los casos, y he visto que los casos de menores reales
#       (no atribuïbles a errores de data entry con la fecha de nacimiento) tienen informado el grupo de edad. Así que si 
#       la edad és inferior a 16 años y AgeGroupc== 1, se deberían eliminar. Los que quedan, menores de 16, son 
#       errores en el data entry. Deben tener AgeGroup a NA pero podemos sacar la otra informacion.
# COMPROBACION: cobatest_tool_final[AgeInYears < 16, .N, by= .(trunc_age= trunc(AgeInYears),AgeGroup)][order(trunc_age)]
vih_tests[, Menors_reals := ifelse(test = AgeInYears < 16 & AgeGroup == 1, yes = 1, no = 0)]
vih_tests <- vih_tests[Menors_reals != 1 | is.na(Menors_reals), ][, Menors_reals := NULL]

# ----------------------------------------------------------- #
## VIH SENSE DUPLICATS                                     ####
# ----------------------------------------------------------- #
## Busquem duplicats de VIH
# - LAURA ---------------------- #
#   En nuestra BBDD, generamos la variable “Id”, para poder identificar repetidores y quedarnos únicamente con las personas testadas, 
#   y poder calcular los indicadores con las personas testadas. 
#   Los centros pueden haber usado cualquiera de estos nombres de variables para su identificador único de cliente. He marcado en 
#   rojo en el Excel de las variables la variable que se debe usar para cada centro, para identificar repetidores. 

# COMPROBACION: vih_tests[, .N]                                                        # registres
# COMPROBACION: vih_tests[, .N] - vih_tests[, uniqueN(Id_main)]                        # registres duplicats
# COMPROBACION: vih_tests[, uniqueN(Id_main, na.rm = T)]                               # persones
# COMPROBACION: vih_tests[, .N, by = .(Id_main)][N > 1, uniqueN(Id_main, na.rm = T)]   # persones repetides
# COMPROBACION: vih_tests[, .N, by = .(Id_main)][N > 4, Id_main]                       # Ids: "00809196910R" "21811197601M" "21208198710N" ...
# COMPROBACION: vih_tests[, .N, by = .(Id_main)][N > 1, ][order(Id_main), Id_main]


## COMPTE!! 
#  Hi ha uns codis a Id_main: .00, .00M, 0000, 000A, 000B, 000G, 000L, 000M, 000P, 000R i 000S,
#  Que poden estar duplicats i estan referits a persones diferents:
# t(vih_tests[Id_main == ".00"]) # només hi ha un
# t(vih_tests[Id_main == ".00M"])   # n'hi ha dos però semblen diferents pel cobatest id
# t(vih_tests[Id_main == "0000"])   # només hi ha un
# t(vih_tests[Id_main == "000A"])   # hi ha 4. tenen cobatest id diferents
# t(vih_tests[Id_main == "000B"])   # hi ha 1. Té cobatest id.
# t(vih_tests[Id_main == "000G"])   # hi ha 2. Tenen cobatest id diferents.
# t(vih_tests[Id_main == "000L"])   # hi ha 3. Tenen cobatest id diferents.
# t(vih_tests[Id_main == "000M"])   # hi ha 4. Tenen cobatest id diferents.
# t(vih_tests[Id_main == "000P"])   # hi ha 1. Té cobatest id.
# t(vih_tests[Id_main == "000R"])   # hi ha 1. Té cobatest id.
# t(vih_tests[Id_main == "000S"])   # hi ha 1. Té cobatest id.
## Adjudiquem-los un id: cobatest_id U cbvctidentifier.
{vih_tests[Id_main == ".00", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == ".00M", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "0000", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000A", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000B", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000G", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000L", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000M", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000P", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000R", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
vih_tests[Id_main == "000S", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]}

# COMPROBACION: vih_tests[, .N]                                                        # registres
# COMPROBACION: vih_tests[, .N] - vih_tests[, uniqueN(Id_main)]                        # registres duplicats
# COMPROBACION: vih_tests[, uniqueN(Id_main, na.rm = T)]                               # persones
# COMPROBACION: vih_tests[, .N, by = .(Id_main)][N > 1, uniqueN(Id_main, na.rm = T)]   # persones repetides


# Elegim, dels duplicats els de data més recent.
# Com funciona:
# A <- copy(vih_tests[Centre == 5, .(Id_main, DateofVisit)])
# A[, .N, by= is.na(Id_main)]
# A[, DateofVisit := as.Date(DateofVisit)]
# A[, Repeted := duplicated(Id_main) ]
# A[, which_max := .I[which.max(DateofVisit)] , by= Id_main]
# B1 <- A[A[, .I[which.max(DateofVisit)], by = Id_main]$V1]
# B2 <- A[A[!is.na(Id_main), .I[which.max(DateofVisit)], by = Id_main]$V1]
vih_tests[, DateofVisit := as.Date(DateofVisit)]

# Traiem duplicats tenint en compte que els Centres 28 (Aids Fondet) i 59 (Exaequo)  no tenen Id_main.
# vih_tested <- vih_tests[vih_tests[!is.na(Id_main), .I[which.max(DateofVisit)], by = Id_main]$V1]
vih_tests_noIdmain <- vih_tests[is.na(Id_main),]
vih_tests_Idmain <- vih_tests[!is.na(Id_main),]

## LAURA (20-12-2020):  De moment no treure els duplicats id que surten per visitar-se en diferents centres.
# Fora duplicats.
# vih_tested_Idmain <- vih_tests_Idmain[vih_tests_Idmain[, .I[which.max(DateofVisit)], by = Id_main]$V1] # Treiem duplicats per id
vih_tested_Idmain <- vih_tests_Idmain[vih_tests_Idmain[, .I[which.max(DateofVisit)], by = .(Centre, Id_main)]$V1]   # Treiem duplicats per id-Centre

# Assumim que no hi ha duplicats en els que no tenen id_main (Centres 28 i 59 no tenen Id_main).
vih_tested <- rbind(vih_tested_Idmain, vih_tests_noIdmain) 


# COMPROBACIO:  vih_tests[,.N] - vih_tested[,.N] # Test prior to most recent for those tested more than once


rm(vih_tests_Idmain,vih_tests_noIdmain,vih_tested_Idmain)

# ----------------------------------------------------------- #
## VIH AMB HIV RESULT                                      ####
# ----------------------------------------------------------- #
# COMPROBACIO:  vih_tested[, .N, by= .(ScreeningTestResult)]       # NA:  Els que no tenen HIV screening test result available. 
# vih_tested[is.na(ScreeningTestResult), .N, by= .(CentreName, ScreeningTestResult)][order(CentreName)] 
# vih_tested[is.na(ScreeningTestResult), .N, by= .(ScreeningTestResult,ScreeningHIVTest)][order(ScreeningTestResult)]
# vih_tested[is.na(ScreeningTestResult), .N, by= .(ScreeningTestResult,EverTested)][order(EverTested)]
# vih_tested[is.na(ScreeningTestResult), .N, by= .(ScreeningTestResult,TestedLastYear)][order(TestedLastYear)]
# vih_tested[is.na(ScreeningTestResult), .N, by= .(ScreeningTestResult,TestedLastYearSameCBVCT)][order(TestedLastYearSameCBVCT)]
# vih_tested[is.na(ScreeningTestResult), .N, by= .(ScreeningTestResult,ConfirmatoryHIVTestResult)][order(ConfirmatoryHIVTestResult)]
vih_tested_final <- vih_tested[!is.na(ScreeningTestResult)]


# ----------------------------------------------------------- #
## VIH SENSE DIAGN PREVIS                                  ####
# ----------------------------------------------------------- #
# COMPROBACIO:  vih_tested[, .N, by= .(ScreeningTestResult)]       # NA:  Els que no tenen HIV screening test result available. 
# COMPROBACIO:  vih_tested[, .N, by= .(ResultLastHIV)][order(N)]   # 1:   Previament diagnosticats
# COMPROBACIO:  vih_tested[!is.na(ScreeningTestResult), .N, by= .(ResultLastHIV)][order(N)]   # 1: Previament diagnosticats que tenen HIV results available. 

## LAURA (20-11-2020):  No eliminar los ResultLastHIV == 1 con ScreeningTestResult == 2.
# COMPROBACIO:   vih_tested_final[ResultLastHIV == 1 & ScreeningTestResult == 2, .(CentreName,COBATEST_Id,ResultLastHIV,ScreeningTestResult)]
# Tenim combinacions:  
# vih_tested_final[, .N, by= .(ResultLastHIV, ScreeningTestResult)][order(ResultLastHIV, ScreeningTestResult)]
#
# (LastHIVResult, ScreeningTestResult) = {(1, 1), (1, 2), (2, 1), (2,2), (NA,1), (NA,2) }
# 
# Hem de treure els (LastHIVResult, ScreeningTestResult) = (1, 1). La resta no.
vih_tested_final <- vih_tested_final[!(ResultLastHIV == 1 & ScreeningTestResult == 1) | is.na(ResultLastHIV)]


# ----------------------------------------------------------- #
## PLANTILLA VIH DADES AGGS                                ####
# ----------------------------------------------------------- #
agg_base_path <- "Data/Received Aggregated data 2019/Aggregated data instructions/Aggregated data excel.xlsx"
agg_base <- setDT(read.xlsx(agg_base_path, sheet = 'HIV Indicators', colNames = FALSE));rm(agg_base_path)


# ----------------------------------------------------------- #
## LOAD VIH AGREGADES                                      ####
# ----------------------------------------------------------- #

# IMPORTANT! 
# Hi ha un pas de preprocessament de dades fet en un excel. 
# 1. Totes les dades agregades segueixen un formulari excel que conté varies pagines amb
#    diferents taules CBVCT en cada pagina.
#    Per la taula CBVCT_1, cal que tots els valors estiguin introduïts. NAs a 0s o a totals.
# 2. Per cada pàgina i per cada taula CBVCT, farem una nova taula afegint les taules dels centres
#    unes a continuació de les altres (row bind). Una nova columna identificarà les dades de cada centre.
# 3. La taula resultant ens permetra agregar per centres.


{aggs_path_1 <- "Data/Received Aggregated data 2019/Aggregated data Received files 2019/"
aggs_path_2 <- "Data/New Data 09 2019/Aggregated data/"
agg_01 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel 2019 _Poland.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_02 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_aides_01012019_31122019.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_03 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_Asocijacija DUGA_Association RAINBOW.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_04 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_HUHIV (1).xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_05 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_LEGEBITRA.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_06 <- setDT(read.xlsx(paste0(aggs_path_1, "Cobatest 2019 Aggregated data excel - Abraço.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_07 <- setDT(read.xlsx(paste0(aggs_path_1, "GDM_Moldavia_Aggregated data excel_updated.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_08 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data excel_ALLIANCE.GLOBAL_updated.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_09 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data report - Fulcrum - 2019_updated.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
agg_10 <- setDT(read.xlsx(paste0(aggs_path_2, "COBATEST.Aggregated data excel.DEMETRA_2019.xlsx"), sheet= 'HIV Indicators', colNames = FALSE))
rm(aggs_path_1, aggs_path_2)}


# IMPORTANT!!!!
# Aquesta part extreu dades dels fitxers excels que els centres han carregat les dades agregades i que
# segueixen una plantilla donada (2019:  rows: 207  cols: 8).
# Per al bon funcionament del codi es fa la FORTA SUPOSICIO de que no ha canviat la seva estructura.
# Assegurar-se sempre que s'estan agafant les taules desitjades.


# ----------------------------------------------------------- #
## CBVCT_1                                                 ####
# ----------------------------------------------------------- #
# CBVCT 1:  Number of clients tested for HIV with a screening test.
# Number of clients screened for HIV (hivtestused != NA or screeningtestresult != NA) and stratified 
# by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted in more than one of the 
# following; msm, sw, pwid, migrants.


# Adquisicio de taules de denominadors (CBVCT_1):
get_CBVCTtable <- function(dt, data_rows_idx, data_cols_idx, names_row_idx= NA) {
  table <- dt[data_rows_idx, ..data_cols_idx]
  if(!is.na(names_row_idx)) {colnames(table) <- c("Category", unlist(dt[names_row_idx, ..data_cols_idx])[-1])}
  return(table)}

{rows <- c(8:12); cols <- c(1:7); names <- 7
agg_00_hivCBVCT_1 <- get_CBVCTtable(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_01_hivCBVCT_1 <- get_CBVCTtable(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_02_hivCBVCT_1 <- get_CBVCTtable(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_03_hivCBVCT_1 <- get_CBVCTtable(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_04_hivCBVCT_1 <- get_CBVCTtable(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_05_hivCBVCT_1 <- get_CBVCTtable(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_06_hivCBVCT_1 <- get_CBVCTtable(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_07_hivCBVCT_1 <- get_CBVCTtable(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_08_hivCBVCT_1 <- get_CBVCTtable(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_09_hivCBVCT_1 <- get_CBVCTtable(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
agg_10_hivCBVCT_1 <- get_CBVCTtable(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
rm(rows,cols,names)}

# Afegim columna Centre
{agg_00_hivCBVCT_1[, Centre:= "BBDD Final"]
agg_01_hivCBVCT_1[, Centre:= "Poland National AIDS Centre"]
agg_02_hivCBVCT_1[, Centre:= "Aides"]
agg_03_hivCBVCT_1[, Centre:= "Asocijacija Duga - Rainbow"]
agg_04_hivCBVCT_1[, Centre:= "HUHIV"]
agg_05_hivCBVCT_1[, Centre:= "Legebitra"]
agg_06_hivCBVCT_1[, Centre:= "Associação Abraço"]
agg_07_hivCBVCT_1[, Centre:= "GenderdocM"]
agg_08_hivCBVCT_1[, Centre:= "Alliance GLobal"]
agg_09_hivCBVCT_1[, Centre:= "Fulcrum"]
agg_10_hivCBVCT_1[, Centre:= "Demetra"]}

# Fem el RowBind
hivCBVCT_1 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_1"))); rm(list= ls(pattern = "_hivCBVCT_1"))


# Poblem valors de "BBDD Final" per a CBVCT_1
{hivCBVCT_1[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1, .N]]
hivCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & Gender == 1, .N]]
hivCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Females := NA]
hivCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & Gender == 3, .N]]
hivCBVCT_1[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & AgeGroup == 1, .N]]
hivCBVCT_1[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & AgeGroup == 2, .N]]
hivCBVCT_1[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1, .N]]
hivCBVCT_1[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & Gender == 1, .N]]
hivCBVCT_1[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & Gender == 2, .N]]
hivCBVCT_1[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & Gender == 3, .N]]
hivCBVCT_1[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & AgeGroup == 1, .N]]
hivCBVCT_1[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & AgeGroup == 2, .N]]
hivCBVCT_1[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1, .N]]
hivCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & Gender == 1, .N]]
hivCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & Gender == 2, .N]]
hivCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & Gender == 3, .N]]
hivCBVCT_1[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & AgeGroup == 1, .N]]
hivCBVCT_1[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & AgeGroup == 2, .N]]
hivCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1, .N]]
hivCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & Gender == 1, .N]]
hivCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & Gender == 2, .N]]
hivCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & Gender == 3, .N]]
hivCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & AgeGroup == 1, .N]]
hivCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & AgeGroup == 2, .N]]
hivCBVCT_1[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[, .N]]
hivCBVCT_1[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[Gender == 1, .N]]
hivCBVCT_1[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[Gender == 2, .N]]
hivCBVCT_1[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[Gender == 3, .N]]
hivCBVCT_1[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[AgeGroup == 1, .N]]
hivCBVCT_1[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[AgeGroup == 2, .N]]}

# fem les sumes
denoms_CBVCT_Totals <- hivCBVCT_1[, .(All= sum(as.numeric(All), na.rm = T), 
                                      Males= sum(as.numeric(Males), na.rm = T), 
                                      Females= sum(as.numeric(Females), na.rm = T), 
                                      Transgender= sum(as.numeric(Transgender), na.rm = T),
                                      `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                      `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                  by= .(Category)]

# # Tornem a posar el nom original de la columna Category.
# setnames(result_hivCBVCT_1, old= "Category", new= unlist(agg_base[7,1]))
# Reconstruim la taula CBVCT1.
result_hivCBVCT_01 <- get_CBVCTtable(agg_base, data_rows_idx = c(6:12), data_cols_idx = c(1:7))
result_hivCBVCT_01[c(3:7), c(1:7)] <- denoms_CBVCT_Totals
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_01 <- rbind(result_hivCBVCT_01, result_hivCBVCT_01[NA]) 

# Comencem a construir una llista amb les dades aggregades rellevants.
hivCBVCT_1[, Descriptiu := 'CBVCT_1']
HIV_data_agregades <- list(CBVCT_1= hivCBVCT_1)
rm(hivCBVCT_1)


# ----------------------------------------------------------- #
## CBVCT_2                                                 ####
# ----------------------------------------------------------- #
# CBVCT 2:  Proportion of clients who reported to have been previously tested for HIV.
# Reporting people previously tested for HIV (evertested=1) as a proportion of all people tested 
# and stratified by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted in more than 
# one of the following; msm, sw, pwid, migrants.
#
#    Number of clients who reported to have been previously tested for HIV (evertested == 1)
#  ------------------------------------------------------------------------------------------- x 100
#                        Number of clients screened for HIV 

get_CBVCTtable_rowsdata <- function(dt, data_rows_idx, data_cols_idx, names_row_idx, categories_rows_idx) {
  table <- dt[data_rows_idx, ..data_cols_idx]
  colnames(table) <- c("Category", unlist(dt[names_row_idx, ..data_cols_idx])[-1])
  table[, Category := dt[categories_rows_idx, 1]]
  return(table)}

# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 2 total: Els numeradors.
{rows <- c(17,21,25,29,33); cols <- c(1:7); names <- 14; cat_rows <- c(15,19,23,27,31)
  agg_00_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_01_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_02_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_03_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_04_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_05_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_06_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_07_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_08_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_09_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_10_hivCBVCT_2 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  rm(rows,cols,names,cat_rows)}

# Afegim columna Centre
{agg_00_hivCBVCT_2[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_2[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_2[, Centre:= "Aides"]
  agg_03_hivCBVCT_2[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_2[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_2[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_2[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_2[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_2[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_2[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_2[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_2 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_2"))); rm(list= ls(pattern = "_hivCBVCT_2"))

# Poblem valors de "BBDD Final" per a CBVCT_2
{numeradors_CBVCT_2[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & EverTested == 1, .N]]
numeradors_CBVCT_2[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & EverTested == 1 & Gender == 1, .N]]
numeradors_CBVCT_2[Category == "MSM" & Centre == "BBDD Final", Females := NA]
numeradors_CBVCT_2[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & EverTested == 1 & Gender == 3, .N]]
numeradors_CBVCT_2[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & EverTested == 1 & AgeGroup == 1, .N]]
numeradors_CBVCT_2[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & EverTested == 1 & AgeGroup == 2, .N]]
numeradors_CBVCT_2[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & EverTested == 1, .N]]
numeradors_CBVCT_2[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & EverTested == 1 & Gender == 1, .N]]
numeradors_CBVCT_2[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & EverTested == 1 & Gender == 2, .N]]
numeradors_CBVCT_2[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & EverTested == 1 & Gender == 3, .N]]
numeradors_CBVCT_2[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & EverTested == 1 & AgeGroup == 1, .N]]
numeradors_CBVCT_2[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & EverTested == 1 & AgeGroup == 2, .N]]
numeradors_CBVCT_2[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & EverTested == 1, .N]]
numeradors_CBVCT_2[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & EverTested == 1 & Gender == 1, .N]]
numeradors_CBVCT_2[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & EverTested == 1 & Gender == 2, .N]]
numeradors_CBVCT_2[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & EverTested == 1 & Gender == 3, .N]]
numeradors_CBVCT_2[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & EverTested == 1 & AgeGroup == 1, .N]]
numeradors_CBVCT_2[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & EverTested == 1 & AgeGroup == 2, .N]]
numeradors_CBVCT_2[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & EverTested == 1, .N]]
numeradors_CBVCT_2[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & EverTested == 1 & Gender == 1, .N]]
numeradors_CBVCT_2[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & EverTested == 1 & Gender == 2, .N]]
numeradors_CBVCT_2[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & EverTested == 1 & Gender == 3, .N]]
numeradors_CBVCT_2[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & EverTested == 1 & AgeGroup == 1, .N]]
numeradors_CBVCT_2[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & EverTested == 1 & AgeGroup == 2, .N]]
numeradors_CBVCT_2[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[EverTested == 1, .N]]
numeradors_CBVCT_2[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[EverTested == 1 & Gender == 1, .N]]
numeradors_CBVCT_2[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[EverTested == 1 & Gender == 2, .N]]
numeradors_CBVCT_2[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[EverTested == 1 & Gender == 3, .N]]
numeradors_CBVCT_2[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[EverTested == 1 & AgeGroup == 1, .N]]
numeradors_CBVCT_2[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[EverTested == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_2_Totals <- numeradors_CBVCT_2[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]

# Warning message:  NAs introducidos por coerción.  a Females hi ha un "Na" enlloc de NA. 


### Reconstruim la taula CBVCT2. Proporcions, Numeradors i Denominadors.
table_rows <- c(13:34)
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hivCBVCT_02 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hivCBVCT_02[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_2_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hivCBVCT_02[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hivCBVCT_02[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_2_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_02 <- rbind(result_hivCBVCT_02, result_hivCBVCT_02[NA])   

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_2[, Descriptiu := 'Numeradors_CBVCT_2']
HIV_data_agregades[["Numeradors_CBVCT_2"]] <- numeradors_CBVCT_2


rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, 
   table_props_rows, numeradors_CBVCT_2, numeradors_CBVCT_2_Totals)


### TEST:   Per comparar que el codi regenera una de les taules excel.
# agg <- copy(agg_01)
# agg_data_numer_rows <- c(17,21,25,29,33)
# agg_data_numer_cols <- c(1:7)
# agg_data_numer_names <- 14
# agg_data_numer_cats <- c(15,19,23,27,31)
# agg_data_denom_rows <- c(8:12)
# agg_data_denom_cols <- c(1:7)
# agg_data_denom_names <- 7
# table_rows <- c(13:34)
# table_cols <- c(1:7)
# table_numerad_rows <- c(5,9,13,17,21)
# table_denom_rows <- table_numerad_rows + 1 
# table_props_rows <- table_numerad_rows - 1 
# cols_with_data <- c(2:7)
# agg_numer <- get_CBVCTtable_rowsdata(agg, data_rows_idx = agg_data_numer_rows, data_cols_idx = agg_data_numer_cols,
#                                        names_row_idx = agg_data_numer_names, categories_rows_idx = agg_data_numer_cats)
# agg_denom <- get_CBVCTtable(agg, data_rows_idx = agg_data_denom_rows, data_cols_idx = agg_data_denom_cols,
#                             names_row_idx = agg_data_denom_names)
# agg_numer[, (colnames(agg_numer)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_numer)]
# agg_denom[, (colnames(agg_denom)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_denom)]
# test <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# test[table_numerad_rows, cols_with_data] <- agg_numer[, ..cols_with_data]
# test[table_denom_rows, cols_with_data] <- agg_denom[, ..cols_with_data]
# test[table_props_rows, cols_with_data] <- round(agg_numer[, ..cols_with_data]/agg_denom[, ..cols_with_data]*100, digits = 2)
# test


# ----------------------------------------------------------- #
## CBVCT_3                                                 ####
# ----------------------------------------------------------- #
# CBVCT 3: Proportion of clients who reported to have been tested for HIV during preceding 12 months.
# Reporting people tested for HIV in the year prior to attendance at the CBVCT service (testedlastyear=1)
# as a proportion of all people screened for HIV and stratified by gender, agegroup2, msm, sw, pwid, 
# migrants. Clients can be counted in more than one of the following; msm, sw, pwid, migrants.
#
#       Number of clients who reported to have been tested for HIV in previous 12 months (testedlastyear == 1)
#  --------------------------------------------------------------------------------------------------------------- x 100
#                                 Number of clients screened for HIV 

# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 3 total: Els numeradors.
{rows <- c(39,43,47,51,55); cols <- c(1:7); names <- 36; cat_rows <- c(37,41,45,49,53)
agg_00_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_01_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_02_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_03_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_04_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_05_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_06_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_07_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_08_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_09_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_10_hivCBVCT_3 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
rm(rows,cols,names,cat_rows)}

# Afegim columna Centre
{agg_00_hivCBVCT_3[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_3[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_3[, Centre:= "Aides"]
  agg_03_hivCBVCT_3[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_3[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_3[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_3[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_3[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_3[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_3[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_3[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_3 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_3"))); rm(list= ls(pattern = "_hivCBVCT_3"))

# Poblem valors de "BBDD Final" per a CBVCT_3
{numeradors_CBVCT_3[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & TestedLastYear == 1, .N]]
  numeradors_CBVCT_3[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & TestedLastYear == 1 & Gender == 1, .N]]
  numeradors_CBVCT_3[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_3[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & TestedLastYear == 1 & Gender == 3, .N]]
  numeradors_CBVCT_3[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & TestedLastYear == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_3[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & TestedLastYear == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_3[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & TestedLastYear == 1, .N]]
  numeradors_CBVCT_3[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & TestedLastYear == 1 & Gender == 1, .N]]
  numeradors_CBVCT_3[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & TestedLastYear == 1 & Gender == 2, .N]]
  numeradors_CBVCT_3[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & TestedLastYear == 1 & Gender == 3, .N]]
  numeradors_CBVCT_3[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & TestedLastYear == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_3[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & TestedLastYear == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_3[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & TestedLastYear == 1, .N]]
  numeradors_CBVCT_3[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & TestedLastYear == 1 & Gender == 1, .N]]
  numeradors_CBVCT_3[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & TestedLastYear == 1 & Gender == 2, .N]]
  numeradors_CBVCT_3[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & TestedLastYear == 1 & Gender == 3, .N]]
  numeradors_CBVCT_3[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & TestedLastYear == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_3[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & TestedLastYear == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_3[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & TestedLastYear == 1, .N]]
  numeradors_CBVCT_3[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & TestedLastYear == 1 & Gender == 1, .N]]
  numeradors_CBVCT_3[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & TestedLastYear == 1 & Gender == 2, .N]]
  numeradors_CBVCT_3[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & TestedLastYear == 1 & Gender == 3, .N]]
  numeradors_CBVCT_3[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & TestedLastYear == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_3[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & TestedLastYear == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_3[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[TestedLastYear == 1, .N]]
  numeradors_CBVCT_3[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[TestedLastYear == 1 & Gender == 1, .N]]
  numeradors_CBVCT_3[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[TestedLastYear == 1 & Gender == 2, .N]]
  numeradors_CBVCT_3[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[TestedLastYear == 1 & Gender == 3, .N]]
  numeradors_CBVCT_3[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[TestedLastYear == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_3[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[TestedLastYear == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_3_Totals <- numeradors_CBVCT_3[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]


### Reconstruim la taula CBVCT3. Proporcions, Numeradors i Denominadors.
table_rows <- c(35:56)
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hivCBVCT_03 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hivCBVCT_03[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_3_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hivCBVCT_03[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hivCBVCT_03[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_3_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_03 <- rbind(result_hivCBVCT_03, result_hivCBVCT_01[NA])  


# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_3[, Descriptiu := 'Numeradors_CBVCT_3']
HIV_data_agregades[["Numeradors_CBVCT_3"]] <- numeradors_CBVCT_3


rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, 
   table_props_rows, numeradors_CBVCT_3, numeradors_CBVCT_3_Totals)


### TEST:   Per comparar que el codi regenera una de les taules excel.
# agg <- copy(agg_01)
# agg_data_numer_rows <- c(39,43,47,51,55)
# agg_data_numer_cols <- c(1:7)
# agg_data_numer_names <- 36
# agg_data_numer_cats <- c(37,41,45,49,53)
# agg_data_denom_rows <- c(8:12)
# agg_data_denom_cols <- c(1:7)
# agg_data_denom_names <- 7
# table_rows <- c(13:34)
# table_cols <- c(1:7)
# table_numerad_rows <- c(5,9,13,17,21)
# table_denom_rows <- table_numerad_rows + 1
# table_props_rows <- table_numerad_rows - 1
# cols_with_data <- c(2:7)
# agg_numer <- get_CBVCTtable_rowsdata(agg, data_rows_idx = agg_data_numer_rows, data_cols_idx = agg_data_numer_cols,
#                                        names_row_idx = agg_data_numer_names, categories_rows_idx = agg_data_numer_cats)
# agg_denom <- get_CBVCTtable(agg, data_rows_idx = agg_data_denom_rows, data_cols_idx = agg_data_denom_cols,
#                             names_row_idx = agg_data_denom_names)
# agg_numer[, (colnames(agg_numer)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_numer)]
# agg_denom[, (colnames(agg_denom)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_denom)]
# test <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# test[table_numerad_rows, cols_with_data] <- agg_numer[, ..cols_with_data]
# test[table_denom_rows, cols_with_data] <- agg_denom[, ..cols_with_data]
# test[table_props_rows, cols_with_data] <- round(agg_numer[, ..cols_with_data]/agg_denom[, ..cols_with_data]*100, digits = 2)
# test



# ----------------------------------------------------------- #
## CBVCT_4                                                 ####
# ----------------------------------------------------------- #
# CBVCT 4: Proportion of clients who reported to have been tested for HIV at the same CBVCT facility 
# during preceding 12 months.
# Reporting people tested for HIV in the year prior to attendance in same CBVCT (testedlastyearsamecbvct=1) as
# a proportion of all people tested and stratified by gender, agegroup2, msm, sw, pwid, migrants. Clients can 
# be counted in more than one of the following; msm, sw, pwid, migrants.

#
#       Number of clients who reported to have been tested for HIV in previous 12 months in same Facility (testedlastyearsamecbvct == 1)
#  -------------------------------------------------------------------------------------------------------------------------------------- x 100
#                                       Number of clients screened for HIV 


# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 4 total: Els numeradors.
{rows <- c(61,65,69,73,77); cols <- c(1:7); names <- 58; cat_rows <- c(59,63,67,71,75)
agg_00_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_01_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_02_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_03_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_04_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_05_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_06_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_07_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_08_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_09_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_10_hivCBVCT_4 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
rm(rows,cols,names,cat_rows)}

# Afegim columna Centre
{agg_00_hivCBVCT_4[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_4[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_4[, Centre:= "Aides"]
  agg_03_hivCBVCT_4[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_4[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_4[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_4[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_4[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_4[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_4[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_4[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_4 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_4")))
rm(list= ls(pattern = "_hivCBVCT_4"))

# Poblem valors de "BBDD Final" per a CBVCT_4
{numeradors_CBVCT_4[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & TestedLastYearSameCBVCT == 1, .N]]
  numeradors_CBVCT_4[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & TestedLastYearSameCBVCT == 1 & Gender == 1, .N]]
  numeradors_CBVCT_4[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_4[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & TestedLastYearSameCBVCT == 1 & Gender == 3, .N]]
  numeradors_CBVCT_4[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_4[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_4[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & TestedLastYearSameCBVCT == 1, .N]]
  numeradors_CBVCT_4[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & TestedLastYearSameCBVCT == 1 & Gender == 1, .N]]
  numeradors_CBVCT_4[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & TestedLastYearSameCBVCT == 1 & Gender == 2, .N]]
  numeradors_CBVCT_4[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & TestedLastYearSameCBVCT == 1 & Gender == 3, .N]]
  numeradors_CBVCT_4[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_4[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_4[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & TestedLastYearSameCBVCT == 1, .N]]
  numeradors_CBVCT_4[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & TestedLastYearSameCBVCT == 1 & Gender == 1, .N]]
  numeradors_CBVCT_4[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & TestedLastYearSameCBVCT == 1 & Gender == 2, .N]]
  numeradors_CBVCT_4[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & TestedLastYearSameCBVCT == 1 & Gender == 3, .N]]
  numeradors_CBVCT_4[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_4[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_4[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & TestedLastYearSameCBVCT == 1, .N]]
  numeradors_CBVCT_4[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & TestedLastYearSameCBVCT == 1 & Gender == 1, .N]]
  numeradors_CBVCT_4[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & TestedLastYearSameCBVCT == 1 & Gender == 2, .N]]
  numeradors_CBVCT_4[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & TestedLastYearSameCBVCT == 1 & Gender == 3, .N]]
  numeradors_CBVCT_4[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_4[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & TestedLastYearSameCBVCT == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_4[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[TestedLastYearSameCBVCT == 1, .N]]
  numeradors_CBVCT_4[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[TestedLastYearSameCBVCT == 1 & Gender == 1, .N]]
  numeradors_CBVCT_4[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[TestedLastYearSameCBVCT == 1 & Gender == 2, .N]]
  numeradors_CBVCT_4[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[TestedLastYearSameCBVCT == 1 & Gender == 3, .N]]
  numeradors_CBVCT_4[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[TestedLastYearSameCBVCT == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_4[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[TestedLastYearSameCBVCT == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_4_Totals <- numeradors_CBVCT_4[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]

### Reconstruim la taula CBVCT4. Proporcions, Numeradors i Denominadors.
table_rows <- c(57:78)
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hivCBVCT_04 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hivCBVCT_04[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_4_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hivCBVCT_04[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hivCBVCT_04[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_4_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_04 <- rbind(result_hivCBVCT_04, result_hivCBVCT_04[NA]) 

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_4[, Descriptiu := 'Numeradors_CBVCT_4']
HIV_data_agregades[["Numeradors_CBVCT_4"]] <- numeradors_CBVCT_4


rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, 
   table_props_rows, numeradors_CBVCT_4, numeradors_CBVCT_4_Totals)

### TEST:   Per comparar que el codi regenera una de les taules excel.
# agg <- copy(agg_06)
# agg_data_numer_rows <- c(61,65,69,73,77)
# agg_data_numer_cols <- c(1:7)
# agg_data_numer_names <- 58
# agg_data_numer_cats <- c(59,63,67,71,75)
# agg_data_denom_rows <- c(8:12)
# agg_data_denom_cols <- c(1:7)
# agg_data_denom_names <- 7
# table_rows <- c(57:78)
# table_cols <- c(1:7)
# table_numerad_rows <- c(5,9,13,17,21)
# table_denom_rows <- table_numerad_rows + 1
# table_props_rows <- table_numerad_rows - 1
# cols_with_data <- c(2:7)
# agg_numer <- get_CBVCTtable_rowsdata(agg, data_rows_idx = agg_data_numer_rows, data_cols_idx = agg_data_numer_cols,
#                                        names_row_idx = agg_data_numer_names, categories_rows_idx = agg_data_numer_cats)
# agg_denom <- get_CBVCTtable(agg, data_rows_idx = agg_data_denom_rows, data_cols_idx = agg_data_denom_cols,
#                             names_row_idx = agg_data_denom_names)
# agg_numer[, (colnames(agg_numer)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_numer)]
# agg_denom[, (colnames(agg_denom)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_denom)]
# test <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# test[table_numerad_rows, cols_with_data] <- agg_numer[, ..cols_with_data]
# test[table_denom_rows, cols_with_data] <- agg_denom[, ..cols_with_data]
# test[table_props_rows, cols_with_data] <- round(agg_numer[, ..cols_with_data]/agg_denom[, ..cols_with_data]*100, digits = 2)
# test


# ----------------------------------------------------------- #
## CBVCT_5                                                 ####
# ----------------------------------------------------------- #
# CBVCT 5: Proportion of clients with reactive screening HIV test result.
# Reporting reactive screening test results (screeningtestresult = 1) as a proportion of all people tested and 
# stratified by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted in more than one of the 
# following; msm, sw, pwid, migrants.
#
#       Number of clients with a reactive screening test (screeningtestresult == 1)
#  -------------------------------------------------------------------------------------- x 100
#                      Number of clients screened for HIV 


# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 5 total: Els numeradors.
{rows <- c(83,87,91,95,99); cols <- c(1:7); names <- 80; cat_rows <- c(81,85,89,93,97)
agg_00_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_01_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_02_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_03_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_04_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_05_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_06_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_07_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_08_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_09_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_10_hivCBVCT_5 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
rm(rows,cols,names,cat_rows)}

# Afegim columna Centre
{agg_00_hivCBVCT_5[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_5[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_5[, Centre:= "Aides"]
  agg_03_hivCBVCT_5[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_5[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_5[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_5[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_5[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_5[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_5[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_5[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_5 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_5")))
rm(list= ls(pattern = "_hivCBVCT_5"))

# Poblem valors de "BBDD Final" per a CBVCT_5
{numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & ScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & ScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & ScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[ScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[ScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[ScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[ScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[ScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[ScreeningTestResult == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_5_Totals <- numeradors_CBVCT_5[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]

### Reconstruim la taula CBVCT5. Proporcions, Numeradors i Denominadors.
table_rows <- c(79:100)
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hivCBVCT_05 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hivCBVCT_05[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_5_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hivCBVCT_05[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hivCBVCT_05[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_5_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_05 <- rbind(result_hivCBVCT_05, result_hivCBVCT_05[NA]) 

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_5[, Descriptiu := 'Numeradors_CBVCT_5']
HIV_data_agregades[["Numeradors_CBVCT_5"]] <- numeradors_CBVCT_5


# !!! numeradors_CBVCT_5_Totals: NO BORRAR, els necessitem mes endavant...
rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, table_props_rows, numeradors_CBVCT_5) 


### TEST:   Per comparar que el codi regenera una de les taules excel.
# agg <- copy(agg_01)
# agg_data_numer_rows <- c(83,87,91,95,99)
# agg_data_numer_cols <- c(1:7)
# agg_data_numer_names <- 80
# agg_data_numer_cats <- c(81,85,89,93,97)
# agg_data_denom_rows <- c(8:12)
# agg_data_denom_cols <- c(1:7)
# agg_data_denom_names <- 7
# table_rows <- c(79:100)
# table_cols <- c(1:7)
# table_numerad_rows <- c(5,9,13,17,21)
# table_denom_rows <- table_numerad_rows + 1
# table_props_rows <- table_numerad_rows - 1
# cols_with_data <- c(2:7)
# agg_numer <- get_CBVCTtable_rowsdata(agg, data_rows_idx = agg_data_numer_rows, data_cols_idx = agg_data_numer_cols,
#                                        names_row_idx = agg_data_numer_names, categories_rows_idx = agg_data_numer_cats)
# agg_denom <- get_CBVCTtable(agg, data_rows_idx = agg_data_denom_rows, data_cols_idx = agg_data_denom_cols,
#                             names_row_idx = agg_data_denom_names)
# agg_numer[, (colnames(agg_numer)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_numer)]
# agg_denom[, (colnames(agg_denom)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_denom)]
# test <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# test[table_numerad_rows, cols_with_data] <- agg_numer[, ..cols_with_data]
# test[table_denom_rows, cols_with_data] <- agg_denom[, ..cols_with_data]
# test[table_props_rows, cols_with_data] <- round(agg_numer[, ..cols_with_data]/agg_denom[, ..cols_with_data]*100, digits = 2)
# test



# ----------------------------------------------------------- #
## CBVCT_6                                                 ####
# ----------------------------------------------------------- #
# CBVCT 6: Proportion of clients with reactive screening HIV test result who were tested with confirmatory HIV test.
# For clients who have a reactive HIV test, confirmatory testing usually takes place in a healthcare facility with a 
# fourth-generation test. Recording of this will depend on the client reporting back to the CBVCT or giving permission
# to be followed-up.
# Reporting confirmatory tests performed (confirmatoryhivtest=1) as a proportion of all reactive screening 
# tests (screeningtestresult=1) and stratified by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted
# in more than one of the following; msm, sw, pwid, migrants.

#
#     Number of clients with reactive screening test who were tested with confirmatory HIV test (screeningtestresult == 1 & confirmatoryhivtest == 1)
#  ---------------------------------------------------------------------------------------------------------------------------------------------------------- x 100
#                            Number of clients with a reactive HIV screening test (screeningtestresult == 1)

# <!> <!> COMPTE <!> <!>: 
# El CBVCT_6 del "Report Cobatest final 2018.pdf" correspòn al CBVCT_7 de "Aggregated data excel.xlsx"

# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 6 total: Els numeradors.
{rows <- c(127,131,136,139,143); cols <- c(1:7); names <- 124; cat_rows <- c(125,129,133,137,141)
agg_00_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_01_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_02_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_03_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_04_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_05_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_06_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_07_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_08_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_09_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_10_hivCBVCT_6 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
rm(rows,cols,names,cat_rows)}

# Afegim columna Centre
{agg_00_hivCBVCT_6[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_6[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_6[, Centre:= "Aides"]
  agg_03_hivCBVCT_6[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_6[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_6[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_6[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_6[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_6[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_6[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_6[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_6 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_6")))
rm(list= ls(pattern = "_hivCBVCT_6"))


# Poblem valors de "BBDD Final" per a CBVCT_6
{numeradors_CBVCT_6[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & ConfirmatoryHIVTest == 1, .N]]
  numeradors_CBVCT_6[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & ConfirmatoryHIVTest == 1 & Gender == 1, .N]]
  numeradors_CBVCT_6[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_6[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & ConfirmatoryHIVTest == 1 & Gender == 3, .N]]
  numeradors_CBVCT_6[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_6[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_6[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & ConfirmatoryHIVTest == 1 , .N]]
  numeradors_CBVCT_6[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & ConfirmatoryHIVTest == 1 & Gender == 1, .N]]
  numeradors_CBVCT_6[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & ConfirmatoryHIVTest == 1 & Gender == 2, .N]]
  numeradors_CBVCT_6[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & ConfirmatoryHIVTest == 1 & Gender == 3, .N]]
  numeradors_CBVCT_6[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_6[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_6[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & ConfirmatoryHIVTest == 1 , .N]]
  numeradors_CBVCT_6[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & ConfirmatoryHIVTest == 1 & Gender == 1, .N]]
  numeradors_CBVCT_6[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & ConfirmatoryHIVTest == 1 & Gender == 2, .N]]
  numeradors_CBVCT_6[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & ConfirmatoryHIVTest == 1 & Gender == 3, .N]]
  numeradors_CBVCT_6[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_6[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_6[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTest == 1 , .N]]
  numeradors_CBVCT_6[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTest == 1 & Gender == 1, .N]]
  numeradors_CBVCT_6[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTest == 1 & Gender == 2, .N]]
  numeradors_CBVCT_6[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTest == 1 & Gender == 3, .N]]
  numeradors_CBVCT_6[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_6[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTest == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_6[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[ConfirmatoryHIVTest == 1 , .N]]
  numeradors_CBVCT_6[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[ConfirmatoryHIVTest == 1 & Gender == 1, .N]]
  numeradors_CBVCT_6[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[ConfirmatoryHIVTest == 1 & Gender == 2, .N]]
  numeradors_CBVCT_6[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[ConfirmatoryHIVTest == 1 & Gender == 3, .N]]
  numeradors_CBVCT_6[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[ConfirmatoryHIVTest == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_6[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[ConfirmatoryHIVTest == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_6_Totals <- numeradors_CBVCT_6[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]

# Warning message: NAs introducidos por coerción.  Legebrita entra "NA".  


## Reconstruim la taula CBVCT6. Proporcions, Numeradors i Denominadors.
table_rows <- c(123:144) # Oju que és la CBVCT 7 de la plantilla. 
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hivCBVCT_06 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hivCBVCT_06[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_6_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hivCBVCT_06[table_denom_rows, cols_with_data] <- numeradors_CBVCT_5_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hivCBVCT_06[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_6_Totals[, ..cols_with_data]/numeradors_CBVCT_5_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_06 <- rbind(result_hivCBVCT_06, result_hivCBVCT_06[NA]) 


# Corregir un desajust PDF i plantilla XLSX. 
result_hivCBVCT_06[1, X1 := gsub(pattern = "CBVCT 7", replacement = "CBVCT 6", x = result_hivCBVCT_06[1,1])]


# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_6[, Descriptiu := 'Numeradors_CBVCT_6']
HIV_data_agregades[["Numeradors_CBVCT_6"]] <- numeradors_CBVCT_6


rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, 
   table_props_rows, numeradors_CBVCT_6_Totals, numeradors_CBVCT_6)


### TEST2:   Per comparar que el codi regenera una de les taules excel.
# agg <- copy(agg_01)
# agg_data_numer_rows <- c(127,131,136,139,143)
# agg_data_numer_cols <- c(1:7)
# agg_data_numer_names <- 124
# agg_data_numer_cats <- c(125,129,133,137,141)
# agg_data_denom_rows <- c(83,87,91,95,99)
# agg_data_denom_cols <- c(1:7)
# agg_data_denom_names <- 80
# table_rows <- c(123:144)
# table_cols <- c(1:7)
# table_numerad_rows <- c(5,9,13,17,21)
# table_denom_rows <- table_numerad_rows + 1
# table_props_rows <- table_numerad_rows - 1
# cols_with_data <- c(2:7)
# agg_numer <- get_CBVCTtable_rowsdata(agg, data_rows_idx = agg_data_numer_rows, data_cols_idx = agg_data_numer_cols,
#                                        names_row_idx = agg_data_numer_names, categories_rows_idx = agg_data_numer_cats)
# agg_denom <- get_CBVCTtable(agg, data_rows_idx = agg_data_denom_rows, data_cols_idx = agg_data_denom_cols,
#                             names_row_idx = agg_data_denom_names)
# agg_numer[, (colnames(agg_numer)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_numer)]
# agg_denom[, (colnames(agg_denom)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_denom)]
# test <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# test[table_numerad_rows, cols_with_data] <- agg_numer[, ..cols_with_data]
# test[table_denom_rows, cols_with_data] <- agg_denom[, ..cols_with_data]
# test[table_props_rows, cols_with_data] <- round(agg_numer[, ..cols_with_data]/agg_denom[, ..cols_with_data]*100, digits = 2)
# test


# ----------------------------------------------------------- #
## CBVCT_7                                                 ####
# ----------------------------------------------------------- #
# CBVCT 7: Proportion of clients with positive confirmatory HIV test result.
# Reporting positive confirmatory test results (confirmatoryhivtestresult=1) as a proportion of all reactive
# screening tests (screeningtestresult=1) and stratified by gender, agegroup2, msm, sw, pwid, migrants. Clients
# can be counted in more than one of the following; msm, sw, pwid, migrants.
#
#
#          Number of clients with positive confirmatory HIV test  (confirmatoryhivtestresult == 1)
#  -------------------------------------------------------------------------------------------------- x 100
#                            Number of clients screened for HIV  (ScreeningHIVTest == 1)

# <!> <!> COMPTE <!> <!>:  
# El CBVCT_7 del "Report Cobatest final 2018.pdf" correspòn al CBVCT_8 de "Aggregated data excel.xlsx"

# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 7 total: Els numeradors.
{rows <- c(149,153,157,161,165); cols <- c(1:7); names <- 146; cat_rows <- c(147,151,155,159,163)
agg_00_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_01_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_02_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_03_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_04_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_05_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_06_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_07_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_08_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_09_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_10_hivCBVCT_7 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
rm(rows,cols,names,cat_rows)}


# Afegim columna Centre
{agg_00_hivCBVCT_7[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_7[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_7[, Centre:= "Aides"]
  agg_03_hivCBVCT_7[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_7[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_7[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_7[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_7[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_7[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_7[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_7[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_7 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_7")))
rm(list= ls(pattern = "_hivCBVCT_7"))

# Poblem valors de "BBDD Final" per a CBVCT_5
{numeradors_CBVCT_7[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  numeradors_CBVCT_7[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_7[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_7[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_7[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_7[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_7[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  numeradors_CBVCT_7[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_7[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_7[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_7[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_7[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_7[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  numeradors_CBVCT_7[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_7[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_7[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_7[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_7[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_7[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  numeradors_CBVCT_7[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_7[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_7[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_7[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_7[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_7[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[ConfirmatoryHIVTestResult == 1, .N]]
  numeradors_CBVCT_7[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_7[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_7[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_7[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_7[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_7_Totals <- numeradors_CBVCT_7[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]

### Reconstruim la taula CBVCT7. Proporcions, Numeradors i Denominadors.
table_rows <- c(145:166) # Oju que és la CBVCT 8 de la plantilla. 
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hivCBVCT_07 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hivCBVCT_07[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_7_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hivCBVCT_07[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hivCBVCT_07[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_7_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_07 <- rbind(result_hivCBVCT_07, result_hivCBVCT_07[NA]) 

# Corregir un desajust PDF i plantilla XLSX. 
result_hivCBVCT_07[1, X1 := gsub(pattern = "CBVCT 8", replacement = "CBVCT 7", x = result_hivCBVCT_07[1,1])]

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_7[, Descriptiu := 'Numeradors_CBVCT_7']
HIV_data_agregades[["Numeradors_CBVCT_7"]] <- numeradors_CBVCT_7


rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, 
   table_props_rows, numeradors_CBVCT_7, numeradors_CBVCT_7_Totals) 


### TEST:   Per comparar que el codi regenera una de les taules excel.
# agg <- copy(agg_01)
# agg_data_numer_rows <- c(149,153,157,161,165)
# agg_data_numer_cols <- c(1:7)
# agg_data_numer_names <- 80
# agg_data_numer_cats <- c(147,151,155,159,163)
# agg_data_denom_rows <- c(8:12)
# agg_data_denom_cols <- c(1:7)
# agg_data_denom_names <- 7
# table_rows <- c(145:166)
# table_cols <- c(1:7)
# table_numerad_rows <- c(5,9,13,17,21)
# table_denom_rows <- table_numerad_rows + 1
# table_props_rows <- table_numerad_rows - 1
# cols_with_data <- c(2:7)
# agg_numer <- get_CBVCTtable_rowsdata(agg, data_rows_idx = agg_data_numer_rows, data_cols_idx = agg_data_numer_cols,
#                                        names_row_idx = agg_data_numer_names, categories_rows_idx = agg_data_numer_cats)
# agg_denom <- get_CBVCTtable(agg, data_rows_idx = agg_data_denom_rows, data_cols_idx = agg_data_denom_cols,
#                             names_row_idx = agg_data_denom_names)
# agg_numer[, (colnames(agg_numer)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_numer)]
# agg_denom[, (colnames(agg_denom)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_denom)]
# test <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# test[table_numerad_rows, cols_with_data] <- agg_numer[, ..cols_with_data]
# test[table_denom_rows, cols_with_data] <- agg_denom[, ..cols_with_data]
# test[table_props_rows, cols_with_data] <- round(agg_numer[, ..cols_with_data]/agg_denom[, ..cols_with_data]*100, digits = 2)
# test


# ----------------------------------------------------------- #
## CBVCT_8                                                 ####
# ----------------------------------------------------------- #
# CBVCT 8: Proportion of clients with false positive screening results.
# A false positive was considered a reactive screening test result followed by a negative confirmatory test result. 
#
#          Number of clients with false positive test result (ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2)
#  ------------------------------------------------------------------------------------------------------------------------ x 100
#                     Number of clients with a reactive HIV screening test (screeningtestresult == 1)

# <!> <!> COMPTE <!> <!>:  
# El CBVCT_8 del "Report Cobatest final 2018.pdf" correspòn al CBVCT_9 de "Aggregated data excel.xlsx"

## MEGI 26-02-2021:   Canviem Denominador de CBVCT_8 a tal com surt a "Report Cobatest final 2018.pdf" encara que les dades agregades (CBVCT_9)
#                     no el calculin d'aquesta manera. 
#
#          Number of clients with false positive test result (ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2)
#  ------------------------------------------------------------------------------------------------------------------------ x 100
#                     Number of clients screened for HIV  (ScreeningHIVTest == 1)


# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 8 total: Els numeradors.
{rows <- c(171,175,179,183,187); cols <- c(1:7); names <- 168; cat_rows <- c(169,173,177,181,185)
agg_00_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_01_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_02_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_03_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_04_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_05_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_06_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_07_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_08_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_09_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
agg_10_hivCBVCT_8 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
rm(rows,cols,names,cat_rows)}


# Afegim columna Centre
{agg_00_hivCBVCT_8[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_8[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_8[, Centre:= "Aides"]
  agg_03_hivCBVCT_8[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_8[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_8[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_8[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_8[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_8[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_8[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_8[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_8 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_8"))); rm(list= ls(pattern = "_hivCBVCT_8"))

# Poblem valors de "BBDD Final" per a CBVCT_8
{numeradors_CBVCT_8[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .N]]
  numeradors_CBVCT_8[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 1, .N]]
  numeradors_CBVCT_8[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_8[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 3, .N]]
  numeradors_CBVCT_8[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 1, .N]]
  numeradors_CBVCT_8[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 2, .N]]
  numeradors_CBVCT_8[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .N]]
  numeradors_CBVCT_8[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 1, .N]]
  numeradors_CBVCT_8[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 2, .N]]
  numeradors_CBVCT_8[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 3, .N]]
  numeradors_CBVCT_8[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 1, .N]]
  numeradors_CBVCT_8[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 2, .N]]
  numeradors_CBVCT_8[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .N]]
  numeradors_CBVCT_8[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 1, .N]]
  numeradors_CBVCT_8[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 2, .N]]
  numeradors_CBVCT_8[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 3, .N]]
  numeradors_CBVCT_8[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 1, .N]]
  numeradors_CBVCT_8[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 2, .N]]
  numeradors_CBVCT_8[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .N]]
  numeradors_CBVCT_8[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 1, .N]]
  numeradors_CBVCT_8[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 2, .N]]
  numeradors_CBVCT_8[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 3, .N]]
  numeradors_CBVCT_8[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 1, .N]]
  numeradors_CBVCT_8[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 & ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 2, .N]]
  numeradors_CBVCT_8[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .N]]
  numeradors_CBVCT_8[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 1, .N]]
  numeradors_CBVCT_8[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 2, .N]]
  numeradors_CBVCT_8[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & Gender == 3, .N]]
  numeradors_CBVCT_8[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 1, .N]]
  numeradors_CBVCT_8[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_8_Totals <- numeradors_CBVCT_8[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]


## Reconstruim la taula CBVCT8. Proporcions, Numeradors i Denominadors.
table_rows <- c(167:188) # Oju que és la CBVCT 9 de la plantilla. 
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hivCBVCT_08 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hivCBVCT_08[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_8_Totals[, ..cols_with_data]
# Afegim denominadors.  # Es redefineix despres de MEGI 26-02-2021
# result_hivCBVCT_08[table_denom_rows, cols_with_data] <- numeradors_CBVCT_5_Totals[, ..cols_with_data]  # Es redefineix despres de MEGI 26-02-2021
result_hivCBVCT_08[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions.  # Es redefineix despres de MEGI 26-02-2021
# result_hivCBVCT_08[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_8_Totals[, ..cols_with_data]/numeradors_CBVCT_5_Totals[, ..cols_with_data]*100, digits = 2)  # Es redefineix despres de MEGI 26-02-2021
result_hivCBVCT_08[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_8_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_08 <- rbind(result_hivCBVCT_08, result_hivCBVCT_08[NA]) 


# Corregir un desajust PDF i plantilla XLSX. 
result_hivCBVCT_08[1, X1 := gsub(pattern = "CBVCT 9", replacement = "CBVCT 8", x = result_hivCBVCT_08[1,1])]

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_8[, Descriptiu := 'Numeradors_CBVCT_8']
HIV_data_agregades[["Numeradors_CBVCT_8"]] <- numeradors_CBVCT_8



rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, 
   table_props_rows, numeradors_CBVCT_8_Totals, numeradors_CBVCT_8, numeradors_CBVCT_5_Totals)


### TEST2:   Per comparar que el codi regenera una de les taules excel.
# agg <- copy(agg_01)
# agg_data_numer_rows <- c(171,175,179,183,187)
# agg_data_numer_cols <- c(1:7)
# agg_data_numer_names <- 168
# agg_data_numer_cats <- c(169,173,177,181,185)
# agg_data_denom_rows <- c(83,87,91,95,99)
# agg_data_denom_cols <- c(1:7)
# agg_data_denom_names <- 80
# table_rows <- c(167:188)
# table_cols <- c(1:7)
# table_numerad_rows <- c(5,9,13,17,21)
# table_denom_rows <- table_numerad_rows + 1
# table_props_rows <- table_numerad_rows - 1
# cols_with_data <- c(2:7)
# agg_numer <- get_CBVCTtable_rowsdata(agg, data_rows_idx = agg_data_numer_rows, data_cols_idx = agg_data_numer_cols,
#                                        names_row_idx = agg_data_numer_names, categories_rows_idx = agg_data_numer_cats)
# agg_denom <- get_CBVCTtable(agg, data_rows_idx = agg_data_denom_rows, data_cols_idx = agg_data_denom_cols,
#                             names_row_idx = agg_data_denom_names)
# agg_numer[, (colnames(agg_numer)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_numer)]
# agg_denom[, (colnames(agg_denom)) := lapply(.SD, function(x) as.numeric(x)), .SDcols= colnames(agg_denom)]
# test <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# test[table_numerad_rows, cols_with_data] <- agg_numer[, ..cols_with_data]
# test[table_denom_rows, cols_with_data] <- agg_denom[, ..cols_with_data]
# test[table_props_rows, cols_with_data] <- round(agg_numer[, ..cols_with_data]/agg_denom[, ..cols_with_data]*100, digits = 2)
# test


# ----------------------------------------------------------- #
## CBVCT_9                                                 ####
# ----------------------------------------------------------- #
# CBVCT 9: Number of clients needed to test to find a positive HIV result.
#
#                        Number of clients screened for HIV (ScreeningHIVTest == 1)
#  ----------------------------------------------------------------------------------------------- x 100
#       Number of clients with positive confirmatory HIV test (confirmatoryhivtestresult == 1)

# Generem estructura del CBVCT_9 transformant estructura CBVCT_2 de l'excel.
{rows <- c(13:34); cols <- c(1:7)
cbvct_09_base <- get_CBVCTtable(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = NA)
cbvct_09_base[1, X1 := "CBVCT 9: Number of clients needed to test to find a positive HIV result."]
cbvct_09_base[4, X1 := "Number of clients needed to test to find a positive HIV result"]
cbvct_09_base[5, X1 := "Numerator: number of clients tested for HIV with a screening test."]
cbvct_09_base[6, X1 := "Denominator: number of clients with positive confirmatory HIV test result."]
cbvct_09_base[8, X1 := "Number of clients needed to test to find a positive HIV result"]
cbvct_09_base[9, X1 := "Numerator: number of clients tested for HIV with a screening test."]
cbvct_09_base[10, X1 := "Denominator: number of clients with positive confirmatory HIV test result."]
cbvct_09_base[12, X1 := "Number of clients needed to test to find a positive HIV result"]
cbvct_09_base[13, X1 := "Numerator: number of clients tested for HIV with a screening test."]
cbvct_09_base[14, X1 := "Denominator: number of clients with positive confirmatory HIV test result."]
cbvct_09_base[16, X1 := "Number of clients needed to test to find a positive HIV result"]
cbvct_09_base[17, X1 := "Numerator: number of clients tested for HIV with a screening test."]
cbvct_09_base[18, X1 := "Denominator: number of clients with positive confirmatory HIV test result."]
cbvct_09_base[20, X1 := "Number of clients needed to test to find a positive HIV result"]
cbvct_09_base[21, X1 := "Numerator: number of clients tested for HIV with a screening test."]
cbvct_09_base[22, X1 := "Denominator: number of clients with positive confirmatory HIV test result."]
rm(rows, cols)}

# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 9 total: Els denominadors.
generate_cbvct_09 <- function(dt, cbvct_09_struct, dt_numerad_rows, dt_denom_rows, cols_with_data) {
  # Files dels numeradors i denominadors.
  numerad_rows <- c(5,9,13,17,21)
  names_rows <- 2
  denom_rows <- numerad_rows + 1
  prop_rows <- numerad_rows - 1
  # Introduim dades
  cbvct_09_struct[numerad_rows, cols_with_data] <- dt[dt_numerad_rows, ..cols_with_data]
  cbvct_09_struct[denom_rows, cols_with_data] <- dt[dt_denom_rows, ..cols_with_data]
  # Calculem proporcions. Primer preparem el dataset sense text per convertir la part de dades a numèrica.
  data_colnames <- cbvct_09_struct[names_rows, ..cols_with_data]
  cbvct_09_struct[names_rows, cols_with_data] <- NA
  sel_cols <- colnames(cbvct_09_struct)[-1]
  cbvct_09_struct[, (sel_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols= sel_cols]
  cbvct_09_struct[prop_rows, cols_with_data] <- round(cbvct_09_struct[numerad_rows, ..cols_with_data]/cbvct_09_struct[denom_rows, ..cols_with_data], digits = 2)
  cbvct_09_struct[, (sel_cols) := lapply(.SD, function(x) ifelse(is.infinite(x), yes = NA, no = x)), .SDcols= sel_cols]
  cbvct_09_struct[, (sel_cols) := lapply(.SD, function(x) as.character(x)), .SDcols= sel_cols] # per treure NNN.00 a NNN.
  cbvct_09_struct[names_rows, cols_with_data] <- data_colnames
  return(cbvct_09_struct)}

# Generem les taules
{num_rows <- c(8:12); denom_rows <- c(149,153,157,161,165); cols <- c(2:7)
  agg_00_hivCBVCT_9 <- copy(cbvct_09_base)
  agg_01_hivCBVCT_9 <- generate_cbvct_09(dt = agg_01, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_02_hivCBVCT_9 <- generate_cbvct_09(dt = agg_02, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_03_hivCBVCT_9 <- generate_cbvct_09(dt = agg_03, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_04_hivCBVCT_9 <- generate_cbvct_09(dt = agg_04, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_05_hivCBVCT_9 <- generate_cbvct_09(dt = agg_05, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_06_hivCBVCT_9 <- generate_cbvct_09(dt = agg_06, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_07_hivCBVCT_9 <- generate_cbvct_09(dt = agg_07, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_08_hivCBVCT_9 <- generate_cbvct_09(dt = agg_08, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_09_hivCBVCT_9 <- generate_cbvct_09(dt = agg_09, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  agg_10_hivCBVCT_9 <- generate_cbvct_09(dt = agg_10, cbvct_09_struct = cbvct_09_base, dt_numerad_rows = num_rows, dt_denom_rows = denom_rows, cols_with_data = cols)
  rm(num_rows,denom_rows,cols)}


# Agafem els numeradors:
{rows <- c(5,9,13,17,21); cols <- c(1:7); names <- 2; cat_rows <- c(3,7,11,15,19) 
  agg_00_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_00_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_01_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_01_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_02_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_02_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_03_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_03_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_04_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_04_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_05_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_05_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_06_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_06_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_07_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_07_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_08_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_08_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_09_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_09_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_10_hivCBVCT_9_nums <- get_CBVCTtable_rowsdata(agg_10_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  rm(rows,cols,cat_rows,names)}

# Afegim columna Centre
{agg_00_hivCBVCT_9_nums[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_9_nums[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_9_nums[, Centre:= "Aides"]
  agg_03_hivCBVCT_9_nums[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_9_nums[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_9_nums[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_9_nums[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_9_nums[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_9_nums[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_9_nums[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_9_nums[, Centre:= "Demetra"]}

# Fem el RowBind
numeradors_CBVCT_9 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_9_nums"))); rm(list= ls(pattern = "_hivCBVCT_9_nums"))

# Poblem valors Numeradors de "BBDD Final" per a CBVCT_9 
{numeradors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 , .N]]
  numeradors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1  & Gender == 1, .N]]
  numeradors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & Gender == 3, .N]]
  numeradors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1, .N]]
  numeradors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & Gender == 1, .N]]
  numeradors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & Gender == 2, .N]]
  numeradors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & Gender == 3, .N]]
  numeradors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 , .N]]
  numeradors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & Gender == 1, .N]]
  numeradors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & Gender == 2, .N]]
  numeradors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & Gender == 3, .N]]
  numeradors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 , .N]]
  numeradors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & Gender == 1, .N]]
  numeradors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & Gender == 2, .N]]
  numeradors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & Gender == 3, .N]]
  numeradors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_9[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[, .N]]
  numeradors_CBVCT_9[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[Gender == 1, .N]]
  numeradors_CBVCT_9[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[Gender == 2, .N]]
  numeradors_CBVCT_9[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[Gender == 3, .N]]
  numeradors_CBVCT_9[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[AgeGroup == 1, .N]]
  numeradors_CBVCT_9[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_9_Totals <- numeradors_CBVCT_9[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]


# Agafem els denominadors:
{rows <- c(6,10,14,18,22); cols <- c(1:7); names <- 2; cat_rows <- c(3,7,11,15,19)
  agg_00_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_00_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_01_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_01_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_02_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_02_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_03_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_03_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_04_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_04_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_05_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_05_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_06_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_06_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_07_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_07_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_08_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_08_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_09_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_09_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_10_hivCBVCT_9_denom <- get_CBVCTtable_rowsdata(agg_10_hivCBVCT_9, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  rm(rows,cols,cat_rows,names)}

# Afegim columna Centre
{agg_00_hivCBVCT_9_denom[, Centre:= "BBDD Final"]
  agg_01_hivCBVCT_9_denom[, Centre:= "Poland National AIDS Centre"]
  agg_02_hivCBVCT_9_denom[, Centre:= "Aides"]
  agg_03_hivCBVCT_9_denom[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hivCBVCT_9_denom[, Centre:= "HUHIV"]
  agg_05_hivCBVCT_9_denom[, Centre:= "Legebitra"]
  agg_06_hivCBVCT_9_denom[, Centre:= "Associação Abraço"]
  agg_07_hivCBVCT_9_denom[, Centre:= "GenderdocM"]
  agg_08_hivCBVCT_9_denom[, Centre:= "Alliance GLobal"]
  agg_09_hivCBVCT_9_denom[, Centre:= "Fulcrum"]
  agg_10_hivCBVCT_9_denom[, Centre:= "Demetra"]}

# Fem el RowBind
denominadors_CBVCT_9 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hivCBVCT_9_denom"))); rm(list= ls(pattern = "_hivCBVCT_9_nums"))

# Poblem valors Numeradors de "BBDD Final" per a CBVCT_9 
{denominadors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", All := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  denominadors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", Males := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  denominadors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  denominadors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", Transgender := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  denominadors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  denominadors_CBVCT_9[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[MSM == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  denominadors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", All := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  denominadors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", Males := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  denominadors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", Females := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  denominadors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", Transgender := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  denominadors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  denominadors_CBVCT_9[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[SW == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  denominadors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", All := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  denominadors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", Males := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  denominadors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", Females := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  denominadors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", Transgender := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  denominadors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  denominadors_CBVCT_9[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[PWID == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  denominadors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", All := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1, .N]]
  denominadors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", Males := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  denominadors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", Females := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  denominadors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", Transgender := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  denominadors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  denominadors_CBVCT_9[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[Migrant == 1 & ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]
  denominadors_CBVCT_9[Category == "All" & Centre == "BBDD Final", All := vih_tested_final[ConfirmatoryHIVTestResult == 1, .N]]
  denominadors_CBVCT_9[Category == "All" & Centre == "BBDD Final", Males := vih_tested_final[ConfirmatoryHIVTestResult == 1 & Gender == 1, .N]]
  denominadors_CBVCT_9[Category == "All" & Centre == "BBDD Final", Females := vih_tested_final[ConfirmatoryHIVTestResult == 1 & Gender == 2, .N]]
  denominadors_CBVCT_9[Category == "All" & Centre == "BBDD Final", Transgender := vih_tested_final[ConfirmatoryHIVTestResult == 1 & Gender == 3, .N]]
  denominadors_CBVCT_9[Category == "All" & Centre == "BBDD Final", `<25 years old` := vih_tested_final[ConfirmatoryHIVTestResult == 1 & AgeGroup == 1, .N]]
  denominadors_CBVCT_9[Category == "All" & Centre == "BBDD Final", `25+ years old` := vih_tested_final[ConfirmatoryHIVTestResult == 1 & AgeGroup == 2, .N]]}

# fem les sumes
denominadors_CBVCT_9_Totals <- denominadors_CBVCT_9[, .(All= sum(as.numeric(All), na.rm = T), 
                                                        Males= sum(as.numeric(Males), na.rm = T), 
                                                        Females= sum(as.numeric(Females), na.rm = T), 
                                                        Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                        `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                        `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                    by= .(Category)]


## Reconstruim la taula CBVCT9 final. Proporcions, Numeradors i Denominadors.
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

result_hivCBVCT_09 <- copy(cbvct_09_base); rm(cbvct_09_base)
# Afegim numeradors.
result_hivCBVCT_09[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_9_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hivCBVCT_09[table_denom_rows, cols_with_data] <- denominadors_CBVCT_9_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hivCBVCT_09[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_9_Totals[, ..cols_with_data]/denominadors_CBVCT_9_Totals[, ..cols_with_data], digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hivCBVCT_09 <- rbind(result_hivCBVCT_09, result_hivCBVCT_09[NA]) 

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_9[, Descriptiu := 'Numeradors_CBVCT_9']
HIV_data_agregades[["Numeradors_CBVCT_9"]] <- numeradors_CBVCT_9
denominadors_CBVCT_9[, Descriptiu := 'Denominadors_CBVCT_9']
HIV_data_agregades[["Denominadors_CBVCT_9"]] <- denominadors_CBVCT_9


rm(denominadors_CBVCT_9_Totals, denominadors_CBVCT_9, denoms_CBVCT_Totals, numeradors_CBVCT_9_Totals, 
   numeradors_CBVCT_9, generate_cbvct_09, get_CBVCTtable, get_CBVCTtable_rowsdata, cols_with_data,
   table_denom_rows, table_numerad_rows, table_props_rows)



# ----------------------------------------------------------- #
## ALL CBVCT                                               ####
# ----------------------------------------------------------- #
# All CBVCT com a plantilla Excel. 
ind_hiv_CBVCT <- Reduce(function(...) rbind(...), mget(ls(pattern = "result_")))

# Lists elements are written to individual worksheets, using list names as sheet names if available
sheets <- list("HIV_CBVCT_1" = result_hivCBVCT_01, 
               "HIV_CBVCT_2" = result_hivCBVCT_02,
               "HIV_CBVCT_3" = result_hivCBVCT_03,
               "HIV_CBVCT_4" = result_hivCBVCT_04,
               "HIV_CBVCT_5" = result_hivCBVCT_05,
               "HIV_CBVCT_6" = result_hivCBVCT_06,
               "HIV_CBVCT_7" = result_hivCBVCT_07,
               "HIV_CBVCT_8" = result_hivCBVCT_08,
               "HIV_CBVCT_9" = result_hivCBVCT_09)


date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = sheets, file = paste0(output_path,"vih_indicadors_Report_",date,".xlsx"), col.names= FALSE)
rm(sheets, output_path, date)


rm(list= ls(pattern= "agg_"))
rm(result_hivCBVCT_01, result_hivCBVCT_02, result_hivCBVCT_03, result_hivCBVCT_04, result_hivCBVCT_05, 
   result_hivCBVCT_06, result_hivCBVCT_07, result_hivCBVCT_08, result_hivCBVCT_09)


# ________________________________________________________ ####
## HIV ANNEX 1 TABLES                                      ####
# ******************************************************** ####
# ----------------------------------------------------------- #
## CBVCT 2                                                 ####
# ----------------------------------------------------------- #
# CBVCT 2: Proportion of clients who reported to have been previously tested for HIV by centre with corresponding 
# estimates of indicators taking into account missing information (EMV)
#
# EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
# data reported for this indicator).

cbvct2_annex <- copy(center_maps)
hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
hivCBVCT_2 <- HIV_data_agregades$Numeradors_CBVCT_2


## DADES DESAGREGADES
{# Recuperem les N per centres.
cbvct2_annex <- merge(x = cbvct2_annex, 
                      y = vih_tested_final[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                      by = "Centre", all = T)
cbvct2_annex <- merge(x = cbvct2_annex, 
                      y = vih_tested_final[EverTested == 1, .('N ever tested' = .N), by= .(Centre)],
                      by = "Centre", all = T)
cbvct2_annex <- merge(x = cbvct2_annex, 
                      y = vih_tested_final[, .('Missing' = sum(is.na(EverTested))), by= .(Centre)],
                      by = "Centre", all = T)

# Calculem els percentatges
cbvct2_annex[, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)]
cbvct2_annex[, EMV := round(`N ever tested`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}


## DADES AGREGADES
{# Aides
cbvct2_annex[Centre == 30, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 30, `N ever tested` :=  hivCBVCT_2[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 30, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# Demetra
cbvct2_annex[Centre == 35, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 35, `N ever tested` :=  hivCBVCT_2[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 35, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# Legebitra
cbvct2_annex[Centre == 39, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 39, `N ever tested` :=  hivCBVCT_2[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 39, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# Poland National AIDS Centre
cbvct2_annex[Centre == 63, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 63, `N ever tested` :=  hivCBVCT_2[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 63, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# Alliance GLobal
cbvct2_annex[Centre == 64, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 64, `N ever tested` :=  hivCBVCT_2[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 64, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# Asocijacija Duga - Rainbow
cbvct2_annex[Centre == 65, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 65, `N ever tested` :=  hivCBVCT_2[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 65, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# HUHIV
cbvct2_annex[Centre == 66, `Persons screened for HIV` := hivCBVCT_1[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 66, `N ever tested` :=  hivCBVCT_2[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 66, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# Associação Abraço
cbvct2_annex[Centre == 67, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 67, `N ever tested` :=  hivCBVCT_2[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 67, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# GenderdocM
cbvct2_annex[Centre == 68, `Persons screened for HIV` := hivCBVCT_1[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 68, `N ever tested` :=  hivCBVCT_2[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 68, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# Fulcrum
cbvct2_annex[Centre == 69, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 69, `N ever tested` :=  hivCBVCT_2[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
cbvct2_annex[Centre == 69, `% ever tested` := round(`N ever tested`/`Persons screened for HIV`*100, digits = 1)] 
# FSE Polonia
cbvct2_annex[Centre == 70, `Persons screened for HIV` := NA] 
cbvct2_annex[Centre == 70, `N ever tested` :=  NA] 
cbvct2_annex[Centre == 70, `% ever tested` := NA]} 


{# Reorder cols and data
cbvct2_annex <- cbvct2_annex[, .(Centre,CentreName,Origen_2019,`Persons screened for HIV`,
                                 `N ever tested`,`% ever tested`,Missing,EMV)]
cbvct2_annex[, Origen_2019 := factor(Origen_2019, 
                                     levels= c('Cobatest_tool','Datos_desagregados','Datos_agregados','No_data'), 
                                     labels =c('Cobatest tool','Datos desagregados','Datos agregados','No data') )]
cbvct2_annex <- cbvct2_annex[order(Origen_2019)]}

rm(hivCBVCT_1, hivCBVCT_2)


# ----------------------------------------------------------- #
## CBVCT 3                                                 ####
# ----------------------------------------------------------- #
# CBVCT 3: Proportion of clients who reported to have been tested for HIV during preceding 12 months by 
# centre with corresponding estimates of indicators taking into account missing information (EMV)
#
# EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
# data reported for this indicator).

cbvct3_annex <- copy(center_maps)
hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
hivCBVCT_3 <- HIV_data_agregades$Numeradors_CBVCT_3

## DADES DESAGREGADES
{# Recuperem les N per centres.
cbvct3_annex <- merge(x = cbvct3_annex, 
                      y = vih_tested_final[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                      by = "Centre", all = T)
cbvct3_annex <- merge(x = cbvct3_annex, 
                      y = vih_tested_final[TestedLastYear == 1, .('N Tested Last Year' = .N), by= .(Centre)],
                      by = "Centre", all = T)
cbvct3_annex <- merge(x = cbvct3_annex, 
                      y = vih_tested_final[, .('Missing' = sum(is.na(TestedLastYear))), by= .(Centre)],
                      by = "Centre", all = T)


# Calculem els percentatges
cbvct3_annex[, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)]
cbvct3_annex[, EMV := round(`N Tested Last Year`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}


## DADES AGREGADES
{# Aides
cbvct3_annex[Centre == 30, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 30, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Aides' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 30, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# Demetra
cbvct3_annex[Centre == 35, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 35, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Demetra' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 35, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# Legebitra
cbvct3_annex[Centre == 39, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 39, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Legebitra' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 39, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)]  
# Poland National AIDS Centre
cbvct3_annex[Centre == 63, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 63, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Poland National AIDS Centre' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 63, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# Alliance GLobal
cbvct3_annex[Centre == 64, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 64, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Alliance GLobal' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 64, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# Asocijacija Duga - Rainbow
cbvct3_annex[Centre == 65, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 65, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 65, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# HUHIV
cbvct3_annex[Centre == 66, `Persons screened for HIV` := hivCBVCT_1[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 66, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'HUHIV' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 66, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# Associação Abraço
cbvct3_annex[Centre == 67, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 67, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Associação Abraço' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 67, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# GenderdocM
cbvct3_annex[Centre == 68, `Persons screened for HIV` := hivCBVCT_1[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 68, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'GenderdocM' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 68, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)] 
# Fulcrum
cbvct3_annex[Centre == 69, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
cbvct3_annex[Centre == 69, `N Tested Last Year` :=  hivCBVCT_3[Centre == 'Fulcrum' & Category == 'All clients', as.numeric(All)]] 
cbvct3_annex[Centre == 69, `% Tested Last Year` := round(`N Tested Last Year`/`Persons screened for HIV`*100, digits = 1)]  
# FSE Polonia
cbvct3_annex[Centre == 70, `Persons screened for HIV` := NA] 
cbvct3_annex[Centre == 70, `N Tested Last Year` :=  NA] 
cbvct3_annex[Centre == 70, `% Tested Last Year` := NA]} 


{# Reorder cols and data
cbvct3_annex <- cbvct3_annex[, .(Centre,CentreName,Origen_2019,`Persons screened for HIV`,
                                 `N Tested Last Year`,`% Tested Last Year`,Missing,EMV)]
cbvct3_annex[, Origen_2019 := factor(Origen_2019, 
                                     levels= c('Cobatest_tool','Datos_desagregados','Datos_agregados','No_data'), 
                                     labels =c('Cobatest tool','Datos desagregados','Datos agregados','No data') )]
cbvct3_annex <- cbvct3_annex[order(Origen_2019)]}

rm(hivCBVCT_1, hivCBVCT_3)



# ----------------------------------------------------------- #
## CBVCT 4                                                 ####
# ----------------------------------------------------------- #
# CBVCT 4: Proportion of clients who reported to have been tested for HIV at the same CBVCT facility during 
# preceding 12 months by centre with corresponding estimates of indicators taking into account missing information (EMV)
#
# EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
# data reported for this indicator).

cbvct4_annex <- copy(center_maps)
hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
hivCBVCT_4 <- HIV_data_agregades$Numeradors_CBVCT_4

## DADES DESAGREGADES
{# Recuperem les N per centres.
  cbvct4_annex <- merge(x = cbvct4_annex, 
                        y = vih_tested_final[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct4_annex <- merge(x = cbvct4_annex, 
                        y = vih_tested_final[TestedLastYearSameCBVCT == 1, .('N Tested Last Year Same CBVCT' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct4_annex <- merge(x = cbvct4_annex, 
                        y = vih_tested_final[, .('Missing' = sum(is.na(TestedLastYearSameCBVCT))), by= .(Centre)],
                        by = "Centre", all = T)
  

  # Calculem els percentatges
  cbvct4_annex[, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)]
  cbvct4_annex[, EMV := round(`N Tested Last Year Same CBVCT`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}
  


## DADES AGREGADES
{# Aides
  cbvct4_annex[Centre == 30, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 30, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Aides' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 30, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # Demetra
  cbvct4_annex[Centre == 35, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 35, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Demetra' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 35, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # Legebitra
  cbvct4_annex[Centre == 39, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 39, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Legebitra' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 39, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # Poland National AIDS Centre
  cbvct4_annex[Centre == 63, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 63, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Poland National AIDS Centre' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 63, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # Alliance GLobal
  cbvct4_annex[Centre == 64, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 64, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Alliance GLobal' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 64, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # Asocijacija Duga - Rainbow
  cbvct4_annex[Centre == 65, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 65, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 65, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # HUHIV
  cbvct4_annex[Centre == 66, `Persons screened for HIV` := hivCBVCT_1[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 66, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'HUHIV' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 66, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # Associação Abraço
  cbvct4_annex[Centre == 67, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 67, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Associação Abraço' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 67, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # GenderdocM
  cbvct4_annex[Centre == 68, `Persons screened for HIV` := hivCBVCT_1[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 68, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'GenderdocM' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 68, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # Fulcrum
  cbvct4_annex[Centre == 69, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct4_annex[Centre == 69, `N Tested Last Year Same CBVCT` :=  hivCBVCT_4[Centre == 'Fulcrum' & Category == 'All clients', as.numeric(All)]] 
  cbvct4_annex[Centre == 69, `% Tested Last Year Same CBVCT` := round(`N Tested Last Year Same CBVCT`/`Persons screened for HIV`*100, digits = 1)] 
  # FSE Polonia
  cbvct4_annex[Centre == 70, `Persons screened for HIV` := NA] 
  cbvct4_annex[Centre == 70, `N Tested Last Year Same CBVCT` :=  NA] 
  cbvct4_annex[Centre == 70, `% Tested Last Year Same CBVCT` := NA]} 


{# Reorder cols and data
  cbvct4_annex <- cbvct4_annex[, .(Centre,CentreName,Origen_2019,`Persons screened for HIV`,
                                   `N Tested Last Year Same CBVCT`,`% Tested Last Year Same CBVCT`,Missing,EMV)]
  cbvct4_annex[, Origen_2019 := factor(Origen_2019, 
                                       levels= c('Cobatest_tool','Datos_desagregados','Datos_agregados','No_data'), 
                                       labels =c('Cobatest tool','Datos desagregados','Datos agregados','No data') )]
  cbvct4_annex <- cbvct4_annex[order(Origen_2019)]}

rm(hivCBVCT_1, hivCBVCT_4)


# ----------------------------------------------------------- #
## CBVCT 5                                                 ####
# ----------------------------------------------------------- #
# CBVCT 5: Proportion of clients with reactive screening HIV test result by centre with corresponding estimates 
# of indicators taking into account missing information (EMV)
#
# EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
# data reported for this indicator).

cbvct5_annex <- copy(center_maps)
hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
hivCBVCT_5 <- HIV_data_agregades$Numeradors_CBVCT_5


# A TENIR EN COMPTE: diferencia dels altres indicadors, partim desde el dataset vih_tested_ind5_ann1 a qui encara no 
# s'ha filtrat per ScreeningTestRestult != NA.  
# Treiem els diagnosticats previament sense el filtre ScreeningTestResult != NA ja que necessitem informar el número 
# de missings que tenim "'Missing' = sum(is.na(ScreeningTestResult))"
# UPDATE 18-02-2021 (Reunió Laura i Megi) tornem a utilitzar el dataset final filtrat igual que la resta d'indicadors. 
# vih_tested_ind5_ann1 <- vih_tested[!(ResultLastHIV == 1 & ScreeningTestResult == 1) | is.na(ResultLastHIV)] 

## DADES DESAGREGADES
# {# Recuperem les N per centres.
#   cbvct5_annex <- merge(x = cbvct5_annex, 
#                         y = vih_tested_ind5_ann1[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
#                         by = "Centre", all = T)
#   cbvct5_annex <- merge(x = cbvct5_annex, 
#                         y = vih_tested_ind5_ann1[ScreeningTestResult == 1, .('N HIV Reactive' = .N), by= .(Centre)],
#                         by = "Centre", all = T)
#   cbvct5_annex <- merge(x = cbvct5_annex, 
#                         y = vih_tested_ind5_ann1[, .('Missing' = sum(is.na(ScreeningTestResult))), by= .(Centre)],
#                         by = "Centre", all = T)
#   
# 
#   # Calculem els percentatges
#   cbvct5_annex[, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)]
#   cbvct5_annex[, EMV := round(`N HIV Reactive`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}


{# Recuperem les N per centres.
  cbvct5_annex <- merge(x = cbvct5_annex, 
                        y = vih_tested_final[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct5_annex <- merge(x = cbvct5_annex, 
                        y = vih_tested_final[ScreeningTestResult == 1, .('N HIV Reactive' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct5_annex <- merge(x = cbvct5_annex, 
                        y = vih_tested_final[, .('Missing' = sum(is.na(ScreeningTestResult))), by= .(Centre)],
                        by = "Centre", all = T)
  
  
  # Calculem els percentatges
  cbvct5_annex[, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)]
  cbvct5_annex[, EMV := round(`N HIV Reactive`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}


## DADES AGREGADES
{# Aides
  cbvct5_annex[Centre == 30, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 30, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 30, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # Demetra
  cbvct5_annex[Centre == 35, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 35, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 35, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # Legebitra
  cbvct5_annex[Centre == 39, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 39, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 39, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # Poland National AIDS Centre
  cbvct5_annex[Centre == 63, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 63, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 63, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # Alliance GLobal
  cbvct5_annex[Centre == 64, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 64, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 64, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # Asocijacija Duga - Rainbow
  cbvct5_annex[Centre == 65, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 65, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 65, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # HUHIV
  cbvct5_annex[Centre == 66, `Persons screened for HIV` := hivCBVCT_1[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 66, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 66, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # Associação Abraço
  cbvct5_annex[Centre == 67, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 67, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 67, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # GenderdocM
  cbvct5_annex[Centre == 68, `Persons screened for HIV` := hivCBVCT_1[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 68, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 68, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # Fulcrum
  cbvct5_annex[Centre == 69, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 69, `N HIV Reactive` :=  hivCBVCT_5[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct5_annex[Centre == 69, `% HIV Reactive` := round(`N HIV Reactive`/`Persons screened for HIV`*100, digits = 1)] 
  # FSE Polonia
  cbvct5_annex[Centre == 70, `Persons screened for HIV` := NA] 
  cbvct5_annex[Centre == 70, `N HIV Reactive` :=  NA] 
  cbvct5_annex[Centre == 70, `% HIV Reactive` := NA]} 


{# Reorder cols and data
  cbvct5_annex <- cbvct5_annex[, .(Centre,CentreName,Origen_2019,`Persons screened for HIV`,
                                   `N HIV Reactive`,`% HIV Reactive`,Missing,EMV)]
  cbvct5_annex[, Origen_2019 := factor(Origen_2019, 
                                       levels= c('Cobatest_tool','Datos_desagregados','Datos_agregados','No_data'), 
                                       labels =c('Cobatest tool','Datos desagregados','Datos agregados','No data') )]
  cbvct5_annex <- cbvct5_annex[order(Origen_2019)]}

# Chequeig. 
cbvct5_annex[, sum(`Persons screened for HIV`, na.rm = T)]
cbvct4_annex[, sum(`Persons screened for HIV`, na.rm = T)]


rm(hivCBVCT_1, hivCBVCT_5)


# ----------------------------------------------------------- #
## CBVCT 6                                                 ####
# ----------------------------------------------------------- #
# CBVCT 6: Proportion of clients with reactive screening HIV test result who were tested with confirmatory 
# HIV test by centre with corresponding estimates of indicators taking into account missing information (EMV) 
#
# EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
# data reported for this indicator).

cbvct6_annex <- copy(center_maps)
hivCBVCT_5 <- HIV_data_agregades$Numeradors_CBVCT_5
hivCBVCT_6 <- HIV_data_agregades$Numeradors_CBVCT_6

vih_tested_final[Centre == 60, .N, by= .(ScreeningTestResult, ConfirmatoryHIVTest)][order(ScreeningTestResult, ConfirmatoryHIVTest)]
## DADES DESAGREGADES
{# Recuperem les N per centres.
  cbvct6_annex <- merge(x = cbvct6_annex, 
                        y = vih_tested_final[ScreeningTestResult == 1, .('Persons reactive HIV result' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct6_annex <- merge(x = cbvct6_annex, 
                        y = vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTest == 1, .('N HIV Confirmatory' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct6_annex <- merge(x = cbvct6_annex, 
                        y = vih_tested_final[ScreeningTestResult == 1, .('Missing' = sum(is.na(ConfirmatoryHIVTest))), by= .(Centre)],
                        by = "Centre", all = T)
  

  # Calculem els percentatges
  cbvct6_annex[, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)]
  cbvct6_annex[, EMV := round(`N HIV Confirmatory`/(`Persons reactive HIV result`- Missing)*100, digits = 1)]}


## DADES AGREGADES
{# Aides
  cbvct6_annex[Centre == 30, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 30, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 30, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # Demetra
  cbvct6_annex[Centre == 35, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 35, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 35, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # Legebitra
  cbvct6_annex[Centre == 39, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 39, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 39, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # Poland National AIDS Centre
  cbvct6_annex[Centre == 63, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 63, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 63, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # Alliance GLobal
  cbvct6_annex[Centre == 64, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 64, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 64, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # Asocijacija Duga - Rainbow
  cbvct6_annex[Centre == 65, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 65, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 65, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # HUHIV
  cbvct6_annex[Centre == 66, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 66, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 66, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # Associação Abraço
  cbvct6_annex[Centre == 67, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 67, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 67, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # GenderdocM
  cbvct6_annex[Centre == 68, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 68, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 68, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # Fulcrum
  cbvct6_annex[Centre == 69, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 69, `N HIV Confirmatory` :=  hivCBVCT_6[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct6_annex[Centre == 69, `% HIV Confirmatory` := round(`N HIV Confirmatory`/`Persons reactive HIV result`*100, digits = 1)] 
  # FSE Polonia
  cbvct6_annex[Centre == 70, `Persons reactive HIV result` := NA] 
  cbvct6_annex[Centre == 70, `N HIV Confirmatory` :=  NA] 
  cbvct6_annex[Centre == 70, `% HIV Confirmatory` := NA]} 


{# Reorder cols and data
  cbvct6_annex <- cbvct6_annex[, .(Centre,CentreName,Origen_2019,`Persons reactive HIV result`,
                                   `N HIV Confirmatory`,`% HIV Confirmatory`,Missing,EMV)]
  cbvct6_annex[, Origen_2019 := factor(Origen_2019, 
                                       levels= c('Cobatest_tool','Datos_desagregados','Datos_agregados','No_data'), 
                                       labels =c('Cobatest tool','Datos desagregados','Datos agregados','No data') )]
  cbvct6_annex <- cbvct6_annex[order(Origen_2019)]}

rm(hivCBVCT_5, hivCBVCT_6)


# ----------------------------------------------------------- #
## CBVCT 7                                                 ####
# ----------------------------------------------------------- #
# CBVCT 7: Proportion of clients with positive confirmatory HIV test result by centre with corresponding 
# estimates of indicators taking into account missing information (EMV)
#
# EMV = numerator is n. Denominator is persons screened for HIV minus number of missing (persons with no 
# data reported for this indicator).

cbvct7_annex <- copy(center_maps)
hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
hivCBVCT_7 <- HIV_data_agregades$Numeradors_CBVCT_7

## DADES DESAGREGADES
{# Recuperem les N per centres.
  cbvct7_annex <- merge(x = cbvct7_annex, 
                        y = vih_tested_final[ScreeningHIVTest == 1, .('Persons screened for HIV' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct7_annex <- merge(x = cbvct7_annex, 
                        y = vih_tested_final[ConfirmatoryHIVTestResult == 1, .('N HIV Confirm result' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct7_annex <- merge(x = cbvct7_annex, 
                        y = vih_tested_final[, .('Missing' = sum(is.na(ConfirmatoryHIVTestResult))), by= .(Centre)],
                        by = "Centre", all = T)
  

  # Calculem els percentatges
  cbvct7_annex[, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)]
  cbvct7_annex[, EMV := round(`N HIV Confirm result`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}


## DADES AGREGADES
{# Aides
  cbvct7_annex[Centre == 30, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 30, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 30, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # Demetra
  cbvct7_annex[Centre == 35, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 35, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 35, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # Legebitra
  cbvct7_annex[Centre == 39, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 39, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 39, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # Poland National AIDS Centre
  cbvct7_annex[Centre == 63, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 63, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 63, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # Alliance GLobal
  cbvct7_annex[Centre == 64, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 64, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 64, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # Asocijacija Duga - Rainbow
  cbvct7_annex[Centre == 65, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 65, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 65, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # HUHIV
  cbvct7_annex[Centre == 66, `Persons screened for HIV` := hivCBVCT_1[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 66, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 66, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # Associação Abraço
  cbvct7_annex[Centre == 67, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 67, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 67, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # GenderdocM
  cbvct7_annex[Centre == 68, `Persons screened for HIV` := hivCBVCT_1[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 68, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 68, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # Fulcrum
  cbvct7_annex[Centre == 69, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 69, `N HIV Confirm result` :=  hivCBVCT_7[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct7_annex[Centre == 69, `% HIV Confirm result` := round(`N HIV Confirm result`/`Persons screened for HIV`*100, digits = 1)] 
  # FSE Polonia
  cbvct7_annex[Centre == 70, `Persons screened for HIV` := NA] 
  cbvct7_annex[Centre == 70, `N HIV Confirm result` :=  NA] 
  cbvct7_annex[Centre == 70, `% HIV Confirm result` := NA]} 


{# Reorder cols and data
  cbvct7_annex <- cbvct7_annex[, .(Centre,CentreName,Origen_2019,`Persons screened for HIV`,
                                   `N HIV Confirm result`,`% HIV Confirm result`,Missing,EMV)]
  cbvct7_annex[, Origen_2019 := factor(Origen_2019, 
                                       levels= c('Cobatest_tool','Datos_desagregados','Datos_agregados','No_data'), 
                                       labels =c('Cobatest tool','Datos desagregados','Datos agregados','No data') )]
  cbvct7_annex <- cbvct7_annex[order(Origen_2019)]}

rm(hivCBVCT_1, hivCBVCT_7)



# ----------------------------------------------------------- #
## CBVCT 8                                                 ####
# ----------------------------------------------------------- #
# CBVCT 8: Proportion of clients with false positive screening results.
# A false positive was considered a reactive screening test result followed by a negative confirmatory test result. 

## MEGI 26-02-2021:   Canviem Denominador de CBVCT_8 a tal com surt a "Report Cobatest final 2018.pdf" encara que les dades agregades (CBVCT_9)
#                     no el calculin d'aquesta manera. 
#
#          Number of clients with false positive test result (ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2)
#  ------------------------------------------------------------------------------------------------------------------------ x 100
#                     Number of clients screened for HIV  (ScreeningHIVTest == 1)


cbvct8_annex <- copy(center_maps)
# hivCBVCT_5 <- HIV_data_agregades$Numeradors_CBVCT_5  # Obsolet desde 26-02-2021
hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
hivCBVCT_8 <- HIV_data_agregades$Numeradors_CBVCT_8

# # COMPROBACIONS
# vih_tested_final[is.na(ScreeningTestResult)]
# vih_tested_final[, .N, by= .(Centre, CentreName, ScreeningTestResult)][order(Centre, ScreeningTestResult)]
# vih_tested_final[, .N, by= .(Centre, CentreName, ConfirmatoryHIVTestResult)][order(Centre, ConfirmatoryHIVTestResult)]

# ## DADES DESAGREGADES   # Obsolet desde 26-02-2021
# {# Recuperem les N per centres.
#   cbvct8_annex <- merge(x = cbvct8_annex, 
#                         y = vih_tested_final[ScreeningTestResult == 1, .('Persons reactive HIV result' = .N), by= .(Centre)],
#                         by = "Centre", all = T)
#   cbvct8_annex <- merge(x = cbvct8_annex, 
#                         y = vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .('N False positive HIV results' = .N), by= .(Centre)],
#                         by = "Centre", all = T)
#   cbvct8_annex <- merge(x = cbvct8_annex, 
#                         y = vih_tested_final[ScreeningTestResult == 1, .('Missing' = sum(is.na(ConfirmatoryHIVTestResult))), by= .(Centre)],
#                         by = "Centre", all = T)
#   
#   
#   # Calculem els percentatges
#   cbvct8_annex[, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)]
#   cbvct8_annex[, EMV := round(`N False positive HIV results`/(`Persons reactive HIV result`- Missing)*100, digits = 1)]}


# ## DADES AGREGADES -- obsolet desde 26-02-2021
# {# Aides
#   cbvct8_annex[Centre == 30, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 30, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 30, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # Demetra
#   cbvct8_annex[Centre == 35, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 35, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 35, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # Legebitra
#   cbvct8_annex[Centre == 39, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 39, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 39, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # Poland National AIDS Centre
#   cbvct8_annex[Centre == 63, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 63, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 63, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # Alliance GLobal
#   cbvct8_annex[Centre == 64, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 64, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 64, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # Asocijacija Duga - Rainbow
#   cbvct8_annex[Centre == 65, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 65, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 65, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # HUHIV
#   cbvct8_annex[Centre == 66, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 66, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 66, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # Associação Abraço
#   cbvct8_annex[Centre == 67, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 67, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 67, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # GenderdocM
#   cbvct8_annex[Centre == 68, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 68, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 68, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # Fulcrum
#   cbvct8_annex[Centre == 69, `Persons reactive HIV result` := hivCBVCT_5[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 69, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
#   cbvct8_annex[Centre == 69, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
#   # FSE Polonia
#   cbvct8_annex[Centre == 70, `Persons reactive HIV result` := NA] 
#   cbvct8_annex[Centre == 70, `N False positive HIV results` :=  NA] 
#   cbvct8_annex[Centre == 70, `% False positive HIV results` := NA]} 


{# Recuperem les N per centres.
  cbvct8_annex <- merge(x = cbvct8_annex, 
                        y = vih_tested_final[ScreeningHIVTest == 1, .(`Persons screened for HIV` = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct8_annex <- merge(x = cbvct8_annex, 
                        y = vih_tested_final[ScreeningTestResult == 1 & ConfirmatoryHIVTestResult == 2, .('N False positive HIV results' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  cbvct8_annex <- merge(x = cbvct8_annex, 
                        y = vih_tested_final[ScreeningTestResult == 1, .('Missing' = sum(is.na(ConfirmatoryHIVTestResult))), by= .(Centre)],
                        by = "Centre", all = T)
  
  
  # Calculem els percentatges
  cbvct8_annex[, `% False positive HIV results` := round(`N False positive HIV results`/`Persons screened for HIV`*100, digits = 1)]
  cbvct8_annex[, EMV := round(`N False positive HIV results`/(`Persons screened for HIV`- Missing)*100, digits = 1)]}



## DADES AGREGADES 
{# Aides
  cbvct8_annex[Centre == 30, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 30, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Aides' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 30, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # Demetra
  cbvct8_annex[Centre == 35, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 35, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Demetra' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 35, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # Legebitra
  cbvct8_annex[Centre == 39, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 39, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Legebitra' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 39, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # Poland National AIDS Centre
  cbvct8_annex[Centre == 63, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 63, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Poland National AIDS Centre' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 63, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # Alliance GLobal
  cbvct8_annex[Centre == 64, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 64, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Alliance GLobal' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 64, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # Asocijacija Duga - Rainbow
  cbvct8_annex[Centre == 65, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 65, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 65, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # HUHIV
  cbvct8_annex[Centre == 66, `Persons screened for HIV` := hivCBVCT_1[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 66, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'HUHIV' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 66, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # Associação Abraço
  cbvct8_annex[Centre == 67, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 67, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Associação Abraço' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 67, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # GenderdocM
  cbvct8_annex[Centre == 68, `Persons screened for HIV` := hivCBVCT_1[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 68, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'GenderdocM' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 68, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # Fulcrum
  cbvct8_annex[Centre == 69, `Persons screened for HIV` := hivCBVCT_1[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 69, `N False positive HIV results` :=  hivCBVCT_8[Centre == 'Fulcrum' & Category == 'All', as.numeric(All)]] 
  cbvct8_annex[Centre == 69, `% False positive HIV results` := round(`N False positive HIV results`/`Persons reactive HIV result`*100, digits = 1)] 
  # FSE Polonia
  cbvct8_annex[Centre == 70, `Persons screened for HIV` := NA] 
  cbvct8_annex[Centre == 70, `N False positive HIV results` :=  NA] 
  cbvct8_annex[Centre == 70, `% False positive HIV results` := NA]} 


{# Reorder cols and data
  cbvct8_annex <- cbvct8_annex[, .(Centre,CentreName,Origen_2019,`Persons screened for HIV`,
                                   `N False positive HIV results`,`% False positive HIV results`,Missing,EMV)]
  cbvct8_annex[, Origen_2019 := factor(Origen_2019, 
                                       levels= c('Cobatest_tool','Datos_desagregados','Datos_agregados','No_data'), 
                                       labels =c('Cobatest tool','Datos desagregados','Datos agregados','No data') )]
  cbvct8_annex <- cbvct8_annex[order(Origen_2019)]}

rm(hivCBVCT_1, hivCBVCT_8)


# ----------------------------------------------------------- #
## ALL CBVCT                                               ####
# ----------------------------------------------------------- #
# Lists elements are written to individual worksheets, using list names as sheet names if available
sheets <- list("HIV_CBVCT_2_Annex1" = cbvct2_annex,
               "HIV_CBVCT_3_Annex1" = cbvct3_annex,
               "HIV_CBVCT_4_Annex1" = cbvct4_annex,
               "HIV_CBVCT_5_Annex1" = cbvct5_annex,
               "HIV_CBVCT_6_Annex1" = cbvct6_annex,
               "HIV_CBVCT_7_Annex1" = cbvct7_annex,
               "HIV_CBVCT_8_Annex1" = cbvct8_annex)

date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = sheets, file = paste0(output_path,"vih_Tables_Annex1_Report_",date,".xlsx"), col.names= TRUE)
rm(sheets, output_path, date)


rm(list= ls(pattern= "_annex"))


# ________________________________________________________ ####
## HIV ANNEX 2 TABLE                                       ####
# ******************************************************** ####

annex2 <- copy(center_maps[Origen_2019 != 'No_data' ])

{# Dades desagregades
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(AgeGroup= all(is.na(AgeGroup))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(MSM= all(is.na(MSM))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(SW= all(is.na(SW))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(PWID= all(is.na(PWID))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(Migrant= all(is.na(Migrant))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT1= all(is.na(ScreeningHIVTest))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT2= (all(is.na(ScreeningHIVTest)) | all(is.na(EverTested)))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT3= (all(is.na(ScreeningHIVTest)) | all(is.na(TestedLastYear)))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT4= (all(is.na(ScreeningHIVTest)) | all(is.na(TestedLastYearSameCBVCT)))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT5= (all(is.na(ScreeningHIVTest)) | all(is.na(ScreeningTestResult)))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT6= (all(is.na(ScreeningTestResult)) | all(is.na(ConfirmatoryHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT7= (all(is.na(ScreeningHIVTest)) | all(is.na(ConfirmatoryHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT8= (all(is.na(ScreeningTestResult)) | all(is.na(ConfirmatoryHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
annex2 <- merge(x =annex2 , y= cobatest_tool_final[, .(CBVCT9= (all(is.na(ConfirmatoryHIVTest)) | all(is.na(ScreeningHIVTest)))), by= .(Centre)], by = 'Centre', all.x= T)
}

{# Taula per dades agregades
agg_CBVCT1 <- HIV_data_agregades$CBVCT_1
annex2[Centre == 30, AgeGroup :=  agg_CBVCT1[Centre == 'Aides' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 30, MSM :=  agg_CBVCT1[Centre == 'Aides' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 30, SW :=  agg_CBVCT1[Centre == 'Aides' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 30, PWID :=  agg_CBVCT1[Centre == 'Aides' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 30, Migrant :=  agg_CBVCT1[Centre == 'Aides' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 35, AgeGroup :=  agg_CBVCT1[Centre == 'Demetra' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 35, MSM :=  agg_CBVCT1[Centre == 'Demetra' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 35, SW :=  agg_CBVCT1[Centre == 'Demetra' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 35, PWID :=  agg_CBVCT1[Centre == 'Demetra' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 35, Migrant :=  agg_CBVCT1[Centre == 'Demetra' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 39, AgeGroup :=  agg_CBVCT1[Centre == 'Legebitra' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 39, MSM :=  agg_CBVCT1[Centre == 'Legebitra' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 39, SW :=  agg_CBVCT1[Centre == 'Legebitra' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 39, PWID :=  agg_CBVCT1[Centre == 'Legebitra' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 39, Migrant :=  agg_CBVCT1[Centre == 'Legebitra' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 63, AgeGroup :=  agg_CBVCT1[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 63, MSM :=  agg_CBVCT1[Centre == 'Poland National AIDS Centre' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 63, SW :=  agg_CBVCT1[Centre == 'Poland National AIDS Centre' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 63, PWID :=  agg_CBVCT1[Centre == 'Poland National AIDS Centre' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 63, Migrant :=  agg_CBVCT1[Centre == 'Poland National AIDS Centre' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 64, AgeGroup :=  agg_CBVCT1[Centre == 'Alliance GLobal' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 64, MSM :=  agg_CBVCT1[Centre == 'Alliance GLobal' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 64, SW :=  agg_CBVCT1[Centre == 'Alliance GLobal' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 64, PWID :=  agg_CBVCT1[Centre == 'Alliance GLobal' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 64, Migrant :=  agg_CBVCT1[Centre == 'Alliance GLobal' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 65, AgeGroup :=  agg_CBVCT1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 65, MSM :=  agg_CBVCT1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 65, SW :=  agg_CBVCT1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 65, PWID :=  agg_CBVCT1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 65, Migrant :=  agg_CBVCT1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 66, AgeGroup :=  agg_CBVCT1[Centre == 'HUHIV' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 66, MSM :=  agg_CBVCT1[Centre == 'HUHIV' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 66, SW :=  agg_CBVCT1[Centre == 'HUHIV' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 66, PWID :=  agg_CBVCT1[Centre == 'HUHIV' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 66, Migrant :=  agg_CBVCT1[Centre == 'HUHIV' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 67, AgeGroup :=  agg_CBVCT1[Centre == 'Associação Abraço' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 67, MSM :=  agg_CBVCT1[Centre == 'Associação Abraço' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 67, SW :=  agg_CBVCT1[Centre == 'Associação Abraço' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 67, PWID :=  agg_CBVCT1[Centre == 'Associação Abraço' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 67, Migrant :=  agg_CBVCT1[Centre == 'Associação Abraço' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 68, AgeGroup :=  agg_CBVCT1[Centre == 'GenderdocM' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 68, MSM :=  agg_CBVCT1[Centre == 'GenderdocM' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 68, SW :=  agg_CBVCT1[Centre == 'GenderdocM' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 68, PWID :=  agg_CBVCT1[Centre == 'GenderdocM' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 68, Migrant :=  agg_CBVCT1[Centre == 'GenderdocM' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 69, AgeGroup :=  agg_CBVCT1[Centre == 'Fulcrum' & Category == 'All', is.na(`<25 years old`) & is.na(`25+ years old`)]]
annex2[Centre == 69, MSM :=  agg_CBVCT1[Centre == 'Fulcrum' & Category == 'MSM', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 69, SW :=  agg_CBVCT1[Centre == 'Fulcrum' & Category == 'SW', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 69, PWID :=  agg_CBVCT1[Centre == 'Fulcrum' & Category == 'IDU', is.na(Males) & is.na(Females) & is.na(Transgender)]]
annex2[Centre == 69, Migrant :=  agg_CBVCT1[Centre == 'Fulcrum' & Category == 'Migrants', is.na(Males) & is.na(Females) & is.na(Transgender)]]}

{annex2[Centre == 30, CBVCT1 :=  agg_CBVCT1[Centre == 'Aides' & Category == 'All', is.na(All)]]
annex2[Centre == 35, CBVCT1 :=  agg_CBVCT1[Centre == 'Demetra' & Category == 'All', is.na(All)]]
annex2[Centre == 39, CBVCT1 :=  agg_CBVCT1[Centre == 'Legebitra' & Category == 'All', is.na(All)]]
annex2[Centre == 63, CBVCT1 :=  agg_CBVCT1[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(All)]]
annex2[Centre == 64, CBVCT1 :=  agg_CBVCT1[Centre == 'Alliance GLobal' & Category == 'All', is.na(All)]]
annex2[Centre == 65, CBVCT1 :=  agg_CBVCT1[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(All)]]
annex2[Centre == 66, CBVCT1 :=  agg_CBVCT1[Centre == 'HUHIV' & Category == 'All', is.na(All)]]
annex2[Centre == 67, CBVCT1 :=  agg_CBVCT1[Centre == 'Associação Abraço' & Category == 'All', is.na(All)]]
annex2[Centre == 68, CBVCT1 :=  agg_CBVCT1[Centre == 'GenderdocM' & Category == 'All', is.na(All)]]
annex2[Centre == 69, CBVCT1 :=  agg_CBVCT1[Centre == 'Fulcrum' & Category == 'All', is.na(All)]]
rm(agg_CBVCT1)} 


{agg_num_CBVCT2 <- HIV_data_agregades$Numeradors_CBVCT_2
  annex2[Centre == 30, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Aides' & Category == 'All', is.na(All)]]
  annex2[Centre == 35, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Demetra' & Category == 'All', is.na(All)]]
  annex2[Centre == 39, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Legebitra' & Category == 'All', is.na(All)]]
  annex2[Centre == 63, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(All)]]
  annex2[Centre == 64, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Alliance GLobal' & Category == 'All', is.na(All)]]
  annex2[Centre == 65, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(All)]]
  annex2[Centre == 66, CBVCT2 :=  agg_num_CBVCT2[Centre == 'HUHIV' & Category == 'All', is.na(All)]]
  annex2[Centre == 67, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Associação Abraço' & Category == 'All', is.na(All)]]
  annex2[Centre == 68, CBVCT2 :=  agg_num_CBVCT2[Centre == 'GenderdocM' & Category == 'All', is.na(All)]]
  annex2[Centre == 69, CBVCT2 :=  agg_num_CBVCT2[Centre == 'Fulcrum' & Category == 'All', is.na(All)]]
  rm(agg_num_CBVCT2)}  

{agg_num_CBVCT3 <- HIV_data_agregades$Numeradors_CBVCT_3
  annex2[Centre == 30, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Aides' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 35, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Demetra' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 39, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Legebitra' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 63, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Poland National AIDS Centre' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 64, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Alliance GLobal' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 65, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 66, CBVCT3 :=  agg_num_CBVCT3[Centre == 'HUHIV' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 67, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Associação Abraço' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 68, CBVCT3 :=  agg_num_CBVCT3[Centre == 'GenderdocM' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 69, CBVCT3 :=  agg_num_CBVCT3[Centre == 'Fulcrum' & Category == 'All clients', is.na(All)]]
  rm(agg_num_CBVCT3)} 

{agg_num_CBVCT4 <- HIV_data_agregades$Numeradors_CBVCT_4
  annex2[Centre == 30, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Aides' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 35, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Demetra' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 39, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Legebitra' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 63, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Poland National AIDS Centre' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 64, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Alliance GLobal' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 65, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 66, CBVCT4 :=  agg_num_CBVCT4[Centre == 'HUHIV' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 67, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Associação Abraço' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 68, CBVCT4 :=  agg_num_CBVCT4[Centre == 'GenderdocM' & Category == 'All clients', is.na(All)]]
  annex2[Centre == 69, CBVCT4 :=  agg_num_CBVCT4[Centre == 'Fulcrum' & Category == 'All clients', is.na(All)]]
  rm(agg_num_CBVCT4)} 

{agg_num_CBVCT5 <- HIV_data_agregades$Numeradors_CBVCT_5
  annex2[Centre == 30, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Aides' & Category == 'All', is.na(All)]]
  annex2[Centre == 35, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Demetra' & Category == 'All', is.na(All)]]
  annex2[Centre == 39, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Legebitra' & Category == 'All', is.na(All)]]
  annex2[Centre == 63, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(All)]]
  annex2[Centre == 64, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Alliance GLobal' & Category == 'All', is.na(All)]]
  annex2[Centre == 65, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(All)]]
  annex2[Centre == 66, CBVCT5 :=  agg_num_CBVCT5[Centre == 'HUHIV' & Category == 'All', is.na(All)]]
  annex2[Centre == 67, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Associação Abraço' & Category == 'All', is.na(All)]]
  annex2[Centre == 68, CBVCT5 :=  agg_num_CBVCT5[Centre == 'GenderdocM' & Category == 'All', is.na(All)]]
  annex2[Centre == 69, CBVCT5 :=  agg_num_CBVCT5[Centre == 'Fulcrum' & Category == 'All', is.na(All)]]
  rm(agg_num_CBVCT5)} 

{agg_num_CBVCT6 <- HIV_data_agregades$Numeradors_CBVCT_6
  annex2[Centre == 30, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Aides' & Category == 'All', is.na(All)]]
  annex2[Centre == 35, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Demetra' & Category == 'All', is.na(All)]]
  annex2[Centre == 39, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Legebitra' & Category == 'All', is.na(All)]]
  annex2[Centre == 63, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(All)]]
  annex2[Centre == 64, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Alliance GLobal' & Category == 'All', is.na(All)]]
  annex2[Centre == 65, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(All)]]
  annex2[Centre == 66, CBVCT6 :=  agg_num_CBVCT6[Centre == 'HUHIV' & Category == 'All', is.na(All)]]
  annex2[Centre == 67, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Associação Abraço' & Category == 'All', is.na(All)]]
  annex2[Centre == 68, CBVCT6 :=  agg_num_CBVCT6[Centre == 'GenderdocM' & Category == 'All', is.na(All)]]
  annex2[Centre == 69, CBVCT6 :=  agg_num_CBVCT6[Centre == 'Fulcrum' & Category == 'All', is.na(All)]]
  rm(agg_num_CBVCT6)} 

{agg_num_CBVCT7 <- HIV_data_agregades$Numeradors_CBVCT_7
  annex2[Centre == 30, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Aides' & Category == 'All', is.na(All)]]
  annex2[Centre == 35, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Demetra' & Category == 'All', is.na(All)]]
  annex2[Centre == 39, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Legebitra' & Category == 'All', is.na(All)]]
  annex2[Centre == 63, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(All)]]
  annex2[Centre == 64, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Alliance GLobal' & Category == 'All', is.na(All)]]
  annex2[Centre == 65, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(All)]]
  annex2[Centre == 66, CBVCT7 :=  agg_num_CBVCT7[Centre == 'HUHIV' & Category == 'All', is.na(All)]]
  annex2[Centre == 67, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Associação Abraço' & Category == 'All', is.na(All)]]
  annex2[Centre == 68, CBVCT7 :=  agg_num_CBVCT7[Centre == 'GenderdocM' & Category == 'All', is.na(All)]]
  annex2[Centre == 69, CBVCT7 :=  agg_num_CBVCT7[Centre == 'Fulcrum' & Category == 'All', is.na(All)]]
  rm(agg_num_CBVCT7)} 

{agg_num_CBVCT8 <- HIV_data_agregades$Numeradors_CBVCT_8
  annex2[Centre == 30, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Aides' & Category == 'All', is.na(All)]]
  annex2[Centre == 35, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Demetra' & Category == 'All', is.na(All)]]
  annex2[Centre == 39, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Legebitra' & Category == 'All', is.na(All)]]
  annex2[Centre == 63, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(All)]]
  annex2[Centre == 64, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Alliance GLobal' & Category == 'All', is.na(All)]]
  annex2[Centre == 65, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(All)]]
  annex2[Centre == 66, CBVCT8 :=  agg_num_CBVCT8[Centre == 'HUHIV' & Category == 'All', is.na(All)]]
  annex2[Centre == 67, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Associação Abraço' & Category == 'All', is.na(All)]]
  annex2[Centre == 68, CBVCT8 :=  agg_num_CBVCT8[Centre == 'GenderdocM' & Category == 'All', is.na(All)]]
  annex2[Centre == 69, CBVCT8 :=  agg_num_CBVCT8[Centre == 'Fulcrum' & Category == 'All', is.na(All)]]
  rm(agg_num_CBVCT8)} 

{agg_num_CBVCT9 <- HIV_data_agregades$Numeradors_CBVCT_9
  annex2[Centre == 30, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Aides' & Category == 'All', is.na(All)]]
  annex2[Centre == 35, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Demetra' & Category == 'All', is.na(All)]]
  annex2[Centre == 39, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Legebitra' & Category == 'All', is.na(All)]]
  annex2[Centre == 63, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Poland National AIDS Centre' & Category == 'All', is.na(All)]]
  annex2[Centre == 64, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Alliance GLobal' & Category == 'All', is.na(All)]]
  annex2[Centre == 65, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Asocijacija Duga - Rainbow' & Category == 'All', is.na(All)]]
  annex2[Centre == 66, CBVCT9 :=  agg_num_CBVCT9[Centre == 'HUHIV' & Category == 'All', is.na(All)]]
  annex2[Centre == 67, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Associação Abraço' & Category == 'All', is.na(All)]]
  annex2[Centre == 68, CBVCT9 :=  agg_num_CBVCT9[Centre == 'GenderdocM' & Category == 'All', is.na(All)]]
  annex2[Centre == 69, CBVCT9 :=  agg_num_CBVCT9[Centre == 'Fulcrum' & Category == 'All', is.na(All)]]
  rm(agg_num_CBVCT9)} 


date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = annex2, file = paste0(output_path,"vih_Annex2_Table_",date,".xlsx"), col.names= TRUE)
rm(annex2, output_path, date)



# ________________________________________________________ ####
## HEPATITIS C INDICATORS                                  ####
# ******************************************************** ####
# ----------------------------------------------------------- #
## VHC TESTS                                               ####
# ----------------------------------------------------------- #
# Filtrem dataset pels que han estat testats per VHC.
vhc_tests <- copy(cobatest_tool_final[!is.na(HCVScreeningTestResult)])

# Processat de les edats.
# LAURA: Respecto a los menores de 16 años, he revisado los casos, y he visto que los casos de menores reales
#       (no atribuïbles a errores de data entry con la fecha de nacimiento) tienen informado el grupo de edad. Así que si 
#       la edad és inferior a 16 años y AgeGroupc== 1, se deberían eliminar. Los que quedan, menores de 16, son 
#       errores en el data entry. Deben tener AgeGroup a NA pero podemos sacar la otra informacion.
# COMPROBACION: cobatest_tool_final[AgeInYears < 16, .N, by= .(trunc_age= trunc(AgeInYears),AgeGroup)][order(trunc_age)]
vhc_tests[, Menors_reals := ifelse(test = AgeInYears < 16 & AgeGroup == 1, yes = 1, no = 0)]
vhc_tests <- vhc_tests[Menors_reals != 1 | is.na(Menors_reals), ][, Menors_reals := NULL]


# ----------------------------------------------------------- #
## VHC SENSE DUPLICATS                                     ####
# ----------------------------------------------------------- #
## Busquem duplicats de VHC
# - LAURA ---------------------- #
#   En nuestra BBDD, generamos la variable “Id”, para poder identificar repetidores y quedarnos únicamente con las personas testadas, 
#   y poder calcular los indicadores con las personas testadas. 
#   Los centros pueden haber usado cualquiera de estos nombres de variables para su identificador único de cliente. He marcado en 
#   rojo en el Excel de las variables la variable que se debe usar para cada centro, para identificar repetidores. 

# COMPROBACION: vhc_tests[, .N]                                                        # registres
# COMPROBACION: vhc_tests[, .N] - vih_tests[, uniqueN(Id_main)]                        # registres duplicats
# COMPROBACION: vhc_tests[, uniqueN(Id_main, na.rm = T)]                               # persones
# COMPROBACION: vhc_tests[, .N, by = .(Id_main)][N > 1, uniqueN(Id_main, na.rm = T)]   # persones repetides
# COMPROBACION: vhc_tests[, .N, by = .(Id_main)][N > 4, Id_main]                       # Ids
# COMPROBACION: vhc_tests[, .N, by = .(Id_main)][N > 1, ][order(Id_main), Id_main]



## COMPTE!! 
#  Hi ha uns codis a Id_main: .00, .00M, 0000, 000A, 000B, 000G, 000L, 000M, 000P, 000R i 000S,
#  Que poden estar duplicats i estan referits a persones diferents:
# t(vhc_tests[Id_main == ".00"])    # cap
# t(vhc_tests[Id_main == ".00M"])   # cap
# t(vhc_tests[Id_main == "0000"])   # cap
# t(vhc_tests[Id_main == "000A"])   # cap
# t(vhc_tests[Id_main == "000B"])   # cap
# t(vhc_tests[Id_main == "000G"])   # hi ha 1. Té cobatest id.
# t(vhc_tests[Id_main == "000L"])   # cap
# t(vhc_tests[Id_main == "000M"])   # cap.
# t(vhc_tests[Id_main == "000P"])   # cap.
# t(vhc_tests[Id_main == "000R"])   # cap.
# t(vhc_tests[Id_main == "000S"])   # cap.
## Adjudiquem-los un id: cobatest_id U cbvctidentifier.
{vhc_tests[Id_main == ".00", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == ".00M", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "0000", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000A", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000B", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000G", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000L", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000M", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000P", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000R", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  vhc_tests[Id_main == "000S", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]}


vhc_tests[, DateofVisit := as.Date(DateofVisit)]

# Traiem duplicats tenint en compte que els Centres 28 i 59 no tenen Id_main.
# vhc_tests <- vhc_tests[vih_tests[!is.na(Id_main), .I[which.max(DateofVisit)], by = Id_main]$V1]
vhc_tests_noIdmain <- vhc_tests[is.na(Id_main),]
vhc_tests_Idmain <- vhc_tests[!is.na(Id_main),]

# Fora duplicats.
vhc_tested_Idmain <- vhc_tests_Idmain[vhc_tests_Idmain[, .I[which.max(DateofVisit)], by = Id_main]$V1]
# Assumim que no hi ha duplicats en els que no tenen id_main (Centres 28 i 59 no tenen Id_main).
vhc_tested <- rbind(vhc_tested_Idmain, vhc_tests_noIdmain) 

rm(vhc_tests_Idmain,vhc_tests_noIdmain,vhc_tested_Idmain)


# ----------------------------------------------------------- #
## PLANTILLA VHC DADES AGGS                                ####
# ----------------------------------------------------------- #
agg_base_path <- "Data/Received Aggregated data 2019/Aggregated data instructions/Aggregated data excel.xlsx"
agg_base <- setDT(read.xlsx(agg_base_path, sheet = 'HCV Indicators', colNames = FALSE));rm(agg_base_path)


# ----------------------------------------------------------- #
## LOAD VHC AGREGADES                                      ####
# ----------------------------------------------------------- #

# IMPORTANT! 
# Hi ha un pas de preprocessament de dades fet en un excel. 
# 1. Totes les dades agregades segueixen un formulari excel que conté varies pagines amb
#    diferents taules CBVCT en cada pagina.
#    Per la taula CBVCT_1, cal que tots els valors estiguin introduïts. NAs a 0s o a totals.
# 2. Per cada pàgina i per cada taula CBVCT, farem una nova taula afegint les taules dels centres
#    unes a continuació de les altres (row bind). Una nova columna identificarà les dades de cada centre.
# 3. La taula resultant ens permetra agregar per centres.


{aggs_path_1 <- "Data/Received Aggregated data 2019/Aggregated data Received files 2019/"
aggs_path_2 <- "Data/New Data 09 2019/Aggregated data/"
agg_01 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel 2019 _Poland.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_02 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_aides_01012019_31122019.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_03 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_Asocijacija DUGA_Association RAINBOW.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_04 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_HUHIV (1).xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_05 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_LEGEBITRA.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_06 <- setDT(read.xlsx(paste0(aggs_path_1, "Cobatest 2019 Aggregated data excel - Abraço.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_07 <- setDT(read.xlsx(paste0(aggs_path_1, "GDM_Moldavia_Aggregated data excel_updated.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_08 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data excel_ALLIANCE.GLOBAL_updated.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_09 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data report - Fulcrum - 2019_updated.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_10 <- setDT(read.xlsx(paste0(aggs_path_2, "COBATEST.Aggregated data excel.DEMETRA_2019.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
agg_11 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data excel FSE polonia JordiA.xlsx"), sheet= 'HCV Indicators', colNames = FALSE))
rm(aggs_path_1, aggs_path_2)}


# IMPORTANT!!!!
# Aquesta part extreu dades dels fitxers excels que els centres han carregat les dades agregades i que
# segueixen una plantilla donada (2019:  rows: 180  cols: 8).
# Per al bon funcionament del codi es fa la FORTA SUPOSICIO de que no ha canviat la seva estructura.
# Assegurar-se sempre que s'estan agafant les taules desitjades.



# ----------------------------------------------------------- #
## CBVCT_1                                                 ####
# ----------------------------------------------------------- #
# CBVCT 1:  Number of clients tested for HCV with a screening test.
# Number of clients screened for HCV (HCVScreeningTestResult != NA) and stratified 
# by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted in more than one of the 
# following; msm, sw, pwid, migrants.


# Adquisicio de taules de denominadors (CBVCT_1):
get_CBVCTtable <- function(dt, data_rows_idx, data_cols_idx, names_row_idx= NA) {
  table <- dt[data_rows_idx, ..data_cols_idx]
  if(!is.na(names_row_idx)) {colnames(table) <- c("Category", unlist(dt[names_row_idx, ..data_cols_idx])[-1])}
  return(table)}

{rows <- c(8:12); cols <- c(1:7); names <- 7
  agg_00_hcvCBVCT_1 <- get_CBVCTtable(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_01_hcvCBVCT_1 <- get_CBVCTtable(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_02_hcvCBVCT_1 <- get_CBVCTtable(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_03_hcvCBVCT_1 <- get_CBVCTtable(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_04_hcvCBVCT_1 <- get_CBVCTtable(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_05_hcvCBVCT_1 <- get_CBVCTtable(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_06_hcvCBVCT_1 <- get_CBVCTtable(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_07_hcvCBVCT_1 <- get_CBVCTtable(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_08_hcvCBVCT_1 <- get_CBVCTtable(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_09_hcvCBVCT_1 <- get_CBVCTtable(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_10_hcvCBVCT_1 <- get_CBVCTtable(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_11_hcvCBVCT_1 <- get_CBVCTtable(agg_11, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  rm(rows,cols,names)}

# Afegim columna Centre
{agg_00_hcvCBVCT_1[, Centre:= "BBDD Final"]
  agg_01_hcvCBVCT_1[, Centre:= "Poland National AIDS Centre"]
  agg_02_hcvCBVCT_1[, Centre:= "Aides"]
  agg_03_hcvCBVCT_1[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hcvCBVCT_1[, Centre:= "HUHIV"]
  agg_05_hcvCBVCT_1[, Centre:= "Legebitra"]
  agg_06_hcvCBVCT_1[, Centre:= "Associação Abraço"]
  agg_07_hcvCBVCT_1[, Centre:= "GenderdocM"]
  agg_08_hcvCBVCT_1[, Centre:= "Alliance GLobal"]
  agg_09_hcvCBVCT_1[, Centre:= "Fulcrum"]
  agg_10_hcvCBVCT_1[, Centre:= "Demetra"]
  agg_11_hcvCBVCT_1[, Centre:= "FSE Polonia"]}

# Fem el RowBind
hcvCBVCT_1 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hcvCBVCT_1"))); rm(list= ls(pattern = "_hcvCBVCT_1"))


# Poblem valors de "BBDD Final" per a CBVCT_1
{hcvCBVCT_1[Category == "MSM" & Centre == "BBDD Final", All := vhc_tested[MSM == 1, .N]]
  hcvCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Males := vhc_tested[MSM == 1 & Gender == 1, .N]]
  hcvCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  hcvCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Transgender := vhc_tested[MSM == 1 & Gender == 3, .N]]
  hcvCBVCT_1[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vhc_tested[MSM == 1 & AgeGroup == 1, .N]]
  hcvCBVCT_1[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vhc_tested[MSM == 1 & AgeGroup == 2, .N]]
  hcvCBVCT_1[Category == "SW" & Centre == "BBDD Final", All := vhc_tested[SW == 1, .N]]
  hcvCBVCT_1[Category == "SW" & Centre == "BBDD Final", Males := vhc_tested[SW == 1 & Gender == 1, .N]]
  hcvCBVCT_1[Category == "SW" & Centre == "BBDD Final", Females := vhc_tested[SW == 1 & Gender == 2, .N]]
  hcvCBVCT_1[Category == "SW" & Centre == "BBDD Final", Transgender := vhc_tested[SW == 1 & Gender == 3, .N]]
  hcvCBVCT_1[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vhc_tested[SW == 1 & AgeGroup == 1, .N]]
  hcvCBVCT_1[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vhc_tested[SW == 1 & AgeGroup == 2, .N]]
  hcvCBVCT_1[Category == "IDU" & Centre == "BBDD Final", All := vhc_tested[PWID == 1, .N]]
  hcvCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Males := vhc_tested[PWID == 1 & Gender == 1, .N]]
  hcvCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Females := vhc_tested[PWID == 1 & Gender == 2, .N]]
  hcvCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Transgender := vhc_tested[PWID == 1 & Gender == 3, .N]]
  hcvCBVCT_1[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vhc_tested[PWID == 1 & AgeGroup == 1, .N]]
  hcvCBVCT_1[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vhc_tested[PWID == 1 & AgeGroup == 2, .N]]
  hcvCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", All := vhc_tested[Migrant == 1, .N]]
  hcvCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Males := vhc_tested[Migrant == 1 & Gender == 1, .N]]
  hcvCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Females := vhc_tested[Migrant == 1 & Gender == 2, .N]]
  hcvCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Transgender := vhc_tested[Migrant == 1 & Gender == 3, .N]]
  hcvCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vhc_tested[Migrant == 1 & AgeGroup == 1, .N]]
  hcvCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vhc_tested[Migrant == 1 & AgeGroup == 2, .N]]
  hcvCBVCT_1[Category == "All" & Centre == "BBDD Final", All := vhc_tested[, .N]]
  hcvCBVCT_1[Category == "All" & Centre == "BBDD Final", Males := vhc_tested[Gender == 1, .N]]
  hcvCBVCT_1[Category == "All" & Centre == "BBDD Final", Females := vhc_tested[Gender == 2, .N]]
  hcvCBVCT_1[Category == "All" & Centre == "BBDD Final", Transgender := vhc_tested[Gender == 3, .N]]
  hcvCBVCT_1[Category == "All" & Centre == "BBDD Final", `<25 years old` := vhc_tested[AgeGroup == 1, .N]]
  hcvCBVCT_1[Category == "All" & Centre == "BBDD Final", `25+ years old` := vhc_tested[AgeGroup == 2, .N]]}

# fem les sumes
denoms_CBVCT_Totals <- hcvCBVCT_1[, .(All= sum(as.numeric(All), na.rm = T), 
                                      Males= sum(as.numeric(Males), na.rm = T), 
                                      Females= sum(as.numeric(Females), na.rm = T), 
                                      Transgender= sum(as.numeric(Transgender), na.rm = T),
                                      `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                      `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                  by= .(Category)]

# # Tornem a posar el nom original de la columna Category.
# setnames(result_hcvCBVCT_01, old= "Category", new= unlist(agg_base[7,1]))
# Reconstruim la taula CBVCT1.
result_hcvCBVCT_01 <- get_CBVCTtable(agg_base, data_rows_idx = c(6:12), data_cols_idx = c(1:7))
result_hcvCBVCT_01[c(3:7), c(1:7)] <- denoms_CBVCT_Totals
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hcvCBVCT_01 <- rbind(result_hcvCBVCT_01, result_hcvCBVCT_01[NA]) 

# Comencem a construir una llista amb les dades aggregades rellevants.
hcvCBVCT_1[, Descriptiu := 'CBVCT_1']
HCV_data_agregades <- list(CBVCT_1= hcvCBVCT_1)
rm(hcvCBVCT_1)


# ----------------------------------------------------------- #
## CBVCT_5                                                 ####
# ----------------------------------------------------------- #
# CBVCT 5: Proportion of clients with reactive screening HCV test result.
# Reporting reactive screening test results (HCVScreeningTestResult = 1) as a proportion of all people tested and 
# stratified by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted in more than one of the 
# following; msm, sw, pwid, migrants.
#
#       Number of clients with a reactive screening test (HCVScreeningTestResult == 1)
#  -------------------------------------------------------------------------------------- x 100
#                      Number of clients screened for HCV 


get_CBVCTtable_rowsdata <- function(dt, data_rows_idx, data_cols_idx, names_row_idx, categories_rows_idx) {
  table <- dt[data_rows_idx, ..data_cols_idx]
  colnames(table) <- c("Category", unlist(dt[names_row_idx, ..data_cols_idx])[-1])
  table[, Category := dt[categories_rows_idx, 1]]
  return(table)}


# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 5 total: Els numeradors.
{rows <- c(83,87,91,95,99); cols <- c(1:7); names <- 80; cat_rows <- c(81,85,89,93,97)
  agg_00_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_01_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_02_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_03_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_04_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_05_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_06_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_07_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_08_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_09_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_10_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_11_hcvCBVCT_5 <- get_CBVCTtable_rowsdata(agg_11, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  rm(rows,cols,names,cat_rows)}

# Afegim columna Centre
{agg_00_hcvCBVCT_5[, Centre:= "BBDD Final"]
  agg_01_hcvCBVCT_5[, Centre:= "Poland National AIDS Centre"]
  agg_02_hcvCBVCT_5[, Centre:= "Aides"]
  agg_03_hcvCBVCT_5[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_hcvCBVCT_5[, Centre:= "HUHIV"]
  agg_05_hcvCBVCT_5[, Centre:= "Legebitra"]
  agg_06_hcvCBVCT_5[, Centre:= "Associação Abraço"]
  agg_07_hcvCBVCT_5[, Centre:= "GenderdocM"]
  agg_08_hcvCBVCT_5[, Centre:= "Alliance GLobal"]
  agg_09_hcvCBVCT_5[, Centre:= "Fulcrum"]
  agg_10_hcvCBVCT_5[, Centre:= "Demetra"]
  agg_11_hcvCBVCT_5[, Centre:= "FSE Polonia"]}

# Fem el RowBind
numeradors_CBVCT_5 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_hcvCBVCT_5")))
rm(list= ls(pattern = "_hcvCBVCT_5"))

# Poblem valors de "BBDD Final" per a CBVCT_5
{numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", All := vhc_tested[MSM == 1 & HCVScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Males := vhc_tested[MSM == 1 & HCVScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Transgender := vhc_tested[MSM == 1 & HCVScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vhc_tested[MSM == 1 & HCVScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vhc_tested[MSM == 1 & HCVScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", All := vhc_tested[SW == 1 & HCVScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Males := vhc_tested[SW == 1 & HCVScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Females := vhc_tested[SW == 1 & HCVScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Transgender := vhc_tested[SW == 1 & HCVScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vhc_tested[SW == 1 & HCVScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vhc_tested[SW == 1 & HCVScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", All := vhc_tested[PWID == 1 & HCVScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Males := vhc_tested[PWID == 1 & HCVScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Females := vhc_tested[PWID == 1 & HCVScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Transgender := vhc_tested[PWID == 1 & HCVScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vhc_tested[PWID == 1 & HCVScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vhc_tested[PWID == 1 & HCVScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", All := vhc_tested[Migrant == 1 & HCVScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Males := vhc_tested[Migrant == 1 & HCVScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Females := vhc_tested[Migrant == 1 & HCVScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Transgender := vhc_tested[Migrant == 1 & HCVScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vhc_tested[Migrant == 1 & HCVScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vhc_tested[Migrant == 1 & HCVScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", All := vhc_tested[HCVScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Males := vhc_tested[HCVScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Females := vhc_tested[HCVScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Transgender := vhc_tested[HCVScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", `<25 years old` := vhc_tested[HCVScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", `25+ years old` := vhc_tested[HCVScreeningTestResult == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_5_Totals <- numeradors_CBVCT_5[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]

### Reconstruim la taula CBVCT5. Proporcions, Numeradors i Denominadors.
table_rows <- c(79:100)
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_hcvCBVCT_05 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_hcvCBVCT_05[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_5_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_hcvCBVCT_05[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_hcvCBVCT_05[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_5_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_hcvCBVCT_05 <- rbind(result_hcvCBVCT_05, result_hcvCBVCT_05[NA]) 

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_5[, Descriptiu := 'Numeradors_CBVCT_5']
HCV_data_agregades[["Numeradors_CBVCT_5"]] <- numeradors_CBVCT_5

rm(list= ls(pattern= "agg_"))
rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, table_props_rows, numeradors_CBVCT_5, 
   get_CBVCTtable, get_CBVCTtable_rowsdata, result_hcvCBVCT_05, result_hcvCBVCT_01, numeradors_CBVCT_5_Totals,
   denoms_CBVCT_Totals ) 


# ________________________________________________________ ####
## SYPHILIS INDICATORS                                     ####
# ******************************************************** ####
# ----------------------------------------------------------- #
## SYPH TESTS                                                ####
# ----------------------------------------------------------- #
# Filtrem dataset pels que han estat testats per SYPH.
syph_tests <- copy(cobatest_tool_final[!is.na(SyphScreeningTestResult)])

# Processat de les edats.
# LAURA: Respecto a los menores de 16 años, he revisado los casos, y he visto que los casos de menores reales
#       (no atribuïbles a errores de data entry con la fecha de nacimiento) tienen informado el grupo de edad. Así que si 
#       la edad és inferior a 16 años y AgeGroupc== 1, se deberían eliminar. Los que quedan, menores de 16, son 
#       errores en el data entry. Deben tener AgeGroup a NA pero podemos sacar la otra informacion.
# COMPROBACION: cobatest_tool_final[AgeInYears < 16, .N, by= .(trunc_age= trunc(AgeInYears),AgeGroup)][order(trunc_age)]
syph_tests[, Menors_reals := ifelse(test = AgeInYears < 16 & AgeGroup == 1, yes = 1, no = 0)]
syph_tests <- syph_tests[Menors_reals != 1 | is.na(Menors_reals), ][, Menors_reals := NULL]

# ----------------------------------------------------------- #
## SYPH SENSE DUPLICATS                                     ####
# ----------------------------------------------------------- #
## Busquem duplicats de SYPH
# - LAURA ---------------------- #
#   En nuestra BBDD, generamos la variable “Id”, para poder identificar repetidores y quedarnos únicamente con las personas testadas, 
#   y poder calcular los indicadores con las personas testadas. 
#   Los centros pueden haber usado cualquiera de estos nombres de variables para su identificador único de cliente. He marcado en 
#   rojo en el Excel de las variables la variable que se debe usar para cada centro, para identificar repetidores. 

# COMPROBACION: syph_tests[, .N]                                                        # registres
# COMPROBACION: syph_tests[, .N] - vih_tests[, uniqueN(Id_main)]                        # registres duplicats
# COMPROBACION: syph_tests[, uniqueN(Id_main, na.rm = T)]                               # persones
# COMPROBACION: syph_tests[, .N, by = .(Id_main)][N > 1, uniqueN(Id_main, na.rm = T)]   # persones repetides
# COMPROBACION: syph_tests[, .N, by = .(Id_main)][N > 4, Id_main]                       # Ids
# COMPROBACION: syph_tests[, .N, by = .(Id_main)][N > 1, ][order(Id_main), Id_main]


## COMPTE!! 
#  Hi ha uns codis a Id_main: .00, .00M, 0000, 000A, 000B, 000G, 000L, 000M, 000P, 000R i 000S,
#  Que poden estar duplicats i estan referits a persones diferents:
# t(syph_tests[Id_main == ".00"])    # hi ha 1. Sense cobatest id.
# t(syph_tests[Id_main == ".00M"])   # cap
# t(syph_tests[Id_main == "0000"])   # hi ha 1. Sense cobatest id.
# t(syph_tests[Id_main == "000A"])   # cap
# t(syph_tests[Id_main == "000B"])   # cap
# t(syph_tests[Id_main == "000G"])   # hi ha 1. Té cobatest id.
# t(syph_tests[Id_main == "000L"])   # cap
# t(syph_tests[Id_main == "000M"])   # cap.
# t(syph_tests[Id_main == "000P"])   # cap.
# t(syph_tests[Id_main == "000R"])   # cap.
# t(syph_tests[Id_main == "000S"])   # cap.
## Adjudiquem-los un id: cobatest_id U cbvctidentifier.
{syph_tests[Id_main == ".00", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == ".00M", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "0000", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000A", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000B", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000G", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000L", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000M", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000P", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000R", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]
  syph_tests[Id_main == "000S", Id_main := paste0(COBATEST_Id, "-", CBVCTIdentifier)]}

# Elegim, dels duplicats els de data més recent.
syph_tests[, DateofVisit := as.Date(DateofVisit)]

# Traiem duplicats tenint en compte que els Centres 28 i 59 no tenen Id_main.
syph_tests_noIdmain <- syph_tests[is.na(Id_main),]
syph_tests_Idmain <- syph_tests[!is.na(Id_main),]

# Fora duplicats.
syph_tested_Idmain <- syph_tests_Idmain[syph_tests_Idmain[, .I[which.max(DateofVisit)], by = Id_main]$V1]
# Assumim que no hi ha duplicats en els que no tenen id_main (Centres 28 i 59 no tenen Id_main).
syph_tested <- rbind(syph_tested_Idmain, syph_tests_noIdmain) 

rm(syph_tests_Idmain,syph_tests_noIdmain,syph_tested_Idmain)

# ----------------------------------------------------------- #
## PLANTILLA SYPH DADES AGGS                                ####
# ----------------------------------------------------------- #
agg_base_path <- "Data/Received Aggregated data 2019/Aggregated data instructions/Aggregated data excel.xlsx"
agg_base <- setDT(read.xlsx(agg_base_path, sheet = 'Syphilis Indicators', colNames = FALSE));rm(agg_base_path)


# ----------------------------------------------------------- #
## LOAD SYPH AGREGADES                                     ####
# ----------------------------------------------------------- #

# IMPORTANT! 
# Hi ha un pas de preprocessament de dades fet en un excel. 
# 1. Totes les dades agregades segueixen un formulari excel que conté varies pagines amb
#    diferents taules CBVCT en cada pagina.
#    Per la taula CBVCT_1, cal que tots els valors estiguin introduïts. NAs a 0s o a totals.
# 2. Per cada pàgina i per cada taula CBVCT, farem una nova taula afegint les taules dels centres
#    unes a continuació de les altres (row bind). Una nova columna identificarà les dades de cada centre.
# 3. La taula resultant ens permetra agregar per centres.


{aggs_path_1 <- "Data/Received Aggregated data 2019/Aggregated data Received files 2019/"
aggs_path_2 <- "Data/New Data 09 2019/Aggregated data/"
agg_01 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel 2019 _Poland.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_02 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_aides_01012019_31122019.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_03 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_Asocijacija DUGA_Association RAINBOW.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_04 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_HUHIV (1).xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_05 <- setDT(read.xlsx(paste0(aggs_path_1, "Aggregated data excel_LEGEBITRA.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_06 <- setDT(read.xlsx(paste0(aggs_path_1, "Cobatest 2019 Aggregated data excel - Abraço.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_07 <- setDT(read.xlsx(paste0(aggs_path_1, "GDM_Moldavia_Aggregated data excel_updated.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_08 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data excel_ALLIANCE.GLOBAL_updated.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_09 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data report - Fulcrum - 2019_updated.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_10 <- setDT(read.xlsx(paste0(aggs_path_2, "COBATEST.Aggregated data excel.DEMETRA_2019.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
agg_11 <- setDT(read.xlsx(paste0(aggs_path_2, "Aggregated data excel FSE polonia JordiA.xlsx"), sheet= 'Syphilis Indicators', colNames = FALSE))
rm(aggs_path_1, aggs_path_2)}


# IMPORTANT!!!!
# Aquesta part extreu dades dels fitxers excels que els centres han carregat les dades agregades i que
# segueixen una plantilla donada (2019:  rows: 180  cols: 8).
# Per al bon funcionament del codi es fa la FORTA SUPOSICIO de que no ha canviat la seva estructura.
# Assegurar-se sempre que s'estan agafant les taules desitjades.




# ----------------------------------------------------------- #
## CBVCT_1                                                 ####
# ----------------------------------------------------------- #
# CBVCT 1:  Number of clients tested for Syphilis with a screening test.
# Number of clients screened for HCV (SyphScreeningTestResult != NA) and stratified 
# by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted in more than one of the 
# following; msm, sw, pwid, migrants.


# Adquisicio de taules de denominadors (CBVCT_1):
get_CBVCTtable <- function(dt, data_rows_idx, data_cols_idx, names_row_idx= NA) {
  table <- dt[data_rows_idx, ..data_cols_idx]
  if(!is.na(names_row_idx)) {colnames(table) <- c("Category", unlist(dt[names_row_idx, ..data_cols_idx])[-1])}
  return(table)}

{rows <- c(8:12); cols <- c(1:7); names <- 7
  agg_00_syphCBVCT_1 <- get_CBVCTtable(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_01_syphCBVCT_1 <- get_CBVCTtable(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_02_syphCBVCT_1 <- get_CBVCTtable(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_03_syphCBVCT_1 <- get_CBVCTtable(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_04_syphCBVCT_1 <- get_CBVCTtable(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_05_syphCBVCT_1 <- get_CBVCTtable(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_06_syphCBVCT_1 <- get_CBVCTtable(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_07_syphCBVCT_1 <- get_CBVCTtable(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_08_syphCBVCT_1 <- get_CBVCTtable(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_09_syphCBVCT_1 <- get_CBVCTtable(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_10_syphCBVCT_1 <- get_CBVCTtable(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  agg_11_syphCBVCT_1 <- get_CBVCTtable(agg_11, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names)
  rm(rows,cols,names)}

# Afegim columna Centre
{agg_00_syphCBVCT_1[, Centre:= "BBDD Final"]
  agg_01_syphCBVCT_1[, Centre:= "Poland National AIDS Centre"]
  agg_02_syphCBVCT_1[, Centre:= "Aides"]
  agg_03_syphCBVCT_1[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_syphCBVCT_1[, Centre:= "HUHIV"]
  agg_05_syphCBVCT_1[, Centre:= "Legebitra"]
  agg_06_syphCBVCT_1[, Centre:= "Associação Abraço"]
  agg_07_syphCBVCT_1[, Centre:= "GenderdocM"]
  agg_08_syphCBVCT_1[, Centre:= "Alliance GLobal"]
  agg_09_syphCBVCT_1[, Centre:= "Fulcrum"]
  agg_10_syphCBVCT_1[, Centre:= "Demetra"]
  agg_11_syphCBVCT_1[, Centre:= "FSE Polonia"]}

# Fem el RowBind
syphCBVCT_1 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_syphCBVCT_1"))); rm(list= ls(pattern = "_syphCBVCT_1"))


# Poblem valors de "BBDD Final" per a CBVCT_1
{syphCBVCT_1[Category == "MSM" & Centre == "BBDD Final", All := vhc_tested[MSM == 1, .N]]
  syphCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Males := vhc_tested[MSM == 1 & Gender == 1, .N]]
  syphCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  syphCBVCT_1[Category == "MSM" & Centre == "BBDD Final", Transgender := vhc_tested[MSM == 1 & Gender == 3, .N]]
  syphCBVCT_1[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := vhc_tested[MSM == 1 & AgeGroup == 1, .N]]
  syphCBVCT_1[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := vhc_tested[MSM == 1 & AgeGroup == 2, .N]]
  syphCBVCT_1[Category == "SW" & Centre == "BBDD Final", All := vhc_tested[SW == 1, .N]]
  syphCBVCT_1[Category == "SW" & Centre == "BBDD Final", Males := vhc_tested[SW == 1 & Gender == 1, .N]]
  syphCBVCT_1[Category == "SW" & Centre == "BBDD Final", Females := vhc_tested[SW == 1 & Gender == 2, .N]]
  syphCBVCT_1[Category == "SW" & Centre == "BBDD Final", Transgender := vhc_tested[SW == 1 & Gender == 3, .N]]
  syphCBVCT_1[Category == "SW" & Centre == "BBDD Final", `<25 years old` := vhc_tested[SW == 1 & AgeGroup == 1, .N]]
  syphCBVCT_1[Category == "SW" & Centre == "BBDD Final", `25+ years old` := vhc_tested[SW == 1 & AgeGroup == 2, .N]]
  syphCBVCT_1[Category == "IDU" & Centre == "BBDD Final", All := vhc_tested[PWID == 1, .N]]
  syphCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Males := vhc_tested[PWID == 1 & Gender == 1, .N]]
  syphCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Females := vhc_tested[PWID == 1 & Gender == 2, .N]]
  syphCBVCT_1[Category == "IDU" & Centre == "BBDD Final", Transgender := vhc_tested[PWID == 1 & Gender == 3, .N]]
  syphCBVCT_1[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := vhc_tested[PWID == 1 & AgeGroup == 1, .N]]
  syphCBVCT_1[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := vhc_tested[PWID == 1 & AgeGroup == 2, .N]]
  syphCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", All := vhc_tested[Migrant == 1, .N]]
  syphCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Males := vhc_tested[Migrant == 1 & Gender == 1, .N]]
  syphCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Females := vhc_tested[Migrant == 1 & Gender == 2, .N]]
  syphCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", Transgender := vhc_tested[Migrant == 1 & Gender == 3, .N]]
  syphCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := vhc_tested[Migrant == 1 & AgeGroup == 1, .N]]
  syphCBVCT_1[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := vhc_tested[Migrant == 1 & AgeGroup == 2, .N]]
  syphCBVCT_1[Category == "All" & Centre == "BBDD Final", All := vhc_tested[, .N]]
  syphCBVCT_1[Category == "All" & Centre == "BBDD Final", Males := vhc_tested[Gender == 1, .N]]
  syphCBVCT_1[Category == "All" & Centre == "BBDD Final", Females := vhc_tested[Gender == 2, .N]]
  syphCBVCT_1[Category == "All" & Centre == "BBDD Final", Transgender := vhc_tested[Gender == 3, .N]]
  syphCBVCT_1[Category == "All" & Centre == "BBDD Final", `<25 years old` := vhc_tested[AgeGroup == 1, .N]]
  syphCBVCT_1[Category == "All" & Centre == "BBDD Final", `25+ years old` := vhc_tested[AgeGroup == 2, .N]]}

# fem les sumes
denoms_CBVCT_Totals <- syphCBVCT_1[, .(All= sum(as.numeric(All), na.rm = T), 
                                       Males= sum(as.numeric(Males), na.rm = T), 
                                       Females= sum(as.numeric(Females), na.rm = T), 
                                       Transgender= sum(as.numeric(Transgender), na.rm = T),
                                       `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                       `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                   by= .(Category)]

# # Tornem a posar el nom original de la columna Category.
# setnames(result_syphCBVCT_01, old= "Category", new= unlist(agg_base[7,1]))
# Reconstruim la taula CBVCT1.
result_syphCBVCT_01 <- get_CBVCTtable(agg_base, data_rows_idx = c(6:12), data_cols_idx = c(1:7))
result_syphCBVCT_01[c(3:7), c(1:7)] <- denoms_CBVCT_Totals
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_syphCBVCT_01 <- rbind(result_syphCBVCT_01, result_syphCBVCT_01[NA]) 

# Comencem a construir una llista amb les dades aggregades rellevants.
syphCBVCT_1[, Descriptiu := 'CBVCT_1']
SYPH_data_agregades <- list(CBVCT_1= syphCBVCT_1)
rm(syphCBVCT_1)




# ----------------------------------------------------------- #
## CBVCT_5                                                 ####
# ----------------------------------------------------------- #
# CBVCT 5: Proportion of clients with reactive screening Syphilis test result.
# Reporting reactive screening test results (SyphScreeningTestResult = 1) as a proportion of all people tested and 
# stratified by gender, agegroup2, msm, sw, pwid, migrants. Clients can be counted in more than one of the 
# following; msm, sw, pwid, migrants.
#
#       Number of clients with a reactive screening test (SyphScreeningTestResult == 1)
#  -------------------------------------------------------------------------------------- x 100
#                      Number of clients screened for Syphilis


get_CBVCTtable_rowsdata <- function(dt, data_rows_idx, data_cols_idx, names_row_idx, categories_rows_idx) {
  table <- dt[data_rows_idx, ..data_cols_idx]
  colnames(table) <- c("Category", unlist(dt[names_row_idx, ..data_cols_idx])[-1])
  table[, Category := dt[categories_rows_idx, 1]]
  return(table)}


# Adquisicio de les dades introduïdes (no calculades amb excel) per generar la CBVCT 5 total: Els numeradors.
{rows <- c(83,87,91,95,99); cols <- c(1:7); names <- 80; cat_rows <- c(81,85,89,93,97)
  agg_00_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_base, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_01_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_01, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_02_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_02, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_03_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_03, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_04_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_04, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_05_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_05, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_06_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_06, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_07_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_07, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_08_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_08, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_09_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_09, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_10_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_10, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  agg_11_syphCBVCT_5 <- get_CBVCTtable_rowsdata(agg_11, data_rows_idx = rows, data_cols_idx = cols, names_row_idx = names, categories_rows_idx = cat_rows)
  rm(rows,cols,names,cat_rows)}

# Afegim columna Centre
{agg_00_syphCBVCT_5[, Centre:= "BBDD Final"]
  agg_01_syphCBVCT_5[, Centre:= "Poland National AIDS Centre"]
  agg_02_syphCBVCT_5[, Centre:= "Aides"]
  agg_03_syphCBVCT_5[, Centre:= "Asocijacija Duga - Rainbow"]
  agg_04_syphCBVCT_5[, Centre:= "HUHIV"]
  agg_05_syphCBVCT_5[, Centre:= "Legebitra"]
  agg_06_syphCBVCT_5[, Centre:= "Associação Abraço"]
  agg_07_syphCBVCT_5[, Centre:= "GenderdocM"]
  agg_08_syphCBVCT_5[, Centre:= "Alliance GLobal"]
  agg_09_syphCBVCT_5[, Centre:= "Fulcrum"]
  agg_10_syphCBVCT_5[, Centre:= "Demetra"]
  agg_11_syphCBVCT_5[, Centre:= "FSE Polonia"]}

# Fem el RowBind
numeradors_CBVCT_5 <- Reduce(function(...) rbind(...), mget(ls(pattern = "_syphCBVCT_5")))
rm(list= ls(pattern = "_syphCBVCT_5"))

# Poblem valors de "BBDD Final" per a CBVCT_5
{numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", All := syph_tested[MSM == 1 & SyphScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Males := syph_tested[MSM == 1 & SyphScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Females := NA]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", Transgender := syph_tested[MSM == 1 & SyphScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", `<25 years old` := syph_tested[MSM == 1 & SyphScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "MSM" & Centre == "BBDD Final", `25+ years old` := syph_tested[MSM == 1 & SyphScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", All := syph_tested[SW == 1 & SyphScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Males := syph_tested[SW == 1 & SyphScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Females := syph_tested[SW == 1 & SyphScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", Transgender := syph_tested[SW == 1 & SyphScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", `<25 years old` := syph_tested[SW == 1 & SyphScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "SW" & Centre == "BBDD Final", `25+ years old` := syph_tested[SW == 1 & SyphScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", All := syph_tested[PWID == 1 & SyphScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Males := syph_tested[PWID == 1 & SyphScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Females := syph_tested[PWID == 1 & SyphScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", Transgender := syph_tested[PWID == 1 & SyphScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", `<25 years old` := syph_tested[PWID == 1 & SyphScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "IDU" & Centre == "BBDD Final", `25+ years old` := syph_tested[PWID == 1 & SyphScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", All := syph_tested[Migrant == 1 & SyphScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Males := syph_tested[Migrant == 1 & SyphScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Females := syph_tested[Migrant == 1 & SyphScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", Transgender := syph_tested[Migrant == 1 & SyphScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", `<25 years old` := syph_tested[Migrant == 1 & SyphScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "Migrants" & Centre == "BBDD Final", `25+ years old` := syph_tested[Migrant == 1 & SyphScreeningTestResult == 1 & AgeGroup == 2, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", All := syph_tested[SyphScreeningTestResult == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Males := syph_tested[SyphScreeningTestResult == 1 & Gender == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Females := syph_tested[SyphScreeningTestResult == 1 & Gender == 2, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", Transgender := syph_tested[SyphScreeningTestResult == 1 & Gender == 3, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", `<25 years old` := syph_tested[SyphScreeningTestResult == 1 & AgeGroup == 1, .N]]
  numeradors_CBVCT_5[Category == "All" & Centre == "BBDD Final", `25+ years old` := syph_tested[SyphScreeningTestResult == 1 & AgeGroup == 2, .N]]}

# fem les sumes
numeradors_CBVCT_5_Totals <- numeradors_CBVCT_5[, .(All= sum(as.numeric(All), na.rm = T), 
                                                    Males= sum(as.numeric(Males), na.rm = T), 
                                                    Females= sum(as.numeric(Females), na.rm = T), 
                                                    Transgender= sum(as.numeric(Transgender), na.rm = T),
                                                    `<25 years old`= sum(as.numeric(`<25 years old`), na.rm = T), 
                                                    `25+ years old`= sum(as.numeric(`25+ years old`), na.rm = T)),
                                                by= .(Category)]

### Reconstruim la taula CBVCT5. Proporcions, Numeradors i Denominadors.
table_rows <- c(79:100)
table_cols <- c(1:7)
table_numerad_rows <- c(5,9,13,17,21)
table_denom_rows <- table_numerad_rows + 1 
table_props_rows <- table_numerad_rows - 1 
cols_with_data <- c(2:7)

# Recuperem la taula. 
result_syphCBVCT_05 <- get_CBVCTtable(dt = agg_base, data_rows_idx = table_rows, data_cols_idx = table_cols)
# Afegim numeradors.
result_syphCBVCT_05[table_numerad_rows, cols_with_data] <- numeradors_CBVCT_5_Totals[, ..cols_with_data]
# Afegim denominadors. 
result_syphCBVCT_05[table_denom_rows, cols_with_data] <- denoms_CBVCT_Totals[, ..cols_with_data]
# Calculem proporcions. 
result_syphCBVCT_05[table_props_rows, cols_with_data] <- round(numeradors_CBVCT_5_Totals[, ..cols_with_data]/denoms_CBVCT_Totals[, ..cols_with_data]*100, digits = 2)
# Padding 1 fila de NAs per ajuntar després tot en una taula.
result_syphCBVCT_05 <- rbind(result_syphCBVCT_05, result_syphCBVCT_05[NA]) 

# afegim a la llista de les dades aggregades rellevants.
numeradors_CBVCT_5[, Descriptiu := 'Numeradors_CBVCT_5']
SYPH_data_agregades[["Numeradors_CBVCT_5"]] <- numeradors_CBVCT_5

rm(list= ls(pattern= "agg_"))
rm(table_rows, table_cols, cols_with_data, table_denom_rows, table_numerad_rows, table_props_rows, numeradors_CBVCT_5, 
   get_CBVCTtable, get_CBVCTtable_rowsdata, result_syphCBVCT_05, result_syphCBVCT_01, numeradors_CBVCT_5_Totals,
   denoms_CBVCT_Totals ) 


# ________________________________________________________ ####
## HIV TABLES REPORT                                       ####
# ******************************************************** ####
# ----------------------------------------------------------- #
## TABLE 1                                                 ####
# ----------------------------------------------------------- #
# Table 1: Summary of people screened for HIV. 

{# Reconstruim l'estructura de la taula. 
  table_1 <- data.table(X1= c('Persons tested','Age Group',NA,'Gender',NA,NA,'Migrant','PWID','SW',NA,NA,
                              'MSM','Previous HIV test','Tested in last 12 months','Test last 12 months in this CBVCT',
                              'False positive','Confirmatory HIV test','Positive confirmatory HIV test'),
                        X2= c(NA,'<25','>=25','Male','Female','Transgender','Yes',NA,'MSW','FSW','TSW',NA,NA,NA,NA,NA,NA,NA))
  
  # Omplenem valors.
  # persons tested.
  persons_tested_total <- as.numeric(ind_hiv_CBVCT[7, 2]); persons_tested_react <- as.numeric(ind_hiv_CBVCT[98, 2])
  table_1[X1 == "Persons tested", Total := persons_tested_total]
  table_1[X1 == "Persons tested", Reactive := persons_tested_react]
  table_1[X1 == "Persons tested", Perc_Reactive := round(persons_tested_react/persons_tested_total*100, digits = 1)]
  rm(persons_tested_total,persons_tested_react)
  # < 25 tested
  less_25_total <- as.numeric(ind_hiv_CBVCT[7, 6]); less_25_react <-  as.numeric(ind_hiv_CBVCT[98, 6]) 
  table_1[X2 == "<25", Total := less_25_total]
  table_1[X2 == "<25", Reactive := less_25_react]
  table_1[X2 == "<25", Perc_Reactive := round(less_25_react/less_25_total*100, digits = 1)]
  rm(less_25_total, less_25_react)
  # >= 25 tested
  greq_25_total <- as.numeric(ind_hiv_CBVCT[7, 7]); greq_25_react <-  as.numeric(ind_hiv_CBVCT[98, 7]) 
  table_1[X2 == ">=25", Total := greq_25_total]
  table_1[X2 == ">=25", Reactive := greq_25_react]
  table_1[X2 == ">=25", Perc_Reactive := round(greq_25_react/greq_25_total*100, digits = 1)]
  rm(greq_25_total, greq_25_react)
  # Male tested
  male_total <- as.numeric(ind_hiv_CBVCT[7, 3]); male_react <-  as.numeric(ind_hiv_CBVCT[98, 3]) 
  table_1[X2 == "Male", Total := male_total]
  table_1[X2 == "Male", Reactive := male_react]
  table_1[X2 == "Male", Perc_Reactive := round(male_react/male_total*100, digits = 1)]
  rm(male_total, male_react)
  # Female tested
  female_total <- as.numeric(ind_hiv_CBVCT[7, 4]); female_react <-  as.numeric(ind_hiv_CBVCT[98, 4]) 
  table_1[X2 == "Female", Total := female_total]
  table_1[X2 == "Female", Reactive := female_react]
  table_1[X2 == "Female", Perc_Reactive := round(female_react/female_total*100, digits = 1)]
  rm(female_total, female_react)
  # Trans tested
  trans_total <- as.numeric(ind_hiv_CBVCT[7, 5]); trans_react <-  as.numeric(ind_hiv_CBVCT[98, 5]) 
  table_1[X2 == "Transgender", Total := trans_total]
  table_1[X2 == "Transgender", Reactive := trans_react]
  table_1[X2 == "Transgender", Perc_Reactive := round(trans_react/trans_total*100, digits = 1)]
  rm(trans_total, trans_react)
  # Migrant tested
  migrant_total <- as.numeric(ind_hiv_CBVCT[6, 2]); migrant_react <-  as.numeric(ind_hiv_CBVCT[94, 2]) 
  table_1[X1 == "Migrant", Total := migrant_total]
  table_1[X1 == "Migrant", Reactive := migrant_react]
  table_1[X1 == "Migrant", Perc_Reactive := round(migrant_react/migrant_total*100, digits = 1)]
  rm(migrant_total, migrant_react)
  # PWID tested
  pwid_total <- as.numeric(ind_hiv_CBVCT[5, 2]); pwid_react <-  as.numeric(ind_hiv_CBVCT[90, 2]) 
  table_1[X1 == "PWID", Total := pwid_total]
  table_1[X1 == "PWID", Reactive := pwid_react]
  table_1[X1 == "PWID", Perc_Reactive := round(pwid_react/pwid_total*100, digits = 1)]
  rm(pwid_total, pwid_react)
  # SW-MSW tested
  sw_msw_total <- as.numeric(ind_hiv_CBVCT[4, 3]); sw_msw_react <-  as.numeric(ind_hiv_CBVCT[86, 3]) 
  table_1[X2 == "MSW", Total := sw_msw_total]
  table_1[X2 == "MSW", Reactive := sw_msw_react]
  table_1[X2 == "MSW", Perc_Reactive := round(sw_msw_react/sw_msw_total*100, digits = 1)]
  rm(sw_msw_total, sw_msw_react)
  # SW-FSW tested
  sw_fsw_total <- as.numeric(ind_hiv_CBVCT[4, 4]); sw_fsw_react <-  as.numeric(ind_hiv_CBVCT[86, 4]) 
  table_1[X2 == "FSW", Total := sw_fsw_total]
  table_1[X2 == "FSW", Reactive := sw_fsw_react]
  table_1[X2 == "FSW", Perc_Reactive := round(sw_fsw_react/sw_fsw_total*100, digits = 1)]
  rm(sw_fsw_total, sw_fsw_react)
  # SW-TSW tested
  sw_tsw_total <- as.numeric(ind_hiv_CBVCT[4, 5]); sw_tsw_react <-  as.numeric(ind_hiv_CBVCT[86, 5]) 
  table_1[X2 == "TSW", Total := sw_tsw_total]
  table_1[X2 == "TSW", Reactive := sw_tsw_react]
  table_1[X2 == "TSW", Perc_Reactive := round(sw_tsw_react/sw_tsw_total*100, digits = 1)]
  rm(sw_tsw_total, sw_tsw_react)
  # MSM tested
  msm_total <- as.numeric(ind_hiv_CBVCT[3, 2]); msm_react <-  as.numeric(ind_hiv_CBVCT[82, 2]) 
  table_1[X1 == "MSM", Total := msm_total]
  table_1[X1 == "MSM", Reactive := msm_react]
  table_1[X1 == "MSM", Perc_Reactive := round(msm_react/msm_total*100, digits = 1)]
  rm(msm_total, msm_react)
  # Previous HIV test
  testedBefore_total <- as.numeric(ind_hiv_CBVCT[29, 2]) 
  table_1[X1 == "Previous HIV test", Total := testedBefore_total]
  rm(testedBefore_total)
  # Tested in last 12 months.
  testedlastyear_total <- as.numeric(ind_hiv_CBVCT[52, 2]) 
  table_1[X1 == "Tested in last 12 months", Total := testedlastyear_total]
  rm(testedlastyear_total)
  # Tested in last 12 months in same facility.
  testedlastyear_sameFac_total <- as.numeric(ind_hiv_CBVCT[75, 2]) 
  table_1[X1 == "Test last 12 months in this CBVCT", Total := testedlastyear_sameFac_total]
  rm(testedlastyear_sameFac_total)
  # False positives.
  falsepositive_total <- as.numeric(ind_hiv_CBVCT[167, 2])
  table_1[X1 == "False positive", Total := falsepositive_total]
  rm(falsepositive_total)  
  # Confirmatory HIV test.
  confirmatory_total <- as.numeric(ind_hiv_CBVCT[121, 2]) 
  table_1[X1 == "Confirmatory HIV test", Total := confirmatory_total]
  rm(confirmatory_total)  
  # Positive confirmatory HIV test.
  confirmatory_positive_total <- as.numeric(ind_hiv_CBVCT[144, 2]) 
  table_1[X1 == "Positive confirmatory HIV test", Total := confirmatory_positive_total]
  rm(confirmatory_positive_total)}


# ----------------------------------------------------------- #
## TABLE 2                                                 ####
# ----------------------------------------------------------- #
{table_2 <- data.table(X1= c('Participating centres (n)','People tested (n)', 'People with a reactive HIV screening test (%)',
                             'People with a reactive HIV screening test (n)', 
                             'People tested with a confirmatory test (%, as % of all reactive results)',
                             'People with positive confirmatory test result (%)'))

n_centers <- center_maps[Origen_2019 != 'No_data', .N]
persons_tested_total <- as.numeric(ind_hiv_CBVCT[7, 2])
persons_tested_react <- as.numeric(ind_hiv_CBVCT[98, 2])
persons_confirmatory_total <- as.numeric(ind_hiv_CBVCT[121, 2]) 
confirmatory_positive_total <- as.numeric(ind_hiv_CBVCT[144, 2]) 

table_2[X1 == 'Participating centres (n)', `2019` := as.character(n_centers)]
table_2[X1 == 'People tested (n)', `2019` := persons_tested_total]
table_2[X1 == 'People with a reactive HIV screening test (%)', `2019` := as.character(round(persons_tested_react/persons_tested_total*100, digits = 1))]
table_2[X1 == 'People with a reactive HIV screening test (n)', `2019` := as.character(persons_tested_react)]
table_2[X1 == 'People tested with a confirmatory test (%, as % of all reactive results)', `2019` := as.character(round(persons_confirmatory_total/persons_tested_react*100, digits = 1))]
table_2[X1 == 'People with positive confirmatory test result (%)', `2019` := as.character(round(confirmatory_positive_total/persons_tested_total*100, digits = 1))]}


rm(persons_tested_total,persons_tested_react,n_centers,persons_confirmatory_total,confirmatory_positive_total)


# Save the results
# Lists elements are written to individual worksheets, using list names as sheet names if available
sheets <- list("Table_1" = table_1, 
               "Table_2" = table_2)

date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = sheets, file = paste0(output_path,"tables_Report_",date,".xlsx"), col.names= TRUE)
rm(sheets, output_path, date)


# ________________________________________________________ ####
## HIV FIGURES REPORT                                      ####
# ******************************************************** ####
# ----------------------------------------------------------- #
##  FIGURE 1                                               ####
# ----------------------------------------------------------- #
# Figure 1: Flowchart of HIV testing data submission - COBATEST Network 2019.
# cobatest_tool_final[Centre %in% center_maps[Origen_2019 %in% c('Cobatest_tool','Datos_desagregados'), Centre], .N,
#                     by= .(ScreeningHIVTest, HCVScreeningTestResult, SyphScreeningTestResult)][order(ScreeningHIVTest, HCVScreeningTestResult, SyphScreeningTestResult)]

vars <- c('HIV Disaggregated data (tool)'= 'n_hiv_coba_tool',
          'HIV Disaggregated data (non tool)'= 'n_hiv_disagg_data',
          'HIV Aggregated data'= 'n_hiv_aggr_data',
          'HIV Tests performed'= 'n_hiv_tests',
          'HIV Aged < 16'= 'n_hiv_tests_less_16yrs',
          'HIV Tests of people Aged >= 16'= 'n_hiv_tests_ge_16yrs',
          'HIV Test prior to most recent for those tested more than once'= 'n_hiv_tests_discarded_as_dupls',
          'People tested for HIV'= 'n_hiv_people_tested',
          'People with No HIV test result available'= 'n_no_HIV_results',
          'People tested for HIV screening test result available'= 'n_HIV_screening_Tests',
          'Previously diagnosed with HIV'= 'n_previoulsy_diagn',
          'People tested with HIV screening test result available who are not previously diagnosed with HIV'= 'n_HIV_scrTst_new')

flowchart <- data.table(Order = 1:length(vars),
                        Label = vars,
                        Title = names(vars))
{
n_hiv_coba_tool <- cobatest_tool_final[ScreeningHIVTest == 1 & Centre %in% center_maps[Origen_2019 == 'Cobatest_tool', Centre], .N]
n_hiv_disagg_data <- cobatest_tool_final[ScreeningHIVTest == 1 & Centre %in% center_maps[Origen_2019 == 'Datos_desagregados', Centre], .N] 
n_hiv_aggr_data <- sum(as.numeric(HIV_data_agregades[['CBVCT_1']][Category == 'All' & Centre != 'BBDD Final', All]))
n_hiv_tests <-  n_hiv_coba_tool + n_hiv_disagg_data + n_hiv_aggr_data
n_hiv_tests_less_16yrs <- cobatest_tool_final[ScreeningHIVTest == 1, .N] - vih_tests[, .N]  # assumim que en datos aggregados ja els han tret.
n_hiv_tests_ge_16yrs <-  vih_tests[, .N] + n_hiv_aggr_data 
n_hiv_tests_discarded_as_dupls <- vih_tests[, .N] - vih_tested[, .N]
n_hiv_people_tested <- vih_tested[, .N] + n_hiv_aggr_data
n_no_HIV_results <- vih_tested[is.na(ScreeningTestResult), .N] # Assumim que en datos agregados no hay missings.
n_HIV_screening_Tests <- vih_tested[!is.na(ScreeningTestResult), .N] + n_hiv_aggr_data  
n_previoulsy_diagn <- vih_tested[!is.na(ScreeningTestResult) & (ResultLastHIV == 1 & ScreeningTestResult == 1), .N] # Assumim que en datos agregados han tret els diagnosticats previs.
n_HIV_scrTst_new <- n_HIV_screening_Tests - vih_tested[!is.na(ScreeningTestResult) & (ResultLastHIV == 1 & ScreeningTestResult == 1), .N]
} 
  
{flowchart[Label == 'n_hiv_coba_tool', Value := n_hiv_coba_tool]
flowchart[Label == 'n_hiv_disagg_data', Value := n_hiv_disagg_data]
flowchart[Label == 'n_hiv_aggr_data', Value := n_hiv_aggr_data]
flowchart[Label == 'n_hiv_tests', Value := n_hiv_tests]
flowchart[Label == 'n_hiv_tests_less_16yrs', Value := n_hiv_tests_less_16yrs]
flowchart[Label == 'n_hiv_tests_ge_16yrs', Value := n_hiv_tests_ge_16yrs]
flowchart[Label == 'n_hiv_tests_discarded_as_dupls', Value := n_hiv_tests_discarded_as_dupls]
flowchart[Label == 'n_hiv_people_tested', Value := n_hiv_people_tested]
flowchart[Label == 'n_no_HIV_results', Value := n_no_HIV_results]
flowchart[Label == 'n_HIV_screening_Tests', Value := n_HIV_screening_Tests]
flowchart[Label == 'n_previoulsy_diagn', Value := n_previoulsy_diagn]
flowchart[Label == 'n_HIV_scrTst_new', Value := n_HIV_scrTst_new]}


# ----------------------------------------------------------- #
##  FIGURE 2                                               ####
# ----------------------------------------------------------- #
# Figure 2: HIV Screening (N) and Reactive Tests (%) by centre in the COBATEST Network 2019.
#
# Dades desagregades
#      - HIV Screening:         ScreeningHIVTest == 1  
#      - HIV Reactive Tests:    ScreeningTestResult == 1

# Dades Agregades
#      - HIV Screening:         CBVCT_1 - All/All 
#      - HIV Reactive Tests:    CBVCT_5 - All/All numerator.

# COMPROBACIO: vih_tests[, .N, by= .(Centre, ScreeningHIVTest)][order(Centre, ScreeningHIVTest)]
# COMPROBACIO: vih_tests[, .N, by= .(Centre, ScreeningTestResult)][order(Centre, ScreeningTestResult)]

{### Dades desagregades amb duplicats: Contem Tests VIH.
  fig2_data <- copy(center_maps)
  # Afegim la N de tests VIH per centre.. 
  fig2_data <- merge(x = fig2_data, 
                     y = vih_tests[, .(N_screened= .N), by= .(Centre)][,.(Centre, N_screened)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Afegim la N de tests reactius (ScreeningTestResult == 1) per centre.
  fig2_data <- merge(x = fig2_data, 
                     y = vih_tests[ScreeningTestResult == 1, .(N_reactive= .N), by= .(Centre, ScreeningTestResult)][,.(Centre,N_reactive)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Imputem els N_reactive NA a 0. Es corresponen a contar quants reactius tenen ScreeningTestResult == 2 o NA. Es a dir 0.
  fig2_data[Origen_2019 %in% c("Cobatest_tool", "Datos_desagregados") & !is.na(N_screened)  & is.na(N_reactive), N_reactive := 0]
  # Afegim el percentatge de reactius respecte els testejats per centre.
  fig2_data[, Perc_reactive := round(N_reactive/N_screened*100, digits = 1)]}

{### Dades desagregades sense duplicats: Contem persones testades.
  # Afegim la N de tests VIH per centre.. 
  fig2_data <- merge(x = fig2_data, 
                     y = vih_tested_final[, .(N_screened_unique= .N), by= .(Centre)][,.(Centre, N_screened_unique)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Afegim la N de tests reactius (ScreeningTestResult == 1) per centre.
  fig2_data <- merge(x = fig2_data, 
                     y = vih_tested_final[ScreeningTestResult == 1, .(N_reactive_unique= .N), by= .(Centre, ScreeningTestResult)][,.(Centre,N_reactive_unique)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Imputem els N_reactive NA a 0. Es corresponen a contar quants reactius tenen ScreeningTestResult == 2 o NA. Es a dir 0.
  fig2_data[Origen_2019 %in% c("Cobatest_tool", "Datos_desagregados") & is.na(N_reactive_unique) & !is.na(N_screened_unique), N_reactive_unique := 0]
  # Afegim el percentatge de reactius respecte els testejats per centre.
  fig2_data[, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]}


{### Dades agregades.
  hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
  numeradors_CBVCT_5 <- HIV_data_agregades$Numeradors_CBVCT_5
  
  fig2_data[Centre == 63, N_screened_unique := hivCBVCT_1[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 63, N_reactive_unique := numeradors_CBVCT_5[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 63, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 30, N_screened_unique := hivCBVCT_1[Centre == "Aides" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 30, N_reactive_unique := numeradors_CBVCT_5[Centre == "Aides" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 30, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 65, N_screened_unique := hivCBVCT_1[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 65, N_reactive_unique := numeradors_CBVCT_5[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 65, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 66, N_screened_unique := hivCBVCT_1[Centre == "HUHIV" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 66, N_reactive_unique := numeradors_CBVCT_5[Centre == "HUHIV" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 66, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 39, N_screened_unique := hivCBVCT_1[Centre == "Legebitra" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 39, N_reactive_unique := numeradors_CBVCT_5[Centre == "Legebitra" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 39, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 67, N_screened_unique := hivCBVCT_1[Centre == "Associação Abraço" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 67, N_reactive_unique := numeradors_CBVCT_5[Centre == "Associação Abraço" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 67, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 68, N_screened_unique := hivCBVCT_1[Centre == "GenderdocM" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 68, N_reactive_unique := numeradors_CBVCT_5[Centre == "GenderdocM" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 68, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 64, N_screened_unique := hivCBVCT_1[Centre == "Alliance GLobal" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 64, N_reactive_unique := numeradors_CBVCT_5[Centre == "Alliance GLobal" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 64, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 69, N_screened_unique := hivCBVCT_1[Centre == "Fulcrum" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 69, N_reactive_unique := numeradors_CBVCT_5[Centre == "Fulcrum" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 69, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig2_data[Centre == 35, N_screened_unique := hivCBVCT_1[Centre == "Demetra" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 35, N_reactive_unique := numeradors_CBVCT_5[Centre == "Demetra" & Category == "All", as.numeric(All)]] 
  fig2_data[Centre == 35, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]}


rm(numeradors_CBVCT_5, hivCBVCT_1)


# ________________________________________________________ ####
## HIV ANNEX 6 TABLES                                      ####
# ******************************************************** ####
# Annex 6: People screened for HIV (N) and reactive tests (n,%) by sociodemographic characteristics of tester and centre in 
# the COBATEST Network.

hivCBVCT1_onlyAggr <- HIV_data_agregades$CBVCT_1[Centre != "BBDD Final"]
hivCBVCT5_onlyAggr <- HIV_data_agregades$Numeradors_CBVCT_5[Centre != "BBDD Final"]

coba_desagg_centers <- center_maps[Origen_2019 %in% c("Cobatest_tool", "Datos_desagregados"), Centre]

{## Persons Tested
# Dades desagregades.
block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
block_data[, Category := 'Persons Tested'][, Row_order := 1]
block_data <- merge(x = block_data,
            y = vih_tested_final[, .("Total (N)"= .N), by= .(Centre)],
            by= "Centre", all.x = T)
block_data <- merge(x = block_data,
            y = vih_tested_final[ScreeningTestResult == 1, .("Reactive (n)"= .N), by= .(Centre)],
            by= "Centre", all.x = T)
# Iniciem dataset total.
annex6_data <- copy(block_data)
# Dades agregades.
block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
block_data[, Category := 'Persons Tested'][, Row_order := 1]
block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", All])]
block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", All])]
block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "All", All])]
block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "All", All])]
block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", All])]
block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", All])]
block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "All", All])]
block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "All", All])]
block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "All", All])]
block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "All", All])]
block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "All", All])]
block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "All", All])]
block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "All", All])]
block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "All", All])]
block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "All", All])]
block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "All", All])]
block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "All", All])]
block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "All", All])]
block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "All", All])]
block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "All", All])]
block_data[Centre == 70, "Total (N)" := NA]
block_data[Centre == 70, "Reactive (n)" := NA]
# Afegim al dataset total.
annex6_data <- rbind(annex6_data, block_data)}

{## Age group < 25 yrs.
# Dades desagregades.
block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
block_data[, Category := 'Age group < 25 yrs'][, Row_order := 2]
block_data <- merge(x = block_data,
                    y = vih_tested_final[AgeGroup == 1, .("Total (N)"= .N), by= .(Centre)],
                    by= "Centre", all.x = T)
block_data <- merge(x = block_data,
                    y = vih_tested_final[AgeGroup == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                    by= "Centre", all.x = T)
# Afegim al dataset total.
annex6_data <- rbind(annex6_data, block_data)
# Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Age group < 25 yrs'][, Row_order := 2]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", `<25 years old`])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", `<25 years old`])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "All", `<25 years old`])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "All", `<25 years old`])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", `<25 years old`])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", `<25 years old`])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "All", `<25 years old`])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "All", `<25 years old`])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "All", `<25 years old`])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "All", `<25 years old`])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "All", `<25 years old`])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "All", `<25 years old`])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "All", `<25 years old`])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "All", `<25 years old`])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "All", `<25 years old`])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "All", `<25 years old`])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "All", `<25 years old`])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "All", `<25 years old`])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "All", `<25 years old`])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "All", `<25 years old`])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
# Afegim al dataset total.
annex6_data <- rbind(annex6_data, block_data)}

{## Age group >= 25 yrs.
# Dades desagregades.
block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
block_data[, Category := 'Age group >= 25 yrs'][, Row_order := 3]
block_data <- merge(x = block_data,
                    y = vih_tested_final[AgeGroup == 2, .("Total (N)"= .N), by= .(Centre)],
                    by= "Centre", all.x = T)
block_data <- merge(x = block_data,
                    y = vih_tested_final[AgeGroup == 2 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                    by= "Centre", all.x = T)
# Afegim al dataset total.
annex6_data <- rbind(annex6_data, block_data)
# Dades agregades.
block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
block_data[, Category := 'Age group >= 25 yrs'][, Row_order := 3]
block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", `25+ years old`])]
block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", `25+ years old`])]
block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "All", `25+ years old`])]
block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "All", `25+ years old`])]
block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", `25+ years old`])]
block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", `25+ years old`])]
block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "All", `25+ years old`])]
block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "All", `25+ years old`])]
block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "All", `25+ years old`])]
block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "All", `25+ years old`])]
block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "All", `25+ years old`])]
block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "All", `25+ years old`])]
block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "All", `25+ years old`])]
block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "All", `25+ years old`])]
block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "All", `25+ years old`])]
block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "All", `25+ years old`])]
block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "All", `25+ years old`])]
block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "All", `25+ years old`])]
block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "All", `25+ years old`])]
block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "All", `25+ years old`])]
block_data[Centre == 70, "Total (N)" := NA]
block_data[Centre == 70, "Reactive (n)" := NA]
# Afegim al dataset total.
annex6_data <- rbind(annex6_data, block_data)}

{## Age group missings.
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Age group Missings'][, Row_order := 4]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[is.na(AgeGroup), .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[is.na(AgeGroup) & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Age group Missings'][, Row_order := 4]
  block_data[, "Total (N)" := NA]
  block_data[, "Reactive (n)" := NA]
  
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data) }

{## Gender Male.
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Male'][, Row_order := 5]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 1, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Male'][, Row_order := 5]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", Males])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", Males])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "All", Males])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "All", Males])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", Males])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", Males])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "All", Males])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "All", Males])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "All", Males])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "All", Males])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "All", Males])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "All", Males])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "All", Males])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "All", Males])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "All", Males])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "All", Males])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "All", Males])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "All", Males])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "All", Males])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "All", Males])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## Gender Female
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Female'][, Row_order := 6]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 2, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 2 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
   # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
   # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Female'][, Row_order := 6]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", Females])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", Females])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "All", Females])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "All", Females])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", Females])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", Females])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "All", Females])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "All", Females])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "All", Females])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "All", Females])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "All", Females])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "All", Females])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "All", Females])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "All", Females])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "All", Females])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "All", Females])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "All", Females])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "All", Females])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "All", Females])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "All", Females])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
   # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## Gender Transgender
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Transgender'][, Row_order := 7]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 3, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 3 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Transgender'][, Row_order := 7]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", Transgender])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "All", Transgender])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "All", Transgender])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "All", Transgender])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", Transgender])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "All", Transgender])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "All", Transgender])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "All", Transgender])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "All", Transgender])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "All", Transgender])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "All", Transgender])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "All", Transgender])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "All", Transgender])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "All", Transgender])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "All", Transgender])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "All", Transgender])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "All", Transgender])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "All", Transgender])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "All", Transgender])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "All", Transgender])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## Gender missings.
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Missings'][, Row_order := 8]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[is.na(Gender), .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[is.na(Gender) & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Gender Missings'][, Row_order := 8]
  block_data[, "Total (N)" := NA]
  block_data[, "Reactive (n)" := NA]
  
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data) }

{## Migrant Yes
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Migrant Yes'][, Row_order := 9]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Migrant == 1, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Migrant == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Migrant Yes'][, Row_order := 9]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "Migrants", All])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "Migrants", All])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "Migrants", All])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "Migrants", All])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "Migrants", All])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "Migrants", All])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "Migrants", All])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "Migrants", All])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "Migrants", All])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "Migrants", All])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "Migrants", All])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "Migrants", All])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "Migrants", All])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "Migrants", All])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "Migrants", All])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "Migrants", All])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "Migrants", All])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "Migrants", All])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "Migrants", All])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "Migrants", All])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## PWID
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'PWID'][, Row_order := 10]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[PWID == 1, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[PWID == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'PWID'][, Row_order := 10]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "IDU", All])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "IDU", All])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "IDU", All])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "IDU", All])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "IDU", All])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "IDU", All])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "IDU", All])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "IDU", All])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "IDU", All])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "IDU", All])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "IDU", All])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "IDU", All])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "IDU", All])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "IDU", All])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "IDU", All])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "IDU", All])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "IDU", All])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "IDU", All])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "IDU", All])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "IDU", All])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## Sex Worker MSW
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Sex Worker MSW'][, Row_order := 11]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 1 & SW == 1, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 1 & SW == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Sex Worker MSW'][, Row_order := 11]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "SW", Males])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "SW", Males])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "SW", Males])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "SW", Males])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "SW", Males])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "SW", Males])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "SW", Males])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "SW", Males])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "SW", Males])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "SW", Males])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "SW", Males])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "SW", Males])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "SW", Males])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "SW", Males])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "SW", Males])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "SW", Males])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "SW", Males])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "SW", Males])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "SW", Males])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "SW", Males])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## Sex Worker FSW
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Sex Worker FSW'][, Row_order := 12]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 2 & SW == 1, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 2 & SW == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Sex Worker FSW'][, Row_order := 12]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "SW", Females])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "SW", Females])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "SW", Females])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "SW", Females])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "SW", Females])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "SW", Females])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "SW", Females])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "SW", Females])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "SW", Females])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "SW", Females])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "SW", Females])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "SW", Females])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "SW", Females])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "SW", Females])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "SW", Females])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "SW", Females])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "SW", Females])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "SW", Females])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "SW", Females])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "SW", Females])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## Sex Worker TSW
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'Sex Worker TSW'][, Row_order := 13]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 3 & SW == 1, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[Gender == 3 & SW == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'Sex Worker TSW'][, Row_order := 13]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "SW", Transgender])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "SW", Transgender])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "SW", Transgender])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "SW", Transgender])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "SW", Transgender])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "SW", Transgender])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "SW", Transgender])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "SW", Transgender])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "SW", Transgender])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "SW", Transgender])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "SW", Transgender])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "SW", Transgender])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "SW", Transgender])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "SW", Transgender])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "SW", Transgender])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "SW", Transgender])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "SW", Transgender])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "SW", Transgender])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "SW", Transgender])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "SW", Transgender])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

{## MSM
  # Dades desagregades.
  block_data <- copy(center_maps[Centre %in% coba_desagg_centers])
  block_data[, Category := 'MSM'][, Row_order := 14]
  block_data <- merge(x = block_data,
                      y = vih_tested_final[MSM == 1, .("Total (N)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
  block_data <- merge(x = block_data,
                      y = vih_tested_final[MSM == 1 & ScreeningTestResult == 1 , .("Reactive (n)"= .N), by= .(Centre)],
                      by= "Centre", all.x = T)
    # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)
  # Dades agregades.
  block_data <- block_data <- copy(center_maps[!Centre %in% coba_desagg_centers])
  block_data[, Category := 'MSM'][, Row_order := 14]
  block_data[Centre == 63, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "MSM", All])]
  block_data[Centre == 63, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Poland National AIDS Centre" & Category == "MSM", All])]
  block_data[Centre == 30, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Aides" & Category == "MSM", All])]
  block_data[Centre == 30, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Aides" & Category == "MSM", All])]
  block_data[Centre == 65, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "MSM", All])]
  block_data[Centre == 65, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Asocijacija Duga - Rainbow" & Category == "MSM", All])]
  block_data[Centre == 66, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "HUHIV" & Category == "MSM", All])]
  block_data[Centre == 66, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "HUHIV" & Category == "MSM", All])]
  block_data[Centre == 39, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Legebitra" & Category == "MSM", All])]
  block_data[Centre == 39, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Legebitra" & Category == "MSM", All])]
  block_data[Centre == 67, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Associação Abraço" & Category == "MSM", All])]
  block_data[Centre == 67, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Associação Abraço" & Category == "MSM", All])]
  block_data[Centre == 68, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "GenderdocM" & Category == "MSM", All])]
  block_data[Centre == 68, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "GenderdocM" & Category == "MSM", All])]
  block_data[Centre == 64, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Alliance GLobal" & Category == "MSM", All])]
  block_data[Centre == 64, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Alliance GLobal" & Category == "MSM", All])]
  block_data[Centre == 69, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Fulcrum" & Category == "MSM", All])]
  block_data[Centre == 69, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Fulcrum" & Category == "MSM", All])]
  block_data[Centre == 35, "Total (N)" := as.numeric(hivCBVCT1_onlyAggr[Centre == "Demetra" & Category == "MSM", All])]
  block_data[Centre == 35, "Reactive (n)" := as.numeric(hivCBVCT5_onlyAggr[Centre == "Demetra" & Category == "MSM", All])]
  block_data[Centre == 70, "Total (N)" := NA]
  block_data[Centre == 70, "Reactive (n)" := NA]
  # Afegim al dataset total.
  annex6_data <- rbind(annex6_data, block_data)}

# Calculem percentatges.
annex6_data[, "Reactive (%)" := round(`Reactive (n)`/`Total (N)`*100, digits = 1)]

# Ordenem
annex6_data <- annex6_data[order(Centre, Row_order)]

# COMPROBACIONS:  annex6_data[Centre == 59, ]   # Visualització de la taula per un centre donat.


rm(coba_desagg_centers, block_data, hivCBVCT1_onlyAggr, hivCBVCT5_onlyAggr)


date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = annex6_data, file = paste0(output_path,"annex6_data_",date,".xlsx"), col.names= TRUE)
rm(output_path, date)


# ________________________________________________________ ####
## HCV FIGURES REPORT                                      ####
# ******************************************************** ####
# ----------------------------------------------------------- #
##  FIGURE 3                                               ####
# ----------------------------------------------------------- #
# Figure 3: Hepatitis C Screening (N) and Reactive Tests (%) by centre in the COBATEST Network 2019.
#
# Dades desagregades
#      - HCV Screening:         HCVScreeningTestResult != NA  
#      - HCV Reactive Tests:    HCVScreeningTestResult == 1

# Dades Agregades
#      - HCV Screening:         CBVCT_1 - All/All 
#      - HCV Reactive Tests:    CBVCT_5 - All/All numerator.

# COMPROBACIO: vhc_tests[, .N, by= .(Centre, HCVScreeningTestResult)][order(Centre, HCVScreeningTestResult)]
# COMPROBACIO: vhc_tests[HCVScreeningTestResult == 1, .N, by= .(Centre, HCVScreeningTestResult)][order(Centre, HCVScreeningTestResult)]


{### Dades desagregades amb duplicats: Contem Tests VHC.
  fig3_data <- copy(center_maps)
  # Afegim la N de tests VIH per centre.. 
  fig3_data <- merge(x = fig3_data, 
                     y = vhc_tests[, .(N_screened= .N), by= .(Centre)][,.(Centre, N_screened)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Afegim la N de tests reactius (HCVScreeningTestResult == 1) per centre.
  fig3_data <- merge(x = fig3_data, 
                     y = vhc_tests[HCVScreeningTestResult == 1, .(N_reactive= .N), by= .(Centre, HCVScreeningTestResult)][,.(Centre,N_reactive)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Imputem els N_reactive NA a 0. Es corresponen a contar quants reactius tenen HCVScreeningTestResult == 2 o NA. Es a dir 0.
  fig3_data[Origen_2019 %in% c("Cobatest_tool", "Datos_desagregados") & !is.na(N_screened)  & is.na(N_reactive), N_reactive := 0]
  # Afegim el percentatge de reactius respecte els testejats per centre.
  fig3_data[, Perc_reactive := round(N_reactive/N_screened*100, digits = 1)]}

{### Dades desagregades sense duplicats: Contem persones testades.
  # Afegim la N de tests VIH per centre.. 
  fig3_data <- merge(x = fig3_data, 
                     y = vhc_tested[, .(N_screened_unique= .N), by= .(Centre)][,.(Centre, N_screened_unique)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Afegim la N de tests reactius (HCVScreeningTestResult == 1) per centre.
  fig3_data <- merge(x = fig3_data, 
                     y = vhc_tested[HCVScreeningTestResult == 1, .(N_reactive_unique= .N), by= .(Centre, HCVScreeningTestResult)][,.(Centre,N_reactive_unique)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Imputem els N_reactive NA a 0. Es corresponen a contar quants reactius tenen HCVScreeningTestResult == 2 o NA. Es a dir 0.
  fig3_data[Origen_2019 %in% c("Cobatest_tool", "Datos_desagregados") & is.na(N_reactive_unique) & !is.na(N_screened_unique), N_reactive_unique := 0]
  # Afegim el percentatge de reactius respecte els testejats per centre.
  fig3_data[, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]}


{### Dades agregades.
  hcvCBVCT_1 <- HCV_data_agregades$CBVCT_1
  numeradors_CBVCT_5 <- HCV_data_agregades$Numeradors_CBVCT_5
  
  fig3_data[Centre == 63, N_screened_unique := hcvCBVCT_1[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 63, N_reactive_unique := numeradors_CBVCT_5[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 63, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 30, N_screened_unique := hcvCBVCT_1[Centre == "Aides" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 30, N_reactive_unique := numeradors_CBVCT_5[Centre == "Aides" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 30, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 65, N_screened_unique := hcvCBVCT_1[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 65, N_reactive_unique := numeradors_CBVCT_5[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 65, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 66, N_screened_unique := hcvCBVCT_1[Centre == "HUHIV" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 66, N_reactive_unique := numeradors_CBVCT_5[Centre == "HUHIV" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 66, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 39, N_screened_unique := hcvCBVCT_1[Centre == "Legebitra" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 39, N_reactive_unique := numeradors_CBVCT_5[Centre == "Legebitra" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 39, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 67, N_screened_unique := hcvCBVCT_1[Centre == "Associação Abraço" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 67, N_reactive_unique := numeradors_CBVCT_5[Centre == "Associação Abraço" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 67, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 68, N_screened_unique := hcvCBVCT_1[Centre == "GenderdocM" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 68, N_reactive_unique := numeradors_CBVCT_5[Centre == "GenderdocM" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 68, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 64, N_screened_unique := hcvCBVCT_1[Centre == "Alliance GLobal" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 64, N_reactive_unique := numeradors_CBVCT_5[Centre == "Alliance GLobal" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 64, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 69, N_screened_unique := hcvCBVCT_1[Centre == "Fulcrum" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 69, N_reactive_unique := numeradors_CBVCT_5[Centre == "Fulcrum" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 69, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 35, N_screened_unique := hcvCBVCT_1[Centre == "Demetra" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 35, N_reactive_unique := numeradors_CBVCT_5[Centre == "Demetra" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 35, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig3_data[Centre == 70, N_screened_unique := hcvCBVCT_1[Centre == "FSE Polonia" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 70, N_reactive_unique := numeradors_CBVCT_5[Centre == "FSE Polonia" & Category == "All", as.numeric(All)]] 
  fig3_data[Centre == 70, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]}


rm(numeradors_CBVCT_5, hcvCBVCT_1)

# ----------------------------------------------------------- #
##  FIGURE 6                                               ####
# ----------------------------------------------------------- #
# Figure 6: Number of people screened for Hepatitis C, proportion of reactive tests and number
# of centres submitting data by year

{fig6_data <- data.table(X1= c('Participating centres (n)','People tested (n)', 
                             'People with a reactive HCV screening test (n)', 
                             'People with a reactive HCV screening test (%)'))


n_centers <- fig3_data[!(is.na(N_screened_unique) & is.na(N_reactive_unique)), .N]
persons_tested_total <- fig3_data[, sum(N_screened_unique, na.rm = T)]
persons_tested_react <- fig3_data[, sum(N_reactive_unique, na.rm = T)]

fig6_data[X1 == 'Participating centres (n)', `2019` := as.character(n_centers)]
fig6_data[X1 == 'People tested (n)', `2019` := persons_tested_total]
fig6_data[X1 == 'People with a reactive HCV screening test (n)', `2019` := as.character(persons_tested_react)]
fig6_data[X1 == 'People with a reactive HCV screening test (%)', `2019` := round(persons_tested_react/persons_tested_total*100, digits = 1)]

rm(persons_tested_total,persons_tested_react,n_centers)}



# ________________________________________________________ ####
## SYPH FIGURES REPORT                                     ####
# ******************************************************** ####
# ----------------------------------------------------------- #
##  FIGURE 4                                               ####
# ----------------------------------------------------------- #
# Figure 4: Syphilis Screening (N) and Reactive Tests (%) by centre in the COBATEST Network 2019.
#
# Dades desagregades
#      - SYPH Screening:         SyphScreeningTestResult != NA  
#      - SYPH Reactive Tests:    SyphScreeningTestResult == 1

# Dades Agregades
#      - SYPH Screening:         CBVCT_1 - All/All 
#      - SYPH Reactive Tests:    CBVCT_5 - All/All numerator.

# COMPROBACIO: vhc_tests[, .N, by= .(Centre, SyphScreeningTestResult)][order(Centre, SyphScreeningTestResult)]
# COMPROBACIO: vhc_tests[SyphScreeningTestResult == 1, .N, by= .(Centre, SyphScreeningTestResult)][order(Centre, SyphScreeningTestResult)]


{### Dades desagregades amb duplicats: Contem Tests VIH.
  fig4_data <- copy(center_maps)
  # Afegim la N de tests VIH per centre.. 
  fig4_data <- merge(x = fig4_data, 
                     y = syph_tests[, .(N_screened= .N), by= .(Centre)][,.(Centre, N_screened)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Afegim la N de tests reactius (SyphScreeningTestResult == 1) per centre.
  fig4_data <- merge(x = fig4_data, 
                     y = syph_tests[SyphScreeningTestResult == 1, .(N_reactive= .N), by= .(Centre, SyphScreeningTestResult)][,.(Centre,N_reactive)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Imputem els N_reactive NA a 0. Es corresponen a contar quants reactius tenen SyphScreeningTestResult == 2 o NA. Es a dir 0.
  fig4_data[Origen_2019 %in% c("Cobatest_tool", "Datos_desagregados") & !is.na(N_screened)  & is.na(N_reactive), N_reactive := 0]
  # Afegim el percentatge de reactius respecte els testejats per centre.
  fig4_data[, Perc_reactive := round(N_reactive/N_screened*100, digits = 1)]}

{### Dades desagregades sense duplicats: Contem persones testades.
  # Afegim la N de tests SYPH per centre.. 
  fig4_data <- merge(x = fig4_data, 
                     y = syph_tested[, .(N_screened_unique= .N), by= .(Centre)][,.(Centre, N_screened_unique)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Afegim la N de tests reactius (SyphScreeningTestResult == 1) per centre.
  fig4_data <- merge(x = fig4_data, 
                     y = syph_tested[SyphScreeningTestResult == 1, .(N_reactive_unique= .N), by= .(Centre, SyphScreeningTestResult)][,.(Centre,N_reactive_unique)], 
                     by.x= "Centre", by.y= "Centre", all= T)
  # Imputem els N_reactive NA a 0. Es corresponen a contar quants reactius tenen SyphScreeningTestResult == 2 o NA. Es a dir 0.
  fig4_data[Origen_2019 %in% c("Cobatest_tool", "Datos_desagregados") & is.na(N_reactive_unique) & !is.na(N_screened_unique), N_reactive_unique := 0]
  # Afegim el percentatge de reactius respecte els testejats per centre.
  fig4_data[, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]}


{### Dades agregades.
  syphCBVCT_1 <- SYPH_data_agregades$CBVCT_1
  numeradors_CBVCT_5 <- SYPH_data_agregades$Numeradors_CBVCT_5
  
  fig4_data[Centre == 63, N_screened_unique := syphCBVCT_1[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 63, N_reactive_unique := numeradors_CBVCT_5[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 63, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 30, N_screened_unique := syphCBVCT_1[Centre == "Aides" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 30, N_reactive_unique := numeradors_CBVCT_5[Centre == "Aides" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 30, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 65, N_screened_unique := syphCBVCT_1[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 65, N_reactive_unique := numeradors_CBVCT_5[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 65, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 66, N_screened_unique := syphCBVCT_1[Centre == "HUHIV" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 66, N_reactive_unique := numeradors_CBVCT_5[Centre == "HUHIV" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 66, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 39, N_screened_unique := syphCBVCT_1[Centre == "Legebitra" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 39, N_reactive_unique := numeradors_CBVCT_5[Centre == "Legebitra" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 39, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 67, N_screened_unique := syphCBVCT_1[Centre == "Associação Abraço" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 67, N_reactive_unique := numeradors_CBVCT_5[Centre == "Associação Abraço" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 67, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 68, N_screened_unique := syphCBVCT_1[Centre == "GenderdocM" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 68, N_reactive_unique := numeradors_CBVCT_5[Centre == "GenderdocM" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 68, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 64, N_screened_unique := syphCBVCT_1[Centre == "Alliance GLobal" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 64, N_reactive_unique := numeradors_CBVCT_5[Centre == "Alliance GLobal" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 64, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 69, N_screened_unique := syphCBVCT_1[Centre == "Fulcrum" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 69, N_reactive_unique := numeradors_CBVCT_5[Centre == "Fulcrum" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 69, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 35, N_screened_unique := syphCBVCT_1[Centre == "Demetra" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 35, N_reactive_unique := numeradors_CBVCT_5[Centre == "Demetra" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 35, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]
  fig4_data[Centre == 70, N_screened_unique := syphCBVCT_1[Centre == "FSE Polonia" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 70, N_reactive_unique := numeradors_CBVCT_5[Centre == "FSE Polonia" & Category == "All", as.numeric(All)]] 
  fig4_data[Centre == 70, Perc_reactive_unique := round(N_reactive_unique/N_screened_unique*100, digits = 1)]}


rm(numeradors_CBVCT_5, syphCBVCT_1)

# ----------------------------------------------------------- #
##  FIGURE 7                                               ####
# ----------------------------------------------------------- #
# Figure 7: Number of people screened for Syphilis, proportion of reactive tests and number of centres submiting data by year

{fig7_data <- data.table(X1= c('Participating centres (n)','People tested (n)', 
                              'People with a reactive HCV screening test (n)', 
                              'People with a reactive HCV screening test (%)'))


n_centers <- fig4_data[!(is.na(N_screened_unique) & is.na(N_reactive_unique)), .N]
persons_tested_total <- fig4_data[, sum(N_screened_unique, na.rm = T)]
persons_tested_react <- fig4_data[, sum(N_reactive_unique, na.rm = T)]

fig7_data[X1 == 'Participating centres (n)', `2019` := as.character(n_centers)]
fig7_data[X1 == 'People tested (n)', `2019` := persons_tested_total]
fig7_data[X1 == 'People with a reactive HCV screening test (n)', `2019` := as.character(persons_tested_react)]
fig7_data[X1 == 'People with a reactive HCV screening test (%)', `2019` := round(persons_tested_react/persons_tested_total*100, digits = 1)]

rm(persons_tested_total,persons_tested_react,n_centers)}



# Save the results
# Lists elements are written to individual worksheets, using list names as sheet names if available
sheets <- list("Figure_1" = flowchart, 
               "Figure_2" = fig2_data,
               "Figure_3" = fig3_data,
               "Figure_4" = fig4_data,
               "Figure_5" = table_2,
               "Figure_6" = fig6_data,
               "Figure_7" = fig7_data)

date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = sheets, file = paste0(output_path,"figures_Report_",date,".xlsx"), col.names= TRUE)
rm(sheets, output_path, date)


# ________________________________________________________ ####
## TRANS X CENTRE                                          ####
# ******************************************************** ####
# Transgender por Centro
trans_center <- copy(center_maps[!Origen_2019 == 'No_data'])

## DADES DESAGREGADES
{# Recuperem les N per centres.
  trans_center <- merge(x = trans_center, 
                        y = vih_tested_final[Gender == 3, .('Trans Screened for HIV' = .N), by= .(Centre)],
                        by = "Centre", all = T)
  trans_center <- merge(x = trans_center, 
                        y = vhc_tested[Gender == 3, .('Trans Screened for HCV' = .N), by= .(Centre)],
                        by = "Centre", all = T, )
  trans_center <- merge(x = trans_center, 
                        y = vhc_tested[Gender == 3, .('Trans Screened for SYPH' = .N), by= .(Centre)],
                        by = "Centre", all = T)}


{## DADES AGREGADES
hivCBVCT_1 <- HIV_data_agregades$CBVCT_1
vhcCBVCT_1 <- HCV_data_agregades$CBVCT_1
syphCBVCT_1 <- SYPH_data_agregades$CBVCT_1

trans_center[Centre == 63, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 63, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 63, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Poland National AIDS Centre" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 30, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Aides" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 30, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Aides" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 30, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Aides" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 65, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 65, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 65, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Asocijacija Duga - Rainbow" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 66, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "HUHIV" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 66, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "HUHIV" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 66, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "HUHIV" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 39, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Legebitra" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 39, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Legebitra" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 39, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Legebitra" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 67, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Associação Abraço" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 67, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Associação Abraço" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 67, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Associação Abraço" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 68, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "GenderdocM" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 68, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "GenderdocM" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 68, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "GenderdocM" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 64, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Alliance GLobal" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 64, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Alliance GLobal" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 64, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Alliance GLobal" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 69, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Fulcrum" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 69, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Fulcrum" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 69, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Fulcrum" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 35, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "Demetra" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 35, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "Demetra" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 35, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "Demetra" & Category == "All", as.numeric(Transgender)]] 
# trans_center[Centre == 70, ("Trans Screened for HIV") := hivCBVCT_1[Centre == "FSE Polonia" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 70, ("Trans Screened for HCV") := vhcCBVCT_1[Centre == "FSE Polonia" & Category == "All", as.numeric(Transgender)]] 
trans_center[Centre == 70, ("Trans Screened for SYPH") := syphCBVCT_1[Centre == "FSE Polonia" & Category == "All", as.numeric(Transgender)]]} 

# set NAs to 0
trans_center[is.na(trans_center), ] <- 0

rm(hivCBVCT_1, vhcCBVCT_1, syphCBVCT_1)


date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/"
# write.xlsx(x = trans_center, file = paste0(output_path,"Trans_perCenter_",date,".xlsx"))
rm(output_path, date)



# ________________________________________________________ ####
## WRITE DATASETS                                          ####
# ******************************************************** ####

date <- gsub(pattern = "-", replacement = "", x = Sys.Date())
output_path <- "C:/Users/USER-PC/Desktop/GAD_CEEISCAT/COBATEST_Indicadors_2019/Outputs/datasets/"
# write.csv(x = cobatest_tool_final, file = paste0(output_path,"cobatest_tool_final_",date,".csv"))
# write.csv(x = vih_tests, file = paste0(output_path,"vih_tests_",date,".csv"))
# write.csv(x = vih_tested_final, file = paste0(output_path,"vih_tested_",date,".csv"))
# write.csv(x = vhc_tests, file = paste0(output_path,"vhc_tests_",date,".csv"))
# write.csv(x = vhc_tested, file = paste0(output_path,"vhc_tested_",date,".csv"))
# write.csv(x = syph_tests, file = paste0(output_path,"syph_tests_",date,".csv"))
# write.csv(x = syph_tested, file = paste0(output_path,"syph_tested_",date,".csv"))
# write.xlsx(x = ind_hiv_CBVCT, file = paste0(output_path,"ind_hiv_CBVCT_",date,".xlsx"))
rm(output_path,date)


