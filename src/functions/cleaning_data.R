library(zoo)


# LINES TO TEST:
# disagg_data <- copy(disagg_cobatest)
# centers_data <- centers

cleaning_vih_data <- function(disagg_data, centers_data) {
  ## VERSION:
  #     09-09-2022
  #
  ## DESCRIPTION:
  #     Script per:   1.  netejar i processar el dataset raw de vih datos desagregados. 
  #                   2.  obtenir el flowchart de vih datos desagregados. 
  #
  ## PARAMETRES: 
  #   - disagg_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2021.R".
  #
  #   - centers_data: data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                   "Centre" (codi del centre cobatest, numèrica)
  #                   "CentreName" (Nom del centre, string)
  #                   "Origen" (tipus de l'origen de dades per l'any yyyy. 
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )   
  
  # Inicialitzem vector Flowchart
  vih_disagg_flowchart <- c('Disaggregated data (Tool)'= NA,
                            'Disaggregated data (Non tool)'= NA,
                            'Aggregated data' = NA,                 # només la deixem indicada a NAs aquí. 
                            'Total data'= NA, 
                            'Non-HIV tests performed' = NA,
                            'HIV Tests performed'= NA,
                            'HIV Aged < 16'= NA,
                            'HIV Tests of people Aged >= 16'= NA,
                            'HIV Tests of people repeated same day'= NA,
                            'HIV Tests of people one per day'= NA,
                            'HIV Test prior to most recent for those tested more than once'= NA,
                            'People tested for HIV'= NA,
                            'People with No HIV test result available'= NA,
                            'People tested for HIV screening test result available'= NA,
                            'Previously diagnosed with HIV'= NA,
                            'People tested with HIV screening test result available who are not previously diagnosed with HIV'= NA) 
  
  
  vih_disagg_flowchart['Disaggregated data (Tool)'] <- disagg_data[Centre %in% centers_data[Origen == 'Cobatest_tool', Centre], .N]
  vih_disagg_flowchart['Disaggregated data (Non tool)'] <- disagg_data[Centre %in% centers_data[Origen == 'datos_desagregados', Centre], .N]
  vih_disagg_flowchart['Total data'] <- vih_disagg_flowchart['Disaggregated data (Tool)'] + vih_disagg_flowchart['Disaggregated data (Non tool)']
  

  ## Testats VIH ####
  # --------------- #
  # Filtrem dataset de dades desagregades pels que han estat testats per VIH.
  vih_tests <- disagg_data[ScreeningHIVTest == 1]
  
  vih_disagg_flowchart['HIV Tests performed'] <- vih_tests[, .N]
  vih_disagg_flowchart['Non-HIV tests performed'] <- disagg_data[ScreeningHIVTest != 1 | is.na(ScreeningHIVTest) , .N]

  ## Majors 16a ####
  # Treiem els menors de 16 anys. Prèviament s'ha depurat la variable AgeInYears en la ETL de dades desagregades. 
  # COMPROBACIO:  Menors de 16a?                  
  #   vih_tests[ , .N, by= .(AgeInYearsNum, AgeGroup)][order(AgeInYearsNum)]
  # 
  # COMPROBACIO:  Menors de 16a, per Centre
  #
  #   vih_tests[AgeInYearsNum < 16, .N, .(AgeInYearsNum, Centre, CentreName, Origen)][order(AgeInYearsNum)]
  #
  # (28-09-2022): Generem un excel exploratori de validació per a la Megi.
  #
  # summ <- vih_tests[AgeInYearsNum < 16, .N, .(AgeInYearsNum, Centre, CentreName, Origen)][order(AgeInYearsNum)]
  # dt <- vih_tests[AgeInYearsNum < 16, .(Centre, CentreName, Origen, Id, CBVCTIdentifier, COBATEST_Id, DateOfBirth, AgeGroup, AgeInYears, AgeInYearsNum)][order(as.numeric(Centre))]
  # sheets <- list(summary= summ, cases= dt)
  # write.xlsx(x = sheets, file = paste0("out/menors_16_x_centre_", TODAY, ".xlsx"))
  
  vih_disagg_flowchart['HIV Aged < 16'] <- vih_tests[AgeInYearsNum < 16, .N]
  vih_tests <- vih_tests[AgeInYearsNum >= 16 | is.na(AgeInYearsNum)]  
  vih_disagg_flowchart['HIV Tests of people Aged >= 16'] <- vih_tests[, .N]
  
  ## Codis Id_main no únics cobatool ####
  # ----------------------------------- #
  ## COMPTE!! 
  #  (2019)  Hi ha uns codis a Id_main: .00, .00M, 0000, 000A, 000B, 000G, 000L, 000M, 000P, 000R i 000S,
  #  Que poden estar duplicats i estan referits a persones diferents (Mirar que no hi hagi més de 1):
  vih_tests[Id_main %in% c('.00', '.00M', '0000', '000A', '000B', '000G', '000L', '000M', '000P', '000R', '000S'), .N, by= Id_main]
  # vih_tests[Id_main == ".00"]     # no n'hi ha cap
  # vih_tests[Id_main == ".00M"]    # no n'hi ha cap
  # vih_tests[Id_main == "0000"]    # no n'hi ha cap
  # vih_tests[Id_main == "000A"]    # no n'hi ha cap
  # vih_tests[Id_main == "000B"]    # no n'hi ha cap
  # vih_tests[Id_main == "000G"]    # n'hi ha 1. 
  # vih_tests[Id_main == "000L"]    # no n'hi ha cap
  # vih_tests[Id_main == "000M"]    # n'hi ha 1.
  # vih_tests[Id_main == "000P"]    # no n'hi ha cap
  # vih_tests[Id_main == "000R"]    # n'hi ha 1.
  # vih_tests[Id_main == "000S"]    # no n'hi ha cap
  
  ## Repetidors VIH ####
  # ------------------ #
  # Les persones poden haver-se testat més d'una vegada. El dataset conté una variable identificadora construida durant el processament
  # de les dades desagregades en que s'han unificat les diferents variables identificadores del datasets desagregats.
  
  ## IMPORTANT CODIFICACIÓ:   Els que tenien Id_main a NAs durant la ETL de datos desagregados entren com "". Nosaltres els tractarem
  #  *********************    com a persones diferents. 
  #
  #                           vih_tests[Id_main == '', .N, Centre]
  #
  #  Tret d'algun error son del centre 28 (Aids fondet) que envia dades anonimitzades. 
  vih_tests[Id_main == '', Id_main := paste0('NO_ID_', as.character(100100 + 1:.N)), .(Centre)]
  # vih_tests[Centre == 28, .(Id, Id_main)]
  # vih_tests[Centre == 59, .(Id, Id_main)]
  
  
  ## COMPROBACIO:  Repetidors. 
  # vih_tests[, .N, by= .(Id_main, Centre, CentreName, Origen)][N > 1][order(-N)]
  # vih_tests[, .N, by= .(Id_main, Centre, CentreName, Origen)][N > 1][, .N, .(Centre, CentreName, Origen)][order(-N)]
  # vih_tests[Centre == 62, .(times= .N), by= .(Id_main, Centre, CentreName, Origen)][, .N, .(CentreName, times)][order(-N)]
  # vih_tests[Centre == 33, .(times= .N), by= .(Id_main, Centre, CentreName, Origen)][, .N, .(CentreName, times)][order(times)]
  # 
  # vih_tests[Id_main == 'VAGEO0295M']
  # vih_tests[Id_main == 'FICEZ0901M']
  # vih_tests[Id_main == '149034']

  
  ## COMPROBAIO: Compte amb els que son repetidors el mateix dia. Casuística.
  id_repetidors <- vih_tests[, .N, by= .(Id_main, Centre)][N > 1, Id_main]
  dt <- vih_tests[Id_main %in% id_repetidors][order(Id_main)]
  dt[, .N,  by= .(Id_main, DateofVisit, Centre)][order(-N)][N > 1]    # Repetidors el mateix dia. OJU!!!

  ## A preguntar I revisar cas a cas amb LAURA.
  #
  # Sobretot mirar els registres amb igual DateofVisit.
  # Primer mirar que no ho haguem introduït nosaltres amb la imputació de l'script DM_01 de datos agregados. 
  #
  # unique(dt[Id_main == '02608197101M'])    # REVISAR, SW= 1 o 2? ScreeningTestResultReceived= 0 o 1?   (2023-04-13 JORDI): MSM= 1 i ScreeningTestResultReceived= 1
  # unique(dt[Id_main == '4812FDO88752'])    # REVISAR, ScreeningTestResult= 1 o 2.  (2023-04-13 JORDI): ScreeningTestResult= 1. Sembla una rectificació.
  # unique(dt[Id_main == '5031FDO87893'])    # REVISAR, ScreeningTestResult= 1 o 2.  (2023-04-13 JORDI): ScreeningTestResult= 1. Sembla una rectificació.
  # unique(dt[Id_main == '5243FDO86892'])    # REVISAR, AgeGroup= 1 o NA. (2023-04-13 JORDI): Ens quedem amb el registre informat.
  # unique(dt[Id_main == '5467FDO24024'])    # REVISAR, ScreeningTestResult= 1 o 2.  (2023-04-13 JORDI): ScreeningTestResult= 1. Sembla una rectificació.
  # unique(dt[Id_main == '5724FDO49313'])    # REVISAR, ScreeningTestResult= 1 o 2.  (2023-04-13 JORDI): ScreeningTestResult= 1. Sembla una rectificació.
  # unique(dt[Id_main == '6064FDO31171'])    # REVISAR, AgeInYears = 23, 27, AgeGroup= 1, 2.  (2023-04-13 JORDI): Els unifico en el del grup 1. 
  # unique(dt[Id_main == '6065FDO40249'])    # REVISAR, AgeInYears = 23, 27, AgeGroup= 1, 2.  (2023-04-13 JORDI): Els unifico en el del grup 2. 
  
  # vih_tests[Id_main == '02608197101M' ]
  vih_tests[!(Id_main == '4701FDO78301' & DateofVisit == '2018-01-29' & is.na(SyphScreeningTest)), ]
  
  {
    vih_tests[Id_main == '02608197101M', `:=`(MSM= 1, ScreeningTestResultReceived= 1)]
    vih_tests[Id_main == '5031FDO87893', `:=`(AgeInYearsNum= 36, AgeGroup= 2)]
    vih_tests[Id_main == '6064FDO31171', `:=`(AgeInYearsNum= 23, AgeGroup= 1)]
    vih_tests[Id_main == '6065FDO40249', `:=`(AgeInYearsNum= 27, AgeGroup= 2)]

    vih_tests <- vih_tests[!(Id_main == '4812FDO88752' & DateofVisit == '2018-03-20' & ScreeningTestResult == 2), ]
    vih_tests <- vih_tests[!(Id_main == '5031FDO87893' & DateofVisit == '2018-02-20' & ScreeningTestResult == 2), ]
    vih_tests <- vih_tests[!(Id_main == '5243FDO86892' & DateofVisit == '2018-03-16' & is.na(AgeGroup)), ]
    vih_tests <- vih_tests[!(Id_main == '5467FDO24024' & DateofVisit == '2018-03-22' & ScreeningTestResult == 2), ]
    vih_tests <- vih_tests[!(Id_main == '5724FDO49313' & DateofVisit == '2018-03-22' & ScreeningTestResult == 2), ]
 
    # acabem d'arreglar els duplicats amb un unique. 
    vih_tests <- unique(vih_tests)}
  
  
  vih_disagg_flowchart['HIV Tests of people one per day'] <- vih_tests[, .N]
  vih_disagg_flowchart['HIV Tests of people repeated same day'] <- vih_disagg_flowchart['HIV Tests of people Aged >= 16'] - vih_tests[, .N]
  
  
  ### En aquest punt passem de registres: tests a registres: individus. Escollim, per als repetidors, el test més recent.
  # Ordenem els id_mains per DateofVisit i agafem els registres més recents per a descartar els repetidors (es visiten més d'una vegada).
  # vih_tests[, .N, by= .(Id_main, Centre, CentreName, Origen)][N > 1][order(-N)]                  
  # vih_tests[, .N, by= .(Id_main, Centre, CentreName, Origen)][N > 1][, .N, .(Centre, CentreName, Origen)][order(-N)]
  # vih_tests[Id_main == "149034"]
  vih_tests <- vih_tests[order(Id_main, -DateofVisit)]
  vih_tests[, ordre := 1:.N, by= .(Id_main, Centre)]   # LAURA (20-12-2020):  De moment no treure els duplicats id que surten per visitar-se en diferents centres.
  vih_tested <- vih_tests[ordre == 1, ]
  vih_tested[, ordre := NULL]
  
  vih_disagg_flowchart['HIV Test prior to most recent for those tested more than once'] <- vih_disagg_flowchart['HIV Tests of people Aged >= 16'] - vih_tested[, .N]
  vih_disagg_flowchart['People tested for HIV'] <- vih_tested[, .N]
  
  
  ## Testats amb resultat ####
  # ------------------------ #
  # COMPROBACIO:    Quins centres no aporten resultats
  # vih_tested[is.na(ScreeningTestResult), .N, by= .(Centre,  CentreName)][order(Centre)]
  
  vih_tested_results <- vih_tested[!is.na(ScreeningTestResult)]
  vih_disagg_flowchart['People with No HIV test result available'] <- vih_tested[is.na(ScreeningTestResult), .N]
  vih_disagg_flowchart['People tested for HIV screening test result available']  <- vih_tested[!is.na(ScreeningTestResult), .N]
  
  
  ## Testats sense diagnostic previ ####
  # ---------------------------------- #
  ## Aquí mirem els ResultLastHIV == 1 (positius prèvis) amb ScreeningTestResult == 2 (Negatiu posterior. )
  
  # COMPROBACIO:  (Previament diagnosticats)    
  # vih_tested_results[, .N, by= .(ResultLastHIV,ScreeningTestResult)][order(ResultLastHIV)]      
  # vih_tested_results[ResultLastHIV ==  1 & ScreeningTestResult == 2, .N, by= .(ResultLastHIV,ScreeningTestResult,Centre)][order(Centre, ResultLastHIV)]      
  # Tenim combinacions:  
  # (LastHIVResult, ScreeningTestResult) = {(1, 1), (1, 2), (2, 1), (2,2), (NA,1), (NA,2), (NA, NA)} 
  #         (1, 1):  Diagnosticats previament que tornen a donar positiu. No té sentit. (Bé si abans no el tenien i ara si.) pero van fora.
  #         (1, 2):  Diagnosticats previament que ara no donden VIH.  No té sentit. FORA. 
  #         (2, 1):  Nous infectats
  #         (2, 2):  Primer test negatiu. 

  ## LAURA (20-11-2020):  No eliminar los ResultLastHIV == 1 con ScreeningTestResult == 2.
  ## LAURA (23-09-2021):  Los ResultLastHIV == 1 con ScreeningTestResult == 2. Jo els eliminaria, no són tants casos. 
  ## JORDI (23-09-2021):  Eliminats durant la creació de disagg_cobatest. 
  
  # Hem de treure els (LastHIVResult, ScreeningTestResult) = (1, 1). La resta no.
  vih_disagg_flowchart['Previously diagnosed with HIV'] <- vih_tested_results[ResultLastHIV == 1 & ScreeningTestResult == 1, .N]
  
  
  # # TOY DATA COMPROBACIO:
  # dt <- data.table(ResultLastHIV= c(1,1,1, 2,2,2, NA,NA,NA),
  #                  ScreeningTestResult= c(1,2,NA, 1,2,NA, 1,2,NA))
  # dt
  # dt[ResultLastHIV != 1 ]    # La negació agafa els valors diferents de 1 menys els NAs
  # dt[!(ResultLastHIV == 1)]  # La negació agafa els valors diferents de 1 menys els NAs
  # dt[!(ResultLastHIV == 1 & ScreeningTestResult == 1)]
  # dt[-which(ResultLastHIV == 1)] # La negació agafa els valors diferents de 1 amb NAs.
  # dt[-which(ResultLastHIV == 1 & ScreeningTestResult == 1)] # Aquest sembla el bo, però OJU!!
  # 
  # dt2 <- data.table(ResultLastHIV= c(2,2,2, NA,NA,NA),
  #                   ScreeningTestResult= c(1,2,NA, 1,2,NA))
  # 
  # dt2[-which(ResultLastHIV == 1 & ScreeningTestResult == 1)] # !!!! FALLA pq no hi ha 1 en el dataset ResultLastHIV
  # 
  # # La millor manera és
  # dt[!(ResultLastHIV == 1 & ScreeningTestResult == 1) | is.na(ResultLastHIV) | is.na(ScreeningTestResult)]
  
  vih_tested_final <- vih_tested_results[!(ResultLastHIV == 1 & ScreeningTestResult == 1) | is.na(ResultLastHIV) | is.na(ScreeningTestResult)] # Millor aquest.
  # vih_tested_final <- vih_tested_results[-which(ResultLastHIV == 1 & ScreeningTestResult == 1)]  # En el dataset gran no afecta. 
  
  
  vih_disagg_flowchart['People tested with HIV screening test result available who are not previously diagnosed with HIV'] <- vih_tested_final[,.N]
  
  
  return (list("vih_clean_data" = vih_tested_final, "flowchart" = vih_disagg_flowchart))
  
  }


## LINES TO TEST:
# disagg_data <- disagg_cobatest
# centers_data <- centers

cleaning_syph_data <- function(disagg_data, centers_data) {
  ## VERSION:
  #     14-09-2022
  #
  ## DESCRIPTION:
  #     Script per:   1.  netejar i processar el dataset raw de syph datos desagregados. 
  #                   2.  obtenir el flowchart de syph datos desagregados. 
  #
  ## PARAMETRES: 
  #   - disagg_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R".
  #
  #   - centers_data: data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                   "Centre" (codi del centre cobatest, numèrica)
  #                   "CentreName" (Nom del centre, string)
  #                   "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )   
  
  # Inicialitzem vector Flowchart
  syph_disagg_flowchart <- c('Disaggregated data (Tool)'= NA,
                            'Disaggregated data (Non tool)'= NA,
                            'Aggregated data' = NA,                 # només la deixem indicada a NAs aquí. 
                            'Total data'= NA, 
                            'Non-SYPH tests performed' = NA,
                            'SYPH Tests performed'= NA,
                            'SYPH Tests of people Aged < 16'= NA,
                            'SYPH Tests of people Aged >= 16'= NA,
                            'SYPH Tests of people repeated same day'= NA,
                            'SYPH Tests of people one per day'= NA,
                            'SYPH Test prior to most recent for those tested more than once'= NA,
                            'People tested for SYPH'= NA)
  
  syph_disagg_flowchart['Disaggregated data (Tool)'] <- disagg_data[Centre %in% centers_data[Origen == 'Cobatest_tool', Centre], .N]
  syph_disagg_flowchart['Disaggregated data (Non tool)'] <- disagg_data[Centre %in% centers_data[Origen == 'datos_desagregados', Centre], .N]
  syph_disagg_flowchart['Total data'] <- syph_disagg_flowchart['Disaggregated data (Tool)'] + syph_disagg_flowchart['Disaggregated data (Non tool)']
  
  
  ## Testats SYPH ####
  # ---------------- #
  # Filtrem dataset de dades desagregades pels que han estat testats per SYPH.
  # disagg_data[, .N, SyphScreeningTest]
  syph_tests <- disagg_data[SyphScreeningTest == 1]
  
  syph_disagg_flowchart['SYPH Tests performed'] <- syph_tests[, .N]
  syph_disagg_flowchart['Non-SYPH tests performed'] <- disagg_data[SyphScreeningTest != 1 | is.na(SyphScreeningTest), .N]
  
  ## Majors 16a ####
  # -------------- #
  # Treiem els menors de 16 anys
  # COMPROBACIO:  Menors de 16a?     syph_tests[ , .N, by= .(AgeInYearsNum, AgeGroup)][order(AgeInYearsNum)]
  syph_disagg_flowchart['SYPH Tests of people Aged < 16'] <- syph_tests[AgeInYearsNum < 16, .N]
  syph_tests <- syph_tests[AgeInYearsNum >= 16 | is.na(AgeInYearsNum)]  
  syph_disagg_flowchart['SYPH Tests of people Aged >= 16'] <- syph_tests[, .N]
  
  ## Codis Id_main no únics cobatool ####
  # ----------------------------------- #
  ## COMPTE!! 
  #  Hi ha uns codis a Id_main: .00, .00M, 0000, 000A, 000B, 000G, 000L, 000M, 000P, 000R i 000S,
  #  Que poden estar duplicats i estan referits a persones diferents (Mirar que no hi hagi més de 1):
  # syph_tests[Id_main == ".00"]     # no n'hi ha cap
  # syph_tests[Id_main == ".00M"]    # no n'hi ha cap
  # syph_tests[Id_main == "0000"]    # no n'hi ha cap
  # syph_tests[Id_main == "000A"]    # no n'hi ha cap
  # syph_tests[Id_main == "000B"]    # no n'hi ha cap
  # syph_tests[Id_main == "000G"]    # no n'hi ha cap
  # syph_tests[Id_main == "000L"]    # no n'hi ha cap
  # syph_tests[Id_main == "000M"]    # no n'hi ha cap
  # syph_tests[Id_main == "000P"]    # no n'hi ha cap
  # syph_tests[Id_main == "000R"]    # no n'hi ha cap
  # syph_tests[Id_main == "000S"]    # no n'hi ha cap
  syph_tests[Id_main %in% c(".00",".00M","0000","000A","000B","000G","000L","000M","000P","000R","000S"), .N, by= Id_main]
  
  ## Repetidors SYPH ####
  # ------------------- #
  # Les persones poden haver-se testat més d'una vegada. El dataset conté una variable identificadora construida durant el processament
  # de les dades desagregades en que s'han unificat les diferents variables identificadores del datasets desagregats.
  
  ## IMPORTANT CODIFICACIÓ:   Els que tenien Id_main a NAs durant la ETL de datos desagregados entren com "". Nosaltres els tractarem
  #  *********************    com a persones diferents. 
  #
  #                           syph_tests[Id_main == '', .N, Centre]
  #
  #  Tret d'algun error son del centre 28 (Aids fondet) que envia dades anonimitzades. 
  syph_tests[Id_main == '', Id_main := as.character(100100 + 1:.N), .(Centre)]
  # syph_tests[Centre == 28, .(Id, Id_main)]
  # syph_tests[Centre == 59, .(Id, Id_main)]
  
  ## COMPROBACIO:  Repetidors. 
  # syph_tests[, .N, by= .(Id_main, Centre)][N > 1][order(-N)]                  
  # syph_tests[Id_main == 'VAGEO0295M']
  
  ## COMPROBAIO: Compte amb els que son repetidors el mateix dia. Casuística.
  id_repetidors <- syph_tests[, .N, by= .(Id_main, Centre)][N > 1, Id_main]
  dt <- syph_tests[Id_main %in% id_repetidors][order(Id_main)]
  dt[, .N,  by= .(Id_main, DateofVisit)][order(-N)][N > 1]    # Repetidors el mateix dia. OJU!!!
  
  ## A preguntar I revisar cas a cas amb LAURA.
  #
  # Sobretot mirar els registres amb igual DateofVisit.
  # Primer mirar que no ho haguem introduït nosaltres amb la imputació de l'script DM_01 de datos agregados. 
  #
  # unique(dt[Id_main == '01709199510A'])    # Cap problema al ser iguals i quedar-nos amb un d'ells.  
  # unique(dt[Id_main == '00308199401D'])    # REVISAR. Pel 28-07-2021 té Ids diferents. La resta iguals. Arreglar.  
  # unique(dt[Id_main == '00408199602M'])    # Cap problema al ser iguals i quedar-nos amb un d'ells. 
  # unique(dt[Id_main == '00907198810R'])    # Cap problema al ser iguals i quedar-nos amb un d'ells. 
  # unique(dt[Id_main == '01912199000R'])    # Cap problema al ser iguals i quedar-nos amb un d'ells. 
  # unique(dt[Id_main == '02205198600P'])    # REVISAR, té dades diferents. ScreeningPostTestCounselling= 1 o 2?  SyphScreeningTestResult = 1 o 2? afecta altres de syph.     (2022-09-12 LAURA): SyphScreeningTestResult = 1
  # unique(dt[Id_main == '10604200000C'])    # REVISAR, té dades diferents. SyphConfirmatoryTestResult= 4 o NA.  Agafem el NA ja que 4 és un NA per nosaltres.
  # unique(dt[Id_main == '12909199000A'])    # REVISAR, SW= 1 o 2? TestedLastYearSameCBVCT= 1 o 2? SyphScreeningTest= 1 o 2 determina totes les variables del test de syphilis?    (2022-09-12 LAURA): SW= 1 i syph omplenada 
  # unique(dt[Id_main == 'DPI-00071704'])    # Cap problema al ser iguals i quedar-nos amb un d'ells.  
  # unique(dt[Id_main == 'emma27061960'])    # REVISAR, té dades diferents. Ens quedem amb el registre  SyphConfirmatoryTest no missing. 
  # unique(dt[Id_main == 'kste06031993'])    # REVISAR, té dades diferents. MSM = 1 o 2? i hcv només informat pel MSM 2.   

  {
    syph_tests[Id_main == '00308199401D' & DateofVisit == '2021-07-28' & Centre == 37, CBVCTIdentifier := '00308199401D' ]
    syph_tests <- syph_tests[!(Id_main == '02205198600P' &  DateofVisit == '2021-04-21' & SyphScreeningTestResult == 2), ]
    syph_tests <- syph_tests[!(Id_main == '10604200000C' & SyphConfirmatoryTestResult == 4) | is.na(SyphConfirmatoryTestResult), ]
    syph_tests <- syph_tests[!(Id_main == '12909199000A' & SW == 2), ]
    syph_tests <- syph_tests[!(Id_main == 'emma27061960' & !is.na(SyphConfirmatoryTest)), ]
    syph_tests <- syph_tests[!(Id_main == 'kste06031993' & !is.na(HCVScreeningTest)), ]
    
    # acabem d'arreglar els duplicats amb un unique. 
    syph_tests <- unique(syph_tests)}
  
  syph_disagg_flowchart['SYPH Tests of people one per day'] <- syph_tests[, .N]
  syph_disagg_flowchart['SYPH Tests of people repeated same day'] <- syph_disagg_flowchart['SYPH Tests of people Aged >= 16'] - syph_tests[, .N]
  
  
  
  ### En aquest punt passem de registres: tests a registres: individus. Escollim, per als repetidors, el test més recent.
  # Ordenem els id_mains per DateofVisit i agafem els registres més recents per a descartar els repetidors (es visiten més d'una vegada).
  syph_tests <- syph_tests[order(Id_main, -DateofVisit)]
  syph_tests[, ordre := 1:.N, by= .(Id_main, Centre)]   # LAURA (20-12-2020):  De moment no treure els duplicats id que surten per visitar-se en diferents centres.
  syph_tested <- syph_tests[ordre == 1, ]
  syph_tested[, ordre := NULL]
  
  syph_disagg_flowchart['SYPH Test prior to most recent for those tested more than once'] <- syph_disagg_flowchart['SYPH Tests of people Aged >= 16'] - syph_tested[, .N]
  syph_disagg_flowchart['People tested for SYPH'] <- syph_tested[, .N]
  
    ## Tests amb resultat ####
  # ---------------------- #
  # COMPROBACIO:   syph_tests[, .N, by= .(SyphScreeningTestResult)]
  syph_tests_results <- syph_tests[!is.na(SyphScreeningTestResult)]
  
  
  return (list("syph_clean_data" = syph_tests_results, "flowchart" = syph_disagg_flowchart))
  
}

## LINES TO TEST:
# disagg_data <- disagg_cobatest
# centers_data <- centers

cleaning_hcv_data <- function(disagg_data, centers_data) {
  ## VERSION:
  #     26-01-2023
  #
  ## DESCRIPTION:
  #     Script per:   1.  netejar i processar el dataset raw de hcv datos desagregados. 
  #                   2.  obtenir el flowchart de hcv datos desagregados. 
  #
  ## PARAMETRES: 
  #   - disagg_data:  data.table generat a partir del codi "DM_01_COBATEST_DISAGG_DATA_ETL_2020.R".
  #
  #   - centers_data: data.table obtingut d'un fitxer EXCEL que ha de contenir 3 columnes obligatòries:
  #                   "Centre" (codi del centre cobatest, numèrica)
  #                   "CentreName" (Nom del centre, string)
  #                   "Origen_yyyy" (tipus de l'origen de dades per l'any yyyy. Pex: Origen_2020
  #                                         - Cobatest_tool
  #                                         - Datos_desagregados
  #                                         - Datos_agregados
  #                                         - No_data           )   
  
  # Inicialitzem vector Flowchart
  hcv_disagg_flowchart <- c('Disaggregated data (Tool)'= NA,
                            'Disaggregated data (Non tool)'= NA,
                            'Aggregated data' = NA,                 # només la deixem indicada a NAs aquí.
                            'Total data'= NA,
                            'Non-HCV tests performed' = NA,
                            'HCV Tests performed'= NA,
                            'HCV Tests of people Aged < 16'= NA,
                            'HCV Tests of people Aged >= 16'= NA,
                            'HCV Tests of people repeated same day'= NA,
                            'HCV Tests of people one per day'= NA,
                            'HCV Test prior to most recent for those tested more than once'= NA,
                            'People tested for HCV'= NA)

  hcv_disagg_flowchart['Disaggregated data (Tool)'] <- disagg_data[Centre %in% centers_data[Origen == 'Cobatest_tool', Centre], .N]
  hcv_disagg_flowchart['Disaggregated data (Non tool)'] <- disagg_data[Centre %in% centers_data[Origen == 'datos_desagregados', Centre], .N]
  hcv_disagg_flowchart['Total data'] <- hcv_disagg_flowchart['Disaggregated data (Tool)'] + hcv_disagg_flowchart['Disaggregated data (Non tool)']


  ## Testats HCV ####
  # ---------------- #
  # Filtrem dataset de dades desagregades pels que han estat testats per HCV.
  hcv_tests <- disagg_data[HCVScreeningTest == 1]

  hcv_disagg_flowchart['HCV Tests performed'] <- hcv_tests[, .N]
  hcv_disagg_flowchart['Non-HCV tests performed'] <- disagg_data[HCVScreeningTest != 1 | is.na(HCVScreeningTest) , .N]

  ## Majors 16a ####
  # -------------- #
  # Treiem els menors de 16 anys
  # COMPROBACIO:  Menors de 16a?     hcv_tests[ , .N, by= .(AgeInYearsNum, AgeGroup)][order(AgeInYearsNum)]
  hcv_disagg_flowchart['HCV Tests of people Aged < 16'] <- hcv_tests[AgeInYearsNum < 16, .N]
  hcv_tests <- hcv_tests[AgeInYearsNum >= 16 | is.na(AgeInYearsNum)]
  hcv_disagg_flowchart['HCV Tests of people Aged >= 16'] <- hcv_tests[, .N]
  
   
  ## Codis Id_main no únics cobatool ####
  # ----------------------------------- #
  ## COMPTE!!
  #  Hi ha uns codis a Id_main: .00, .00M, 0000, 000A, 000B, 000G, 000L, 000M, 000P, 000R i 000S,
  #  Que poden estar duplicats i estan referits a persones diferents (Mirar que no hi hagi més de 1):
  # hcv_tests[Id_main == ".00"]     # no n'hi ha cap
  # hcv_tests[Id_main == ".00M"]    # no n'hi ha cap
  # hcv_tests[Id_main == "0000"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000A"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000B"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000G"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000L"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000M"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000P"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000R"]    # no n'hi ha cap
  # hcv_tests[Id_main == "000S"]    # no n'hi ha cap
  hcv_tests[Id_main %in% c(".00",".00M","0000","000A","000B","000G","000L","000M","000P","000R","000S"), .N, by= Id_main]

  ## Repetidors HCV ####
  # ------------------- #
  # Les persones poden haver-se testat més d'una vegada. El dataset conté una variable identificadora construida durant el processament
  # de les dades desagregades en que s'han unificat les diferents variables identificadores del datasets desagregats.

  ## IMPORTANT CODIFICACIÓ:   Els que tenien Id_main a NAs durant la ETL de datos desagregados entren com "". Nosaltres els tractarem
  #  *********************    com a persones diferents. 
  #
  #                           hcv_tests[Id_main == '', .N, Centre]
  
  #  Tret d'algun error son del centre 28 (Aids fondet) que envia dades anonimitzades. 
  hcv_tests[Id_main == '', Id_main := as.character(100100 + 1:.N), .(Centre)]
  # hcv_tests[Centre == 28, .(Id, Id_main)]
  # hcv_tests[Centre == 59, .(Id, Id_main)]
  
  ## COMPROBAIO: Compte amb els que son repetidors el mateix dia. Casuística.
  id_repetidors <- hcv_tests[, .N, by= .(Id_main, Centre)][N > 1, Id_main]
  dt <- hcv_tests[Id_main %in% id_repetidors][order(Id_main)]
  dt[, .N,  by= .(Id_main, DateofVisit)][order(-N)][N > 1]    # Repetidors el mateix dia. OJU!!!
  
  ## A preguntar I revisar cas a cas amb LAURA.
  #
  # Sobretot mirar els registres amb igual DateofVisit.
  # Primer mirar que no ho haguem introduït nosaltres amb la imputació de l'script DM_01 de datos agregados. 
  #
  # unique(dt[Id_main == '00907198810R'])    # Cap problema al ser iguals i quedar-nos amb un d'ells.  
  # unique(dt[Id_main == '10604200000C'])    # REVISAR, té dades diferents. SyphConfirmatoryTestResult= 4 o NA.  Agafem el NA ja que 4 és un NA per nosaltres.
  # unique(dt[Id_main == '3404'])            # REVISAR, té dades diferents. ScreeningHIVTest = 1 o 2.  ScreeningHIVTest == 1.
  # unique(dt[Id_main == '669'])             # REVISAR, té dades diferents. ScreeningHIVTest = 1 o 2.  ScreeningHIVTest == 1.
  # unique(dt[Id_main == 'DPI-00071704'])    # Cap problema al ser iguals i quedar-nos amb un d'ells.  
  # unique(dt[Id_main == 'emma27061960'])    # REVISAR, té dades diferents. Ens quedem amb el registre  SyphConfirmatoryTest no missing. 

  {
    hcv_tests <- hcv_tests[!(Id_main == '12909199000A' & SW == 2), ]
    hcv_tests <- hcv_tests[!(Id_main == '3404' & ScreeningHIVTest == 2), ]
    hcv_tests <- hcv_tests[!(Id_main == '669' & ScreeningHIVTest == 2), ]
    hcv_tests <- hcv_tests[!(Id_main == 'emma27061960' & !is.na(SyphConfirmatoryTest)), ]
    
    # acabem d'arreglar els duplicats amb un unique. 
    hcv_tests <- unique(hcv_tests)}
  
  hcv_disagg_flowchart['HCV Tests of people one per day'] <- hcv_tests[, .N]
  hcv_disagg_flowchart['HCV Tests of people repeated same day'] <- hcv_disagg_flowchart['HCV Tests of people Aged >= 16'] - hcv_tests[, .N]
  
  
  ### En aquest punt passem de registres: tests a registres: individus. Escollim, per als repetidors, el test més recent.
  # Ordenem els id_mains per DateofVisit i agafem els registres més recents per a descartar els repetidors (es visiten més d'una vegada).
  hcv_tests <- hcv_tests[order(Id_main, -DateofVisit)]
  hcv_tests[, ordre := 1:.N, by= .(Id_main, Centre)]   # LAURA (20-12-2020):  De moment no treure els duplicats id que surten per visitar-se en diferents centres.
  hcv_tested <- hcv_tests[ordre == 1, ]
  hcv_tested[, ordre := NULL]
  
  
  hcv_disagg_flowchart['HCV Test prior to most recent for those tested more than once'] <- hcv_disagg_flowchart['HCV Tests of people Aged >= 16'] - hcv_tested[, .N]
  hcv_disagg_flowchart['People tested for HCV'] <- hcv_tested[, .N]

  ## Tests amb resultat ####
  # ---------------------- #
  # COMPROBACIO:   
  hcv_tests[, .N, by= .(HCVScreeningTestResult)]
  hcv_tests_results <- hcv_tests[!is.na(HCVScreeningTestResult)]


  return (list("hcv_clean_data" = hcv_tests_results, "flowchart" = hcv_disagg_flowchart))
  
}
