import excel "C:\Users\Laura\OneDrive - Generalitat de Catalunya\Confinament\Dades COBATEST 2019\Megi\hivtest-2020-05-29-135503.xls", firstrow case(lower)

rename *, lower

rename (cbvct_name cbvct_city testing_site date_of_visit cbvct_identifier client_identifier date_of_birth foreign_national country_of_birth) /// 
(cbvctname cbvctcity testingsite dateofvisit cbvctidentifier clientidentifier dateofbirth foreignnational countryofbirth)
rename (previous_hiv_test date_last_test result_last_hiv_test test_12months_cbvct sex_with condom_use sex_worker unprotected_sex_sw2 unprotected_sex_idu unprotected_sex_hiv_positive unprotected_sex_msm) ///
 (previoushivtest datelasttest resultlasthivtest test12monthscbvct sexwith condomuse sexworker unprotectedsexsw2 unprotectedsexidu unprotectedsexhivpositive unprotectedsexmsm)
rename (intravenous_drug_use test_used screening_test_result confirm_hiv_test date_confirm_hiv_test confirm_hiv_test_res linkage_healthcare linkage_date cd4_count cd4_date) ///
(intravenousdruguse testused screeningtestresult confirmhivtest dateconfirmhivtest confirmhivtestres linkagehealthcare linkagedate cd4count cd4date)
rename (previous_syphilis date_last_syphilis syphilis_test date_syphilis_test type_syphilis_test syphilis_rapid_test_result syphilis_confirmation date_syphilis_confirmation syphilis_diagnosis) ///
 (previoussyphilis datelastsyphilis syphilistest datesyphilistest typesyphilistest syphilisrapidtestresult syphilisconfirmation datesyphilisconfirmation syphilisdiagnosis)
rename (previous_hcv date_previous_hcv hcv_test hcv_test_date hcv_test_type hcv_rapid_test_result hcv_confirmation hcv_confirmation_date hcv_diagnosis hep_a_vaccination hep_b_vaccionation) ///
 (previoushcv dateprevioushcv hcvtest hcvtestdate hcvtesttype hcvrapidtestresult hcvconfirmation hcvconfirmationdate hcvdiagnosis hepavaccination hepbvaccionation)
 

//create centros
gen centros=.
replace centros=1 if cbvctname=="A. Antisida Lleida"
replace centros=1 if cbvctname=="A. Antisida Lleidai)"
replace centros=2 if cbvctname=="ACASC"
replace centros=3 if cbvctname=="ACAS Girona"
replace centros=4 if cbvctname=="Actuavallès"
replace centros=4 if cbvctname=="Actuavallès " 
replace centros=5 if cbvctname=="Àmbit Prevenció"
replace centros=6 if cbvctname=="Assexora'Tgn"
replace centros=7 if cbvctname=="CAS Lluís Companys"
replace centros=7 if cbvctname=="SAPS-Creu Roja"
replace centros=8 if cbvctname=="Centre Jove d&#146;'Anticoncepció i Sexualitat"
replace centros=8 if cbvctname=="Centre Jove d&#146;Antico"
replace centros=8 if cbvctname=="Centre Jove d&#146;Anticoncepció i sexualitat"
replace centros=8 if cbvctname=="Centre Jove d’Antico"
replace centros=8 if cbvctname=="Centre Jove d’AnticoBCN"
replace centros=9 if cbvctname=="Creu Roja Tarragona"
replace centros=10 if cbvctname=="Gais Positius"
replace centros=11 if cbvctname=="StopSida"

replace centros=12 if cbvctname=="ACAVIH"
replace centros=13 if cbvctname=="ACCAS"
replace centros=14 if cbvctname=="ASOCIACION SILOÉ"
replace centros=15 if cbvctname=="ASOCIACIÓN SOMOS LGT"
replace centros=16 if cbvctname=="AVACOS"
replace centros=17 if cbvctname=="Algarabía"
replace centros=18 if cbvctname=="Asociación Concordia"
replace centros=19 if cbvctname=="Associació Lambda"
replace centros=20 if cbvctname=="Comité Anti-Sida Asturias CCASIPA  "
replace centros=20 if cbvctname=="Comité Anti-Sida  Asturias CCASiPA"
replace centros=20 if cbvctname=="Comité Anti-Sida Ast"
replace centros=20 if cbvctname=="Comité Anti-Sida Asturias (CCASiPA)"
replace centros=20 if cbvctname=="Comité Anti-Sida Asturias CCASIPA"
replace centros=20 if cbvctname=="Comité Anti-Sida Asturias CCASiPA"
replace centros=20 if cbvctname=="Comité Anti-Sida Asturias CCASiPA "
replace centros=20 if cbvctname=="Comité Anti-Sida Asturias CCASiPA"
replace centros=20 if cbvctname=="Comité Anti-Sida AsturiasCCASiPA"
replace centros=20 if cbvctname=="CCASiPA "
replace centros=20 if cbvctname=="CCASiPA"
replace centros=20 if cbvctname=="CASiPA"
replace centros=20 if cbvctname=="CCASIPA"
replace centros=20 if cbvctname=="CCASiPa"
replace centros=20 if cbvctname=="CCASSiPA"
replace centros=20 if cbvctname=="CASiPA"
replace centros=20 if cbvctname=="CCSiPA"
replace centros=20 if cbvctname=="Comité Anti-Sida Asturias CCASiPA  "
replace centros=21 if cbvctname=="CAS Gibraltar"
replace centros=22 if cbvctname=="CASDA"
replace centros=23 if cbvctname=="CIBE Marítim"
replace centros=24 if cbvctname=="IEMAKAIE"
replace centros=24 if cbvctname=="IEMEKAIE"
replace centros=25 if cbvctname=="Mujer Gades"
replace centros=26 if cbvctname=="OMSIDA"
replace centros=27 if cbvctname=="Castelló LGTBI"

replace centros=28 if cbvctname=="AF Checkpoint AAR (Denmark)"
replace centros=28 if cbvctname=="AF Checkpoint Aarhus (Danmark)"
replace centros=28 if cbvctname=="AF Checkpoint CPH  (Denmark)"
replace centros=28 if cbvctname=="AF Checkpoint CPH (Denmark)"
replace centros=28 if cbvctname=="AF Checkpoint CPH (Denmark) "
replace centros=28 if cbvctname=="AF Checkpoint København (Danmark)"
replace centros=28 if cbvctname=="AIDS Fondet"
replace centros=28 if cbvctname=="SA"
replace centros=29 if cbvctname=="AIDS-Hilfe Dortmund (Germany)"
replace centros=29 if cbvctname=="AIDS-Hilfe Essen (Germany)"
replace centros=30 if cbvctname=="Aides Lille (France)"
replace centros=30 if cbvctname=="Aides Lyon (France)"
replace centros=31 if cbvctname=="Ath Checkpoint (Greece)"
replace centros=32 if cbvctname=="Baltic HIV Associati"
replace centros=33 if cbvctname=="CHECKPOIN QUEENS"
replace centros=33 if cbvctname=="CHECKPOINT QUEENS"
replace centros=33 if cbvctname=="Checkpoint Queens"
replace centros=33 if cbvctname=="CHECKPOINT CARAVAN"
replace centros=33 if cbvctname=="CKECKPOINT CARAVAN"
replace centros=33 if cbvctname=="CHECKPOINT CARAVAN "
replace centros=33 if cbvctname=="CHECKPOINT QUEENS "
replace centros=33 if cbvctname=="CHECKPOINT ARAS"
replace centros=33 if cbvctname=="ARAS"
replace centros=33 if cbvctname=="COMUNICA"
replace centros=34 if cbvctname=="Checkpoint LX (Portugal)"
replace centros=35 if cbvctname=="Demetra"
replace centros=36 if cbvctname=="Fondazione LILA Mila"
replace centros=36 if cbvctname=="Fondazione LILA MilaNO"
replace centros=36 if cbvctname=="Fondazione LILA MilaNP"
replace centros=36 if cbvctname=="Fondazione LILA MilanO"
replace centros=36 if cbvctname=="Fondazione LILA Milano"
replace centros=36 if cbvctname=="LILA Milano (Italy)"
replace centros=36 if cbvctname=="Lila Milano (Italy)"
replace centros=37 if cbvctname=="Checkpoint Milano"
replace centros=38 if cbvctname=="Gay-alliance"
replace centros=39 if cbvctname=="Legebitra (Slovenia)"
replace centros=39 if cbvctname=="Legebitra"
replace centros=40 if cbvctname=="PRAKSIS"
replace centros=41 if cbvctname=="Thess Checkpoint (Greece)"
replace centros=42 if cbvctname=="Háttér"
replace centros=42 if cbvctname=="Háttér Society"
replace centros=43 if cbvctname=="Odyseus"
replace centros=44 if cbvctname=="Prima"

replace centros=45 if cbvctname=="Aimer Jeunes"
replace centros=46 if cbvctname=="Marolles"
replace centros=47 if cbvctname=="Plate-Forme"
replace centros=48 if cbvctname=="SIPS "
replace centros=48 if cbvctname=="SIPS"
replace centros=49 if cbvctname=="Jette"
replace centros=50 if cbvctname=="Uccle"
replace centros=51 if cbvctname=="Watermael-Boitsfort"

replace centros=52 if cbvctname=="BSA.cat"
replace centros=53 if cbvctname=="Punt Salut Jove"
replace centros=54 if cbvctname=="Chile"


label define dcentros 1 "Associació Antisida Lleida" 2 "ACASC" 3 "ACAS Girona" 4 "Actuavallès" 5 "Àmbit Prevenció" 6 "Assexora'Tgn" /// 
7 "CAS Lluís Companys/SAPS" 8 "CJAS" 9 "Creu Roja Tarragona" 10 "Gais Positius" 11 "Stop Sida" 12 "ACAVIH" 13 "ACCAS" 14 "ASOCIACION SILOÉ" /// 
15 "ASOCIACIÓN SOMOS LGT" 16 "AVACOS" 17 "Algarabía" 18 "Asociación Concordia" 19 "Associació Lambda" 20 "CCASiPA" 21 "CAS Gibraltar" ///
22 "CASDA" 23 "CIBE Marítim" 24 "IEMAKAIE" 25 "Mujer Gades" 26 "OMSIDA" 27 "Castelló LGTBI" 28 "AIDS Fondet" 29 "AIDS-Hilfe" ///
30 "Aides" 31 "Athens Checkpoint" 32 "Baltic HIV Association" 33 "ARAS" 34 "Checkpoint LX" 35 "Demetra" 36 "LILA Milano" ///
37 "Checkpoint Milano" 38 "Gay-alliance" 39 "Legebitra" 40 "PRAKSIS" 41 "Thessalonik Checkpoint" 42 "Háttér Society" 43 "Odyseus" 44 "Prima"  ///
45 "Aimer Jeunes" 46 "Marolles" 47 "Plate-Forme" 48 "SIPS " 49 "Jette" 50 "Uccle" 51 "Watermael-Boitsfort" , replace
label values centros dcentros


//COBATEST
keep if centros>=1 & centros<=51


//match country coding to name
destring countryofbirth, replace
merge m:1 countryofbirth using "C:\Users\Laura\OneDrive - Generalitat de Catalunya\Confinament\COBATEST\Dades COBATEST 2019\Megi\country code.dta"
drop if cbvctname==""

rename *, lower

//code dates 15 missing day, 6 missing month
gen dateofvisit_rec = dateofvisit, after(dateofvisit)
**corregir algunes dates
replace dateofvisit_rec = "10/12/2019" if dateofvisit == "10/12/19"
replace dateofvisit_rec = "02/10/2019" if dateofvisit == "2/10/2019"
replace dateofvisit_rec = "29/07/2019" if dateofvisit == "29/7/2019"
//assignar día 15 en el cas de 00 en el dia (només si hi ha casos amb 00)
gen str dayofvisit = substr(dateofvisit_rec,1,2)
gen str monthofvisit = substr(dateofvisit_rec,4,2)
gen str yearofvisit = substr(dateofvisit_rec,7,4)
destring dayofvisit, replace
destring monthofvisit, replace
destring yearofvisit, replace
gen dateofvisit2 = mdy(monthofvisit, dayofvisit, yearofvisit)
format dateofvisit2 %td

//gender
replace gender=. if gender==0


//dateofbirth
gen dateofbirth_rec = dateofbirth, after(dateofbirth)

//arreglar algunes dates
rename clientidentifier cobaid
replace dateofbirth_rec ="20/07/1997" if cobaid=="020-07-199700M"
replace dateofbirth_rec ="15/10/1985" if cobaid=="115-10-198511G"
replace dateofbirth_rec ="17/09/1997" if cobaid=="117-09-199711M"
replace dateofbirth_rec ="24/04/1989" if cobaid=="224-04-198920A"
replace dateofbirth_rec ="31/11/1988" if cobaid=="031-11-198800J"
replace dateofbirth_rec ="06/07/1989" if cbvctidentifier=="10607198910L"
replace dateofbirth_rec ="24/03/1982" if cobaid=="224-03-198201L"
replace dateofbirth_rec ="28/12/1989" if cobaid=="128-12-198922Z"
replace dateofbirth_rec ="26/02/1982" if cobaid=="026-02-198200X"
replace dateofbirth_rec ="13/03/1959" if cobaid=="113-03-195912M"
replace dateofbirth_rec ="24/12/1965" if cobaid=="124-12-196500M"
replace dateofbirth_rec ="09/02/2001" if cobaid=="009-02-200100N"
replace dateofbirth_rec ="10/01/2001" if cobaid=="110-01-200100E"
replace dateofbirth_rec ="04/04/2005" if cobaid=="004-04-200501M"
replace dateofbirth_rec ="08/08/2000" if cobaid=="108-08-200001M"
replace dateofbirth_rec ="20/09/1985" if cobaid=="020-09-198501A"
replace dateofbirth_rec ="21/04/1991" if cobaid=="121-04-199110F"
replace dateofbirth_rec ="23/05/1988" if cbvctidentifier=="02305198801G"
replace dateofbirth_rec ="10/02/1990" if cobaid=="010-02-199001D"
replace dateofbirth_rec ="10/02/1990" if cbvctidentifier=="00703198802C"
replace dateofbirth_rec ="03/05/1997" if cobaid=="103-05-199700E"
replace dateofbirth_rec ="09/08/1999" if cobaid=="009-08-199900M"
replace dateofbirth_rec ="09/08/1999" if cobaid=="022-04-195410D"
replace dateofbirth_rec ="10/04/1964" if cobaid=="010-04-196400U"
replace dateofbirth_rec ="06/07/1970" if cobaid=="006-07-197010N"
replace dateofbirth_rec ="15/10/1978" if cobaid=="015-10-197800A"
replace dateofbirth_rec ="06/07/1988" if cbvctidentifier=="00607198811B"
replace dateofbirth_rec ="26/08/2003" if cobaid=="126-08-200320C"
replace dateofbirth_rec ="04/10/1953" if cobaid=="004-10-195300M"
replace dateofbirth_rec ="17/02/1952" if cobaid=="117-02-199520M"
replace dateofbirth_rec ="06/05/1977" if cobaid=="106-04-197700M"
replace dateofbirth_rec ="04/02/1996" if cbvctidentifier=="10402199611F"
replace dateofbirth_rec ="01/06/1998" if cobaid=="001-06-199800E"
replace dateofbirth_rec ="30/06/1958" if cobaid=="030-06-195801M"
replace dateofbirth_rec ="23/07/1993" if cobaid=="123-07-199300M"
replace dateofbirth_rec ="04/11/2000" if cobaid=="104-11-200001R"
replace dateofbirth_rec ="13/11/1982" if cobaid=="013-11-198211M"
replace dateofbirth_rec ="11/10/1988" if cobaid=="111-10-198800S"
replace dateofbirth_rec ="15/11/1989" if cobaid=="115-11-198911F"
replace dateofbirth_rec ="15/05/1988" if cobaid=="115-05-198800C"
replace dateofbirth_rec ="02/06/1969" if cobaid=="002-06-196901M"
replace dateofbirth_rec ="16/09/1999" if cobaid=="016-09-199900D"
replace dateofbirth_rec ="13/09/1987" if cobaid=="013-09-198740M"
replace dateofbirth_rec ="29/11/1996" if cobaid=="129-11-199601M"
replace dateofbirth_rec ="20/02/1965" if cbvctidentifier=="02002196500L"
replace dateofbirth_rec ="22/05/2000" if cobaid=="122-05-200002M"
replace dateofbirth_rec ="10/04/1983" if cobaid=="110-04-198310A"
replace dateofbirth_rec ="08/09/1969" if cbvctidentifier=="00809196910R"
replace dateofbirth_rec ="05/09/1979" if cobaid=="005-09-197900M"
replace dateofbirth_rec ="24/09/1991" if cobaid=="124-09-199910M"
replace dateofbirth_rec ="11/03/1983" if cobaid=="111-03-198321G"
replace dateofbirth_rec ="10/05/1980" if cobaid=="110-05-198010F"
replace dateofbirth_rec ="18/02/1988" if cobaid=="118-02-198800E"
replace dateofbirth_rec ="06/07/1985" if cobaid=="006-07-198500H"
replace dateofbirth_rec ="02/12/1990" if cobaid=="002-12-199000A"
replace dateofbirth_rec ="11/12/1984" if cobaid=="011-12-198411E"
replace dateofbirth_rec ="23/11/1975" if cobaid=="023-11-197510M"
replace dateofbirth_rec ="14/10/1991" if cobaid=="014-10-199910M"
replace dateofbirth_rec ="25/08/1967" if cobaid=="125-08-196711A"
replace dateofbirth_rec ="02/08/1982" if cobaid=="002-08-198270M"
replace dateofbirth_rec ="07/01/1987" if cobaid=="107-01-198700M"
replace dateofbirth_rec ="15/07/1986" if cbvctidentifier=="11507198600H"
replace dateofbirth_rec ="27/11/1981" if cobaid=="027-11-198130J"
replace dateofbirth_rec ="14/10/1988" if cobaid=="114-10-198800M"
replace dateofbirth_rec ="05/07/1989" if cobaid=="005-07-198911C"
replace dateofbirth_rec ="09/07/1999" if cobaid=="009-07-199900V"
replace dateofbirth_rec ="09/11/2000" if cobaid=="109-11-200020M"
replace dateofbirth_rec ="19/03/1995" if cobaid=="119-03-199510J"
replace dateofbirth_rec ="12/07/1998" if cobaid=="112-07-199801E"
replace dateofbirth_rec ="12/07/1998" if cbvctidentifier=="02406199100D"
replace dateofbirth_rec ="17/06/1974" if cobaid=="017-06-197400D"
replace dateofbirth_rec ="14/06/1993" if cobaid=="114-06-199302M"
replace dateofbirth_rec ="05/11/1989" if cobaid=="105-11-198911M"
replace dateofbirth_rec ="26/12/1994" if cbvctidentifier=="02612199401P"
replace dateofbirth_rec ="06/12/1977" if cobaid=="006-12-197730M"
replace dateofbirth_rec ="28/02/1996" if cobaid=="128-02-199600C"
replace dateofbirth_rec ="01/03/1988" if cobaid=="01988-03-0100A"
replace dateofbirth_rec ="22/08/1991" if cobaid=="01991-08-2200L"
replace dateofbirth_rec ="29/01/1991" if cobaid=="21991-01-2910Y"
replace dateofbirth_rec ="08/11/1990" if cobaid=="01990-11-0832N"
replace dateofbirth_rec ="02/09/1979" if cobaid=="21979-09-0245T"
replace dateofbirth_rec ="28/02/1987" if cobaid=="21987-02-2802E"
replace dateofbirth_rec ="05/05/1983" if cobaid=="21983-05-0522D"
replace dateofbirth_rec ="22/12/1999" if cobaid=="01999-12-2200Y"
replace dateofbirth_rec ="20/04/1983" if cobaid=="21983-04-2022A"
replace dateofbirth_rec ="14/07/1988" if cobaid=="21988-07-1430C"
replace dateofbirth_rec ="27/06/1997" if cbvctidentifier=="12706199701C"
replace dateofbirth_rec ="02/11/1977" if cbvctidentifier=="02111197700A"

//assignar día 15 en el cas de 00 en el dia (només si hi ha casos amb 00)
gen str dayofbirth = substr(dateofbirth_rec,1,2)
gen str monthofbirth = substr(dateofbirth_rec,4,2)
gen str yearofbirth = substr(dateofbirth_rec,7,4)
replace dayofbirth= "01" if dayofbirth=="_1"
replace dayofbirth= "02" if dayofbirth=="_2"
replace dayofbirth="15" if dayofbirth=="00"
replace monthofbirth="01" if monthofbirth=="_1" | monthofbirth=="1_"
replace monthofbirth="06" if monthofbirth=="00"
replace yearofbirth="." if yearofbirth=="0000" | yearofbirth=="___7"
replace yearofbirth="1973" if yearofbirth=="0073"
replace yearofbirth="1979" if yearofbirth=="0079"
replace yearofbirth="1984" if yearofbirth=="0084"
replace yearofbirth="1991" if yearofbirth=="0091"
replace yearofbirth="1997" if yearofbirth=="0097"
replace yearofbirth="1990" if yearofbirth=="0190"
replace yearofbirth="1994" if yearofbirth=="0194"
replace yearofbirth="2000" if yearofbirth=="0200"
replace yearofbirth="1931" if yearofbirth=="1331"
replace yearofbirth="1984" if yearofbirth=="1884"
replace yearofbirth="1993" if yearofbirth=="1893"
replace yearofbirth="1994" if yearofbirth=="2994"
replace yearofbirth="1990" if yearofbirth=="__90"
replace yearofbirth="1991" if yearofbirth=="__91"
replace yearofbirth="1992" if yearofbirth=="__92"
replace yearofbirth="1993" if yearofbirth=="__93"
replace yearofbirth="1995" if yearofbirth=="__95"
destring dayofbirth, replace
destring monthofbirth, replace
destring yearofbirth, replace
gen dateofbirth2 = mdy(monthofbirth, dayofbirth, yearofbirth)
format dateofbirth2 %td


//date of last hiv test
gen str dayoflast = substr(datelasttest,1,2)
gen str monthoflast = substr(datelasttest,4,2)
gen str yearoflast = substr(datelasttest,7,4)
replace dayoflast="15" if dayoflast=="00"
replace monthoflast="06" if monthoflast=="00"
destring dayoflast, replace
destring monthoflast, replace
destring yearoflast, replace
gen dateoflasthiv = mdy(monthoflast, dayoflast, yearoflast)
format dateoflasthiv %td

//dateofconfirmatory HIV test
gen str dayconfirmatory = substr(dateconfirmhivtest,1,2)
gen str monthconfirmatory = substr(dateconfirmhivtest,4,2)
gen str yearconfirmatory = substr(dateconfirmhivtest,7,4)
replace dayconfirmatory="15" if dayconfirmatory=="00"
replace monthconfirmatory="06" if monthconfirmatory=="00"
destring dayconfirmatory, replace
destring monthconfirmatory, replace
destring yearconfirmatory, replace
gen dateconfirmatorytest = mdy(monthoflast, dayoflast, yearoflast)
format dateconfirmatorytest %td

//date of last syphilis test
gen str dayoflastSyph = substr(datelastsyphilis,1,2)
gen str monthoflastSyph = substr(datelastsyphilis,4,2)
gen str yearoflastSyph = substr(datelastsyphilis,7,4)
replace dayoflastSyph="15" if dayoflastSyph=="00"
replace dayoflastSyph="01" if dayoflastSyph=="1/"
replace dayoflastSyph="15" if dayoflastSyph=="99"
replace dayoflastSyph=" " if dayoflastSyph=="Xx"
replace monthoflastSyph="06" if monthoflastSyph=="00"
replace monthoflastSyph="01" if monthoflastSyph=="1/"
replace monthoflastSyph="02" if monthoflastSyph=="/2"
replace monthoflastSyph="06" if monthoflastSyph=="99"
replace monthoflastSyph=" " if monthoflastSyph=="Xx"
replace yearoflastSyph=" " if (yearoflastSyph=="00" | yearoflastSyph=="0000")
replace yearoflastSyph="2001" if yearoflastSyph=="/201"
replace yearoflastSyph="2018" if yearoflastSyph=="18"
replace yearoflastSyph="2019" if yearoflastSyph=="19"
replace yearoflastSyph="2020" if yearoflastSyph=="20"
replace yearoflastSyph="2019" if yearoflastSyph=="0219"
replace yearoflastSyph=" " if (yearoflastSyph=="99" | yearoflastSyph=="999" | yearoflastSyph=="9999")
destring dayoflastSyph, replace
destring monthoflastSyph, replace
destring yearoflastSyph, replace
gen dateoflastSyphilis = mdy(monthoflastSyph, dayoflastSyph, yearoflastSyph) 
format dateoflastSyphilis %td

//timesincelastSyphtest
gen timesinceSyph=(dateofvisit2-dateoflastSyphilis)/365.25
//testedlastyearSyph
gen testedlastyearSyph=.
replace testedlastyearSyph=1 if timesinceSyph<1 
replace testedlastyearSyph=2 if timesinceSyph>=1 & timesinceSyph<.
replace testedlastyearSyph=. if timesinceSyph==.

//date of last HCV test
gen str dayoflasthcv = substr(dateprevioushcv,1,2)
gen str monthoflasthcv = substr(dateprevioushcv,4,2)
gen str yearoflasthcv = substr(dateprevioushcv,7,4)
replace dayoflasthcv="15" if dayoflasthcv=="00"
replace monthoflasthcv="06" if monthoflasthcv=="00"
replace yearoflasthcv=" " if (yearoflasthcv=="00" | yearoflasthcv=="0000")
destring dayoflasthcv, replace
destring monthoflasthcv, replace
destring yearoflasthcv, replace
gen dateoflasthcv = mdy(monthoflasthcv, dayoflasthcv, yearoflasthcv) 
format dateoflasthcv %td

//timesincelasthcvtest
gen timesincehcv=(dateofvisit2-dateoflasthcv)/365.25
//testedlastyearhcv
gen testedlastyearhcv=.
replace testedlastyearhcv=1 if timesincehcv<1 
replace testedlastyearhcv=2 if timesincehcv>=1 & timesincehcv<.
replace testedlastyearhcv=. if timesincehcv==.

//genvar age
gen ageinyears=.
replace ageinyears= (dateofvisit2 - dateofbirth2) if age==.
replace ageinyears= ageinyears/365.25
replace ageinyears=age if centros==6

//age group2
gen agegroup2=.
replace agegroup2=1 if ageinyears<25 & ageinyears>=13
replace agegroup2=2 if ageinyears>=25 & ageinyears<=110 & ageinyears<.


//genvar agegroup according to ECDC cats
recode ageinyears (65/110=5 ">65") (45/65=4 ">=45-65") (25/45=3 ">=25-45") (16/25=2 ">=16-25") (13/16=1 "<16") (else=.), gen (agegroupECDC) label(dagegroupECDC)

**grups edat DEVO
recode ageinyears (50/max=4 ">50")(35/50=3 ">=36-50")(20/50=2 ">=21-35")(13/20=1 "<20")(else=.), gen (agegroupDEVO)label(dagegroupDEVO)

//msm and tsm
gen msm=.
replace msm=1 if gender==1 & sexwith==1
replace msm=1 if gender==1 & sexwith==3
replace msm=1 if gender==3 & sexwith==1 // trans person sex with men
replace msm=1 if gender==3 & sexwith==3 // trans person sex with men
replace msm=2 if gender==2
replace msm=2 if gender==1 & sexwith==2

//recode sw
rename sexworker sw
replace sw=. if sw==0

//create sw gender
gen swgen=.
replace swgen=1 if gender==1 & sw==1 
replace swgen=2 if gender==2 & sw==1
replace swgen=3 if gender==3 & sw==1
replace swgen=0 if sw==.

//recode pwid
rename intravenousdruguse pwid
replace pwid=. if pwid==0

//recode migrant
replace foreignnational=. if foreignnational==0
rename foreignnational migrant

//evertested
rename previoushivtest evertested
replace evertested=. if evertested==0

//timesincelasthivtest
gen timesince=(dateofvisit2-dateoflasthiv)/365.25
//testedlastyear
rename test12monthscbvct testedlastyearsamecbvct
gen testedlastyear=.
replace testedlastyear=1 if timesince<1 | (timesince==. & testedlastyearsamecbvct==1)
replace testedlastyear=2 if timesince>=1 & timesince<.
replace testedlastyear=. if timesince==.
replace testedlastyearsamecbvct=. if testedlastyearsamecbvct==0
rename resultlasthivtest resultlasthiv
replace resultlasthiv=. if resultlasthiv==0

//unprotected sex in last 12 months with
rename unprotectedsexsw2 unprotectedsw
replace unprotectedsw=. if unprotectedsw==0

rename unprotectedsexidu unprotectedidu
replace unprotectedidu=. if unprotectedidu==0

rename unprotectedsexhivpositive unprotectedhiv
replace unprotectedhiv=. if unprotectedhiv==0

rename unprotectedsexmsm unprotectedmsm
replace unprotectedmsm=. if unprotectedmsm==0

//hiv screening
rename testused hivtestused
replace hivtestused=. if hivtestused==0
gen screeninghivtest=.
replace screeninghivtest=1 if hivtestused!=.
replace screeningtestresult=. if screeningtestresult==0


//confirmatoryhivtest
replace confirmhivtest=. if confirmhivtest==0
replace confirmhivtestres=. if confirmhivtestres==0
replace linkagehealthcare=. if linkagehealthcare==0

//syphilis
rename previoussyphilis sypheverdiagnosed
replace sypheverdiagnosed=. if sypheverdiagnosed==0
replace syphilistest=. if syphilistest==0
rename syphilistest syphilistest
rename typesyphilistest syphtestused
replace syphtestused=. if syphtestused==0
rename syphilisrapidtestresult syphscreeningtestresult
replace syphscreeningtestresult=. if syphscreeningtestresult==0
rename syphilisconfirmation syphconfirmatorytest
replace syphconfirmatorytest=. if syphconfirmatorytest==0
rename syphilisdiagnosis syphconfirmatorytestresult
replace syphconfirmatorytestresult=. if syphconfirmatorytestresult==0

//hcv
rename previoushcv hcveverdiagnosed
replace hcveverdiagnosed=. if hcveverdiagnosed==0
replace hcvtest=. if hcvtest==0
rename hcvtesttype hcvtestused
replace hcvtestused=. if hcvtestused==0
rename hcvrapidtestresult hcvscreeningtestresult
replace hcvscreeningtestresult=. if hcvscreeningtestresult==0
rename hcvconfirmation hcvconfirmatorytest
replace hcvconfirmatorytest=. if hcvconfirmatorytest==0
rename hcvdiagnosis hcvconfirmatorytestresult
replace hcvconfirmatorytestresult=. if hcvconfirmatorytestresult==0

*************
**generem la variable grups de transmissió
gen transmissiongroup=.
replace transmissiongroup=1 if pwid==1
replace transmissiongroup=2 if gender==3 & sw==1 & pwid!=1
replace transmissiongroup=3 if gender==1 & sw==1 & pwid!=1
replace transmissiongroup=4 if gender==1 & msm==1 & sw!=1 & pwid!=1
replace transmissiongroup=5 if gender==2 & sw==1 & pwid!=1
replace transmissiongroup=6 if gender==2 & sw!=1 & pwid!=1
replace transmissiongroup=7 if gender==1 & msm!=1 & sw!=1 & pwid!=1
replace transmissiongroup=8 if gender==3 & sw!=1 & pwid!=1
label define dtransmissiongroup 1 "PWID" 2 "Trans SW" 3 "Male SW" 4 "MSM" 5 "Female SW" 6 "Women" 7 "Heterosexual men" 8 "Trans no SW"
label variable transmissiongroup "dtransmissiongroup"


*******GENEREM UN ID PER TOTS****************
//give all cases id (Anna)
gen cobaid2=cobaid
egen cobaidconcat= concat(cbvctidentifier gender dateofbirth migrant tourist municipality)
egen cobaidconcat2= concat(cobaid cbvctidentifier gender dateofbirth migrant tourist municipality)


**extraiem els 3 últims dígits del cbvctidentifier, corresponent a germans grans, germanes gran, inicial nom mare
gen cobaid3=cobaid, after(cobaid) 
format cobaid3 %-17s
gen germans= usubstr(cobaid3,12,.), after(cobaid3)
replace germans=ustrupper(germans)
format germans %-7s
**extraiem el dia, mes i any de naixement en format cadena
gen str dayofbirth2 = substr(dateofbirth_rec,1,2)
gen str monthofbirth2 = substr(dateofbirth_rec,4,2)
gen str yearofbirth2 = substr(dateofbirth_rec,7,4)
**recodifiquem la variable gender en 0 1 2 enlloc de 1 2 3
recode gender (1= 0)  (2 = 1)  (3 = 2), generate(gender2)

**generem de nou el identificador (enlloc de fer servir el que es genera de forma automàtica)
egen cobaidconcat3= concat(gender2 dayofbirth2 monthofbirth2 yearofbirth2 germans)

**aliniem a la dreta el cbvctidentifier
format cbvctidentifier %-17s
**eliminem espais en blanc a l'esquerra
replace cbvctidentifier=ustrltrim(cbvctidentifier)
**substituïm en el cas de ACASC el H/M del principi del cbvctidentifier, per 0/1
gen Hcbvctidentifier=ustrpos(cbvctidentifier,"H")
gen Mcbvctidentifier=ustrpos(cbvctidentifier,"M")
gen cbvctidentifier2=cbvctidentifier
replace cbvctidentifier2=usubinstr(cbvctidentifier2, "H", "0",1) if Hcbvctidentifier==1
replace cbvctidentifier2=usubinstr(cbvctidentifier2, "M", "1",1) if Mcbvctidentifier==1
**tot el cbvctidentifier en majúscules
replace cbvctidentifier2=ustrupper(cbvctidentifier2)


**generem el ID comú que farem servir per mirar repetidors
gen ID=cobaidconcat3
replace ID=cbvctidentifier2 if centros==1 | centros==6 | centros==8 | centros==12 | centros==19 | centros==25 | centros==27 | centros==33 ///
| centros==37 | centros==43 | centros==45 | centros==46 | centros==47 | centros==48 | centros==49 | centros==50 | centros==51

replace ID="00506198121B" if centros==2 & cbvctidentifier2=="1050198121B"
replace ID="02910199600A" if centros==2 & cbvctidentifier2=="029011996A"
replace ID="00211199101F" if centros==2 & cbvctidentifier2=="1211199101F"
replace ID="01107198000G" if centros==2 & cbvctidentifier2=="0110719800G"
replace ID="01503199800E" if centros==2 & cbvctidentifier2=="0503199800E"
replace ID="10704198901J" if centros==2 & cbvctidentifier2=="107041987901J"
replace ID="00211197400G" if centros==2 & cbvctidentifier2=="0211197400G"
replace ID="11101200000A" if centros==2 & cbvctidentifier2=="1111200000A"
replace ID="00111197511M" if centros==2 & cbvctidentifier2=="010111197511M"
replace ID="01601196843A" if centros==2 & cbvctidentifier2=="016011968"
replace ID="10110199001M" if centros==2 & cbvctidentifier2=="010110199001M"
replace ID="00505199700C" if centros==2 & cbvctidentifier2=="005051699700C"
replace ID="02812196610A" if centros==2 & cbvctidentifier2=="028121966610A"
replace ID="00103199110F" if centros==2 & cbvctidentifier2=="0013199110F"
replace ID="01510199011C" if centros==2 & cbvctidentifier2=="015101999011C"
replace ID="00808198611A" if centros==2 & cbvctidentifier2=="0808198611A"
replace ID="01004200000M" if centros==2 & cbvctidentifier2=="0104200000M"
replace ID="01008198540M" if centros==2 & cbvctidentifier2=="010081985"
replace ID="00109198100C" if centros==2 & cbvctidentifier2=="0010.9198100C"
replace ID="02007198701M" if centros==2 & cbvctidentifier2=="020071987101M"
replace ID="12212200000M" if centros==2 & cbvctidentifier2=="1221220000M"
replace ID="02907198200"  if centros==2 & cbvctidentifier2=="129071982"
replace ID="11905198802A" if centros==2 & cbvctidentifier2=="118905198802A"

replace ID="10312198011A" if centros==4 & cobaidconcat3=="10312198098011A"
replace ID="16021990051M" if centros==4 & cobaidconcat3=="106021990051M"
replace ID="11601198602N" if centros==4 & cobaidconcat3=="116011986102N"

replace ID=cbvctidentifier2 if centros==7 & cobaidconcat3=="00508196400"
replace ID=cbvctidentifier2 if centros==7 & cobaidconcat3=="00503197800"
replace ID=cbvctidentifier2 if centros==7 & cobaidconcat3=="12110198800"
replace ID=cbvctidentifier2 if centros==7 & cobaidconcat3=="01609198600"

replace ID="00103198810A" if centros==11 & cobaidconcat3=="0010319881010A"
replace ID="20601199110M" if centros==11 & cobaidconcat3=="2060119911010M"
replace ID="02405198414I" if centros==11 & cobaidconcat3=="0240519841114I"
replace ID="02405198414I" if centros==11 & cobaidconcat3=="0240519841114I"
replace ID=cbvctidentifier2 if centros==11 & cbvctidentifier2=="21007197502M"
replace ID=cbvctidentifier2 if centros==11 & cbvctidentifier2=="21007197502M"
replace ID=cbvctidentifier2 if centros==11 & cbvctidentifier2=="01707199200M"


replace ID=cbvctidentifier2 if centros==5 & cbvctidentifier2=="00411199510M"
replace ID=cbvctidentifier2 if centros==5 & cbvctidentifier2=="02504198202M"

replace ID=cobaidconcat3 if centros==27 & cbvctidentifier2=="0141197100E"

replace ID=cobaidconcat3 if centros==37 & ID==""
replace ID=cobaidconcat3 if centros==37 & ID=="0050319907212F"
replace ID=cobaidconcat3 if centros==37 & ID=="008041987110S"
replace ID=cobaidconcat3 if centros==37 & ID=="0081219100M"
replace ID="01005199800K" if centros==37 & ID=="010051998900K"
replace ID=cobaidconcat3 if centros==37 & ID=="012102198210E"
replace ID=cobaidconcat3 if centros==37 & ID=="0165197501A"
replace ID=cobaidconcat3 if centros==37 & ID=="0191199700I"
replace ID=cobaidconcat3 if centros==37 & ID=="030061998"
replace ID=cobaidconcat3 if centros==37 & ID=="101071988500M"
replace ID="10810198000S" if centros==37 & ID=="108100198000S"
replace ID=cobaidconcat3 if centros==37 & ID=="1179199801M"
replace ID="11806199700D" if centros==37 & ID=="1180619970D"
replace ID=cobaidconcat3 if centros==37 & ID=="118211199910C"
replace ID=cobaidconcat3 if centros==37 & ID=="1200418900F"
replace ID=cobaidconcat3 if centros==37 & ID=="1200418900F"

replace ID="19641001JOBOM" if centros==47 & ID=="01/10/1964JOBOM"
replace ID="19593007MAES" if centros==47 & ID=="1959-30-07MAES"
replace ID="19611217SEMA" if centros==47 & ID=="1961-12-17SEMA"
replace ID="19611018RISO" if centros==47 & ID=="1961/10/18RISO"
replace ID="19640104JIBIM" if centros==47 & ID=="1964-01-04-JIBIM"
replace ID="19651108MOAM" if centros==47 & ID=="1965-11-08MOAM"
replace ID="19690707" if centros==47 & ID=="1969-07-07"
replace ID="19761604NALA" if centros==47 & ID=="1976-16-04 NALA"
replace ID="19770612FAWI" if centros==47 & ID=="1977-06-12 FAWI"
replace ID="19790929KACRF" if centros==47 & ID=="1979-09-29KACRF"
replace ID="19800529SAHEM" if centros==47 & ID=="1980-05-29SAHEM"
replace ID="19800809TACO" if centros==47 & ID=="1980-08-09 TACO"
replace ID="19810210ANCAM" if centros==47 & ID=="1981/02/10 ANCAM"
replace ID="19842406CYMO" if centros==47 & ID=="1984/24/06CYMO"
replace ID="19850804GAHI" if centros==47 & ID=="1985-08-04 GAHI"
replace ID="19870512LOSA" if centros==47 & ID=="1987-05-12LOSA"
replace ID="19890915MOCH" if centros==47 & ID=="1989-09-15MOCH"
replace ID="19891201GANKM" if centros==47 & ID=="1989-12-01 GANK-M"
replace ID="119911306ILAN" if centros==47 & ID=="1991-13-06ILAN"
replace ID="19920617HECOF" if centros==47 & ID=="1992-06-17HECOF"
replace ID="19922312JOGE" if centros==47 & ID=="1992-23-12JOGE"
replace ID="19923107ADCL" if centros==47 & ID=="1992-31-07ADCL"
replace ID="19920825WAMO" if centros==47 & ID=="1992/08/25 WAMO"
replace ID="19941128XXXX" if centros==47 & ID=="1994-11/28/XXXX"
replace ID="19940423GWPEF" if centros==47 & ID=="1994/04/23 GWPEF"
replace ID="19970321AGABM" if centros==47 & ID=="1997/03/21 AGABM"
replace ID="19972003MAMA" if centros==47 & ID=="1997/20/03MAMA"
replace ID="19973007XXXX" if centros==47 & ID=="1997/30/07/XXXX"
replace ID="19980507KEREM" if centros==47 & ID=="1998-05-07KEREM"
replace ID="19991005WAKAF" if centros==47 & ID=="1999-10-05-WAKAF"
replace ID="20000522WMMFM" if centros==47 & ID=="2000/05/22WMMFM"
replace ID="24061984" if centros==47 & ID=="24-06-1984"
replace ID="" if centros==47 & ID=="AAAAMMDDXXXXM"
replace ID="" if centros==47 & ID=="PLATEFORME PREVENTION SIDA"
replace ID="" if centros==47 & ID=="PPSIDA"
replace ID="" if centros==47 & ID=="XXXXXXXXXXXXF"
replace ID="" if centros==47 & ID=="XXXXXXXXXXXXM"



***mirar els duplicats de mateix formulari(stop sida en tenia...)
duplicates tag ID  dateofvisit2  centros, generate(Duplicateform)

gsort +ID +dateofvisit2 +centros -form
quietly by ID dateofvisit2 centros: gen dupdup = cond(_N==1,0,_n)

***eliminem els duplicats de mateix formulari entrat 2 vegades
drop if centros!=11 & dupdup>1
drop if centros==11 & form=="COBATEST" & dupdup>1


//recode test results for people who did not have syph or hcv test
replace syphscreeningtestresult=. if syphilistest==2
replace syphconfirmatorytest=. if syphilistest==2
replace syphconfirmatorytestresult=. if syphilistest==2
replace hcvscreeningtestresult=. if hcvtest==2
replace hcvconfirmatorytestresult=. if hcvtest==2
replace hcvconfirmatorytest=. if hcvtest==2

************COMPTEM CASOS QUE S'HAN FET PROVES PER MÉS D'UNA INFECCIÓ*****************
gen hivtest2=inlist(screeninghivtest,1)
gen syphtest2=inlist(syphilistest,1)
gen hcvtest2=inlist(hcvtest,1)
egen float Ntestsinfeccions = rowtotal(hivtest2 syphtest2 hcvtest2) 
egen float Ntestshivsyph = rowtotal(hivtest2 syphtest2) if hcvtest2!=1
egen float Ntestshivsyph2 = rowtotal(hivtest2 syphtest2)
egen float Ntestshivhcv = rowtotal(hivtest2 hcvtest2) if syphtest2!=1
egen float Ntestshivhcv2 = rowtotal(hivtest2 hcvtest2)
egen float Ntestssyphcv = rowtotal(syphtest2 hcvtest2) if hivtest2!=1
egen float Ntestssyphcv2 = rowtotal(syphtest2 hcvtest2)

************COMPTEM CASOS AMB MÉS D'UN POSITIU***************************************
gen screeningtestresult2=inlist(screeningtestresult,1)
gen syphscreeningtestresult2=inlist(syphscreeningtestresult,1)
gen hcvscreeningtestresult2=inlist(hcvscreeningtestresult,1)
egen float Nreactiveresults = rowtotal(screeningtestresult2 syphscreeningtestresult2 hcvscreeningtestresult2)
egen float Nreactiveshivsyph = rowtotal(screeningtestresult2 syphscreeningtestresult2) if hcvscreeningtestresult2!=1
egen float Nreactiveshivhcv =  rowtotal(screeningtestresult2 hcvscreeningtestresult2) if syphscreeningtestresult2!=1
egen float Nreactivessyphhcv = rowtotal(syphscreeningtestresult2 hcvscreeningtestresult2) if screeningtestresult2!=1
egen float Nreactiveshivsyph2 = rowtotal(screeningtestresult2 syphscreeningtestresult2)
egen float Nreactiveshivhcv2 =  rowtotal(screeningtestresult2 hcvscreeningtestresult2) 
egen float Nreactivessyphhcv2 = rowtotal(syphscreeningtestresult2 hcvscreeningtestresult2)

//recode test results for people who did not have syph or hcv test
replace syphscreeningtestresult=. if syphilistest==2
replace syphconfirmatorytest=. if syphilistest==2
replace syphconfirmatorytestresult=. if syphilistest==2
replace hcvscreeningtestresult=. if hcvtest==2
replace hcvconfirmatorytestresult=. if hcvtest==2
replace hcvconfirmatorytest=. if hcvtest==2

//unify identifier cobatestid
rename cobaid3 cobatestid
rename cbvctidentifier clientidentifier
replace clientidentifier=""

//cd4count recoded to number
rename hcvconfirmatorytest hcvrnatest
replace cd4count="" if cd4count=="NO HO SE"
destring cd4count, replace

//date of cd4 count
gen str dayofcd4 = substr(cd4date,1,2)
gen str monthofcd4 = substr(cd4date,4,2)
gen str yearofcd4 = substr(cd4date,7,4)
replace dayofcd4="15" if dayofcd4=="00"
replace monthofcd4="06" if monthofcd4=="00"
destring dayofcd4, replace
destring monthofcd4, replace
destring yearofcd4, replace
gen cd4date2 = mdy(monthofcd4, dayofcd4, yearofcd4)
format cd4date2 %td
drop cd4date
rename cd4date2 cd4date


//drop repeat testers//
*to count people tested
//identify persons tested
gsort +ID -dateofvisit2 
quietly by ID: gen dup = cond(_N==1, 0, _n) , after(dupdup)
replace dup=0 if (ID=="" | ID==".00" | ID==".00M" | ID=="0000" | ID=="000A" | ID=="000B" /// 
					| ID=="000G" | ID=="000L" | ID=="000M" | ID=="000P" | ID=="000R" | ID=="000S")

//drop if dup >1//


*save the database with the necessary variables
rename  (centros cbvctidentifier2 cobatestid ID dup gender ageinyears dateofbirth2 agegroup2 agegroupECDC msm sw pwid ///
		migrant dateofvisit2 evertested testedlastyear testedlastyearsamecbvct pretest_counselling screeninghivtest ///
		screeningtestresult date_test_result test_result_received post_test_counselling confirmhivtest dateconfirmhivtest ///
		confirmhivtestres confirm_hiv_test_res_rec_date confirm_hiv_test_res_rec linkagehealthcare linkagedate cd4count cd4date ///
		hivtestused sypheverdiagnosed testedlastyearSyph syphilistest syphscreeningtestresult syphconfirmatorytest syphconfirmatorytestresult syphtestused ///
		hcveverdiagnosed testedlastyearhcv hcvtest hcvscreeningtestresult hcvrnatest hcvconfirmatorytestresult hcvtestused) ///
        (centre CBVCTIdentifier COBATEST_Id Id RepeatTester Gender AgeInYears DateOfBirth AgeGroupc AgeGroupECDC MSM SW PWID ///
	    Migrant DateofVisit EverTested TestedLastYear TestedLastYearSameCBVCT PreTestCounselling ScreeningHIVTest ///
        ScreeningTestResult DateScreeningTestResult ScreeningTestResultReceived ScreeningPostTestCounselling ConfirmatoryHIVTest DateConfirmatoryTest ///
        ConfirmatoryHIVTestResult DateConfirmatoryTestResult ConfirmatoryTestResultReceived LinkageToHealthCare Datedlinkage CD4Count DateCD4Count ///
        HIVTestUsed SyphEverDiagnosed SyphTestedLastYear SyphScreeningTest SyphScreeningTestResult SyphConfirmatoryTest SyphConfirmatoryTestResult SyphTestUsed /// 
        HCVEverDiagnosed HCVTestedLastYear HCVScreeningTest HCVScreeningTestResult HCVRNATest HCVConfirmatoryTestResult HCVTestUsed)


keep    centre CBVCTIdentifier COBATEST_Id Id RepeatTester Gender AgeInYears DateOfBirth AgeGroupc AgeGroupECDC MSM SW PWID ///
	    Migrant DateofVisit EverTested TestedLastYear TestedLastYearSameCBVCT PreTestCounselling ScreeningHIVTest ///
        ScreeningTestResult DateScreeningTestResult ScreeningTestResultReceived ScreeningPostTestCounselling ConfirmatoryHIVTest DateConfirmatoryTest ///
        ConfirmatoryHIVTestResult DateConfirmatoryTestResult ConfirmatoryTestResultReceived LinkageToHealthCare Datedlinkage CD4Count DateCD4Count ///
        HIVTestUsed SyphEverDiagnosed SyphTestedLastYear SyphScreeningTest SyphScreeningTestResult SyphConfirmatoryTest SyphConfirmatoryTestResult SyphTestUsed /// 
        HCVEverDiagnosed HCVTestedLastYear HCVScreeningTest HCVScreeningTestResult HCVRNATest HCVConfirmatoryTestResult HCVTestUsed


order   centre CBVCTIdentifier COBATEST_Id Id RepeatTester Gender AgeInYears DateOfBirth AgeGroupc AgeGroupECDC MSM SW PWID ///
	    Migrant DateofVisit EverTested TestedLastYear TestedLastYearSameCBVCT PreTestCounselling ScreeningHIVTest ///
        ScreeningTestResult DateScreeningTestResult ScreeningTestResultReceived ScreeningPostTestCounselling ConfirmatoryHIVTest DateConfirmatoryTest ///
        ConfirmatoryHIVTestResult DateConfirmatoryTestResult ConfirmatoryTestResultReceived LinkageToHealthCare Datedlinkage CD4Count DateCD4Count ///
        HIVTestUsed SyphEverDiagnosed SyphTestedLastYear SyphScreeningTest SyphScreeningTestResult SyphConfirmatoryTest SyphConfirmatoryTestResult SyphTestUsed /// 
        HCVEverDiagnosed HCVTestedLastYear HCVScreeningTest HCVScreeningTestResult HCVRNATest HCVConfirmatoryTestResult HCVTestUsed
	 
save "C:\Users\Laura\OneDrive - Generalitat de Catalunya\Confinament\COBATEST\Dades COBATEST 2019\COBATEST 2019 clean data for indicators.dta", replace




