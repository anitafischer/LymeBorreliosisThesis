
###########################################################################################################
### PACKAGES INSTALLIEREN UND LADEN ###
install.packages("lme4")
install.packages("lmerTest")
install.packages("glmmTMB")
install.packages("vroom")
install.packages("readr")
install.packages("readxl")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("glmmTMB")
install.packages("tidyr")
install.packages("lubridate")
install.packages("car")
install.packages("lmtest")
install.packages("FSA")
install.packages("GGally")
install.packages("glmmLasso")
install.packages("glmnet")
install.packages("writexl")
install.packages("psych")
install.packages("e1071")
install.packages("corrplot")
install.packages("MASS")
install.packages("arm")
install.packages("broom.mixed")
install.packages("performance")
install.packages("plotly")
install.packages("naniar")
library(naniar)
library(plotly)
library(broom.mixed)
library(performance)
library(arm) 
library(MASS)
library(GGally)
library(corrplot)
library(e1071)
library(psych)
library(writexl)
library(glmnet)
library(glmmLasso)
library(FSA)
library(car)
library(lmtest)
library(lubridate)
library(tidyr)
library(readr)
library(vroom)
library(lme4)
library(lmerTest)
library(readxl)
library(dplyr)
library(openxlsx)
library(glmmTMB)


setwd("P:/Thesis/Saarland")
getwd()



###########################################################################################################
### EINLESEN DER DATENSÄTZE###


## SAARLAND ##

saarland_pop1 <- read_excel("saarland_pop_1.xlsx")

merzig_wetter <- read.csv("S_03904.txt", header = TRUE, sep = ";")
neunkirchen_wetter <- read.csv("S_03545.txt", header = TRUE, sep = ";")
saarlouis_wetter <- read.csv("S_00460.txt", header = TRUE, sep = ";")
saarpfalz_wetter <- read.csv("S_04336.txt", header = TRUE, sep = ";")
wendel_wetter <- read.csv("S_05029.txt", header = TRUE, sep = ";")
verbandsaar_wetter <- read.csv("S_04336.txt", header = TRUE, sep = ";")

merzig_rki <- read_csv2("merzig_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
neunkirchen_rki <- read_csv2("neunkirchen_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
saarlouis_rki <- read_csv2("saalouis_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
saarpfalz_rki <- read_csv2("saarpfalz_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
wendel_rki <- read_csv2("wendel_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
verbandsaar_rki <- read_csv2("verbandsaar_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))

## SACHSEN ##

sachsen_pop1 <- read_excel("sachsen_pop_1.xlsx")


chemnitz_wetter <- read.csv("S_00853.txt", header = TRUE, sep = ";")
erzgebirgskreis_wetter <- read.csv("S_03166.txt", header = TRUE, sep = ";")
mittelsachsen_wetter <- read.csv("S_00131.txt", header = TRUE, sep = ";")
vogtland_wetter <- read.csv("S_03946.txt", header = TRUE, sep = ";")
zwickau_wetter <- read.csv("S_00222.txt", header = TRUE, sep = ";")
bautzen_wetter <- read.csv("S_00314.txt", header = TRUE, sep = ";")
dresden_wetter <- read.csv("S_01050.txt", header = TRUE, sep = ";")
görlitz_wetter <- read.csv("S_01684.txt", header = TRUE, sep = ";")
meißen_wetter <- read.csv("S_03234.txt", header = TRUE, sep = ";")
sächsischeschweiz_wetter <- read.csv("S_02985.txt", header = TRUE, sep = ";")
leipzig_wetter <- read.csv("S_02928.txt", header = TRUE, sep = ";")
nordsachsen_wetter <- read.csv("S_02641.txt", header = TRUE, sep = ";")


chemnitz_rki <- read_csv2("chemnitz_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
erzgebirgskreis_rki <- read_csv2("erzgebirgskreis_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
mittelsachsen_rki <- read_csv2("mittelsachsen_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
vogtland_rki <- read_csv2("vogtland_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
zwickau_rki <- read_csv2("zwickau_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
bautzen_rki <- read_csv2("bautzen_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
dresden_rki <- read_csv2("dresden_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
görlitz_rki <- read_csv2("görlitz_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
sächsischeschweiz_rki <- read_csv2("sächsischeschweiz_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
leipzig_rki <- read_csv2("leipzig_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
nordsachsen_rki <- read_csv2("nordsachsen_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))
meißen_rki <- read_csv2("meißen_ID.csv", skip = 1, col_names = c("Meldejahr", "männlich", "weiblich", "STATIONS_ID"))


###########################################################################################################
### MELDEJAHR IN DWD DATEIEN HINZUFÜGEN ###


#Funktion
extract_year <- function(date) {
  as.integer(substr(as.character(date), 1, 4))
}

## SAARLAND ##
merzig_wetter <- merzig_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )

neunkirchen_wetter <- neunkirchen_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
saarlouis_wetter <- saarlouis_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
saarpfalz_wetter <- saarpfalz_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
wendel_wetter <- wendel_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
verbandsaar_wetter <- verbandsaar_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )

## SACHSEN ##

chemnitz_wetter <- chemnitz_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
erzgebirgskreis_wetter <- erzgebirgskreis_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
mittelsachsen_wetter <- mittelsachsen_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
vogtland_wetter <- vogtland_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
zwickau_wetter <- zwickau_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
bautzen_wetter <- bautzen_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
dresden_wetter <- dresden_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
görlitz_wetter <- görlitz_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
meißen_wetter <- meißen_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
sächsischeschweiz_wetter <- sächsischeschweiz_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )
leipzig_wetter <- leipzig_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )

nordsachsen_wetter <- nordsachsen_wetter %>%
  mutate(
    Meldejahr = extract_year(MESS_DATUM_BEGINN)
  )


###########################################################################################################
### UMSTRUKTURIEREN DER RKI DATEN ZU LONG FORMAT ###


## SAARLAND ##
merzig_rki_long <- merzig_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
neunkirchen_rki_long <- neunkirchen_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
saarlouis_rki_long <- saarlouis_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
saarpfalz_rki_long <- saarpfalz_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
wendel_rki_long <- wendel_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
verbandsaar_rki_long <- verbandsaar_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")

## SACHSEN ##
chemnitz_rki_long <- chemnitz_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
erzgebirgskreis_rki_long <- erzgebirgskreis_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
mittelsachsen_rki_long <- mittelsachsen_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
vogtland_rki_long <- vogtland_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
zwickau_rki_long <- zwickau_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
bautzen_rki_long <- bautzen_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
dresden_rki_long <- dresden_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
görlitz_rki_long <- görlitz_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
sächsischeschweiz_rki_long <- sächsischeschweiz_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
leipzig_rki_long <- leipzig_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
nordsachsen_rki_long <- nordsachsen_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")
meißen_rki_long <- meißen_rki %>%
  pivot_longer(cols = c("männlich", "weiblich"),
               names_to = "Geschlecht",
               values_to = "Anzahl")



###########################################################################################################
### RKI UND DWD DATEN LEFT JOIN ###

## SAARLAND ##
merzig_data <- merzig_rki_long %>%
  left_join(merzig_wetter, by = c("Meldejahr", "STATIONS_ID"))
neunkirchen_data <- neunkirchen_rki_long %>%
  left_join(neunkirchen_wetter, by = c("Meldejahr", "STATIONS_ID"))
saarlouis_data <- saarlouis_rki_long %>%
  left_join(saarlouis_wetter, by = c("Meldejahr", "STATIONS_ID"))
saarpfalz_data <- saarpfalz_rki_long %>%
  left_join(saarpfalz_wetter, by = c("Meldejahr", "STATIONS_ID"))
wendel_data <-wendel_rki_long %>%
  left_join(wendel_wetter, by = c("Meldejahr", "STATIONS_ID"))
verbandsaar_data <- verbandsaar_rki_long %>%
  left_join(verbandsaar_wetter, by = c("Meldejahr", "STATIONS_ID"))



## SACHSEN ##
chemnitz_data <- chemnitz_rki_long %>%
  left_join(chemnitz_wetter, by = c("Meldejahr", "STATIONS_ID"))
erzgebirgskreis_data <- erzgebirgskreis_rki_long %>%
  left_join(erzgebirgskreis_wetter, by = c("Meldejahr", "STATIONS_ID"))
mittelsachsen_data <- mittelsachsen_rki_long %>%
  left_join(mittelsachsen_wetter, by = c("Meldejahr", "STATIONS_ID"))
vogtland_data <- vogtland_rki_long %>%
  left_join(vogtland_wetter, by = c("Meldejahr", "STATIONS_ID"))
zwickau_data <- zwickau_rki_long %>%
  left_join(zwickau_wetter, by = c("Meldejahr", "STATIONS_ID"))
bautzen_data <- bautzen_rki_long %>%
  left_join(bautzen_wetter, by = c("Meldejahr", "STATIONS_ID"))
dresden_data <- dresden_rki_long %>%
  left_join(dresden_wetter, by = c("Meldejahr", "STATIONS_ID"))
görlitz_data <- görlitz_rki_long %>%
  left_join(görlitz_wetter, by = c("Meldejahr", "STATIONS_ID"))
sächsischeschweiz_data <- sächsischeschweiz_rki_long %>%
  left_join(sächsischeschweiz_wetter, by = c("Meldejahr", "STATIONS_ID"))
leipzig_data <- leipzig_rki_long %>%
  left_join(leipzig_wetter, by = c("Meldejahr", "STATIONS_ID"))
nordsachsen_data <- nordsachsen_rki_long %>%
  left_join(nordsachsen_wetter, by = c("Meldejahr", "STATIONS_ID"))
meißen_data <- meißen_rki_long %>%
  left_join(meißen_wetter, by = c("Meldejahr", "STATIONS_ID"))



###########################################################################################################
### BEVÖLKERUNGSDATEN ZU LONG FORMAT ###

## SAARLAND ##

saarland_pop_1_long <- saarland_pop1 %>%
  pivot_longer(cols = c( "Insgesamt", "Männlich", "Weiblich", "je km2"),
               names_to = "Geschlecht",
               values_to = "Wert")
saarland_pop_1_long <- saarland_pop1 %>% pivot_longer(cols = c(Männlich, Weiblich),
             names_to = "Geschlecht",
             values_to = "Wert")

saarland_pop_1_long$Bundesland <- "Saarland"

## SACHSEN ##
sachsen_pop_1_long <- sachsen_pop1 %>%
  pivot_longer(cols = c( "Insgesamt", "Männlich", "Weiblich", "je km2"),
               names_to = "Geschlecht",
               values_to = "Wert")
sachsen_pop_1_long <- sachsen_pop1 %>% pivot_longer(cols = c(Männlich, Weiblich),
                                                      names_to = "Geschlecht",
                                                      values_to = "Wert")
sachsen_pop_1_long$Bundesland <- "Sachsen"



###########################################################################################################
### FORMATIERUNG DER WETTERSTATIONS ID ###

## SAARLAND ##

merzig_rki_long$STATIONS_ID <- as.character( merzig_rki_long$STATIONS_ID)
saarland_pop_1_long$STATIONS_ID <- as.character(saarland_pop_1_long$STATIONS_ID)
merzig_data$STATIONS_ID <- as.character(merzig_data$STATIONS_ID)
neunkirchen_data$STATIONS_ID <- as.character(neunkirchen_data$STATIONS_ID)
saarlouis_data$STATIONS_ID <- as.character(saarlouis_data$STATIONS_ID)
saarpfalz_data$STATIONS_ID <- as.character(saarpfalz_data$STATIONS_ID)
wendel_data$STATIONS_ID <- as.character(wendel_data$STATIONS_ID)
verbandsaar_data$STATIONS_ID <- as.character(verbandsaar_data$STATIONS_ID)

## SACHSEN ##

sachsen_pop_1_long$STATIONS_ID <- as.character(sachsen_pop_1_long$STATIONS_ID)
chemnitz_data$STATIONS_ID <- as.character(chemnitz_data$STATIONS_ID)
erzgebirgskreis_data$STATIONS_ID <- as.character(erzgebirgskreis_data$STATIONS_ID)
mittelsachsen_data$STATIONS_ID <- as.character(mittelsachsen_data$STATIONS_ID)
vogtland_data$STATIONS_ID <- as.character(vogtland_data$STATIONS_ID)
zwickau_data$STATIONS_ID <- as.character(zwickau_data$STATIONS_ID)
bautzen_data$STATIONS_ID <- as.character(bautzen_data$STATIONS_ID)
dresden_data$STATIONS_ID <- as.character(dresden_data$STATIONS_ID)
görlitz_data$STATIONS_ID <- as.character(görlitz_data$STATIONS_ID)
sächsischeschweiz_data$STATIONS_ID <- as.character(sächsischeschweiz_data$STATIONS_ID)
leipzig_data$STATIONS_ID <- as.character(leipzig_data$STATIONS_ID)
nordsachsen_data$STATIONS_ID <- as.character(nordsachsen_data$STATIONS_ID)
meißen_data$STATIONS_ID <- as.character(meißen_data$STATIONS_ID)




###########################################################################################################
### BEVÖLKERUNGSDATEN LEFT JOIN MIT WETTER UND RKI DATEN ###

## SAARLAND ##
saarland_pop_1_long <- saarland_pop_1_long %>%
  mutate(Geschlecht = tolower(Geschlecht))

join_merzig <- merzig_data %>%
  left_join(saarland_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_neunkirchen <- neunkirchen_data %>%
  left_join(saarland_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_saarlouis <- saarlouis_data %>%
  left_join(saarland_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_saarpfalz <- saarpfalz_data %>%
  left_join(saarland_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_wendel <- wendel_data %>%
  left_join(saarland_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_verbandsaar <- verbandsaar_data %>%
  left_join(saarland_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))




## SACHSEN ##
sachsen_pop_1_long <- sachsen_pop_1_long %>%
  mutate(Geschlecht = tolower(Geschlecht))

join_chemnitz <- chemnitz_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_erzgebirgskreis <- erzgebirgskreis_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_mittelsachsen <- mittelsachsen_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_vogtland <- vogtland_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_zwickau <- zwickau_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_bautzen <- bautzen_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_dresden <- dresden_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_görlitz <- görlitz_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_sächsischeschweiz <- sächsischeschweiz_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_leipzig <- leipzig_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_nordsachsen <- nordsachsen_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))
join_meißen <- meißen_data %>%
  left_join(sachsen_pop_1_long, by = c("STATIONS_ID", "Geschlecht"))







###########################################################################################################
### MISSINGS FORMATIEREN ###


#Funktion
replace_999 <- function(x) {
  x[x == -999.00] <- NA
  return(x)
}


## SAARLAND ##
join_saar <- join_verbandsaar %>% mutate_all(replace_999)
join_merzig <- join_merzig %>% mutate_all(replace_999)
join_neunkirchen <- join_neunkirchen %>% mutate_all(replace_999)
join_saarlouis <- join_saarlouis %>% mutate_all(replace_999)
join_saarpfalz <- join_saarpfalz %>% mutate_all(replace_999)
join_wendel <- join_wendel %>% mutate_all(replace_999)


# SACHSEN ##
join_chemnitz <- join_chemnitz %>% mutate_all(replace_999)
join_erzgebirgskreis <- join_erzgebirgskreis %>% mutate_all(replace_999)
join_mittelsachsen <- join_mittelsachsen %>% mutate_all(replace_999)
join_vogtland <- join_vogtland %>% mutate_all(replace_999)
join_zwickau <- join_zwickau %>% mutate_all(replace_999)
join_bautzen <- join_bautzen %>% mutate_all(replace_999)
join_dresden <- join_dresden %>% mutate_all(replace_999)
join_görlitz <- join_görlitz %>% mutate_all(replace_999)
join_sächsischeschweiz <- join_sächsischeschweiz %>% mutate_all(replace_999)
join_leipzig <- join_leipzig %>% mutate_all(replace_999)
join_nordsachsen <- join_nordsachsen %>% mutate_all(replace_999)
join_meißen <- join_meißen %>% mutate_all(replace_999)







###########################################################################################################
### DURCHSCHNITTSWERTE BILDEN ###

#1 Durchschnitt Wintertemperatur
#2 Durchschnitt Frühling Niederschlag
#3 Durchschnitt Sommer Niederschlag
#4 Durchschnitt Sommer Temperatur



## SAARLAND ##


#STADTVERBAND SAARBÜCKEN UND SAARPFALZ
#(gemeinsam, da selbe Wetterstation)

#Einlesen monatlicher DWD-Daten
stadtverband_saarpfalz_wetter_monat_hist <- read.csv("stadtverband_saarpfalz_monat_hist.txt", header = TRUE, sep = ";")
stadtverband_saarpfalz_wetter_monat_hist <- stadtverband_saarpfalz_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung 
stadtverband_saarpfalz_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(stadtverband_saarpfalz_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
stadtverband_saarpfalz_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(stadtverband_saarpfalz_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
stadtverband_saarpfalz_wetter_monat_hist$Meldejahr <- lubridate::year(stadtverband_saarpfalz_wetter_monat_hist$MESS_DATUM_BEGINN)
stadtverband_saarpfalz_wetter_monat_hist$Monat <- lubridate::month(stadtverband_saarpfalz_wetter_monat_hist$MESS_DATUM_BEGINN)
stadtverband_saarpfalz_wetter_monat_hist <- stadtverband_saarpfalz_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

#1 
stadtverband_saarpfalz_wetter_monat_hist <- stadtverband_saarpfalz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  group_by(Meldejahr) %>%  
  filter(Monat %in% c(12, 1, 2)) %>%  
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(stadtverband_saarpfalz_wetter_monat_hist, by = "Meldejahr")  
stadtverband_saarpfalz_wetter_monat_hist <- stadtverband_saarpfalz_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_stadtverband_saarpfalz <- stadtverband_saarpfalz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))  

#3 
regen_avg_sommer_stadtverband_saarpfalz <- stadtverband_saarpfalz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  


#4 
temperatur_avg_sommer_stadtverband_saarpfalz <- stadtverband_saarpfalz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>% 
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))  



#RIGHT JOIN
stadtverband_saarpfalz_wetter_monat_hist <- stadtverband_saarpfalz_wetter_monat_hist %>%
  right_join(regen_avg_stadtverband_saarpfalz, by = "Meldejahr")
stadtverband_saarpfalz_wetter_monat_hist <- stadtverband_saarpfalz_wetter_monat_hist %>%
  right_join(regen_avg_sommer_stadtverband_saarpfalz, by = "Meldejahr")
stadtverband_saarpfalz_wetter_monat_hist <- stadtverband_saarpfalz_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_stadtverband_saarpfalz, by = "Meldejahr")






#NEUNKIRCHEN

#Einlesen monatlicher DWD-Daten
neunkirchen_wetter_monat_hist <- read.csv("neunkirchen_monat_hist.txt", header = TRUE, sep = ";")
neunkirchen_wetter_monat_hist <- neunkirchen_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
neunkirchen_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(neunkirchen_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
neunkirchen_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(neunkirchen_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
neunkirchen_wetter_monat_hist$Meldejahr <- lubridate::year(neunkirchen_wetter_monat_hist$MESS_DATUM_BEGINN)
neunkirchen_wetter_monat_hist$Monat <- lubridate::month(neunkirchen_wetter_monat_hist$MESS_DATUM_BEGINN)
neunkirchen_wetter_monat_hist <- neunkirchen_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

#1 
neunkirchen_wetter_monat_hist <- neunkirchen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  group_by(Meldejahr) %>% 
  filter(Monat %in% c(12, 1, 2)) %>%  
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(neunkirchen_wetter_monat_hist, by = "Meldejahr") 
neunkirchen_wetter_monat_hist <- neunkirchen_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_neunkirchen <- neunkirchen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))  

#3 
regen_avg_sommer_neunkirchen <- neunkirchen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>% 
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  

#4 
temperatur_avg_sommer_neunkirchen <- neunkirchen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))  



# RIGHT JOIN
neunkirchen_wetter_monat_hist <- neunkirchen_wetter_monat_hist %>%
  right_join(regen_avg_neunkirchen, by = "Meldejahr")
neunkirchen_wetter_monat_hist <- neunkirchen_wetter_monat_hist %>%
  right_join(regen_avg_sommer_neunkirchen, by = "Meldejahr")
neunkirchen_wetter_monat_hist <- neunkirchen_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_neunkirchen, by = "Meldejahr")



#MERZIG
#Einlesen monatlicher DWD-Daten
merzig_wetter_monat_hist <- read.csv("merzig_monat_hist.txt", header = TRUE, sep = ";")
merzig_wetter_monat_hist <- merzig_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
merzig_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(merzig_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
merzig_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(merzig_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
merzig_wetter_monat_hist$Meldejahr <- lubridate::year(merzig_wetter_monat_hist$MESS_DATUM_BEGINN)
merzig_wetter_monat_hist$Monat <- lubridate::month(merzig_wetter_monat_hist$MESS_DATUM_BEGINN)
merzig_wetter_monat_hist <- merzig_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

#1 
merzig_wetter_monat_hist <- merzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  group_by(Meldejahr) %>%  
  filter(Monat %in% c(12, 1, 2)) %>%  
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(merzig_wetter_monat_hist, by = "Meldejahr") 
merzig_wetter_monat_hist <- merzig_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_merzig <- merzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))  

#3 
regen_avg_sommer_merzig <- merzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  

#4 
temperatur_avg_sommer_merzig <- merzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))  



#RIGHT JOIN
merzig_wetter_monat_hist <- merzig_wetter_monat_hist %>%
  right_join(regen_avg_merzig, by = "Meldejahr")
merzig_wetter_monat_hist <- merzig_wetter_monat_hist %>%
  right_join(regen_avg_sommer_merzig, by = "Meldejahr")
merzig_wetter_monat_hist <- merzig_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_merzig, by = "Meldejahr")




#SAARLOUIS

#Einlesen monatlicher DWD-Daten
saarlouis_wetter_monat_hist <- read.csv("saarlouis_monat_hist.txt", header = TRUE, sep = ";")
saarlouis_wetter_monat_hist <- saarlouis_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
saarlouis_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(saarlouis_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
saarlouis_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(saarlouis_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
saarlouis_wetter_monat_hist$Meldejahr <- lubridate::year(saarlouis_wetter_monat_hist$MESS_DATUM_BEGINN)
saarlouis_wetter_monat_hist$Monat <- lubridate::month(saarlouis_wetter_monat_hist$MESS_DATUM_BEGINN)
saarlouis_wetter_monat_hist <- saarlouis_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

#1 
saarlouis_wetter_monat_hist <- saarlouis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  group_by(Meldejahr) %>%  
  filter(Monat %in% c(12, 1, 2)) %>%  
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(saarlouis_wetter_monat_hist, by = "Meldejahr") 
saarlouis_wetter_monat_hist <- saarlouis_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_saarlouis <- saarlouis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))  

#3 
regen_avg_sommer_saarlouis <- saarlouis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  

#4 
temperatur_avg_sommer_saarlouis <- saarlouis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))  

# RIGHT JOIN
saarlouis_wetter_monat_hist <- saarlouis_wetter_monat_hist %>%
  right_join(regen_avg_saarlouis, by = "Meldejahr")
saarlouis_wetter_monat_hist <- saarlouis_wetter_monat_hist %>%
  right_join(regen_avg_sommer_saarlouis, by = "Meldejahr")
saarlouis_wetter_monat_hist <- saarlouis_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_saarlouis, by = "Meldejahr")





#WENDEL

#Einlesen monatlicher DWD-Daten
wendel_wetter_monat_hist <- read.csv("wendel_monat_hist.txt", header = TRUE, sep = ";")
wendel_wetter_monat_hist <- wendel_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
wendel_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(wendel_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
wendel_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(wendel_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
wendel_wetter_monat_hist$Meldejahr <- lubridate::year(wendel_wetter_monat_hist$MESS_DATUM_BEGINN)
wendel_wetter_monat_hist$Monat <- lubridate::month(wendel_wetter_monat_hist$MESS_DATUM_BEGINN)
wendel_wetter_monat_hist <- wendel_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

#1 
wendel_wetter_monat_hist <- wendel_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  group_by(Meldejahr) %>%  
  filter(Monat %in% c(12, 1, 2)) %>%  
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(wendel_wetter_monat_hist, by = "Meldejahr") 
wendel_wetter_monat_hist <- wendel_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_wendel <- wendel_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))  

#3 
regen_avg_sommer_wendel <- wendel_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  

#4 
temperatur_avg_sommer_wendel <- wendel_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))  

# RIGHT JOIN
wendel_wetter_monat_hist <- wendel_wetter_monat_hist %>%
  right_join(regen_avg_wendel, by = "Meldejahr")
wendel_wetter_monat_hist <- wendel_wetter_monat_hist %>%
  right_join(regen_avg_sommer_wendel, by = "Meldejahr")
wendel_wetter_monat_hist <- wendel_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_wendel, by = "Meldejahr")






## SACHSEN ##


#CHEMNITZ

# Einlesen monatlicher DWD-Daten
chemnitz_wetter_monat_hist <- read.csv("chemnitz_monat_hist.txt", header = TRUE, sep = ";")
chemnitz_wetter_monat_hist <- chemnitz_wetter_monat_hist %>% mutate_all(replace_999)

# Formatierung
chemnitz_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(chemnitz_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
chemnitz_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(chemnitz_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
chemnitz_wetter_monat_hist$Meldejahr <- lubridate::year(chemnitz_wetter_monat_hist$MESS_DATUM_BEGINN)
chemnitz_wetter_monat_hist$Monat <- lubridate::month(chemnitz_wetter_monat_hist$MESS_DATUM_BEGINN)
chemnitz_wetter_monat_hist <- chemnitz_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

# 1
chemnitz_wetter_monat_hist <- chemnitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(chemnitz_wetter_monat_hist, by = "Meldejahr")
chemnitz_wetter_monat_hist <- chemnitz_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

# 2
regen_avg_chemnitz <- chemnitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

# 3
regen_avg_sommer_chemnitz <- chemnitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

# 4
temperatur_avg_sommer_chemnitz <- chemnitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))


# RIGHT JOIN
chemnitz_wetter_monat_hist <- chemnitz_wetter_monat_hist %>%
  right_join(regen_avg_chemnitz, by = "Meldejahr")
chemnitz_wetter_monat_hist <- chemnitz_wetter_monat_hist %>%
  right_join(regen_avg_sommer_chemnitz, by = "Meldejahr")
chemnitz_wetter_monat_hist <- chemnitz_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_chemnitz, by = "Meldejahr")




#ERZGEBIRGSKREIS

# Einlesen monatlicher DWD-Daten
erzgebirgskreis_wetter_monat_hist <- read.csv("erzgebirgskreis_monat_hist.txt", header = TRUE, sep = ";")
erzgebirgskreis_wetter_monat_hist <- erzgebirgskreis_wetter_monat_hist %>% mutate_all(replace_999)

# Formatierung
erzgebirgskreis_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(erzgebirgskreis_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
erzgebirgskreis_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(erzgebirgskreis_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
erzgebirgskreis_wetter_monat_hist$Meldejahr <- lubridate::year(erzgebirgskreis_wetter_monat_hist$MESS_DATUM_BEGINN)
erzgebirgskreis_wetter_monat_hist$Monat <- lubridate::month(erzgebirgskreis_wetter_monat_hist$MESS_DATUM_BEGINN)
erzgebirgskreis_wetter_monat_hist <- erzgebirgskreis_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

# 1
erzgebirgskreis_wetter_monat_hist <- erzgebirgskreis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(erzgebirgskreis_wetter_monat_hist, by = "Meldejahr")
erzgebirgskreis_wetter_monat_hist <- erzgebirgskreis_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

# 2
regen_avg_erzgebirgskreis <- erzgebirgskreis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

# 3
regen_avg_sommer_erzgebirgskreis <- erzgebirgskreis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

# 4
temperatur_avg_sommer_erzgebirgskreis <- erzgebirgskreis_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))

# RIGHT JOIN
erzgebirgskreis_wetter_monat_hist <- erzgebirgskreis_wetter_monat_hist %>%
  right_join(regen_avg_erzgebirgskreis, by = "Meldejahr")
erzgebirgskreis_wetter_monat_hist <- erzgebirgskreis_wetter_monat_hist %>%
  right_join(regen_avg_sommer_erzgebirgskreis, by = "Meldejahr")
erzgebirgskreis_wetter_monat_hist <- erzgebirgskreis_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_erzgebirgskreis, by = "Meldejahr")




#MITTELSACHSEN
# Einlesen monatlicher DWD-Daten

mittelsachsen_wetter_monat_hist <- read.csv("mittelsachsen_monat_hist.txt", header = TRUE, sep = ";")
mittelsachsen_wetter_monat_hist <- mittelsachsen_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
mittelsachsen_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(mittelsachsen_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
mittelsachsen_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(mittelsachsen_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
mittelsachsen_wetter_monat_hist$Meldejahr <- lubridate::year(mittelsachsen_wetter_monat_hist$MESS_DATUM_BEGINN)
mittelsachsen_wetter_monat_hist$Monat <- lubridate::month(mittelsachsen_wetter_monat_hist$MESS_DATUM_BEGINN)
mittelsachsen_wetter_monat_hist <- mittelsachsen_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1 
mittelsachsen_wetter_monat_hist <- mittelsachsen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>% 
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>% 
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(mittelsachsen_wetter_monat_hist, by = "Meldejahr")
mittelsachsen_wetter_monat_hist <- mittelsachsen_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))


#2 
regen_avg_mittelsachsen <- mittelsachsen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>% 
  group_by(Meldejahr) %>% 
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))  

#3 
regen_avg_sommer_mittelsachsen <- mittelsachsen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>% 
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  


#4 
temperatur_avg_sommer_mittelsachsen <- mittelsachsen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE)) 



# RIGHT JOIN
mittelsachsen_wetter_monat_hist <- mittelsachsen_wetter_monat_hist %>%
  right_join(regen_avg_mittelsachsen, by = "Meldejahr")
mittelsachsen_wetter_monat_hist <- mittelsachsen_wetter_monat_hist %>%
  right_join(regen_avg_sommer_mittelsachsen, by = "Meldejahr")
mittelsachsen_wetter_monat_hist <- mittelsachsen_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_mittelsachsen, by = "Meldejahr")




#VOGTLANDKREIS
# Einlesen monatlicher DWD-Daten
vogtland_wetter_monat_hist <- read.csv("vogtland_monat_hist.txt", header = TRUE, sep = ";")
vogtland_wetter_monat_hist <- vogtland_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
vogtland_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(vogtland_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
vogtland_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(vogtland_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
vogtland_wetter_monat_hist$Meldejahr <- lubridate::year(vogtland_wetter_monat_hist$MESS_DATUM_BEGINN)
vogtland_wetter_monat_hist$Monat <- lubridate::month(vogtland_wetter_monat_hist$MESS_DATUM_BEGINN)
vogtland_wetter_monat_hist <- vogtland_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1 
vogtland_wetter_monat_hist <- vogtland_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  group_by(Meldejahr) %>% 
  filter(Monat %in% c(12, 1, 2)) %>% 
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(vogtland_wetter_monat_hist, by = "Meldejahr")
vogtland_wetter_monat_hist <- vogtland_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_vogtland <- vogtland_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>% 
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE)) 

#3 
regen_avg_sommer_vogtland <- vogtland_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>% 
  filter(Monat %in% c(5, 6, 7)) %>% 
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  

#4 
temperatur_avg_sommer_vogtland <- vogtland_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>% 
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE)) 



#RIGHT JOIN
vogtland_wetter_monat_hist <- vogtland_wetter_monat_hist %>%
  right_join(regen_avg_vogtland, by = "Meldejahr")
vogtland_wetter_monat_hist <- vogtland_wetter_monat_hist %>%
  right_join(regen_avg_sommer_vogtland, by = "Meldejahr")
vogtland_wetter_monat_hist <- vogtland_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_vogtland, by = "Meldejahr")




# ZWICKAU

# Einlesen monatlicher DWD-Daten
zwickau_wetter_monat_hist <- read.csv("zwickau_monat_hist.txt", header = TRUE, sep = ";")
zwickau_wetter_monat_hist <- zwickau_wetter_monat_hist %>% mutate_all(replace_999)

# Formatierung
zwickau_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(zwickau_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
zwickau_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(zwickau_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
zwickau_wetter_monat_hist$Meldejahr <- lubridate::year(zwickau_wetter_monat_hist$MESS_DATUM_BEGINN)
zwickau_wetter_monat_hist$Monat <- lubridate::month(zwickau_wetter_monat_hist$MESS_DATUM_BEGINN)
zwickau_wetter_monat_hist <- zwickau_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1
zwickau_wetter_monat_hist <- zwickau_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(zwickau_wetter_monat_hist, by = "Meldejahr")
zwickau_wetter_monat_hist <- zwickau_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2
regen_avg_zwickau <- zwickau_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

#3
regen_avg_sommer_zwickau <- zwickau_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

#4
temperatur_avg_sommer_zwickau <- zwickau_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))


# RIGHT JOIN
zwickau_wetter_monat_hist <- zwickau_wetter_monat_hist %>%
  right_join(regen_avg_zwickau, by = "Meldejahr")
zwickau_wetter_monat_hist <- zwickau_wetter_monat_hist %>%
  right_join(regen_avg_sommer_zwickau, by = "Meldejahr")
zwickau_wetter_monat_hist <- zwickau_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_zwickau, by = "Meldejahr")



#DRESDEN

#Einlesen monatlicher DWD_Daten
dresden_wetter_monat_hist <- read.csv("dresden_monat_hist.txt", header = TRUE, sep = ";")
dresden_wetter_monat_hist <- dresden_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
dresden_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(dresden_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
dresden_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(dresden_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
dresden_wetter_monat_hist$Meldejahr <- lubridate::year(dresden_wetter_monat_hist$MESS_DATUM_BEGINN)
dresden_wetter_monat_hist$Monat <- lubridate::month(dresden_wetter_monat_hist$MESS_DATUM_BEGINN)
dresden_wetter_monat_hist <- dresden_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1 
dresden_wetter_monat_hist <- dresden_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  group_by(Meldejahr) %>%  
  filter(Monat %in% c(12, 1, 2)) %>%  
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(dresden_wetter_monat_hist, by = "Meldejahr")
dresden_wetter_monat_hist <- dresden_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_dresden <- dresden_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))  

#3
regen_avg_sommer_dresden <- dresden_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>% 
  filter(Monat %in% c(5, 6, 7)) %>% 
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  


#4 
temperatur_avg_sommer_dresden <- dresden_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))  


#RIGHT JOIN
dresden_wetter_monat_hist <- dresden_wetter_monat_hist %>%
  right_join(regen_avg_dresden, by = "Meldejahr")
dresden_wetter_monat_hist <- dresden_wetter_monat_hist %>%
  right_join(regen_avg_sommer_dresden, by = "Meldejahr")
dresden_wetter_monat_hist <- dresden_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_dresden, by = "Meldejahr")




#BAUTZEN 
#Einlesen monatlicher DWD_Daten
bautzen_wetter_monat_hist <- read.csv("bautzen_monat_hist.txt", header = TRUE, sep = ";")
bautzen_wetter_monat_hist <- bautzen_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
bautzen_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(bautzen_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
bautzen_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(bautzen_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
bautzen_wetter_monat_hist$Meldejahr <- lubridate::year(bautzen_wetter_monat_hist$MESS_DATUM_BEGINN)
bautzen_wetter_monat_hist$Monat <- lubridate::month(bautzen_wetter_monat_hist$MESS_DATUM_BEGINN)
bautzen_wetter_monat_hist <- bautzen_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1 
bautzen_wetter_monat_hist <- bautzen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>% 
  group_by(Meldejahr) %>%  
  filter(Monat %in% c(12, 1, 2)) %>%  
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%  
  right_join(bautzen_wetter_monat_hist, by = "Meldejahr")
bautzen_wetter_monat_hist <- bautzen_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_bautzen <- bautzen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE)) 

#3 
regen_avg_sommer_bautzen <- bautzen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>% 
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))  

#4 
temperatur_avg_sommer_bautzen <- bautzen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%  
  filter(Monat %in% c(5, 6, 7)) %>%  
  group_by(Meldejahr) %>%  
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))  



# Füge die berechneten Mittelwerte zurück zu den ursprünglichen Daten hinzu
bautzen_wetter_monat_hist <- bautzen_wetter_monat_hist %>%
  right_join(regen_avg_bautzen, by = "Meldejahr")
bautzen_wetter_monat_hist <- bautzen_wetter_monat_hist %>%
  right_join(regen_avg_sommer_bautzen, by = "Meldejahr")
bautzen_wetter_monat_hist <- bautzen_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_bautzen, by = "Meldejahr")




#GÖRLITZ 
#Einlesen monatlicher DWD-Daten

görlitz_wetter_monat_hist <- read.csv("görlitz_monat_hist.txt", header = TRUE, sep = ";")
görlitz_wetter_monat_hist <- görlitz_wetter_monat_hist %>% mutate_all(replace_999)
#Formatierung
görlitz_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(görlitz_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
görlitz_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(görlitz_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
görlitz_wetter_monat_hist$Meldejahr <- lubridate::year(görlitz_wetter_monat_hist$MESS_DATUM_BEGINN)
görlitz_wetter_monat_hist$Monat <- lubridate::month(görlitz_wetter_monat_hist$MESS_DATUM_BEGINN)
görlitz_wetter_monat_hist <- görlitz_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)

#1
görlitz_wetter_monat_hist <- görlitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(görlitz_wetter_monat_hist, by = "Meldejahr")

görlitz_wetter_monat_hist <- görlitz_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))


#2
regen_avg_görlitz <- görlitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

#3
regen_avg_sommer_görlitz <- görlitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

#4
temperatur_avg_sommer_görlitz <- görlitz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))



#RIGHT JOIN
görlitz_wetter_monat_hist <- görlitz_wetter_monat_hist %>%
  right_join(regen_avg_görlitz, by = "Meldejahr")
görlitz_wetter_monat_hist <- görlitz_wetter_monat_hist %>%
  right_join(regen_avg_sommer_görlitz, by = "Meldejahr")
görlitz_wetter_monat_hist <- görlitz_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_görlitz, by = "Meldejahr")




#MEIßEN 
#Einlesen monatlicher DWD-Daten
meißen_wetter_monat_hist <- read.csv("meißen_monat_hist.txt", header = TRUE, sep = ";")
meißen_wetter_monat_hist <- meißen_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
meißen_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(meißen_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
meißen_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(meißen_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
meißen_wetter_monat_hist$Meldejahr <- lubridate::year(meißen_wetter_monat_hist$MESS_DATUM_BEGINN)
meißen_wetter_monat_hist$Monat <- lubridate::month(meißen_wetter_monat_hist$MESS_DATUM_BEGINN)
meißen_wetter_monat_hist <- meißen_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1
meißen_wetter_monat_hist <- meißen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(meißen_wetter_monat_hist, by = "Meldejahr")
meißen_wetter_monat_hist <- meißen_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2
regen_avg_meißen <- meißen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

#3
regen_avg_sommer_meißen <- meißen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

#4
temperatur_avg_sommer_meißen <- meißen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))





# RIGHT JOIN
meißen_wetter_monat_hist <- meißen_wetter_monat_hist %>%
  right_join(regen_avg_meißen, by = "Meldejahr")
meißen_wetter_monat_hist <- meißen_wetter_monat_hist %>%
  right_join(regen_avg_sommer_meißen, by = "Meldejahr")
meißen_wetter_monat_hist <- meißen_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_meißen, by = "Meldejahr")




#SÄCHSISCHE SCHWEIZ
#Einlesen monatlicher DWD-Daten
sächsischeschweiz_wetter_monat_hist <- read.csv("sächsischeschweiz_monat_hist.txt", header = TRUE, sep = ";")
sächsischeschweiz_wetter_monat_hist <- sächsischeschweiz_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
sächsischeschweiz_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(sächsischeschweiz_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
sächsischeschweiz_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(sächsischeschweiz_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
sächsischeschweiz_wetter_monat_hist$Meldejahr <- lubridate::year(sächsischeschweiz_wetter_monat_hist$MESS_DATUM_BEGINN)
sächsischeschweiz_wetter_monat_hist$Monat <- lubridate::month(sächsischeschweiz_wetter_monat_hist$MESS_DATUM_BEGINN)
sächsischeschweiz_wetter_monat_hist <- sächsischeschweiz_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1
sächsischeschweiz_wetter_monat_hist <- sächsischeschweiz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(sächsischeschweiz_wetter_monat_hist, by = "Meldejahr")

sächsischeschweiz_wetter_monat_hist <- sächsischeschweiz_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2
regen_avg_sächsischeschweiz <- sächsischeschweiz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

#3
regen_avg_sommer_sächsischeschweiz <- sächsischeschweiz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

#4
temperatur_avg_sommer_sächsischeschweiz <- sächsischeschweiz_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))


# RIGHT JOIN
sächsischeschweiz_wetter_monat_hist <- sächsischeschweiz_wetter_monat_hist %>%
  right_join(regen_avg_sächsischeschweiz, by = "Meldejahr")
sächsischeschweiz_wetter_monat_hist <- sächsischeschweiz_wetter_monat_hist %>%
  right_join(regen_avg_sommer_sächsischeschweiz, by = "Meldejahr")
sächsischeschweiz_wetter_monat_hist <- sächsischeschweiz_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_sächsischeschweiz, by = "Meldejahr")




#LEIPZIG
#Einlesen monatlicher DWD-Daten
leipzig_wetter_monat_hist <- read.csv("leipzig_monat_hist.txt", header = TRUE, sep = ";")
leipzig_wetter_monat_hist <- leipzig_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
leipzig_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(leipzig_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
leipzig_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(leipzig_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
leipzig_wetter_monat_hist$Meldejahr <- lubridate::year(leipzig_wetter_monat_hist$MESS_DATUM_BEGINN)
leipzig_wetter_monat_hist$Monat <- lubridate::month(leipzig_wetter_monat_hist$MESS_DATUM_BEGINN)
leipzig_wetter_monat_hist <- leipzig_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1 
leipzig_wetter_monat_hist <- leipzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(leipzig_wetter_monat_hist, by = "Meldejahr")
leipzig_wetter_monat_hist <- leipzig_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_leipzig <- leipzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

#3
regen_avg_sommer_leipzig <- leipzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

#4 
temperatur_avg_sommer_leipzig <- leipzig_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))


# RIGHT JOIN
leipzig_wetter_monat_hist <- leipzig_wetter_monat_hist %>%
  right_join(regen_avg_leipzig, by = "Meldejahr")
leipzig_wetter_monat_hist <- leipzig_wetter_monat_hist %>%
  right_join(regen_avg_sommer_leipzig, by = "Meldejahr")
leipzig_wetter_monat_hist <- leipzig_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_leipzig, by = "Meldejahr")



#NORDSACHSEN
#Einlesen monatlicher DWD-Daten
nordsachsen_wetter_monat_hist <- read.csv("nordsachsen_monat_hist.txt", header = TRUE, sep = ";")
nordsachsen_wetter_monat_hist <- nordsachsen_wetter_monat_hist %>% mutate_all(replace_999)

#Formatierung
nordsachsen_wetter_monat_hist$MESS_DATUM_BEGINN <- as.Date(as.character(nordsachsen_wetter_monat_hist$MESS_DATUM_BEGINN), format = "%Y%m%d")
nordsachsen_wetter_monat_hist$MESS_DATUM_ENDE <- as.Date(as.character(nordsachsen_wetter_monat_hist$MESS_DATUM_ENDE), format = "%Y%m%d")
nordsachsen_wetter_monat_hist$Meldejahr <- lubridate::year(nordsachsen_wetter_monat_hist$MESS_DATUM_BEGINN)
nordsachsen_wetter_monat_hist$Monat <- lubridate::month(nordsachsen_wetter_monat_hist$MESS_DATUM_BEGINN)
nordsachsen_wetter_monat_hist <- nordsachsen_wetter_monat_hist %>%
  filter(Meldejahr >= 2010)


#1 
nordsachsen_wetter_monat_hist <- nordsachsen_wetter_monat_hist %>%
mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  group_by(Meldejahr) %>%
  filter(Monat %in% c(12, 1, 2)) %>%
  summarise(Durchschnitt_MO_TT = mean(MO_TT, na.rm = TRUE)) %>%
  right_join(nordsachsen_wetter_monat_hist, by = "Meldejahr")
nordsachsen_wetter_monat_hist <- nordsachsen_wetter_monat_hist %>%
  mutate(Vorjahres_Durchschnitt_Winter_TT = lag(Durchschnitt_MO_TT, default = NA))

#2 
regen_avg_nordsachsen <- nordsachsen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_RR = mean(MO_RR, na.rm = TRUE))

#3 
regen_avg_sommer_nordsachsen <- nordsachsen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Durchschnitt_Mai_Juni_Juli_RR = mean(MO_RR, na.rm = TRUE))

#4 
temperatur_avg_sommer_nordsachsen <- nordsachsen_wetter_monat_hist %>%
  mutate(Monat = as.integer(format(MESS_DATUM_BEGINN, "%m"))) %>%
  filter(Monat %in% c(5, 6, 7)) %>%
  group_by(Meldejahr) %>%
  summarise(Temperatur_Durchschnitt_Mai_Juni_Juli_TT = mean(MO_TT, na.rm = TRUE))

#RIGHT JOIN
nordsachsen_wetter_monat_hist <- nordsachsen_wetter_monat_hist %>%
  right_join(regen_avg_nordsachsen, by = "Meldejahr")
nordsachsen_wetter_monat_hist <- nordsachsen_wetter_monat_hist %>%
  right_join(regen_avg_sommer_nordsachsen, by = "Meldejahr")
nordsachsen_wetter_monat_hist <- nordsachsen_wetter_monat_hist %>%
  right_join(temperatur_avg_sommer_nordsachsen, by = "Meldejahr")




###########################################################################################################
### KREISE ZU BUNDESLAND ZUSAMMENFÜGEN ###

### UNIQUE YEARS##

## SAARLAND ##
#stadtverband_saarpfalz
stadtverband_saarpfalz_wetter_monat_hist_unique <- stadtverband_saarpfalz_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
#neunkirchen
neunkirchen_wetter_monat_hist_unique <- neunkirchen_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
#merzig
merzig_wetter_monat_hist_unique <- merzig_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
#saarlouis
saarlouis_wetter_monat_hist_unique <- saarlouis_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
#wendel
wendel_wetter_monat_hist_unique <- wendel_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)



## SACHSEN ##
chemnitz_wetter_monat_hist_unique <- chemnitz_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
erzgebirgskreis_wetter_monat_hist_unique <- erzgebirgskreis_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
mittelsachsen_wetter_monat_hist_unique <- mittelsachsen_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
vogtland_wetter_monat_hist_unique <- vogtland_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
zwickau_wetter_monat_hist_unique <- zwickau_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
dresden_wetter_monat_hist_unique <- dresden_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
bautzen_wetter_monat_hist_unique <- bautzen_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
görlitz_wetter_monat_hist_unique <- görlitz_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
meißen_wetter_monat_hist_unique <- meißen_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
sächsischeschweiz_wetter_monat_hist_unique <- sächsischeschweiz_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
leipzig_wetter_monat_hist_unique <- leipzig_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)
nordsachsen_wetter_monat_hist_unique <- nordsachsen_wetter_monat_hist %>%
  distinct(Meldejahr, STATIONS_ID, .keep_all = TRUE)



### JOIN ###


## #SAARLAND ##

#neunkirchen

neunkirchen_wetter_monat_hist_unique <- neunkirchen_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_neunkirchen <- join_neunkirchen %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_neunkirchen <- join_neunkirchen %>%
  left_join(neunkirchen_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#saarland verbund, saarpfalz

stadtverband_saarpfalz_wetter_monat_hist_unique <- stadtverband_saarpfalz_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_verbandsaar <- join_verbandsaar %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_saarpfalz <- join_saarpfalz %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))


join_verbandsaar <- join_verbandsaar %>%
  left_join(stadtverband_saarpfalz_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

join_saarpfalz <- join_saarpfalz %>%
  left_join(stadtverband_saarpfalz_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#merzig
merzig_wetter_monat_hist_unique <- merzig_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_merzig <- join_merzig %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_merzig<- join_merzig %>%
  left_join(merzig_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#saarlouis
saarlouis_wetter_monat_hist_unique <- saarlouis_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_saarlouis <- join_saarlouis %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_saarlouis<- join_saarlouis %>%
  left_join(saarlouis_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#wendel
wendel_wetter_monat_hist_unique <- wendel_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_wendel <- join_wendel %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_wendel<- join_wendel %>%
  left_join(wendel_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#SAARLAND BUNDESLAND
join_saarland <- bind_rows(join_merzig, join_neunkirchen, join_saarlouis, join_saarpfalz, join_wendel, join_verbandsaar)
join_saarland <- join_saarland %>% filter(Meldejahr >= 2010)




## SACHSEN ##

#chemnitz
chemnitz_wetter_monat_hist_unique <- chemnitz_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_chemnitz <- join_chemnitz %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_chemnitz<- join_chemnitz %>%
  left_join(chemnitz_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))


#erzebirgskreis
erzgebirgskreis_wetter_monat_hist_unique <- erzgebirgskreis_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_erzgebirgskreis <- join_erzgebirgskreis %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_erzgebirgskreis<- join_erzgebirgskreis %>%
  left_join(erzgebirgskreis_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#mittelsachsen
mittelsachsen_wetter_monat_hist_unique <- mittelsachsen_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_mittelsachsen <- join_mittelsachsen %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_mittelsachsen<- join_mittelsachsen %>%
  left_join(mittelsachsen_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#votgland
vogtland_wetter_monat_hist_unique <- vogtland_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_vogtland <- join_vogtland %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_vogtland<- join_vogtland %>%
  left_join(vogtland_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))


#zwickau
zwickau_wetter_monat_hist_unique <- zwickau_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_zwickau <- join_zwickau %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_zwickau<- join_zwickau %>%
  left_join(zwickau_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#dresden
dresden_wetter_monat_hist_unique <- dresden_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_dresden <- join_dresden %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_dresden<- join_dresden %>%
  left_join(dresden_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))


#bautzen
bautzen_wetter_monat_hist_unique <- bautzen_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_bautzen <- join_bautzen %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_bautzen<- join_bautzen %>%
  left_join(bautzen_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))


#görlitz
görlitz_wetter_monat_hist_unique <- görlitz_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_görlitz <- join_görlitz %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_görlitz<- join_görlitz %>%
  left_join(görlitz_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))

#meißen
meißen_wetter_monat_hist_unique <- meißen_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_meißen <- join_meißen %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_meißen<- join_meißen %>%
  left_join(meißen_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))


#sächsischeschweiz

sächsischeschweiz_wetter_monat_hist_unique <- sächsischeschweiz_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_sächsischeschweiz <- join_sächsischeschweiz %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_sächsischeschweiz<- join_sächsischeschweiz %>%
  left_join(sächsischeschweiz_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))


#leipzig
leipzig_wetter_monat_hist_unique <- leipzig_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_leipzig <- join_leipzig %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_leipzig<- join_leipzig %>%
  left_join(leipzig_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))


#nordsachsen
nordsachsen_wetter_monat_hist_unique <- nordsachsen_wetter_monat_hist_unique %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))
join_nordsachsen <- join_nordsachsen %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID))

join_nordsachsen<- join_nordsachsen %>%
  left_join(nordsachsen_wetter_monat_hist_unique, by = c("STATIONS_ID", "Meldejahr"))



#SACHSEN BUNDESLAND
join_sachsen <- bind_rows(join_chemnitz, join_erzgebirgskreis, join_mittelsachsen, join_vogtland, join_zwickau, join_dresden, join_bautzen, join_görlitz, join_meißen, join_sächsischeschweiz, join_leipzig, join_nordsachsen)
join_sachsen <- join_sachsen %>% filter(Meldejahr >= 2010)




###########################################################################################################
### DATACLEANING ###

## SAARLAND ## 
names(join_saarland)[names(join_saarland) == "Insgesamt"] <- "Bevölkerung_Insg"
names(join_saarland)[names(join_saarland) == "Anzahl"] <- "Inzidenz"
names(join_saarland)[names(join_saarland) == "Wert"] <- "Bevölkerung"
names(join_saarland)[names(join_saarland) == "Wert"] <- "Bevölkerung_Geschlecht"
names(join_saarland)[names(join_saarland) == "je km2"] <- "Bevölkerung_jekm2"
names(join_saarland)[names(join_saarland) == "Durchschnitt_MO_TT"] <- "Wintertemperatur"
names(join_saarland)[names(join_saarland) == "Vorjahres_Durchschnitt_Winter_TT"] <- "Wintertemperatur_Vorjahr"
names(join_saarland)[names(join_saarland) == "Durchschnitt_Mai_Juni_RR"] <- "Frühlingsniederschlag"
names(join_saarland)[names(join_saarland) == "Durchschnitt_Mai_Juni_Juli_RR"] <- "Sommerniederschlag"
names(join_saarland)[names(join_saarland) == "Temperatur_Durchschnitt_Mai_Juni_Juli_TT"] <- "Sommertemperatur"
names(join_saarland)[names(join_saarland) == "JA_TT"] <- "Jahrestemperatur"
names(join_saarland)[names(join_saarland) == "JA_RR"] <- "Jahresniederschlag"
names(join_saarland)[names(join_saarland) == "JA_SD_S"] <- "Sonnenschein"


join_saarland$Inzidenz <- as.numeric(gsub(",", ".", join_saarland$Inzidenz))

## SACHSEN #
names(join_sachsen)[names(join_sachsen) == "Insgesamt"] <- "Bevölkerung_Insg"
names(join_sachsen)[names(join_sachsen) == "Anzahl"] <- "Inzidenz"
names(join_sachsen)[names(join_sachsen) == "Wert"] <- "Bevölkerung"
names(join_sachsen)[names(join_sachsen) == "Wert"] <- "Bevölkerung_Geschlecht"
names(join_sachsen)[names(join_sachsen) == "je km2"] <- "Bevölkerung_jekm2"
names(join_sachsen)[names(join_sachsen) == "Durchschnitt_MO_TT"] <- "Wintertemperatur"
names(join_sachsen)[names(join_sachsen) == "Vorjahres_Durchschnitt_Winter_TT"] <- "Wintertemperatur_Vorjahr"
names(join_sachsen)[names(join_sachsen) == "Durchschnitt_Mai_Juni_RR"] <- "Frühlingsniederschlag"
names(join_sachsen)[names(join_sachsen) == "Durchschnitt_Mai_Juni_Juli_RR"] <- "Sommerniederschlag"
names(join_sachsen)[names(join_sachsen) == "Temperatur_Durchschnitt_Mai_Juni_Juli_TT"] <- "Sommertemperatur"
names(join_sachsen)[names(join_sachsen) == "JA_TT"] <- "Jahrestemperatur"
names(join_sachsen)[names(join_sachsen) == "JA_RR"] <- "Jahresniederschlag"
names(join_sachsen)[names(join_sachsen) == "JA_SD_S"] <- "Sonnenschein"

join_sachsen$Inzidenz <- as.numeric(gsub(",", ".", join_sachsen$Inzidenz))




###########################################################################################################
### BUNDESLÄNDER ZUSAMMENFÜGEN ###
final <- bind_rows(join_sachsen, join_saarland)

###########################################################################################################
### DATACLEANING ###

#unbenötigte Variablen entfernen
final <- final[, !names(final) %in% c("MESS_DATUM_BEGINN.y", "MESS_DATUM_ENDE.y",  
                                      "MESS_DATUM_BEGINN.x", "MESS_DATUM_ENDE.x",
                                      "eor.x", "eor.y",
                                      "QN_4.x", "QN_4.y", "QN_6.x", "QN_6.y",
                                      "JA_N", "JA_FK", "JA_MX_FX", "MO_N", "MO_TT",
                                      "MO_TX","MO_TN","MO_FK","MO_FX","MO_TX","MO_SD_S","MO_RRS", "MO_RR", "Monat", 
                                      "MX_TX", "MX_FX", "MX_TN", "MX_RS", "JA_MX_RS", "JA_TN", 
                                      "JA_TX", "JA_MX_TX", "JA_MX_TN",
                                      "Fläche in km", "Bevölkerung_jekm2")]





#Missings
missing_summary <- final %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "missing_{col}"))
print(missing_summary)


vis_miss (final)



#Sonnenschein hat 224 Missings -> Entfernen der Variable
final <- final[, !names(final) %in% c( "Sonnenschein")]

#Gesamtinzidenz
final <- final %>%
  group_by(Meldejahr, LandkreisID) %>%
  mutate(Gesamt_Inzidenz = sum(Inzidenz)) %>%
  ungroup()


#Nochmals überprüfen auf -999
replace_999 <- function(x) {
  x[x == -999.00] <- NA
  return(x)
}

final <- final %>% mutate_all(replace_999)

#Entfernen von Missings
final_na <- na.omit(final)

#Runden der Inzidenz
final_na$Inzidenz <- round(final_na$Inzidenz)

#Logarithmieren der Inzidenz
final_na$Inzidenz_log <- log(final_na$Inzidenz + 1)


###########################################################################################################
### DESKRIPTIV ###

#Mean, Min, Max
summary_final <- final %>%
  group_by(Bundesland) %>%
  summarise(
    Mittelwert_Inzidenz = mean(Inzidenz, na.rm = TRUE),
    SD_Inzidenz = sd(Inzidenz, na.rm = TRUE),
    Minimum_Inzidenz = min(Inzidenz, na.rm = TRUE),
    Maximum_Inzidenz = max(Inzidenz, na.rm = TRUE),
    Missings_Inzidenz = sum(is.na(Inzidenz)),
    
    Mittelwert_Gesamt_Inzidenz = mean(Gesamt_Inzidenz, na.rm = TRUE),
    SD_Gesamt_Inzidenz = sd(Gesamt_Inzidenz, na.rm = TRUE),
    Minimum_Gesamt_Inzidenz = min(Gesamt_Inzidenz, na.rm = TRUE),
    Maximum_Gesamt_Inzidenz = max(Gesamt_Inzidenz, na.rm = TRUE),
    Missings_Gesamt_Inzidenz = sum(is.na(Gesamt_Inzidenz)),
    
    Mittelwert_Sonnenschein = mean(Sonnenschein, na.rm = TRUE),
    SD_Sonnenschein = sd(Sonnenschein, na.rm = TRUE),
    Minimum_Sonnenschein = min(Sonnenschein, na.rm = TRUE),
    Maximum_Sonnenschein = max(Sonnenschein, na.rm = TRUE),
    Missings_Sonnenschein = sum(is.na(Sonnenschein)),
    
    Mittelwert_Jahrestemperatur = mean(Jahrestemperatur, na.rm = TRUE),
    SD_Jahrestemperatur = sd(Jahrestemperatur, na.rm = TRUE),
    Minimum_Jahrestemperatur = min(Jahrestemperatur, na.rm = TRUE),
    Maximum_Jahrestemperatur = max(Jahrestemperatur, na.rm = TRUE),
    Missings_Jahrestemperatur = sum(is.na(Jahrestemperatur)),
    
    Mittelwert_Jahresniederschlag = mean(Jahresniederschlag, na.rm = TRUE),
    SD_Jahresniederschlag = sd(Jahresniederschlag, na.rm = TRUE),
    Minimum_Jahresniederschlag = min(Jahresniederschlag, na.rm = TRUE),
    Maximum_Jahresniederschlag = max(Jahresniederschlag, na.rm = TRUE),
    Missings_Jahresniederschlag = sum(is.na(Jahresniederschlag)),
    
    Mittelwert_Wintertemperatur = mean(Wintertemperatur, na.rm = TRUE),
    SD_Wintertemperatur = sd(Wintertemperatur, na.rm = TRUE),
    Minimum_Wintertemperatur = min(Wintertemperatur, na.rm = TRUE),
    Maximum_Wintertemperatur = max(Wintertemperatur, na.rm = TRUE),
    Missings_Wintertemperatur = sum(is.na(Wintertemperatur)),
    
    Mittelwert_WintertemperaturVorjahr = mean(Wintertemperatur_Vorjahr, na.rm = TRUE),
    SD_WintertemperaturVorjahr = sd(Wintertemperatur_Vorjahr, na.rm = TRUE),
    Minimum_WintertemperaturVorjahr = min(Wintertemperatur_Vorjahr, na.rm = TRUE),
    Maximum_WintertemperaturVorjahr = max(Wintertemperatur_Vorjahr, na.rm = TRUE),
    Missings_WintertemperaturVorjahr = sum(is.na(Wintertemperatur_Vorjahr)),
    
    Mittelwert_Sommertemperatur = mean(Sommertemperatur, na.rm = TRUE),
    SD_Sommertemperatur = sd(Sommertemperatur, na.rm = TRUE),
    Minimum_Sommertemperatur = min(Sommertemperatur, na.rm = TRUE),
    Maximum_Sommertemperatur = max(Sommertemperatur, na.rm = TRUE),
    Missings_Sommertemperatur = sum(is.na(Sommertemperatur)),
    
    Mittelwert_Sommerniederschlag = mean(Sommerniederschlag, na.rm = TRUE),
    SD_Sommerniederschlag = sd(Sommerniederschlag, na.rm = TRUE),
    Minimum_Sommerniederschlag = min(Sommerniederschlag, na.rm = TRUE),
    Maximum_Sommerniederschlag = max(Sommerniederschlag, na.rm = TRUE),
    Missings_Sommerniederschlag = sum(is.na(Sommerniederschlag)),
    
    Mittelwert_Frühlingsniederschlag = mean(Frühlingsniederschlag, na.rm = TRUE),
    SD_Frühlingsniederschlag = sd(Frühlingsniederschlag, na.rm = TRUE),
    Minimum_Frühlingsniederschlag = min(Frühlingsniederschlag, na.rm = TRUE),
    Maximum_Frühlingsniederschlag = max(Frühlingsniederschlag, na.rm = TRUE),
    Missings_Frühlingsniederschlag = sum(is.na(Frühlingsniederschlag)),
    
    Mittelwert_Bevölkerung = mean(Bevölkerung, na.rm = TRUE),
    SD_Bevölkerung = sd(Bevölkerung, na.rm = TRUE),
    Minimum_Bevölkerung = min(Bevölkerung, na.rm = TRUE),
    Maximum_Bevölkerung = max(Bevölkerung, na.rm = TRUE),
    Missings_Bevölkerung = sum(is.na(Bevölkerung)),
    
    Mittelwert_Bevölkerung_Insg = mean(Bevölkerung_Insg, na.rm = TRUE),
    SD_Bevölkerung_Insg = sd(Bevölkerung_Insg, na.rm = TRUE),
    Minimum_Bevölkerung_Insg = min(Bevölkerung_Insg, na.rm = TRUE),
    Maximum_Bevölkerung_Insg = max(Bevölkerung_Insg, na.rm = TRUE),
    Missings_Bevölkerung_Insg = sum(is.na(Bevölkerung_Insg)),
    
    Mittelwert_Bevölkerungsentwicklung = mean(Bevölkerungsentwicklung, na.rm = TRUE),
    SD_Bevölkerungsentwicklung = sd(Bevölkerungsentwicklung, na.rm = TRUE),
    Minimum_Bevölkerungsentwicklung = min(Bevölkerungsentwicklung, na.rm = TRUE),
    Maximum_Bevölkerungsentwicklung = max(Bevölkerungsentwicklung, na.rm = TRUE),
    Missings_Bevölkerungsenticklung = sum(is.na(Bevölkerungsentwicklung)),
    
  )



write_xlsx(summary_final, "summary_final.xlsx")


###########################################################################################################
### IMPUTATION  ###

#Meldejahre hinzufügen
final_na_extended <- final_na

years_to_add <- seq(from = max(final_na$Meldejahr) + 1, to = 2045)

new_rows <- list()

# Iterieren 
unique_combinations <- unique(final_na_extended %>% select(STATIONS_ID, Geschlecht, Kreis))

for (i in 1:nrow(unique_combinations)) {
  for (year in years_to_add) {
    new_row <- data.frame(
      Meldejahr = year,
      STATIONS_ID = unique_combinations$STATIONS_ID[i],
      Geschlecht = unique_combinations$Geschlecht[i],
      Kreis = unique_combinations$Kreis[i],
      Wintertemperatur = NA,
      SSP1_2_6_Wintertemperatur = ifelse(year == 2040, final_na_extended$Wintertemperatur[which(final_na_extended$Meldejahr == 2014)] + 1.0, NA),
      SSP3_7_Wintertemperatur = ifelse(year == 2040, final_na_extended$Wintertemperatur[which(final_na_extended$Meldejahr == 2014)] + 1.5, NA),
      Sommertemperatur = NA,
      SSP1_2_6_Sommertemperatur = ifelse(year == 2040, final_na_extended$Sommertemperatur[which(final_na_extended$Meldejahr == 2014)] + 1.5, NA),
      SSP3_7_Sommertemperatur = ifelse(year == 2040, final_na_extended$Sommertemperatur[which(final_na_extended$Meldejahr == 2014)] + 1.5, NA),
      Frühlingsniederschlag = NA,
      SSP1_2_6_Frühlingsniederschlag = NA,
      SSP3_7_Frühlingsniederschlag = NA,
      Sommerniederschlag = NA,
      SSP1_2_6_Sommerniederschlag = NA,
      SSP3_7_Sommerniederschlag = NA,
      SSP1_2_6_Jahrestemperatur = NA,
      SSP3_7_Jahrestemperatur = NA,
      SSP1_2_6_Jahresniederschlag = NA,
      SSP3_7_Jahresniederschlag = NA,
      Bevölkerung_imp = NA,
      Bevölkerung_Insg_imp = NA,
      stringsAsFactors = FALSE  
    )
    
    new_rows[[length(new_rows) + 1]] <- new_row
  }
}

new_rows_df <- bind_rows(new_rows)

final_na_extended <- bind_rows(final_na_extended, new_rows_df)


#Interpolieren IPCC Daten
final_na_extended <- final_na_extended %>%
  group_by(STATIONS_ID, Geschlecht, Kreis) %>%
  arrange(Meldejahr) %>%
  mutate(
    SSP1_2_6_Wintertemperatur = ifelse(!is.na(Wintertemperatur), Wintertemperatur, SSP1_2_6_Wintertemperatur),
    SSP3_7_Wintertemperatur = ifelse(!is.na(Wintertemperatur), Wintertemperatur, SSP3_7_Wintertemperatur),
    SSP1_2_6_Sommertemperatur = ifelse(!is.na(Sommertemperatur), Sommertemperatur, SSP1_2_6_Sommertemperatur),
    SSP3_7_Sommertemperatur = ifelse(!is.na(Sommertemperatur), Sommertemperatur, SSP3_7_Sommertemperatur),
    SSP1_2_6_Sommerniederschlag = ifelse(!is.na(Sommerniederschlag), Sommerniederschlag, SSP1_2_6_Sommerniederschlag),
    SSP3_7_Sommerniederschlag = ifelse(!is.na(Sommerniederschlag), Sommerniederschlag, SSP3_7_Sommerniederschlag),
    SSP1_2_6_Frühlingsniederschlag = ifelse(!is.na(Frühlingsniederschlag), Frühlingsniederschlag, SSP1_2_6_Frühlingsniederschlag),
    SSP3_7_Frühlingsniederschlag = ifelse(!is.na(Frühlingsniederschlag), Frühlingsniederschlag, SSP3_7_Frühlingsniederschlag),
    SSP1_2_6_Jahrestemperatur = ifelse(!is.na(Jahrestemperatur), Jahrestemperatur, SSP1_2_6_Jahrestemperatur),
    SSP3_7_Jahrestemperatur = ifelse(!is.na(Jahrestemperatur), Jahrestemperatur, SSP3_7_Jahrestemperatur),
    SSP1_2_6_Jahresniederschlag = ifelse(!is.na(Jahresniederschlag), Jahresniederschlag, SSP1_2_6_Jahresniederschlag),
    SSP3_7_Jahresniederschlag = ifelse(!is.na(Jahresniederschlag), Jahresniederschlag, SSP3_7_Jahresniederschlag)
  ) %>%
  ungroup()


final_na_extended <- final_na_extended %>%
  mutate(
    SSP1_2_6_Frühlingsniederschlag = ifelse(Meldejahr == 2040,
                                            Frühlingsniederschlag[which(Meldejahr == 2014)] * 1.10,
                                            SSP1_2_6_Frühlingsniederschlag),
    SSP3_7_Frühlingsniederschlag = ifelse(Meldejahr == 2040,
                                          Frühlingsniederschlag[which(Meldejahr == 2014)] * 0.90,
                                          SSP3_7_Frühlingsniederschlag),
    SSP1_2_6_Sommerniederschlag = ifelse(Meldejahr == 2040,
                                         Sommerniederschlag[which(Meldejahr == 2014)] * 1.10,
                                         SSP1_2_6_Sommerniederschlag),
    SSP3_7_Sommerniederschlag = ifelse(Meldejahr == 2040,
                                       Sommerniederschlag[which(Meldejahr == 2014)] * 0.90,
                                       SSP3_7_Sommerniederschlag),
    SSP1_2_6_Sommertemperatur = ifelse(Meldejahr == 2040,
                                       Sommertemperatur[which(Meldejahr == 2014)] + 1.5,
                                       SSP1_2_6_Sommertemperatur),
    SSP3_7_Sommertemperatur = ifelse(Meldejahr == 2040,
                                     Sommertemperatur[which(Meldejahr == 2014)] + 1.5,
                                     SSP3_7_Sommertemperatur),
    SSP1_2_6_Wintertemperatur = ifelse(Meldejahr == 2040,
                                       Wintertemperatur[which(Meldejahr == 2014)] + 1.0,
                                       SSP1_2_6_Wintertemperatur),
    SSP3_7_Wintertemperatur = ifelse(Meldejahr == 2040,
                                     Wintertemperatur[which(Meldejahr == 2014)] + 1.5,
                                     SSP3_7_Wintertemperatur),
    SSP1_2_6_Jahrestemperatur = ifelse(Meldejahr == 2040,
                                       Jahrestemperatur[which(Meldejahr == 2014)] + 1.0,
                                       SSP1_2_6_Jahrestemperatur),
    SSP3_7_Jahrestemperatur = ifelse(Meldejahr == 2040,
                                     Jahrestemperatur[which(Meldejahr == 2014)] + 1.25,
                                     SSP3_7_Jahrestemperatur),
    SSP1_2_6_Jahresniederschlag = ifelse(Meldejahr == 2040,
                                         Jahresniederschlag[which(Meldejahr == 2014)] * 1.05,
                                         SSP1_2_6_Jahresniederschlag),
    SSP3_7_Jahresniederschlag = ifelse(Meldejahr == 2040,
                                       Jahresniederschlag[which(Meldejahr == 2014)] * 0.95,
                                       SSP3_7_Jahresniederschlag)
  )


final_na_extended <- final_na_extended %>%
 group_by(STATIONS_ID, Geschlecht, Kreis) %>%
  arrange(Meldejahr) %>%
  mutate(across(
    c(Wintertemperatur, Sommertemperatur, Frühlingsniederschlag, Sommerniederschlag, Jahresniederschlag,
      SSP1_2_6_Wintertemperatur, SSP3_7_Wintertemperatur,
      SSP1_2_6_Sommertemperatur, SSP3_7_Sommertemperatur,
      SSP1_2_6_Frühlingsniederschlag, SSP3_7_Frühlingsniederschlag,
      SSP1_2_6_Sommerniederschlag, SSP3_7_Sommerniederschlag,
      SSP1_2_6_Jahrestemperatur, SSP3_7_Jahrestemperatur,
      SSP1_2_6_Jahresniederschlag, SSP3_7_Jahresniederschlag),
    ~zoo::na.approx(., x = Meldejahr, rule = 2))) %>%
  ungroup()




#Interpolieren Bevölkerungsdaten

#Meldejahr in dem alle Landkreise Daten aufweisen
vollständige_jahre <- final_na_extended %>%
  group_by(Meldejahr) %>%
  summarise(anzahl_kreise = n_distinct(Kreis)) %>%
  filter(anzahl_kreise == n_distinct(final_na_extended$Kreis)) %>%
  pull(Meldejahr)
print(vollständige_jahre)




bev_2045 <- final_na_extended %>%
  filter(Meldejahr == 2020) %>%
  mutate(
    Meldejahr = 2045,
    Bevölkerung_imp = Bevölkerung * (1 + Bevölkerungsentwicklung),
    Bevölkerung_Insg_imp = Bevölkerung_Insg * (1 + Bevölkerungsentwicklung)
  )


final_na_extended <- bind_rows(final_na_extended, bev_2045)

final_na_extended <- final_na_extended %>%
  group_by(STATIONS_ID, Geschlecht, Kreis) %>%
  arrange(Meldejahr) %>%
  mutate(
    Bevölkerung_imp = ifelse(Meldejahr <= 2023 & !is.na(Bevölkerung), Bevölkerung, Bevölkerung_imp),
    Bevölkerung_Insg_imp = ifelse(Meldejahr <= 2023 & !is.na(Bevölkerung_Insg), Bevölkerung_Insg, Bevölkerung_Insg_imp)
  ) %>%
  ungroup()



interpolate_linear <- function(df, value_col) {
  df <- df %>% arrange(Meldejahr)
  
  interpolated_values <- approx(
    x = df$Meldejahr,
    y = df[[value_col]],
    xout = seq(min(df$Meldejahr), max(df$Meldejahr)),
    method = "linear",
    rule = 2
  )
  
  df_interpolated <- df %>%
    mutate(
      !!value_col := interpolated_values$y[match(Meldejahr, interpolated_values$x)]
    )
  
  return(df_interpolated)
}

final_na_extended <- final_na_extended %>%
  group_by( Geschlecht, Kreis) %>%
  arrange(Meldejahr) %>%
  do(interpolate_linear(., "Bevölkerung_imp")) %>%
  do(interpolate_linear(., "Bevölkerung_Insg_imp")) %>%
  ungroup()





ggplot(lyme_SSP_1_2_6, aes(x = Meldejahr, y = SSP1_2_6_Sommertemperatur, color = Kreis)) +
  geom_line() +
  labs(title = "Interpolierte Sommertemperatur",
       x = "Jahr",
       y = "Bevölkerung")



## Übernehmen der Daten für neue Meldejahre

#Bundesland

final_na_extended <- final_na_extended %>%
  group_by(STATIONS_ID, Geschlecht, Kreis) %>%
  mutate(Bundesland = first(Bundesland[!is.na(Bundesland)])) %>%
  ungroup()

#LandkreisID

final_na_extended <- final_na_extended %>%
  group_by(STATIONS_ID, Geschlecht, Kreis) %>%
  mutate(LandkreisID = first(LandkreisID[!is.na(LandkreisID)])) %>%
  ungroup()

#Bevölkerungsentwicklung

final_na_extended <- final_na_extended %>%
  group_by(STATIONS_ID, Geschlecht, Kreis) %>%
  mutate(Bevölkerungsentwicklung = first(Bevölkerungsentwicklung[!is.na(Bevölkerungsentwicklung)])) %>%
  ungroup()


#Log Transformation
final_na_extended$Inzidenz_log <- log(final_na_extended$Inzidenz + 1)


#Wintertemperatur Vorjahr laggen
final_na_extended <- final_na_extended %>%
  mutate(Vorjahres_SSP1_2_6_Wintertemperatur = lag(SSP1_2_6_Wintertemperatur, default = NA))

final_na_extended <- final_na_extended %>%
  mutate(Vorjahres_SSP3_7_Wintertemperatur = lag(SSP3_7_Wintertemperatur, default = NA))




###########################################################################################################
### Überflüssige Variablen entfernen  ###

lyme <- final_na_extended[, !names(final_na_extended) %in% c("Jahrestemperatur", "Wintertemperatur", "Wintertemperatur_Vorjahr", "Frühlingsniederschlag", 
                                                             "Sommerniederschlag", "Sommertemperatur", "Bevölkerung", "Bevölkerung_Insg", "Bevölkerung_jekm2")]
lyme$Inzidenz <- round(lyme$Inzidenz)
lyme <- lyme %>%
  filter(Meldejahr <= 2040)




###########################################################################################################
### Datensätze mit SSP Szenarien erstellen  ###
lyme_SSP_1_2_6 <- lyme[, !names(lyme) %in% c("SSP3_7_Wintertemperatur", "Vorjahres_SSP3_7_Wintertemperatur", "SSP3_7_Frühlingsniederschlag", "SSP3_7_Sommerniederschlag", "SSP3_7_Sommertemperatur", 
                                             "SSP3_7_Jahrestemperatur", "SSP3_7_Jahresniederschlag", "Jahrestemperatur", "Jahresniederschlag")]
lyme_SSP_3_7 <- lyme[, !names(lyme) %in% c("SSP1_2_6_Wintertemperatur", "Vorjahres_SSP1_2_6_Wintertemperatur", "SSP1_2_6_Frühlingsniederschlag", "SSP1_2_6_Sommerniederschlag", "SSP1_2_6_Sommertemperatur", 
                                           "SSP1_2_6_Jahrestemperatur", "SSP1_2_6_Jahresniederschlag", "Jahrestemperatur", "Jahresniederschlag")]


###########################################################################################################
### DESKRIPTIV ###

summary_lyme <- lyme %>%
  group_by(Bundesland) %>%
  summarise(
    
    Mittelwert_SSP1_2_6_Wintertemperatur = mean(SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    SD_SSP1_2_6_Wintertemperatur = sd(SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    Minimum_SSP1_2_6_Wintertemperatur = min(SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    Maximum_SSP1_2_6_Wintertemperatur = max(SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    
    Mittelwert_SSP3_7_Wintertemperatur = mean(SSP3_7_Wintertemperatur, na.rm = TRUE),
    SD_SSP3_7_Wintertemperatur = sd(SSP3_7_Wintertemperatur, na.rm = TRUE),
    Minimum_SSP3_7_Wintertemperatur = min(SSP3_7_Wintertemperatur, na.rm = TRUE),
    Maximum_SSP3_7_Wintertemperatur = max(SSP3_7_Wintertemperatur, na.rm = TRUE),
    
    Mittelwert_SSP1_2_6_Sommertemperatur = mean(SSP1_2_6_Sommertemperatur, na.rm = TRUE),
    SD_SSP1_2_6_Sommertemperatur = sd(SSP1_2_6_Sommertemperatur, na.rm = TRUE),
    Minimum_SSP1_2_6_Sommertemperatur = min(SSP1_2_6_Sommertemperatur, na.rm = TRUE),
    Maximum_SSP1_2_6_Sommertemperatur = max(SSP1_2_6_Sommertemperatur, na.rm = TRUE),
    
    Mittelwert_SSP3_7_Sommertemperatur = mean(SSP3_7_Sommertemperatur, na.rm = TRUE),
    SD_SSP3_7_Sommertemperatur = sd(SSP3_7_Sommertemperatur, na.rm = TRUE),
    Minimum_SSP3_7_Sommertemperatur = min(SSP3_7_Sommertemperatur, na.rm = TRUE),
    Maximum_SSP3_7_Sommertemperatur = max(SSP3_7_Sommertemperatur, na.rm = TRUE),
    
    Mittelwert_SSP1_2_6_Frühlingsniederschlag = mean(SSP1_2_6_Frühlingsniederschlag, na.rm = TRUE),
    SD_SSP1_2_6_Frühlingsniederschlag = sd(SSP1_2_6_Frühlingsniederschlag, na.rm = TRUE),
    Minimum_SSP1_2_6_Frühlingsniederschlag = min(SSP1_2_6_Frühlingsniederschlag, na.rm = TRUE),
    Maximum_SSP1_2_6_Frühlingsniederschlag = max(SSP1_2_6_Frühlingsniederschlag, na.rm = TRUE),
    
    Mittelwert_SSP3_7_Frühlingsniederschlag = mean(SSP3_7_Frühlingsniederschlag, na.rm = TRUE),
    SD_SSP3_7_Frühlingsniederschlag = sd(SSP3_7_Frühlingsniederschlag, na.rm = TRUE),
    Minimum_SSP3_7_Frühlingsniederschlag = min(SSP3_7_Frühlingsniederschlag, na.rm = TRUE),
    Maximum_SSP3_7_Frühlingsniederschlag = max(SSP3_7_Frühlingsniederschlag, na.rm = TRUE),
    
    Mittelwert_SSP1_2_6_Sommerniederschlag = mean(SSP1_2_6_Sommerniederschlag, na.rm = TRUE),
    SD_SSP1_2_6_Sommerniederschlag = sd(SSP1_2_6_Sommerniederschlag, na.rm = TRUE),
    Minimum_SSP1_2_6_Sommerniederschlag = min(SSP1_2_6_Sommerniederschlag, na.rm = TRUE),
    Maximum_SSP1_2_6_Sommerniederschlag = max(SSP1_2_6_Sommerniederschlag, na.rm = TRUE),
    
    Mittelwert_SSP3_7_Sommerniederschlag = mean(SSP3_7_Sommerniederschlag, na.rm = TRUE),
    SD_SSP3_7_Sommerniederschlag = sd(SSP3_7_Sommerniederschlag, na.rm = TRUE),
    Minimum_SSP3_7_Sommerniederschlag = min(SSP3_7_Sommerniederschlag, na.rm = TRUE),
    Maximum_SSP3_7_Sommerniederschlag = max(SSP3_7_Sommerniederschlag, na.rm = TRUE),
    
    Mittelwert_SSP1_2_6_Jahrestemperatur = mean(SSP1_2_6_Jahrestemperatur, na.rm = TRUE),
    SD_SSP1_2_6_Jahrestemperatur = sd(SSP1_2_6_Jahrestemperatur, na.rm = TRUE),
    Minimum_SSP1_2_6_Jahrestemperatur = min(SSP1_2_6_Jahrestemperatur, na.rm = TRUE),
    Maximum_SSP1_2_6_Jahrestemperatur = max(SSP1_2_6_Jahrestemperatur, na.rm = TRUE),
    
    Mittelwert_SSP3_7_Jahrestemperatur = mean(SSP3_7_Jahrestemperatur, na.rm = TRUE),
    SD_SSP3_7_Jahrestemperatur = sd(SSP3_7_Jahrestemperatur, na.rm = TRUE),
    Minimum_SSP3_7_Jahrestemperatur = min(SSP3_7_Jahrestemperatur, na.rm = TRUE),
    Maximum_SSP3_7_Jahrestemperatur = max(SSP3_7_Jahrestemperatur, na.rm = TRUE),
    
    Mittelwert_SSP1_2_6_Jahresniederschlag = mean(SSP1_2_6_Jahresniederschlag, na.rm = TRUE),
    SD_SSP1_2_6_Jahresniederschlag = sd(SSP1_2_6_Jahresniederschlag, na.rm = TRUE),
    Minimum_SSP1_2_6_Jahresniederschlag = min(SSP1_2_6_Jahresniederschlag, na.rm = TRUE),
    Maximum_SSP1_2_6_Jahresniederschlag = max(SSP1_2_6_Jahresniederschlag, na.rm = TRUE),
    
    Mittelwert_SSP3_7_Jahresniederschlag = mean(SSP3_7_Jahresniederschlag, na.rm = TRUE),
    SD_SSP3_7_Jahresniederschlag = sd(SSP3_7_Jahresniederschlag, na.rm = TRUE),
    Minimum_SSP3_7_Jahresniederschlag = min(SSP3_7_Jahresniederschlag, na.rm = TRUE),
    Maximum_SSP3_7_Jahresniederschlag = max(SSP3_7_Jahresniederschlag, na.rm = TRUE),
    
    Mittelwert_Bevölkerung_imp = mean(Bevölkerung_imp, na.rm = TRUE),
    SD_Bevölkerung_imp = sd(Bevölkerung_imp, na.rm = TRUE),
    Minimum_Bevölkerung_imp = min(Bevölkerung_imp, na.rm = TRUE),
    Maximum_Bevölkerung_imp = max(Bevölkerung_imp, na.rm = TRUE),
    
    Mittelwert_Bevölkerung_Insg_imp = mean(Bevölkerung_Insg_imp, na.rm = TRUE),
    SD_Bevölkerung_Insg_imp = sd(Bevölkerung_Insg_imp, na.rm = TRUE),
    Minimum_Bevölkerung_Insg_imp = min(Bevölkerung_Insg_imp, na.rm = TRUE),
    Maximum_Bevölkerung_Insg_imp = max(Bevölkerung_Insg_imp, na.rm = TRUE),
    
    Mittelwert_Bevölkerungsentwicklung = mean(Bevölkerungsentwicklung, na.rm = TRUE),
    SD_Bevölkerungsentwicklung = sd(Bevölkerungsentwicklung, na.rm = TRUE),
    Minimum_Bevölkerungsentwicklung = min(Bevölkerungsentwicklung, na.rm = TRUE),
    Maximum_Bevölkerungsentwicklung = max(Bevölkerungsentwicklung, na.rm = TRUE),
    
    Mittelwert_Vorjahres_SSP1_2_6_Wintertemperatur = mean(Vorjahres_SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    SD_Vorjahres_SSP1_2_6_Wintertemperatur = sd(Vorjahres_SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    Minimum_Vorjahres_SSP1_2_6_Wintertemperatur = min(Vorjahres_SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    Maximum_Vorjahres_SSP1_2_6_Wintertemperatur = max(Vorjahres_SSP1_2_6_Wintertemperatur, na.rm = TRUE),
    
    Mittelwert_Vorjahres_SSP3_7_Wintertemperatur = mean(Vorjahres_SSP3_7_Wintertemperatur, na.rm = TRUE),
    SD_Vorjahres_SSP3_7_Wintertemperatur = sd(Vorjahres_SSP3_7_Wintertemperatur, na.rm = TRUE),
    Minimum_Vorjahres_SSP3_7_Wintertemperatur = min(Vorjahres_SSP3_7_Wintertemperatur, na.rm = TRUE),
    Maximum_Vorjahres_SSP3_7_Wintertemperatur = max(Vorjahres_SSP3_7_Wintertemperatur, na.rm = TRUE)
  )




#write_xlsx(summary_lyme, "summary_lyme.xlsx")

Inzidenz_1_2_6 <- lyme_SSP_1_2_6 %>%
  group_by(Bundesland) %>%
  summarise(
    Mittelwert_Inzidenz = mean(Inzidenz, na.rm = TRUE),
    SD_Inzidenz = sd(Inzidenz, na.rm = TRUE),
    Minimum_Inzidenz = min(Inzidenz, na.rm = TRUE),
    Maximum_Inzidenz = max(Inzidenz, na.rm = TRUE),
    
    Mittelwert_Gesamt_Inzidenz = mean(Gesamt_Inzidenz, na.rm = TRUE),
    SD_Gesamt_Inzidenz = sd(Gesamt_Inzidenz, na.rm = TRUE),
    Minimum_Gesamt_Inzidenz = min(Gesamt_Inzidenz, na.rm = TRUE),
    Maximum_Gesamt_Inzidenz = max(Gesamt_Inzidenz, na.rm = TRUE),
  )

Inzidenz_3_7 <- lyme_SSP_3_7 %>%
  group_by(Bundesland) %>%
  summarise(
    Mittelwert_Inzidenz = mean(Inzidenz, na.rm = TRUE),
    SD_Inzidenz = sd(Inzidenz, na.rm = TRUE),
    Minimum_Inzidenz = min(Inzidenz, na.rm = TRUE),
    Maximum_Inzidenz = max(Inzidenz, na.rm = TRUE),
    
    Mittelwert_Gesamt_Inzidenz = mean(Gesamt_Inzidenz, na.rm = TRUE),
    SD_Gesamt_Inzidenz = sd(Gesamt_Inzidenz, na.rm = TRUE),
    Minimum_Gesamt_Inzidenz = min(Gesamt_Inzidenz, na.rm = TRUE),
    Maximum_Gesamt_Inzidenz = max(Gesamt_Inzidenz, na.rm = TRUE),
  )






#OUTCOME Inzidenz
summary(final_na$Gesamt_Inzidenz)
sd(final_na$Gesamt_Inzidenz)  

# Histogramm
hist(final_na$Gesamt_Inzidenz,
     main = "Histogramm der Lyme-Borreliose Inzidenzen",
     xlab = "Lyme-Borreliose Inzidenz",
     breaks = 50,
     col = "lightpink",
     border = "black", 
     xlim = range(0, max(final_na$Gesamt_Inzidenz, na.rm = TRUE) * 1.1),
     ylim = range(0, max(hist(final_na$Gesamt_Inzidenz, breaks = 50, plot = FALSE)$counts) * 1.2),
     ylab = "",
     cex.main = 2,
     cex.lab = 1.5,
     cex.axis = 1.5
)

box()
axis(1, at = seq(0, max(final_na$Gesamt_Inzidenz, na.rm = TRUE), by = 10), las = 1, cex.axis = 1.5)
axis(2, las = 2, labels = FALSE, cex.axis = 1.5)


#Schiefe, Kurtosis, Modus
skewness(final_na$Gesamt_Inzidenz, na.rm = TRUE)
kurtosis(final_na$Gesamt_Inzidenz, na.rm = TRUE)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

mode_value <- get_mode(final_na$Gesamt_Inzidenz)
mode_value1 <- get_mode(lyme_SSP_1_2_6$Gesamt_Inzidenz)
mode_value2 <- get_mode(lyme_SSP_3_7$Gesamt_Inzidenz)



# Q-Q-Plot
qqnorm(final_na$Gesamt_Inzidenz, 
       main = "Q-Q-Plot der Lyme-Borreliose Inzidenz Gesamt",
       xlab = "Theoretische Quantile", 
       ylab = "Beobachtete Quantile", 
       pch = 16,  
       col = "pink",
       cex.main = 2,
       cex.lab = 1.5,
       cex.axis = 1.5
)

qqline(final_na$Gesamt_Inzidenz, col = "black")
qqpoints <- qqnorm(final_na$Gesamt_Inzidenz, plot = FALSE) 
points(qqpoints$x, qqpoints$y, col = "pink", pch = 16, bg = "pink")



#Quartile und IQR
Q1 <- quantile(final_na$Gesamt_Inzidenz, 0.25, na.rm = TRUE)
Q3 <- quantile(final_na$Gesamt_Inzidenz, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1


Q1 <- quantile(final_na$Inzidenz, 0.25, na.rm = TRUE)
Q3 <- quantile(final_na$Inzidenz, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1


# Boxplot
boxplot(final_na$Gesamt_Inzidenz, 
        main = "Boxplot der Lyme-Borreliose Inzidenz Gesamt", 
        ylab = "Inzidenz", 
        col = "lightpink",     
        horizontal = FALSE,
        cex.main = 2,
        cex.lab = 1.5,
        cex.axis = 1.5
)

iqr <- IQR(final_na$Gesamt_Inzidenz, na.rm = TRUE)
cat("Interquartilsbereich (IQR):", iqr, "\n")


boxplot(final_na$Inzidenz, main="Boxplot der Lyme-Borreliose Inzidenz", ylab="Inzidenz", col="lightpink",     
        horizontal = FALSE)
iqr <- IQR(final_na$Inzidenz, na.rm = TRUE)
cat("Interquartilsbereich (IQR):", iqr, "\n")


#Outlier
outliers_final_na <- boxplot.stats(final_na$Gesamt_Inzidenz)$out
print(outliers_final_na)




#Unterschidede in Inzidenz basierend auf Gruppierung
kruskal.test(Inzidenz ~ Geschlecht, data = final_na)
kruskal.test(Inzidenz ~ STATIONS_ID, data = final_na)
kruskal.test(Inzidenz ~ Kreis, data = final_na)
kruskal.test(Inzidenz ~ Meldejahr, data = final_na)
kruskal.test(Inzidenz ~ Bundesland, data = final_na)




###########################################################################################################
### VORAUSSETZUNGEN  ###


#MULTIKOLLINEARITÄT


#Korrelationsmatrix für bis 2024
corr_data <- final_na %>% select(Inzidenz, Bevölkerung_Insg, Jahrestemperatur, Jahresniederschlag,
                                 Wintertemperatur_Vorjahr, Sommertemperatur, Sommerniederschlag, Frühlingsniederschlag)

cor_matrix <- cor(corr_data)
print(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 1.5, tl.col = "black", 
         tl.srt = 45, diag = FALSE)

title(main = "Korrelationsmatrix 2011-2023", line = 4)  

#Korrelationsmatrix für imputiert SSP_1_2_6

corr_data_SSP1 <- lyme_SSP_1_2_6 %>% select(Inzidenz, Bevölkerung_Insg, Jahrestemperatur, Jahresniederschlag,
                                            Wintertemperatur_Vorjahr, Sommertemperatur, Sommerniederschlag, Frühlingsniederschlag)
corr_data_SSP1 <- na.omit(corr_data_SSP1)

cor_matrix_SSP1 <- cor(corr_data_SSP1)
print(cor_matrix_SSP1)
corrplot(cor_matrix_SSP1,method = "circle", type = "upper", tl.cex = 1.5, tl.col = "black",tl.srt = 45, diag = FALSE)
title(main = "Korrelationsmatrix SSP1-2.6", line = 4)  



#Korrelationsmatrix für imputiert SSP_3_7

corr_data_SSP3 <- lyme_SSP_3_7 %>% select(Inzidenz, Bevölkerung_Insg, Jahrestemperatur, Jahresniederschlag,
                                          Wintertemperatur_Vorjahr, Sommertemperatur, Sommerniederschlag, Frühlingsniederschlag)
corr_data_SSP3 <- na.omit(corr_data_SSP3)

cor_matrix_SSP3 <- cor(corr_data_SSP3)
print(cor_matrix_SSP3)
corrplot(cor_matrix_SSP3, method = "circle", type = "upper", tl.cex = 1.5, tl.col = "black",tl.srt = 45, diag = FALSE)

title(main = "Korrelationsmatrix SSP3-7", line = 4)  





#LINEARITÄT
p <- ggpairs(final_na, 
             columns = c("Gesamt_Inzidenz", "Bevölkerung_Insg", 
                         "Sommertemperatur", "Frühlingsniederschlag", 
                         "Sommerniederschlag", "Jahrestemperatur", 
                         "Jahresniederschlag", "Wintertemperatur_Vorjahr"),
             upper = list(continuous = wrap("smooth", method = "loess", color = "darkred", size = 0.5)),
             lower = list(continuous = wrap("points", color = "lightpink", alpha = 0.7)),
             title = "Scatterplot-Matrix mit LOESS-Glättungslinien") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    strip.text.x = element_text(size = 14, angle = 45),  
    strip.text.y = element_text(size = 14, angle = 45),  
    strip.background = element_blank(),  
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 13)
  )

jpeg("scatterplot_matrix.jpg", width = 1600, height = 1600)  
dev.off()


ggpairs(final_na, 
        columns = c( "Inzidenz", "Bevölkerung_Insg", 
                    "Sommertemperatur", "Frühlingsniederschlag", "Sommerniederschlag",
                    "Jahrestemperatur", "Jahresniederschlag", "Wintertemperatur_Vorjahr"),
        upper = list(continuous = wrap("cor", size = 5, color = "black")),
        lower = list(continuous = wrap("points", color = "lightpink", alpha = 0.7)),
        title = "Scatterplot-Matrix mit Korrelationskoeffizienten")






###########################################################################################################
### REGRESSION  ###


final_na$LandkreisID <- as.factor(final_na$LandkreisID)
final_na$Meldejahr <- as.factor(final_na$Meldejahr)
final_na$Geschlecht <- as.factor(final_na$Geschlecht)
final_na$Bundesland <- as.factor(final_na$Bundesland)

options(scipen = 999)  
# Deaktiviert die wissenschaftliche Notation



#GLMM FULL
glmm_model <- glmer(Inzidenz ~ Bevölkerung  + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag + Bundesland +
                      (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                    data = final_na, family = poisson(link = "log"))
summary(glmm_model)
#aic  5492.5 





#GLMM RESKALIERT UND MIT CONTROL NLOPTWRAP
final_na_scaled <- final_na
predictors <- c( "Bevölkerung", "Jahrestemperatur", "Jahresniederschlag",
                "Wintertemperatur_Vorjahr","Sommertemperatur", "Sommerniederschlag", "Frühlingsniederschlag")

final_na_scaled[predictors] <- lapply(final_na_scaled[predictors], scale)


glmm_model2 <- glmer(Inzidenz ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag + 
                      (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                    data = final_na_scaled, family = poisson(link = "log"),
                    control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))


summary(glmm_model2)
#aic 5496.1


#CONTROL NLOPTWRAP
glmm_model3 <- glmer(Inzidenz ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag + Bundesland +
                       (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                     data = final_na, family = poisson(link = "log"),
                     control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_model3)
#  5492.5




#ÜBERDISPERSION

pearson_resid <- residuals(glmm_model3, type = "pearson")
pearson_chi2 <- sum(pearson_resid^2)
num_obs <- nrow(final_na)
num_params <- length(fixef(glmm_model3)) + length(unlist(ranef(glmm_model3, condVar = FALSE)))
df_residuals <- num_obs - num_params
pearson_chi2_per_df <- pearson_chi2 / df_residuals
pearson_chi2_per_df
#6,4 -> overdispersion -> neg bin modell



#NEGATIV BINOMIAL
glmm_model_negative1 <- glmer.nb(Inzidenz ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag + Bundesland +
                       (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                     data = final_na,
                     control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_model_negative1)
#aic 3971.2



#LOG TRANSFORMATION
final_na_scaled$Inzidenz_log <- log(final_na_scaled$Inzidenz + 1)


#NEGATIV BINOMIAL MIT LOG
glmm_model_negative2 <- glmer.nb(Inzidenz_log ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag + Bundesland +
                                   (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                                 data = final_na,
                                 control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_model_negative2)
#aic  1534.6 



#NEGATIV BINOMIAL RESCALED 
glmm_model_negative3 <- glmer.nb(Inzidenz ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag + Bundesland +
                                   (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                                 data = final_na_scaled,
                                 control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_model_negative3)
#aic  3971.2


#NEGATIV BINOMIAL RESCALED MIT LOG
glmm_model_negative4 <- glmer.nb(Inzidenz_log ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag + Bundesland +
                                   (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                                 data = final_na_scaled,
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmm_model_negative4)
#  1534.6 


#glmm_model_negative2 höchste Modellgüte


#NEGATIV BINOMIAL BIVARIAT
glmm_model_negative5 <- glmer.nb(Inzidenz_log ~   Jahrestemperatur +
                                   (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                                 data = final_na,
                                 control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_model_negative5)
# 1528.6 


###########################################################################################################
### RESIDUEN  ###

residuals_glmm <- residuals(glmm_model_negative2)
fitted_values <- fitted(glmm_model_negative2)

# Residuen vs. Angepasste Werte Plot 
#Homoskedastizität
plot(fitted_values, residuals_glmm, 
     xlab = "Angepasste Werte", 
     ylab = "Residuen", 
     main = "Residuals vs Fitted Values",
     pch = 16, col = "lightpink")
abline(h = 0, col = "darkred")  


# QQ-Plot der Residuen
qqnorm(residuals_glmm, main = "QQ-Plot der Pearson-Residuals", col = "pink", pch = 16,
       xlab = "Theoretische Quantile", 
       ylab = "Beobachtete Quantile",
       cex.main = 2,
       cex.lab = 1.5,
       cex.axis = 1.5)

qqline(residuals_glmm, col = "darkred")

# Histogramm der Residuen
hist(residuals_glmm, breaks = 30, 
     main = "Histogramm der Residuen", 
     xlab = "Residuen", 
     col = "lightpink", 
     border = "black",
     cex.main = 2,
     cex.lab = 1.5,
     cex.axis = 1.5)


# Leverage-Werte berechnen
model_lev <- augment(glmm_model_negative2)
leverage <- model_lev$.hat

# Residuen vs. Leverage Plot
plot(leverage, residuals_glmm, 
     xlab = "Leverage", 
     ylab = "Residuen", 
     main = "Residuals vs Leverage",
     pch = 16, col = "lightpink",
     cex.main = 2,
     cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, col = "darkred")




check_model(glmm_model_negative2)








###########################################################################################################
### WEGLASSEN VON VARIABLEN PRÜFEN  ###


glmm_test1 <- glmer.nb(Inzidenz_log ~ Jahrestemperatur +
                                   (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                                 data = final_na,
                                 control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test1)
#  1528.6 


glmm_test2 <- glmer.nb(Inzidenz_log ~ Bevölkerung + Jahrestemperatur +
                        (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                      data = final_na,
                      control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test2)
#  1529.3
glmm_test3 <- glmer.nb(Inzidenz_log ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag  +
                        (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                      data = final_na,
                      control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test3)
#  1530.0   
glmm_test4 <- glmer.nb(Inzidenz_log ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr  +
                        (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                      data = final_na,
                      control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test4)

#  1531.9
glmm_test5 <- glmer.nb(Inzidenz_log ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur  +
                         (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                       data = final_na,
                       control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test5)
# 1533.8 

glmm_test6 <- glmer.nb(Inzidenz_log ~ Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag +
                         (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                       data = final_na,
                       control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test6)
# 1535.7 

#fullmodel
glmm_test8 <- glmer.nb(Inzidenz_log ~  Bevölkerung + Jahrestemperatur + Jahresniederschlag + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag  + Bundesland +
                         (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                       data = final_na,
                       control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test8)
# 1534.6



#gruppiert herausnehmen
glmm_test9 <- glmer.nb(Inzidenz_log ~  Jahrestemperatur  + Wintertemperatur_Vorjahr + Sommertemperatur  + Bundesland +
                         (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                       data = final_na,
                       control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test9)
#  1526.9

glmm_test10 <- glmer.nb(Inzidenz_log ~  Jahrestemperatur +  Sommerniederschlag + Frühlingsniederschlag  + Bundesland +
                         (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                       data = final_na,
                       control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test10)
#   1527.0


glmm_test11 <- glmer.nb(Inzidenz_log ~  Bevölkerung  + Wintertemperatur_Vorjahr + Sommertemperatur + Sommerniederschlag + Frühlingsniederschlag  + Bundesland +
                         (1 | LandkreisID) + (1 | Meldejahr) + (1 | Geschlecht),
                       data = final_na,
                       control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 2e5)))
summary(glmm_test11)
#  1530.8


#wenig Verbesserung im AIC und Modellqualität







###########################################################################################################
###  PRÄDIKTION ###


#PRÄDIKTION SSP1-2.6

#Variablennamen müssen übereinstimmen mit Variablennamen des Datensatzes der Regression
lyme_SSP_1_2_6 <- lyme_SSP_1_2_6 %>%
  rename(
    Frühlingsniederschlag = SSP1_2_6_Frühlingsniederschlag  ,
    Jahresniederschlag = SSP1_2_6_Jahresniederschlag  ,
    Sommerniederschlag = SSP1_2_6_Sommerniederschlag ,
    Wintertemperatur = SSP1_2_6_Wintertemperatur  ,
    Sommertemperatur = SSP1_2_6_Sommertemperatur  ,
    Jahrestemperatur = SSP1_2_6_Jahrestemperatur  ,
    Bevölkerung  = Bevölkerung_imp ,
    Bevölkerung_Insg  = Bevölkerung_Insg_imp ,
    Wintertemperatur_Vorjahr = Vorjahres_SSP1_2_6_Wintertemperatur
  )





vorhersage_daten <- lyme_SSP_1_2_6 

vorhersage_daten$predicted_Inzidenz_log <- predict(glmm_model_negative2, newdata = vorhersage_daten, type = "response", re.form = NA)
vorhersage_daten$predicted_Inzidenz <- exp(vorhersage_daten$predicted_Inzidenz_log)


vorhersage_daten <- vorhersage_daten %>%
  select(-Inzidenz, -Inzidenz_log)


vorhersage_daten <- vorhersage_daten %>%
  rename(
    Inzidenz = predicted_Inzidenz,
    Inzidenz_log = predicted_Inzidenz_log
  )


vorhersage_daten <- vorhersage_daten %>%
  filter(Meldejahr >= 2024)


lyme_SSP_1_2_6 <- lyme_SSP_1_2_6 %>%
  filter(Meldejahr < 2024) %>%     
  bind_rows(vorhersage_daten)       



#GESAMTINZIDENZ HINZUFÜGEN

lyme_SSP_1_2_6 <- lyme_SSP_1_2_6 %>%
  group_by(Meldejahr, LandkreisID) %>%
  mutate(Gesamt_Inzidenz = sum(Inzidenz)) %>%
  ungroup()



###########################################################################################################
### GRAFIKEN  ###

# Inzidenz nach Temperatur pro Landkreis 
ggplot(lyme_SSP_1_2_6, aes(x = Jahrestemperatur, y = Gesamt_Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP1-2.6 Inzidenz vs. Temperatur nach Landkreis",
       x = "Temperatur", 
       y = "SSP1-2.6 Inzidenz") +
  theme_classic() +
  scale_x_continuous(breaks = seq(8, max(lyme_SSP_1_2_6$Jahrestemperatur), by = 0.5),
                     expand = c(0, 0)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")



#Inzidenz nach Temperatur stratifziert nach Geschlecht
ggplot(lyme_SSP_1_2_6, aes(x = Jahrestemperatur, y = Inzidenz, color = Geschlecht)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP1-2.6 Inzidenz vs. Jahrestemperatur nach Geschlecht",
       x = "Jahrestemperatur (°C)", 
       y = "SSP1-2.6 Inzidenz",
       color = "Geschlecht") +
  scale_x_continuous(breaks = seq(7, max(lyme_SSP_1_2_6$Jahrestemperatur), by = 1.0),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#Inzidenz nach Temperatur 
ggplot(lyme_SSP_1_2_6, aes(x = Jahrestemperatur, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  labs(title = "SSP1-2.6 Inzidenz vs. Jahrestemperatur",
       x = "Jahrestemperatur (°C)", 
       y = "SSP1-2.6 Inzidenz") +
  scale_x_continuous(breaks = seq(7, max(lyme_SSP_1_2_6$Jahrestemperatur), by = 1.0),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#Inzidenz nach Temperatur nach Bundesland
ggplot(lyme_SSP_1_2_6, aes(x = Jahrestemperatur, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  labs(title = "SSP1-2.6 Inzidenz vs. Jahrestemperatur",
       x = "Jahrestemperatur (°C)", 
       y = "SSP1-2.6 Inzidenz") +
  scale_x_continuous(breaks = seq(7, max(lyme_SSP_1_2_6$Jahrestemperatur), by = 1.0),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  facet_wrap(~ Bundesland)


#Inzidenz nach Meldejahr stratifziert nach Geschlecht
ggplot(lyme_SSP_1_2_6, aes(x = Meldejahr, y = Inzidenz, color = Geschlecht)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP1-2.6 Inzidenz vs. Meldejahr nach Geschlecht",
       x = "Meldejahr", 
       y = "SSP1-2.6 Inzidenz",
       color = "Geschlecht") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


# Inzidenz nach Meldejahr pro Landkreis stratifziert nach Geschlecht
ggplot(lyme_SSP_1_2_6, aes(x = Meldejahr, y = Inzidenz, color = Geschlecht)) +
  geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP1-2.6 Inzidenz vs. Meldejahr nach Geschlecht und Landkreis",
       x = "Meldejahr", 
       y = "SSP1-2.6 Inzidenz",
       color = "Geschlecht") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")



#Inzidenz nach Meldejahr nach Bundesland
ggplot(lyme_SSP_1_2_6, aes(x = Meldejahr, y = Gesamt_Inzidenz, color = Bundesland)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP1-2.6 Inzidenz vs. Meldejahr nach Bundesland",
       x = "Meldejahr", 
       y = "SSP1-2.6 Inzidenz",
       color = "Bundesland") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Bundesland, scales = "free")



ggplot(lyme_SSP_1_2_6, aes(x = Meldejahr, y = Inzidenz, color = Kreis)) +
  geom_line() +
  labs(title = "SSP1-2.6 Inzidenz vs. Meldejahr",
       x = "Jahr",
       y = "Inzidenz")



# Inzidenz nach Meldejahr pro Landkreis 
ggplot(lyme_SSP_1_2_6, aes(x = Meldejahr, y = Gesamt_Inzidenz)) +
  geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0, colour = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", colour = "darkred") +
  labs(title = "SSP1-2.6 Inzidenz vs. Meldejahr nach Landkreis",
       x = "Meldejahr", 
       y = "SSP1-2.6 Inzidenz") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")


#Inzidenz beschreiben
hist(lyme_SSP_1_2_6$Gesamt_Inzidenz,
     main = "Histogramm der prognostizierten Lyme-Borreliose Inzidenzen SSP1-2.6",
     xlab = "Lyme-Borreliose Inzidenz",
     breaks = 50,
     col = "lightpink",
     border = "black", 
     xlim = range(0, max(lyme_SSP_1_2_6$Gesamt_Inzidenz, na.rm = TRUE) * 1.1),
     ylim = range(0, max(hist(lyme_SSP_1_2_6$Gesamt_Inzidenz, breaks = 50, plot = FALSE)$counts) * 1.2),
     ylab = "",
     cex.main = 2,
     cex.lab = 1.5,
     cex.axis = 1.5
)

box()
axis(1, at = seq(0, max(lyme_SSP_1_2_6$Gesamt_Inzidenz, na.rm = TRUE), by = 10), las = 1, cex.axis = 1.5)
axis(2, las = 2, labels = FALSE, cex.axis = 1.5)

###########################################################################################################


#PRÄDIKTION SSP3-7
lyme_SSP_3_7 <- lyme_SSP_3_7 %>%
  rename(
    Frühlingsniederschlag = SSP3_7_Frühlingsniederschlag  ,
    Jahresniederschlag = SSP3_7_Jahresniederschlag  ,
    Sommerniederschlag = SSP3_7_Sommerniederschlag ,
    Wintertemperatur = SSP3_7_Wintertemperatur  ,
    Sommertemperatur = SSP3_7_Sommertemperatur  ,
    Jahrestemperatur = SSP3_7_Jahrestemperatur  ,
    Bevölkerung  = Bevölkerung_imp ,
    Bevölkerung_Insg  = Bevölkerung_Insg_imp ,
    Wintertemperatur_Vorjahr = Vorjahres_SSP3_7_Wintertemperatur
  )



vorhersage_daten_2 <- lyme_SSP_3_7 

vorhersage_daten_2$predicted_Inzidenz_log <- predict(glmm_model_negative2, newdata = vorhersage_daten_2, type = "response", re.form = NA)
vorhersage_daten_2$predicted_Inzidenz <- exp(vorhersage_daten_2$predicted_Inzidenz_log)

vorhersage_daten_2 <- vorhersage_daten_2 %>%
  select(-Inzidenz, -Inzidenz_log)

vorhersage_daten_2 <- vorhersage_daten_2 %>%
  rename(
    Inzidenz = predicted_Inzidenz,
    Inzidenz_log = predicted_Inzidenz_log
  )

vorhersage_daten_2 <- vorhersage_daten_2 %>%
  filter(Meldejahr >= 2024)


lyme_SSP_3_7 <- lyme_SSP_3_7 %>%
  filter(Meldejahr < 2024) %>%     
  bind_rows(vorhersage_daten_2)       



#GESAMTINZIDENZ HINZUFÜGEN

lyme_SSP_3_7 <- lyme_SSP_3_7 %>%
  group_by(Meldejahr, LandkreisID) %>%
  mutate(Gesamt_Inzidenz = sum(Inzidenz)) %>%
  ungroup()


###########################################################################################################
### GRAFIKEN  ###

# Inzidenz nach Temperatur pro Landkreis 
ggplot(lyme_SSP_3_7, aes(x = Jahrestemperatur, y = Gesamt_Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP3-7 Inzidenz vs. Temperatur nach Landkreis",
       x = "Temperatur", 
       y = "SSP3-7 Inzidenz") +
  scale_x_continuous(breaks = seq(8, max(lyme_SSP_3_7$Jahrestemperatur), by = 0.5),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")



#Inzidenz nach Temperatur stratifziert nach Geschlecht
ggplot(lyme_SSP_3_7, aes(x = Jahrestemperatur, y = Inzidenz, color = Geschlecht)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP3-7 Inzidenz vs. Jahrestemperatur nach Geschlecht",
       x = "Jahrestemperatur (°C)", 
       y = "SSP3-7 Inzidenz",
       color = "Geschlecht") +
  scale_x_continuous(breaks = seq(7, max(lyme_SSP_3_7$Jahrestemperatur), by = 1.0),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#Inzidenz nach Temperatur 
ggplot(lyme_SSP_3_7, aes(x = Jahrestemperatur, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  labs(title = "SSP3-7 Inzidenz vs. Jahrestemperatur",
       x = "Jahrestemperatur (°C)", 
       y = "SSP3-7 Inzidenz") +
  scale_x_continuous(breaks = seq(7, max(lyme_SSP_3_7$Jahrestemperatur), by = 1.0),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#Inzidenz nach Temperatur nach Bundesland
ggplot(lyme_SSP_3_7, aes(x = Jahrestemperatur, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  labs(title = "SSP3-7 Inzidenz vs. Jahrestemperatur",
       x = "Jahrestemperatur (°C)", 
       y = "SSP3-7 Inzidenz") +
  scale_x_continuous(breaks = seq(7, max(lyme_SSP_3_7$Jahrestemperatur), by = 1.0),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  facet_wrap(~ Bundesland)


#Inzidenz nach Meldejahr stratifziert nach Geschlecht
ggplot(lyme_SSP_3_7, aes(x = Meldejahr, y = Inzidenz, color = Geschlecht)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP3-7 Inzidenz vs. Meldejahr nach Geschlecht",
       x = "Meldejahr", 
       y = "SSP3-7 Inzidenz",
       color = "Geschlecht") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


# Inzidenz nach Meldejahr pro Landkreis stratifziert nach Geschlecht
ggplot(lyme_SSP_3_7, aes(x = Meldejahr, y = Inzidenz, color = Geschlecht)) +
  geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP3-7 Inzidenz vs. Meldejahr nach Geschlecht und Landkreis",
       x = "Meldejahr", 
       y = "SSP3-7 Inzidenz",
       color = "Geschlecht") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")




#Inzidenz nach Meldejahr nach Bundesland
ggplot(lyme_SSP_3_7, aes(x = Meldejahr, y = Gesamt_Inzidenz, color = Bundesland)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "SSP3-7 Inzidenz vs. Meldejahr nach Bundesland",
       x = "Meldejahr", 
       y = "SSP3-7 Inzidenz",
       color = "Bundesland") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Bundesland, scales = "free")




# Inzidenz nach Meldejahr pro Landkreis 
ggplot(lyme_SSP_3_7, aes(x = Meldejahr, y = Gesamt_Inzidenz)) +
  geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0, colour = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", colour = "darkred") +
  labs(title = "SSP3-7 Inzidenz vs. Meldejahr nach Landkreis",
       x = "Meldejahr", 
       y = "SSP3-7 Inzidenz") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")



#Inzidenz beschreiben

hist(lyme_SSP_3_7$Gesamt_Inzidenz,
     main = "Histogramm der prognostizierten Lyme-Borreliose Inzidenzen SSP3-7",
     xlab = "Lyme-Borreliose Inzidenz",
     breaks = 50,
     col = "lightpink",
     border = "black", 
     xlim = range(0, max(lyme_SSP_3_7$Gesamt_Inzidenz, na.rm = TRUE) * 1.1),
     ylim = range(0, max(hist(lyme_SSP_3_7$Gesamt_Inzidenz, breaks = 50, plot = FALSE)$counts) * 1.2),
     ylab = "",
     cex.main = 2,
     cex.lab = 1.5,
     cex.axis = 1.5
)

box()
axis(1, at = seq(0, max(lyme_SSP_3_7$Gesamt_Inzidenz, na.rm = TRUE), by = 10), las = 1, cex.axis = 1.5)
axis(2, las = 2, labels = FALSE, cex.axis = 1.5)
###########################################################################################################
## beide Histogramme

library(ggplot2)

# Erstellen eines kombinierten Datensatzes mit einem Szenario-Label
lyme_SSP_3_7$Szenario <- "SSP3-7"
lyme_SSP_1_2_6$Szenario <- "SSP1-2.6"

# Kombinieren der beiden Datensätze
combined_data <- rbind(lyme_SSP_3_7, lyme_SSP_1_2_6)

# Erstellen des Histogramms
ggplot(combined_data, aes(x = Gesamt_Inzidenz, fill = Szenario)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50, color = "black") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  labs(title = "Vergleich der prognostizierten Lyme-Borreliose Inzidenzen",
       x = "Lyme-Borreliose Inzidenz", y = "Häufigkeit") +
  theme_minimal()



###########################################################################################################

#BIS 2024 

ggplot(final_na, aes(x = Jahrestemperatur, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "Inzidenz vs. Temperatur nach Landkreis",
       x = "Temperatur", 
       y = "Inzidenz") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  facet_wrap(~ Kreis, scales = "free")


ggplot(final_na, aes(x = Jahrestemperatur, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = " Inzidenz vs. Temperatur",
       x = "Temperatur", 
       y = "Inzidenz") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10)) 


ggplot(final_na, aes(x = Meldejahr, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = "Inzidenz vs. Meldejahr nach Landkreis",
       x = "Meldejahr", 
       y = "Inzidenz") +
  scale_x_continuous(breaks = seq(2011, max(final_na$Meldejahr), by = 2.0),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")


ggplot(final_na, aes(x = Meldejahr, y = Inzidenz)) +
  geom_point(size = 3, alpha = 0.7, color = "lightpink") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("lightpink", "darkred")) +
  labs(title = " Inzidenz vs. Meldejahr",
       x = "Meldejahr", 
       y = "Inzidenz") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10)) 




ggplot(final_na, aes(x = Meldejahr, y = Inzidenz, color = Kreis)) +
  geom_line() +
  labs(title = "Inzidenz vs. Meldejahr",
       x = "Jahr",
       y = "Inzidenz")





###########################################################################################################

#Vergleich Szenarien

data_combined <- lyme_SSP_1_2_6 %>%
  filter(Meldejahr >= 2024) %>%
  select(Meldejahr, LandkreisID, Kreis, Inzidenz_1_2_6 = Gesamt_Inzidenz) %>%
  left_join(lyme_SSP_3_7 %>%
              filter(Meldejahr >= 2024) %>%
              select(Meldejahr, Geschlecht, LandkreisID, Inzidenz_3_7 = Gesamt_Inzidenz),
            by = c("Meldejahr", "LandkreisID"))

data_long <- data_combined %>%
  pivot_longer(cols = c(Inzidenz_1_2_6, Inzidenz_3_7),
               names_to = "Szenario", values_to = "Inzidenz")

ggplot(data_long, aes(x = Meldejahr, y = Inzidenz, color = Szenario)) +
  geom_line(size = 1) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Lyme-Borreliose Inzidenzen nach Szenario und Landkreis (2024-2040)",
       x = "Meldejahr",
       y = "Inzidenz",
       color = "Szenario") +
  scale_color_manual(values = c("Inzidenz_1_2_6" = "lightpink", "Inzidenz_3_7" = "darkred")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.position = "bottom",
    strip.text = element_text(size = 16)
  ) +
  facet_wrap(~ Kreis, scales = "free")


###########################################################################################################
#Temperatur

temp <- plot_ly(lyme_SSP_1_2_6, x = ~Meldejahr) %>%
  add_markers(y = ~Wintertemperatur_Vorjahr, name = 'Wintertemperatur', marker = list(color = 'lightpink', size = 8)) %>%
  add_markers(y = ~Sommertemperatur, name = 'Sommertemperatur', marker = list(color = 'red', size = 8)) %>%
  add_markers(y = ~Jahrestemperatur, name = 'Jahrestemperatur', marker = list(color = 'darkred', size = 8)) %>%
  layout(
    title = list(text = "Temperaturentwicklung 2011-2040 SSP1-2.6", font = list(size = 28, color = 'black')),
    xaxis = list(
      title = list(text = "Jahr", font = list(size = 24, color = 'black')),
      tickfont = list(size = 20)  
    ),
    yaxis = list(
      title = list(text = "Temperatur in °C", font = list(size = 24, color = 'black')),
      tickfont = list(size = 20)  
    ),
    legend = list(font = list(size = 20, color = 'black')),
    margin = list(t = 100)
  )

temp



temp2 <- plot_ly(lyme_SSP_3_7, x = ~Meldejahr) %>%
  add_markers(y = ~Wintertemperatur_Vorjahr, name = 'Wintertemperatur', marker = list(color = 'lightpink', size = 8)) %>%
  add_markers(y = ~Sommertemperatur, name = 'Sommertemperatur', marker = list(color = 'red', size = 8)) %>%
  add_markers(y = ~Jahrestemperatur, name = 'Jahrestemperatur', marker = list(color = 'darkred', size = 8)) %>%
  layout(
    title = list(text = "Temperaturentwicklung 2011-2040 SSP3-7", font = list(size = 28, color = 'black')),
    xaxis = list(
      title = list(text = "Jahr", font = list(size = 24, color = 'black')),
      tickfont = list(size = 20)  
    ),
    yaxis = list(
      title = list(text = "Temperatur in °C", font = list(size = 24, color = 'black')),
      tickfont = list(size = 20) 
    ),
    legend = list(font = list(size = 20, color = 'black')),
    margin = list(t = 100)
  )

temp2


#Niederschlag
regen <- plot_ly(lyme_SSP_1_2_6, x = ~Meldejahr) %>%
  add_markers(y = ~Jahresniederschlag, name = 'Jahresniederschlag', marker = list(color = 'lightpink', size = 8)) %>%
  add_markers(y = ~Sommerniederschlag, name = 'Sommerniederschlag', marker = list(color = 'red', size = 8)) %>%
  add_markers(y = ~Frühlingsniederschlag, name = 'Frühlingsniederschlag', marker = list(color = 'darkred', size = 8)) %>%
  layout(
    title = list(text = "Niederschlagsentwicklung 2011-2040 SSP1-2.6", font = list(size = 28, color = 'black')),
    xaxis = list(title = list(text = "Jahr", font = list(size = 24, color = 'black')),
                 tickfont = list(size = 20)), 
    yaxis = list(title = list(text = "Niederschlagsmenge in mm", font = list(size = 24, color = 'black')),
                 tickfont = list(size = 20)),  
    legend = list(font = list(size = 20, color = 'black')),
    margin = list(t = 100)
  )

regen



regen2 <- plot_ly(lyme_SSP_3_7, x = ~Meldejahr) %>%
  add_markers(y = ~Jahresniederschlag, name = 'Jahresniederschlag', marker = list(color = 'lightpink', size = 8)) %>%
  add_markers(y = ~Sommerniederschlag, name = 'Sommerniederschlag', marker = list(color = 'red', size = 8)) %>%
  add_markers(y = ~Frühlingsniederschlag, name = 'Frühlingsniederschlag', marker = list(color = 'darkred', size = 8)) %>%
  layout(
    title = list(text = "Niederschlagsentwicklung 2011-2040 SSP3-7", font = list(size = 28, color = 'black')),
    xaxis = list(title = list(text = "Jahr", font = list(size = 24, color = 'black')),
                 tickfont = list(size = 20)),  
    yaxis = list(title = list(text = "Niederschlagsmenge in mm", font = list(size = 24, color = 'black')),
                 tickfont = list(size = 20)),  
    legend = list(font = list(size = 20, color = 'black')),
    margin = list(t = 100)
  )

regen2



###########################################################################################################
write.csv(final_na, "final_na.csv", row.names = FALSE)
write.csv(final, "final.csv", row.names = FALSE)
write.csv(lyme_SSP_1_2_6, "lyme_SSP1_2_6.csv", row.names = FALSE)
write.csv(lyme_SSP_3_7, "lyme_SSP_3_7.csv", row.names = FALSE)




