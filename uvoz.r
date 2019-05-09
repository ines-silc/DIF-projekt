library(pdftools)
library(plyr)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

uvozi.spl_2019 <- function() {
  data <- read_csv2("podatki/SSP2019_SPL.csv",
                    col_names = c("BLC_ID",	"BLC_NAME",	"K2_ID",	"K2_NAME",
                                  "K3_ID",	"K3_NAME", "K4_ID",	"K4_NAME",
                                  "SPS2019"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

spl_2019 <- uvozi.spl_2019()

uvozi.pos_2019 <- function() {
  data <- read_csv2("podatki/SSP2019_POS.csv",
                    col_names = c("BLC_I", "BLC_NAME", "NADSKUPINA_ID", "NADSKUPINA_NAME",
                                  "SPU_ID", "SPU_NAME", "PU_ID", "PU_NAME", "POL_ID", "POL_NAME",
                                  "PRG_ID", "PRG_NAME", "POD_ID", "POD_NAME", "SPP2019"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

pos_2019 <- uvozi.pos_2019()

###########################################################################
uvozi.spl_2018 <- function() {
  data <- read_csv2("podatki/SP2018_SPL.csv",
                    col_names = c("BLC_ID",	"BLC_NAME",	"K2_ID",	"K2_NAME",
                                  "K3_ID",	"K3_NAME", "K4_ID",	"K4_NAME",
                                  "SPS2018"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

spl_2018 <- uvozi.spl_2018()

uvozi.pos_2018 <- function() {
  data <- read_csv2("podatki/SP2018_POS.csv",
                    col_names = c("BLC_I", "BLC_NAME", "NADSKUPINA_ID", "NADSKUPINA_NAME",
                                  "SPU_ID", "SPU_NAME", "PU_ID", "PU_NAME", "POL_ID", "POL_NAME",
                                  "PRG_ID", "PRG_NAME", "POD_ID", "POD_NAME", "SPP2018"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

pos_2018 <- uvozi.pos_2018()

###########################################################################
uvozi.spl_2017 <- function() {
  data <- read_csv2("podatki/SP2017_SPL.csv",
                    col_names = c("BLC_ID",	"BLC_NAME",	"K2_ID",	"K2_NAME",
                                  "K3_ID",	"K3_NAME", "K4_ID",	"K4_NAME",
                                  "SPS2017"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

spl_2017 <- uvozi.spl_2017()

uvozi.pos_2017 <- function() {
  data <- read_csv2("podatki/SP2017_POS.csv",
                    col_names = c("BLC_I", "BLC_NAME", "NADSKUPINA_ID", "NADSKUPINA_NAME",
                                  "SPU_ID", "SPU_NAME", "PU_ID", "PU_NAME", "POL_ID", "POL_NAME",
                                  "PRG_ID", "PRG_NAME", "POD_ID", "POD_NAME", "SPP2017"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1) %>% mutate(PRG_ID = parse_integer(as.character(PRG_ID))) %>% mutate(POL_ID = parse_integer(as.character(POL_ID))) %>% mutate(POD_ID = parse_integer(as.character(POD_ID)))
  return(data)}

pos_2017 <- uvozi.pos_2017()

###########################################################################
uvozi.spl_2016 <- function() {
  data <- read_csv2("podatki/SP2016_SPL.csv",
                    col_names = c("BLC_ID",	"BLC_NAME",	"K2_ID",	"K2_NAME",
                                  "K3_ID",	"K3_NAME", "K4_ID",	"K4_NAME",
                                  "SPS2016"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

spl_2016 <- uvozi.spl_2016()

uvozi.pos_2016 <- function() {
  data <- read_csv2("podatki/SP2016_POS.csv",
                    col_names = c("BLC_I", "BLC_NAME", "NADSKUPINA_ID", "NADSKUPINA_NAME",
                                  "SPU_ID", "SPU_NAME", "PU_ID", "PU_NAME", "POL_ID", "POL_NAME",
                                  "PRG_ID", "PRG_NAME", "POD_ID", "POD_NAME", "SPP2016"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

pos_2016 <- uvozi.pos_2016()

###########################################################################
uvozi.spl_2015 <- function() {
  data <- read_csv2("podatki/SP2015_SPL.csv",
                    col_names = c("BLC_ID",	"BLC_NAME",	"K2_ID",	"K2_NAME",
                                  "K3_ID",	"K3_NAME", "K4_ID",	"K4_NAME",
                                  "SPS2015"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

spl_2015 <- uvozi.spl_2015()

uvozi.pos_2015 <- function() {
  data <- read_csv2("podatki/SP2015_POS.csv",
                    col_names = c("BLC_I", "BLC_NAME", "NADSKUPINA_ID", "NADSKUPINA_NAME",
                                  "SPU_ID", "SPU_NAME", "PU_ID", "PU_NAME", "POL_ID", "POL_NAME",
                                  "PRG_ID", "PRG_NAME", "POD_ID", "POD_NAME", "SPP2015"),
                    locale = locale(decimal_mark = ",", encoding = "Utf-8"),
                    skip = 1)
  return(data)}

pos_2015 <- uvozi.pos_2015()

#############################################################################
splosni <- full_join(spl_2015, spl_2016) 
splosni <- full_join(splosni, spl_2017)
splosni <- full_join(splosni, spl_2018)
splosni <- full_join(splosni, spl_2019)


posebni <- full_join(pos_2015, pos_2016) 
posebni <- full_join(posebni, pos_2017)
posebni <- full_join(posebni, pos_2018) 
posebni <- full_join(posebni, pos_2019) 
write.csv(posebni, file = "podatki/posebni.csv")



# 70 - davcni prihodki
# 71 - nedavcni prihodki
# 72 - kapitalski prihodki
# 73 - prejete donacije
# 74 - transferni prihodki
# 78 - prejeta sredstva iz EU in drugih držav

# 40 - tekoci odhodki
# 41 - tekoci transferi
# 42 - investicijski odhodki
# 45 - placila sredstev v proračun EU

davcni_prihodki <- filter(splosni, splosni$K2_ID == 70)

davcni_prihodki2015 <- sum(davcni_prihodki$SPS2015, na.rm=TRUE)
davcni_prihodki2016 <- sum(davcni_prihodki$SPS2016, na.rm=TRUE)
davcni_prihodki2017 <- sum(davcni_prihodki$SPS2017, na.rm=TRUE)
davcni_prihodki2018 <- sum(davcni_prihodki$SPS2018, na.rm=TRUE)
davcni_prihodki2019 <- sum(davcni_prihodki$SPS2019, na.rm=TRUE)

nedavcni_prihodki <- filter(splosni, splosni$K2_ID == 71)

nedavcni_prihodki2015 <- sum(nedavcni_prihodki$SPS2015, na.rm=TRUE)
nedavcni_prihodki2016 <- sum(nedavcni_prihodki$SPS2016, na.rm=TRUE)
nedavcni_prihodki2017 <- sum(nedavcni_prihodki$SPS2017, na.rm=TRUE)
nedavcni_prihodki2018 <- sum(nedavcni_prihodki$SPS2018, na.rm=TRUE)
nedavcni_prihodki2019 <- sum(nedavcni_prihodki$SPS2019, na.rm=TRUE)

kapitalski_prihodki <- filter(splosni, splosni$K2_ID == 72)

kapitalski_prihodki2015 <- sum(kapitalski_prihodki$SPS2015, na.rm=TRUE)
kapitalski_prihodki2016 <- sum(kapitalski_prihodki$SPS2016, na.rm=TRUE)
kapitalski_prihodki2017 <- sum(kapitalski_prihodki$SPS2017, na.rm=TRUE)
kapitalski_prihodki2018 <- sum(kapitalski_prihodki$SPS2018, na.rm=TRUE)
kapitalski_prihodki2019 <- sum(kapitalski_prihodki$SPS2019, na.rm=TRUE)

prejete_donacije <- filter(splosni, splosni$K2_ID == 73)

prejete_donacije2015 <- sum(prejete_donacije$SPS2015, na.rm=TRUE)
prejete_donacije2016 <- sum(prejete_donacije$SPS2016, na.rm=TRUE)
prejete_donacije2017 <- sum(prejete_donacije$SPS2017, na.rm=TRUE)
prejete_donacije2018 <- sum(prejete_donacije$SPS2018, na.rm=TRUE)
prejete_donacije2019 <- sum(prejete_donacije$SPS2019, na.rm=TRUE)

transferni_prihodki <- filter(splosni, splosni$K2_ID == 74)

transferni_prihodki2015 <- sum(transferni_prihodki$SPS2015, na.rm=TRUE)
transferni_prihodki2016 <- sum(transferni_prihodki$SPS2016, na.rm=TRUE)
transferni_prihodki2017 <- sum(transferni_prihodki$SPS2017, na.rm=TRUE)
transferni_prihodki2018 <- sum(transferni_prihodki$SPS2018, na.rm=TRUE)
transferni_prihodki2019 <- sum(transferni_prihodki$SPS2019, na.rm=TRUE)

EU_prihodki <- filter(splosni, splosni$K2_ID == 78)

EU_prihodki2015 <- sum(EU_prihodki$SPS2015, na.rm=TRUE)
EU_prihodki2016 <- sum(EU_prihodki$SPS2016, na.rm=TRUE)
EU_prihodki2017 <- sum(EU_prihodki$SPS2017, na.rm=TRUE)
EU_prihodki2018 <- sum(EU_prihodki$SPS2018, na.rm=TRUE)
EU_prihodki2019 <- sum(EU_prihodki$SPS2019, na.rm=TRUE)

tekoci_odhodki <- filter(splosni, splosni$K2_ID == 40)

tekoci_odhodki2015 <- sum(tekoci_odhodki$SPS2015, na.rm=TRUE)
tekoci_odhodki2016 <- sum(tekoci_odhodki$SPS2016, na.rm=TRUE)
tekoci_odhodki2017 <- sum(tekoci_odhodki$SPS2017, na.rm=TRUE)
tekoci_odhodki2018 <- sum(tekoci_odhodki$SPS2018, na.rm=TRUE)
tekoci_odhodki2019 <- sum(tekoci_odhodki$SPS2019, na.rm=TRUE)

tekoci_transferi <- filter(splosni, splosni$K2_ID == 41)

tekoci_transferi2015 <- sum(tekoci_transferi$SPS2015, na.rm=TRUE)
tekoci_transferi2016 <- sum(tekoci_transferi$SPS2016, na.rm=TRUE)
tekoci_transferi2017 <- sum(tekoci_transferi$SPS2017, na.rm=TRUE)
tekoci_transferi2018 <- sum(tekoci_transferi$SPS2018, na.rm=TRUE)
tekoci_transferi2019 <- sum(tekoci_transferi$SPS2019, na.rm=TRUE)


investicijski_odhodki <- filter(splosni, splosni$K2_ID == 42)

investicijski_odhodki2015 <- sum(investicijski_odhodki$SPS2015, na.rm=TRUE)
investicijski_odhodki2016 <- sum(investicijski_odhodki$SPS2016, na.rm=TRUE)
investicijski_odhodki2017 <- sum(investicijski_odhodki$SPS2017, na.rm=TRUE)
investicijski_odhodki2018 <- sum(investicijski_odhodki$SPS2018, na.rm=TRUE)
investicijski_odhodki2019 <- sum(investicijski_odhodki$SPS2019, na.rm=TRUE)

EU_placila <- filter(splosni, splosni$K2_ID == 45)

EU_placila2015 <- sum(investicijski_odhodki$SPS2015, na.rm=TRUE)
EU_placila2016 <- sum(EU_placila$SPS2016, na.rm=TRUE)
EU_placila2017 <- sum(EU_placila$SPS2017, na.rm=TRUE)
EU_placila2018 <- sum(EU_placila$SPS2018, na.rm=TRUE)
EU_placila2019 <- sum(EU_placila$SPS2019, na.rm=TRUE)




prilivi2015 <- sum(c(davcni_prihodki2015, nedavcni_prihodki2015, kapitalski_prihodki2015, prejete_donacije2015,
                     transferni_prihodki2015,EU_prihodki2015), na.rm=TRUE) 
prilivi2016 <- sum(c(davcni_prihodki2016, nedavcni_prihodki2016, kapitalski_prihodki2016, prejete_donacije2016,
                     transferni_prihodki2016,EU_prihodki2016), na.rm=TRUE)
prilivi2017 <- sum(c(davcni_prihodki2017, nedavcni_prihodki2017, kapitalski_prihodki2017, prejete_donacije2017,
                     transferni_prihodki2017,EU_prihodki2017), na.rm=TRUE)
prilivi2018 <- sum(c(davcni_prihodki2018, nedavcni_prihodki2018, kapitalski_prihodki2018, prejete_donacije2018,
                     transferni_prihodki2018,EU_prihodki2018), na.rm=TRUE)
prilivi2019 <- sum(c(davcni_prihodki2019, nedavcni_prihodki2019, kapitalski_prihodki2019, prejete_donacije2019,
                     transferni_prihodki2019,EU_prihodki2019),na.rm=TRUE)

odlivi2015 <- sum(c(tekoci_odhodki2015,tekoci_transferi2015, investicijski_odhodki2015, EU_placila2015), na.rm=TRUE)
odlivi2016 <- sum(c(tekoci_odhodki2016,tekoci_transferi2016, investicijski_odhodki2016, EU_placila2016), na.rm=TRUE)
odlivi2017 <- sum(c(tekoci_odhodki2017,tekoci_transferi2017, investicijski_odhodki2017, EU_placila2017), na.rm=TRUE)
odlivi2018 <- sum(c(tekoci_odhodki2018,tekoci_transferi2018, investicijski_odhodki2018, EU_placila2018), na.rm=TRUE)
odlivi2019 <- sum(c(tekoci_odhodki2019,tekoci_transferi2019, investicijski_odhodki2019, EU_placila2019), na.rm=TRUE)



deficit2015 <- prilivi2015 - odlivi2015
deficit2016 <- prilivi2016 - odlivi2016
deficit2017 <- prilivi2017 - odlivi2017
deficit2018 <- prilivi2018 - odlivi2018
deficit2019 <- prilivi2019 - odlivi2019

proracun_po_letih <- data.frame(row.names = c("Davčni prihodki", "Nedavčni prihodki", "Kapitalski prihodki", 
                                              "Prejete donacije", "Transferni prihodki", "Prihodki iz Evropske Unije",
                                              "Tekoci odhodki", "Tekoci transferi", "Investicijski odhodki", "Placila v proračun Evropske Unije",
                                              "Celotni prihodki", "Celotni odhodki", "Deficit"),
                                "SPS2015" = c(davcni_prihodki2015, nedavcni_prihodki2015, kapitalski_prihodki2015, prejete_donacije2015,
                                           transferni_prihodki2015,EU_prihodki2015,tekoci_odhodki2015,tekoci_transferi2015,
                                           investicijski_odhodki2015, EU_placila2015, prilivi2015, odlivi_2015, deficit2015))

proracun_po_letih$"SPS2016" <- c(davcni_prihodki2016, nedavcni_prihodki2016, kapitalski_prihodki2016, prejete_donacije2016,
                              transferni_prihodki2016,EU_prihodki2016,tekoci_odhodki2016,tekoci_transferi2016,
                              investicijski_odhodki2016, EU_placila2016, prilivi2016, odlivi_2016, deficit2016)

proracun_po_letih$"SPS2017" <- c(davcni_prihodki2017, nedavcni_prihodki2017, kapitalski_prihodki2017, prejete_donacije2017,
                                 transferni_prihodki2017,EU_prihodki2017,tekoci_odhodki2017,tekoci_transferi2017,
                                 investicijski_odhodki2017, EU_placila2017, prilivi2017, odlivi_2017, deficit2017)

proracun_po_letih$"SPS2018" <- c(davcni_prihodki2018, nedavcni_prihodki2018, kapitalski_prihodki2018, prejete_donacije2018,
                                 transferni_prihodki2018, EU_prihodki2018,tekoci_odhodki2018,tekoci_transferi2018,
                                 investicijski_odhodki2018, EU_placila2018, prilivi2018, odlivi_2018, deficit2018)

proracun_po_letih$"SPS2019" <- c(davcni_prihodki2019, nedavcni_prihodki2019, kapitalski_prihodki2019, prejete_donacije2019,
                                 transferni_prihodki2019,EU_prihodki2019,tekoci_odhodki2019,tekoci_transferi2019,
                                 investicijski_odhodki2019, EU_placila2019, prilivi2019, odlivi_2019, deficit2019)


"K4_ID = 4010 <- pokojnine v tekočih odhodkih"
odlivi_po_letih <- c(odlivi_2015, odlivi_2016, odlivi_2017, odlivi2018, odlivi_2019)
pokojnine_po_letih <- filter(tekoci_odhodki, tekoci_odhodki$K4_ID == 4010) %>% select(9:13)
"pokojnine_po_letih <- pokojnine_po_letih / c(odlivi2015, odlivi2016, odlivi2017, odlivi2018, odlivi2019)"


splosni$pro_2016 <- (round((splosni$SPS2016)/(splosni$SPS2015), 4) -1) * 100 
splosni$pro_2017 <- (round((splosni$SPS2017)/(splosni$SPS2016), 4) -1) * 100
splosni$pro_2018 <- (round((splosni$SPS2018)/(splosni$SPS2017), 4) -1) * 100
splosni$pro_2019 <- (round((splosni$SPS2019)/(splosni$SPS2018), 4) -1) * 100
splosni <- splosni[,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 11, 15, 12, 16, 13, 17)]
write.csv(splosni, file = "podatki/splosni.csv")

write.csv(prilivi, file = "podatki/prilivi.csv")
write.csv(odlivi, file = "podatki/odlivi.csv")

############################################################################
uvozi.demografijo <- function() {
  data<- read_csv("podatki/demografija.csv",
                  col_names = c("Država", "Leta 0-14 (%)", "Leta 15-64 (%)", "Leta 64+ (%)"),
                  locale = locale(decimal_mark = ".", encoding = "Utf-8"),
                  skip = 2)#%>% mutate("Leta 0-14 (%)" = parse_double(as.character("Leta 0-14 (%)")))
  return(data)}

demografija <- uvozi.demografijo()
  
#Procent ljudi nad 50, ki dobivajo pokojnino
procentualno_pokojnina <- read_csv("podatki/procentualno_pokojnina.csv")
prvo_leto_pokojnine <- read_csv("podatki/prvo_leto_pokojnine.csv")

############################################################################
library(readxl)
drzavni_odhodki <- read_excel("podatki/drzavni_odhodki.xls", na = ":", skip= 3, n_max = 44)
drzavni_prihodki <- read_excel("podatki/drzavni_prihodki.xls", na = ":", skip = 2, n_max = 41)
############################################################################

uvozi.pokojnine_BDP <- function() {
  data<- read_csv("podatki/pokojnine_GDP.csv",
                  col_names = c("Leto","Država","SPDEPB","SPDEPM","UNIT","Skupaj","Flag and Footnotes"),
                  locale = locale(decimal_mark = ".", encoding = "Utf-8"),
                  skip = 1)
  return(data)}

pokojnine_BDP <- uvozi.pokojnine_BDP()
pokojnine_BDP <- pokojnine_BDP[,c(1, 2, 6)]

uvozi.pokojnine_NOMINALNO <- function() {
  data<- read_csv("podatki/pokojnine_NOMINALNO.csv",
                  col_names = c("Leto","Država","SPDEPB","SPDEPM","UNIT","Skupaj","Flag and Footnotes"),
                  locale = locale(decimal_mark = ".", encoding = "Utf-8"),
                  skip = 1)
  return(data)}

pokojnine_NOMINALNO <- uvozi.pokojnine_NOMINALNO()
pokojnine_NOMINALNO <- pokojnine_NOMINALNO[,c(1, 2, 6)]
##########################################################################

graf_pokojnine_BDP <- ggplot(data=pokojnine_BDP, aes(x=pokojnine_BDP$Leto, y=pokojnine_BDP$Skupaj, fill=pokojnine_BDP$Država)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(),
           size =.3) +
  xlab("Država") + ylab("% BDP") +labs(fill = "Kategorija") +
  ggtitle("Pokojnine v državah v % BDP")

plot_pokojnine_BDP <- ggplotly(graf_pokojnine_BDP)

########################################################################
graf_pokojnine_NOMINALNO <- ggplot(data=pokojnine_NOMINALNO, aes(x=pokojnine_NOMINALNO$Leto, y=pokojnine_NOMINALNO$Skupaj, fill=pokojnine_NOMINALNO$Država)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(),
           size =.3) +
  xlab("Država") + ylab("Skupaj") +labs(fill = "Kategorija") +
  ggtitle("Pokojnine nominalno v €")

plot_pokojnine_NOMINALNO <- ggplotly(graf_pokojnine_NOMINALNO)
