library(pdftools)
library(plyr)
library(dplyr)
library(readr)

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

splosni$pro_2016 <- (round((splosni$SPS2016)/(splosni$SPS2015), 4) -1) * 100 
splosni$pro_2017 <- (round((splosni$SPS2017)/(splosni$SPS2016), 4) -1) * 100
splosni$pro_2018 <- (round((splosni$SPS2018)/(splosni$SPS2017), 4) -1) * 100
splosni$pro_2019 <- (round((splosni$SPS2019)/(splosni$SPS2018), 4) -1) * 100
splosni <- splosni[,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 11, 15, 12, 16, 13, 17)]
write.csv(splosni, file = "podatki/splosni.csv")


prilivi <- filter(splosni, splosni$K3_ID >= 700)
odlivi <- filter(splosni, splosni$K3_ID < 700)

vsota_prilivi2015 <- sum(prilivi$SPS2015, na.rm=TRUE)
vsota_prilivi2016 <- sum(prilivi$SPS2016, na.rm=TRUE)
vsota_prilivi2017 <- sum(prilivi$SPS2017, na.rm=TRUE)
vsota_prilivi2018 <- sum(prilivi$SPS2018, na.rm=TRUE)
vsota_prilivi2019 <- sum(prilivi$SPS2019, na.rm=TRUE)

vsota_odlivi2015 <- sum(odlivi$SPS2015, na.rm=TRUE)
vsota_odlivi2016 <- sum(odlivi$SPS2016, na.rm=TRUE)
vsota_odlivi2017 <- sum(odlivi$SPS2017, na.rm=TRUE)
vsota_odlivi2018 <- sum(odlivi$SPS2018, na.rm=TRUE)
vsota_odlivi2019 <- sum(odlivi$SPS2019, na.rm=TRUE)

deficit2015 <- vsota_prilivi2015 - vsota_odlivi2015
deficit2016 <- vsota_prilivi2016 - vsota_odlivi2016
deficit2017 <- vsota_prilivi2017 - vsota_odlivi2017
deficit2018 <- vsota_prilivi2018 - vsota_odlivi2018
deficit2019 <- vsota_prilivi2019 - vsota_odlivi2019

write.csv(prilivi, file = "podatki/prilivi.csv")
write.csv(odlivi, file = "podatki/odlivi.csv")