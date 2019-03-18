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
write.csv(splosni, file = "podatki/splosni.csv")

posebni <- full_join(pos_2015, pos_2016) 
posebni <- full_join(posebni, pos_2017)
posebni <- full_join(posebni, pos_2018) 
posebni <- full_join(posebni, pos_2019) 
write.csv(posebni, file = "podatki/posebni.csv")


