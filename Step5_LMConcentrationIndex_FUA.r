####################################################################################################
#################Program for calculating a Labour Market Concentration Index from CEDEFOP OJA data
#####################################################################################################
#The geographical unit for this index is the Functional Urban Area (FUA), as defined by Eurostat

# Copyright 2020 DESTATIS
#  
# Licensed under the EUPL, Version 1.2 or – as soon as they will be approved by the European Commission - subsequent versions of the EUPL (the "Licence");
# You may not use this work except in compliance with the Licence.
# You may obtain a copy of the Licence at:
#  *https://joinup.ec.europa.eu/software/page/eupl5
#  
# Unless required by applicable law or agreed to inwriting, software distributed under the Licence is distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the Licence for the specific language governing permissions and limitations under the Licence.


#adapt paths according to your own file structure

path <- "alldata_june20/"
resultspath <- "Results/alldata_june20/"


options(scipen = 999)

library(Hmisc)
library(data.table)
library(dplyr)
library(tidyr)
library(openxlsx)
library(hhi)
library(sf)
library(stringi)
library(fst)
library(readxl)

rm(list = ls())

# load the data and select the relevant subset

dframe <- read_fst(paste0(path,"OJVsample_step1_redux.fst"), c("general_id", "grab_date", "expire_date", "idsector", "qtr", "idesco_level_4", "idregion", "companyname", "idprovince" ), as.data.table = TRUE)

dframe <- dframe[startsWith(dframe$idregion, "DE"), ]

# add an industry sector coding which is consistent with that used by the German federal employment agency

nace <- read_excel("NACE.xls")
nace$charsec <- as.character(nace$charsec)

#distinguish between letter and number codes for sectors in CEDEFOP data:

dframe$numsec <- as.numeric(dframe$idsector)
dframe$charsec <- dframe$idsector
dframe$charsec[!is.na(dframe$numsec)] <- NA

#merge the iab classification to both letter and number codes

dframe <- left_join(x = dframe, y = subset(nace, select = c(-charsec)), by = "numsec")

#change the nace table to eliminate duplicates in charsec
colnames(nace) <- c("naceiab_char",     "charsec"  ,   "numsec"    ,  "Bezeichnung_char")
nace <- unique(subset(nace, select = -numsec))

dframe <-left_join(x = dframe, y = nace , by = "charsec")

dframe$naceiab[is.na(dframe$naceiab)] <- dframe$naceiab_char[is.na(dframe$naceiab)]
dframe$Bezeichnung[is.na(dframe$Bezeichnung)] <- dframe$Bezeichnung_char[is.na(dframe$Bezeichnung)]

dframe <- subset(dframe, select = c(-naceiab_char, -numsec, -charsec, -Bezeichnung_char))
setDT(dframe)

# recode missings to NA

dframe$companyname[dframe$companyname == ""] <- NA

# replace all missing company names with unique strings
# this turned out to be too arbitrary, replaced with median imputation
#
# no <- seq_len(length(dframe$companyname))
# no <- paste0("missing",no)
#
# dframe$companyname[dframe$companyname==""] <- no[dframe$companyname==""]
#
# rm(no)
#

# median imputation
# count unique obs per occupation, region, quarter, company (preliminary, only for imputation)
dframe[, ccount := .N, by = list(idesco_level_4, idregion, qtr, companyname)]

dframe[is.na(dframe$companyname),]$ccount <- NA

med <- median(dframe$ccount, na.rm = TRUE)

dframe <- dframe[order(dframe$idregion, dframe$qtr, dframe$idesco_level_4, dframe$companyname), ]

dframe[, imp := as.integer(rep((1:.N), each = med, length.out = .N)), by = list(idesco_level_4, idregion, qtr)]

dframe$imp <- paste0("missing", dframe$imp)

dframe$companyname[is.na(dframe$companyname)] <- dframe$imp[is.na(dframe$companyname)]


# read in the shapefiles ======================================

nuts <- st_read("Auxiliary data/NUTS_RG_10M_2016_3035.shp")
de <- nuts[nuts$CNTR_CODE == "DE" & nuts$LEVL_CODE==0,]

sfile <- st_read("Auxiliary data/CommutingZonesShapefile/Germany.shp")

# keep only xy dimensions
sflile <- st_zm(sfile, drop = TRUE, what = "ZM")

sfile <- subset(sfile, select =  -c(class_code, iso3, name))

sfile$fuacode_si <- as.character(sfile$fuacode_si)

st_geometry(sfile) <- sfile$geometry

# merge FUA data ====================================

#keep only obs with nuts non-missing

dframe <- subset(dframe, !is.na(idprovince))

#fua <- read.csv("Auxiliary data/kreise_zu_FUAs_raeumlich_verbindung.csv", sep = ";", stringsAsFactors = FALSE)
fua <- read_excel("Auxiliary data/Kreise_zu_FUAs_raumlich_Verbindung_modified.xlsx")

fua <- subset(fua, select= c("fuacode_si", "NUTS"))

colnames(fua) <- c("fuacode_si", "idprovince")
dframe <- left_join(dframe,fua)

length(unique(dframe$fuacode_si))

describe(dframe$fuacode_si)

dframe <- dframe[!is.na(dframe$fuacode_si),]



# Calculate the Herfindahl Hirschman Index =============

# compute market shares by quarter, FUA and esco level 4 occupation

# create grid of occupation, geo unit and quarter

grid <- expand.grid(esco = unique(dframe$idesco_level_4), geo = unique(dframe$fuacode_si), qtr = unique(dframe$qtr), stringsAsFactors = FALSE)

# count obs per occupation, region, quarter
setDT(dframe)
dframe <- select(dframe, -c("ccount", "imp"))

dframe[, ncount := .N, by = list(idesco_level_4, fuacode_si, qtr)]

# count obs per occupation, region, quarter, company
dframe[, ccount := .N, by = list(idesco_level_4, fuacode_si, qtr, companyname)]

#market shares
dframe$mshare <- ((dframe$ccount) / (dframe$ncount)) * 100

dframe$ms2 <- (dframe$mshare)^2

hhi <- data.frame()

for (i in 1:dim(grid)[1]) {
  # count obs per cell and company
  subset <- unique(dframe[idesco_level_4 == grid[i, 1] & fuacode_si == grid[i, 2] & qtr == grid[i, 3], c("idesco_level_4", "fuacode_si", "qtr", "mshare", "ms2", "companyname", "ncount"), with = FALSE])

  subset$hhi <- sum(subset$ms2)

  subset <- subset[1, !c("companyname") ]

  hhi <- rbind(hhi, subset)
}

save(hhi, file = "HHI_data_FUA.rdata")

# load(file = "HHI_data_FUA.rdata")

hhi <- na.omit(hhi)


totalmean <- mean(hhi$hhi)
totalmean

totalmedian <- median(hhi$hhi)
totalmedian  

describe(hhi$hhi)

ecdf(hhi$hhi)(2500) 

hhi <- hhi[, .(idesco_level_4, mshare, ms2, ncount, hhi, wmean = mean(hhi)), by = list(fuacode_si, qtr) ]

hhigeo <- unique(hhi[, c("fuacode_si", "qtr", "wmean")])

hhigeo <- data.table(left_join(hhigeo, sfile, by = "fuacode_si"))

hhigeo$fuaname <- as.character(hhigeo$fuaname)
hhigeo$fuaname_en <- as.character(hhigeo$fuaname_en)


hhigeo$wmean <- round(hhigeo$wmean)

st_geometry(hhigeo) <- hhigeo$geometry

hhigeo <- st_zm(hhigeo, drop = TRUE, what = "ZM")

hhigeo_q1 <- subset(hhigeo, qtr == "2018-q3")
hhigeo_q1$label <- paste0(hhigeo_q1$fuaname_en, "\n ", as.character(hhigeo_q1$wmean))

hhigeo_q2 <- subset(hhigeo, qtr == "2018-q4")
hhigeo_q2$label <- paste0(hhigeo_q2$fuaname_en, "\n ", as.character(hhigeo_q2$wmean))

hhigeo_q3 <- subset(hhigeo, qtr == "2019-q1")
hhigeo_q3$label <- paste0(hhigeo_q3$fuaname_en, "\n ", as.character(hhigeo_q3$wmean))

# Graphs ===========

ggplot(hhigeo_q1) +
  geom_sf( aes(fill = wmean)) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title = "Labour market concentration index Q3-2018\naverage over all occupations") +
  scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
  geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
  geom_sf(data=de,alpha = 0)

ggsave("Results/HHI_q32018_fua.png", width = 15, height = 10, units = "cm")


ggplot(hhigeo_q2) +
  geom_sf(aes(fill = wmean)) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title = "Labour market concentration index Q4-2018\naverage over all occupations") +
  scale_fill_continuous(name = "Labour market concentration index", low="blue", high="orange") +
  geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
  geom_sf(data=de,alpha = 0)

ggsave("Results/HHI_q42018_fua.png", width = 15, height = 10, units = "cm")


ggplot(hhigeo_q3) +
  geom_sf(aes(fill = wmean)) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title = "Labour market concentration index Q1-2019\naverage over all occupations") +
  scale_fill_continuous(name = "Labour market concentration index", low="blue", high="orange") +
  geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
  geom_sf(data=de,alpha = 0)

ggsave("Results/HHI_q12019_fua.png", width = 15, height = 10, units = "cm")


# HHI tables by region --------------------------


table <- data.frame(cbind(hhigeo_q1$fuacode_si, hhigeo_q1$fuaname_en, hhigeo_q1$wmean, hhigeo_q2$wmean, hhigeo_q3$wmean))

table <- table[!is.na(table[,2]),]

colnames(table) <- c("FUA", "Name", "Avg. Q3 2018", "Avg. Q4 2018", "Avg. Q1 2019" )

write.xlsx(table,
           file = "Results/HHI_FUA.xlsx", sheetName = "Sheet1",
           col.names = TRUE, append = FALSE
)

# in German ------------------------

table <- data.frame(cbind(hhigeo_q1$fuacode_si, hhigeo_q1$fuaname, hhigeo_q1$wmean, hhigeo_q2$wmean, hhigeo_q3$wmean))

table <- table[!is.na(table[,2]),]

colnames(table) <- c("FUA", "Name", "Avg. Q3 2018", "Avg. Q4 2018", "Avg. Q1 2019" )

write.xlsx(table,
           file = "Results/HHI_FUA_de.xlsx", sheetName = "Sheet1",
           col.names = TRUE, append = FALSE
)


############### Average HHI across all quarters =====================

#hhi_tmean <- hhi %>% group_by(fuacode_si, idesco_level_4) %>% summarise(totalmean = mean(hhi))


hhi_tmean <- hhi[, .(idesco_level_4, mshare, ms2, ncount, hhi, tmean = mean(hhi)), by = list(fuacode_si) ]

hhigeo_tmean <- unique(hhi_tmean[, c("fuacode_si", "tmean")])

hhigeo_tmean <- data.table(left_join(hhigeo_tmean, sfile, by = "fuacode_si"))

hhigeo_tmean$fuaname <- as.character(hhigeo_tmean$fuaname)
hhigeo_tmean$fuaname_en <- as.character(hhigeo_tmean$fuaname_en)


hhigeo_tmean$tmean <- round(hhigeo_tmean$tmean)

st_geometry(hhigeo_tmean) <- hhigeo_tmean$geometry

hhigeo_tmean <- st_zm(hhigeo_tmean, drop = TRUE, what = "ZM")

hhigeo_tmean$label <- paste0(hhigeo_tmean$fuaname_en, "\n ", as.character(hhigeo_tmean$tmean))

test <- hhigeo_tmean[is.na(hhigeo_tmean$fuaname),]

ggplot(hhigeo_tmean) +
  geom_sf( aes(fill = tmean)) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title = "Labour market concentration index 07.2018 - 03.2019\naverage over occupations and quarters") +
  scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
  geom_sf_text(aes(label = fuaname_en), size = 2.5, colour = "black")+
  geom_sf(data=de,alpha = 0)

ggsave("Results/maps/HHI_avgfrom_q32018_toq12019_fua.png", width = 20, height = 13.3, units = "cm")

# German #

ggplot(hhigeo_tmean) +
  geom_sf( aes(fill = tmean)) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title = "Arbeitsmarkt-konzentrations Index 07.2018 - 03.2019\nDurchschnitt über alle Berufe") +
  scale_fill_continuous(name = "Arbeitsmarkt-\nkonzentrations Index",low="blue", high="orange") +
  geom_sf_text(aes(label = fuaname), size = 2.5, colour = "black")+
  geom_sf(data=de,alpha = 0)

ggsave("Results/maps/HHI_avgfrom_q32018_toq12019_fua_de.png", width = 20, height = 13.3, units = "cm")

