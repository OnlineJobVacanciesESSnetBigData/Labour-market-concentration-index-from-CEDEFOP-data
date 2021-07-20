####################################################################################################
#################Program for calculating a Labour Market Concentration Index from CEDEFOP OJA data
#####################################################################################################
#The geographical unit for this index is the Functional Urban Area (FUA), as defined by Eurostat

#adapt paths according to your own file structure

path <- "Data/alldata_12_2020/"
resultspath <- "Results/alldata_12_2020/LMC/"

dir.create(resultspath, showWarnings = FALSE)

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


# load the data and select the relevant subset

years <- c(2018, 2019, 2020)

dframe <- data.table()

for (year in years) { 
  tmp <- read.fst(paste0(path,"OJVsample_step1_redux_", year, ".fst"), as.data.table = TRUE, columns = c("general_id", "grab_date", "expire_date", "idsector", "qtr", "idesco_level_4", "idregion", "companyname", "idprovince" ))
  dframe <- rbind(dframe, tmp)
  
}

rm(tmp)
setDT(dframe)

dframe <- dframe[startsWith(dframe$idregion, "DE"), ]

# recode missings to NA

dframe$companyname[dframe$companyname == ""] <- NA

# median imputation
# count unique obs per occupation, region, quarter, company (preliminary, only for imputation)
dframe[, ccount := .N, by = list(idesco_level_4, idregion, qtr, companyname)]

dframe[is.na(dframe$companyname),]$ccount <- NA

med <- median(dframe$ccount, na.rm = TRUE)

dframe <- dframe[order(dframe$idregion, dframe$qtr, dframe$idesco_level_4, dframe$companyname), ]

dframe[, imp := as.integer(rep((1:.N), each = med, length.out = .N)), by = list(idesco_level_4, idregion, qtr)]

dframe$imp <- paste0("missing", dframe$imp)

dframe$companyname[is.na(dframe$companyname)] <- dframe$imp[is.na(dframe$companyname)]

describe(dframe$companyname)

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

describe(dframe$idprovince)

dframe <- subset(dframe, !is.na(idprovince))

fua <- read_excel("Auxiliary data/Kreise_zu_FUAs_raumlich_Verbindung_v2.xlsx")

fua <- subset(fua, select= c("fuacode_si", "NUTS"))

colnames(fua) <- c("fuacode_si", "idprovince")

dframe <- left_join(dframe,fua)

length(unique(dframe$fuacode_si))

describe(dframe$fuacode_si)

dframe <- dframe[!is.na(dframe$fuacode_si),]

#clean up occupation/region cells with a lack of observations ==========

dframe[, testcount := .N, by = list(idesco_level_4, fuacode_si, qtr)]

dframe <- dframe[testcount > 1]

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

save(hhi, file = paste0(resultspath, "HHI_data_FUA.rdata"))

# load(file = "HHI_data_FUA.rdata")

setDT(hhi)

#results 

describe(hhi)

quarters <- unique(hhi$qtr)

for (i in quarters) {

qhhi <- hhi[qtr == i] 

qhhi <- na.omit(qhhi)

totalmean <- mean(qhhi$hhi)
totalmean

totalmedian <- median(qhhi$hhi)
totalmedian  

q10 <- quantile(qhhi$hhi, 0.1)
q25 <- quantile(qhhi$hhi, 0.25)
q75 <- quantile(qhhi$hhi, 0.75)
q90 <- quantile(qhhi$hhi, 0.90)

#describe(hhi$hhi)

threshold25 <- ecdf(qhhi$hhi)(2500) 
threshold2 <- ecdf(qhhi$hhi)(2000)
threshold1 <- ecdf(qhhi$hhi)(1000) 

min <- min(qhhi$hhi)
max <- max(qhhi$hhi)

resmat <- data.frame(variables = c("mean", "median", "q10", "q25", "q75", "q90", "ecdf1000", "ecdf2000", "ecdf2500", "min", "max"), values = c(totalmean, totalmedian, q10, q25, q75, q90, threshold1, threshold2, threshold25, min, max))
                                   
write.xlsx(resmat, file = paste0(resultspath, "Resultsmatrix_hhi_fua_", i ,".xlsx"), sheetName = "Sheet1", col.names = TRUE, append = FALSE)

}

hhi <- hhi[, .(idesco_level_4, mshare, ms2, ncount, hhi, wmean = mean(hhi)), by = list(fuacode_si, qtr) ]

hhigeo <- unique(hhi[, c("fuacode_si", "qtr", "wmean")])

hhigeo <- data.table(left_join(hhigeo, sfile, by = "fuacode_si"))

hhigeo$fuaname <- as.character(hhigeo$fuaname)
hhigeo$fuaname_en <- as.character(hhigeo$fuaname_en)

hhigeo$wmean <- round(hhigeo$wmean)

st_geometry(hhigeo) <- hhigeo$geometry

hhigeo <- st_zm(hhigeo, drop = TRUE, what = "ZM")

hhigeo$label <- paste0(hhigeo$fuaname_en, "\n ", as.character(hhigeo$wmean))

# Graphs ===========

for (i in quarters) {
  
  tmp <- subset(hhigeo, qtr == i)
  
  ggplot(tmp) +
    geom_sf( aes(fill = wmean)) + theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent")) +
    labs(title = paste0("Labour market concentration index ", i ,"\naverage over all occupations")) +
    scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange", limits = c(0,10000)) +
    geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
    geom_sf(data=de,alpha = 0)
  
  ggsave(paste0(resultspath, "HHI_",i ,"_fua.png"), width = 25, height = 18, units = "cm")
  
}


# HHI tables by region --------------------------

table <- data.frame( fuacode_si = unique(hhigeo$fuacode_si), fuaname_en =unique(hhigeo$fuaname_en))

for (i in quarters) {
  
  tmp <- subset(hhigeo, qtr == i, c("fuacode_si", "wmean"))
  tmp <- st_set_geometry(tmp, NULL)
  
  colnames(tmp) <- c("fuacode_si", paste0("Avg. LMC ",i))
  
  table <- left_join(table, tmp, by="fuacode_si")
  
}
  write.xlsx(table,
           file = paste0(resultspath, "HHI_FUA_en.xlsx"), sheetName = "Sheet1",
           col.names = TRUE, append = FALSE)

# in German ------------------------

  
  table <- data.frame( fuacode_si = unique(hhigeo$fuacode_si), fuaname =unique(hhigeo$fuaname))
  
  for (i in quarters) {
    
    tmp <- subset(hhigeo, qtr == i, c("fuacode_si", "wmean"))
    tmp <- st_set_geometry(tmp, NULL)
    
    colnames(tmp) <- c("fuacode_si", paste0("Durchschn. LMC ",i))
    
    table <- left_join(table, tmp, by="fuacode_si")
    
  }
  write.xlsx(table,
             file = paste0(resultspath, "HHI_FUA_de.xlsx"), sheetName = "Sheet1",
             col.names = TRUE, append = FALSE)

############### Average HHI per year =====================

hhi$year <- as.numeric(substr(hhi$qtr, 1,4))
  
for (i in years) {
    
  tmp <- subset(hhi, year == i)

  hhi_tmean <- tmp[, .(idesco_level_4, mshare, ms2, ncount, hhi, tmean = mean(hhi)), by = list(fuacode_si) ]
  
  hhigeo_tmean <- unique(hhi_tmean[, c("fuacode_si", "tmean")])
  
  hhigeo_tmean <- data.table(left_join(hhigeo_tmean, sfile, by = "fuacode_si"))
  
  hhigeo_tmean$fuaname <- as.character(hhigeo_tmean$fuaname)
  hhigeo_tmean$fuaname_en <- as.character(hhigeo_tmean$fuaname_en)

  hhigeo_tmean$tmean <- round(hhigeo_tmean$tmean)
  
  st_geometry(hhigeo_tmean) <- hhigeo_tmean$geometry
  
  hhigeo_tmean <- st_zm(hhigeo_tmean, drop = TRUE, what = "ZM")
  
  hhigeo_tmean$label <- paste0(hhigeo_tmean$fuaname_en, "\n ", as.character(hhigeo_tmean$tmean))
  
  test <- hhigeo_tmean[is.na(hhigeo_tmean$fuaname),]
  
  savetab <- hhigeo_tmean[,c("fuacode_si", "tmean", "fuaname", "fuaname_en")]
  savetab <- st_set_geometry(savetab, NULL) 
  savetab$year <- i
  
  write.xlsx(savetab, file = paste0(resultspath, "HHI_avg_", i , "_fua.xlsx"), sheetName = "Sheet1", col.names = TRUE, append = FALSE)

  ggplot(hhigeo_tmean) +
    geom_sf( aes(fill = tmean)) + theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent")) +
    labs(title = paste0("Labour market concentration index ", i ,"\naverage over occupations and quarters")) +
    scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
    geom_sf_text(aes(label = fuaname_en), size = 2.5, colour = "black")+
    geom_sf(data=de,alpha = 0)

  ggsave(paste0(resultspath, "HHI_avg_", i , "_fua.png"), width = 20, height = 13.3, units = "cm")
  
  
# German #

  ggplot(hhigeo_tmean) +
    geom_sf( aes(fill = tmean)) + theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent")) +
    labs(title = paste0("Arbeitsmarkt-konzentrations Index ", i ,"\nDurchschnitt über alle Berufe")) +
    scale_fill_continuous(name = "Arbeitsmarkt-\nkonzentrations Index",low="blue", high="orange") +
    geom_sf_text(aes(label = fuaname), size = 2.5, colour = "black")+
    geom_sf(data=de,alpha = 0)

  ggsave(paste0(resultspath, "HHI_avg_", i , "_fua_de.png"), width = 20, height = 13.3, units = "cm")

}
  
rm(dframe)