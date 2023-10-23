library(readxl)
library(stringr)
library(data.table)
library(tidytable)
library(dplyr)

#a <- read_excel("C:/Users/mberl/Desktop/Berlino/3G/Backup Drive/Backup CAGO 11-08-2023/3G_CAGO_APR_A7_Estrazione.xlsx")
#excel_sheets("C:/Users/mberl/Desktop/Berlino/3G/Backup Drive/Backup CAGO 11-08-2023/3G_CAGO_APR_A7_Estrazione.xlsx")
#
#a <- read_excel("C:/Users/mberl/Desktop/Berlino/3G/Backup Drive/Backup CAGO 11-08-2023/3G_CAGO_APR_A7_Estrazione.xlsx", sheet= "CAGO_Apr_A7_PAGEERY",range = cell_cols("A:U"))
#str(a)
#a <- as.data.frame(a)
#str(a)

#vector of file names
files <- list.files(path = "C:/Users/mberl/Desktop/Berlino/3G/Backup Drive/Backup CAGO 11-08-2023/", pattern=".xlsx")
files
class(files)
# Concat the directory to the file names
files <- str_c("C:/Users/mberl/Desktop/Berlino/3G/Backup Drive/Backup CAGO 11-08-2023/", files)

#provide names to each element of the vector
names(files) <- c("File1","File2","File3","File4","File5","File6","File7","File8","File9","File10",
"File11","File12","File13","File14","File15","File16","File17","File18","File19","File20",
"File21","File22","File23","File24","File25","File26","File27","File28","File29","File30",
"File31","File32","File33","File34","File35","File36","File37","File38","File39","File40",
"File41","File42","File43","File44","File45","File46","File47","File48","File49","File50",
"File51","File52","File53","File54","File55","File56","File57","File58","File59","File60",
"File61","File62","File63","File64","File65","File66","File67")
files

files <- as.data.frame(files)
files <- files[-c(44,43),]
files <- as.data.frame(files)
files <- files[-43,]
files <- as.data.frame(files)
files <- files[-14,]
files <- as.data.frame(files)
files <- files[-32,]

# apply a function to each element of the vector
data <- map_df(.x=files, .f = read_xlsx,.id = "data.source")
data <- filter(data,!Trawl=="NA")
data2 <- map_df(.x=files, .f = read_xlsx, sheet= 2,.id = "data.source")
data2 <- filter(data2,!Trawl=="NA")


