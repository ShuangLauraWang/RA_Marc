# Date: Oct 10, 2018
# Author: Shuang Wang
# Institution: Department of Economics, Boston University

rm(list = ls()) # clear worksapce
cat("\014") #clear the console

wd <- '/Users/Laura_S/Dropbox/RapsonRysman_EVs/Data'
setwd(wd)

data2012.org <- read.csv(file = "DMV/MMMyr_Tract_2012.csv", header = TRUE, sep = ",")
data2012 <- data2012.org[complete.cases(data2012.org), ]
data2012 <- data2012[order(data2012$TRACT), ]
data2012$MKT_SIZE_T <- ave(data2012$Transaction_count, data2012$TRACT, FUN = sum)
data2012$MKT_SIZE_R <- ave(data2012$registration_counts, data2012$TRACT, FUN = sum)
data2012$MKT_SHARE <- data2012$Transaction_count/data2012$MKT_SIZE_T