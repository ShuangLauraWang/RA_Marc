# Date: Oct 10, 2018
# Author: Shuang Wang
# Institution: Department of Economics, Boston University

rm(list = ls()) # clear worksapce
cat("\014") #clear the console

########################### import packages ########################### 

library(xtable)
library(parallel)

########################### set directory ############################## 

datawd <- '/Users/Laura_S/Dropbox/RapsonRysman_EVs/Data/'
codewd <- '/Users/Laura_S/OneDrive - Boston University/RA_Marc/Code/'
outputwd <- '/Users/Laura_S/OneDrive - Boston University/RA_Marc/Output/'

########################### import functions ########################### 

source(paste0(codewd, "functions.R"))

########################### import data ################################ 

time.span <- 2012 : 2017
data.auto <- GetData(wd = datawd, year = time.span)

########################### trim data ################################## 

sales.vol.sum <- SalesVolumnSum(x = "Transaction_count", 
                                by = c("MAKE_NAME", "MODEL_NAME","DMV_YEAR"), 
                                data = data.auto)

cutoff <- ave(sales.vol.sum$Transaction_count, sales.vol.sum$DMV_YEAR,
              FUN = function(x){
                       q <- quantile(x, probs = 0.75)
               })

keep <- sales.vol.sum[sales.vol.sum$Transaction_count > cutoff, 
                      colnames(sales.vol.sum) != "Transaction_count"]


########################### merge income distribution ###################

# load estimated income distribution
load(paste0(outputwd, "est.income.dist.RData"))

data.auto$TRACT <- paste0(0, data.auto$TRACT)
data.auto <- data.auto[data.auto$TRACT %in% unique(est.income.dist$TRACT), ]


# simulate from income distribution
R <- 100
Y <- length(time.span)
NT <- nrow(est.income.dist)
income.temp <- mapply(rlnorm, meanlog = est.income.dist$meanlog, sdlog = est.income.dist$sdlog, MoreArgs = list(n = R * Y))
k <- kronecker(1 : Y, matrix(1, nrow = R, ncol = NT))
income <- do.call(cbind, lapply(split(income.temp, k), matrix, nrow = R))

consumer <- data.frame(TRACT = rep(est.income.dist$TRACT, times = Y),
                       DMV_YEAR = rep(time.span, each = NT))
consumer$income <- t(income)


# merge simulated income with automobile data
data.est <- merge(data.auto, consumer, 
                  by = c("TRACT", "DMV_YEAR"), 
                  all.x = T)


########################### estimation ###################


data.est$EV <- data.est$MOTIVE_POWER != "Conventional"

# observed market share
MKT_SHARE <- data.est$Transaction_count/
        ave(data.est$Transaction_count, data.est$TRACT, data.est$DMV_YEAR, FUN = sum)


argnames <- c("income*price", "odometer", "price", "EV")
scale <- setNames(c(1e-11, 1e-06, 1e-05, 1e-02), argnames)

# set up parallel computing
no_cores <- detectCores() - 1
cluster <- makeCluster(no_cores)

fit <- optim(par = setNames(rep(0, 4), argnames),
             fn = obj.fn,
             x = c("Odometer_ave", "Trans_price_mean", "EV"), 
             data = data.est,
             control = list(parscale = scale))
fit$par

# Questions: 
# (1)product level, no. of models(MODEL/YEAR/MOTIVE_POWER: 588, MODEL: 355, BRAND: 45), how to aggregate prices & odometer, weighted average? motive_power as category variable? coefficient beta_bar + beta * income + unobservables? only for price
# (2)new purchase? market: year/tract
# (2)outside good; market size?
# (3)balance data;  maybe product dummies
# (4)least square: matching the observed no. of purchase with the predicted ones
# focus a subset of products:
# Table: model/year: no. of sales(across tracts), order by 

