# This file uses "ACS_16_5YR_B19080_with_ann.csv", fit quintiles in census tract 
# a log normal distribution 
# Date: Sep 5, 2018
# Author: Shuang Wang
# Institution: Department of Economics, Boston University



rm(list = ls()) # clear worksapce
cat("\014") #clear the console

###########################    Library Packages     #################################


#install.packages("alabama")
library(alabama)


###########################    Import Data     #################################

setwd("~/OneDrive - Boston University/RA_Marc/Data")

data.quintile <- read.csv(file = "ACS_16_5YR_B19080/ACS_16_5YR_B19080_with_ann.csv", header = TRUE, sep = ",")
data.population <- read.csv(file = "ACS_16_5YR_B10063/ACS_16_5YR_B10063_with_ann.csv", header = TRUE, sep = ",")

#delete the description row
data.quintile <- data.quintile[-1, ] 
data.population <- data.population[-1, ] 


keep.quintile <- c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD02", 
                   "HD01_VD03", "HD01_VD04", "HD01_VD05", "HD01_VD06")

data.quintile <- data.quintile[, keep.quintile]
colnames(data.quintile) <- c("GEO_id", "GEO_id2", "Geography", "Quintile1", 
                             "Quintile2", "Quintile3", "Quintile4", "Top5")

keep.population <- c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD01")

data.population <- data.population[, keep.population]
colnames(data.population) <- c("GEO_id", "GEO_id2", "Geography", "Population")

data <- merge(data.quintile, data.population, by = c("GEO_id", "GEO_id2", "Geography"))

# delete tracts without observed quantiles (84 tracts deleted)
data<- data[apply(data, 1, FUN = function(x) sum(x %in% c("-", "**")) < 5), ] 

row.names(data) <- 1 : nrow(data)


##########      Summary     ####################

# find tracts with "2,500-" or "250,000+"

top.coded <- apply(as.matrix(data), 1, function(x){sum(x %in% c("2,500-", "250,000+")) })


##########      Fit each row a log normal distribution     ####################


# functions

obj.fn <- function(params, q){
        # This function is the objective function for log normal distribution fit
        # 
        # args: (1)params: meanlog & sdlog of a log normal
        #       (2)q: quantiles to be fitted
        #       (3)weights: weights for each quantile
        #       
        # output:  sum of squares of the differences between the 
        #          observed quantiles and the predicted ones     
        mu <- params[1]
        sigma <- params[2]
        
        p <- p[!is.na(q)]
        q <- q[!is.na(q)]
        
        diff <- q - qlnorm(p, mu, sigma)
        
        obj.value <- sum(diff^2)
        
        return(obj.value)
}

optim.fit.constr <- function(x){
        
        # This function distinguishes two cases:
        # (1) 2 or less quantiles are observed, bad empirical fits, return NA's
        # (2) more than 2 quantiles are observed, estimate with auglag(), which 
        # allows inequality constraints.
        # 
        # args: (1)x: each row of data
        #       
        # output: optimal logmean and sdmeand of a log normal distribution
        
        lb <- max(which(x == "2,500-"))
        ub <- min(which(x == "250,000+"))
        
        x <- as.numeric(x)
        
        if (lb >= 3 | ub <= 3 | sum(!is.na(x)) < 2)
                return(c(NA, NA))
        else{
                
                hin <- function(params, q){
                        # inequality constrainsts
                        
                        mu <- params[1]
                        sigma <- params[2]
                        
                        h <- rep(NA,1)
                        
                        i <- 1
                        
                        if (lb > -Inf){
                                h[i] <- 2500 - qlnorm(p[lb], mu, sigma)
                                i <- i + 1
                        }
                        if (ub < Inf)        
                                h[i] <- qlnorm(p[ub], mu, sigma) - 250000
                }
                
                return(auglag(par = c(0, 1), 
                              fn = obj.fn, hin = hin, q = x, 
                              control.outer = list(trace = F))$par)
                
        }
}

# cut values for quantiles
p <- c(seq(from = 0.2, to = 0.8, by = 0.2), 0.95)

output.constr <- apply(data[, c("Quintile1", "Quintile2", "Quintile3", "Quintile4", "Top5")], 1, FUN = optim.fit.constr)

# output summary
apply(output.constr, 1, summary)


##########      Construct California Income Distribution      ##################

rincome <- function(N){
        # This function takes random draws from the estimated track-level 
        # log normal distributions, each tract is weighted by population
        
        # args: (1)N: number of draws
        # 
        # output: random draws
        # 
        tracts <- sample(1 : nrow(data), N, replace = T, prob = data$Population)
        apply(output.constr[, tracts], 2, 
              FUN = function(x){
                      rlnorm(1, meanlog = x[1], sdlog = x[2])
              })
}


# eliminate incompete cases (NA's)
data <- data[complete.cases(t(output.constr)),]
output.constr <- output.constr[ , complete.cases(t(output.constr))]

income.sample <- rincome(100000)

x <- seq(0, 5e5, length=1000)
which.min <- which(output.constr[1, ] == min(output.constr[1, ], na.rm = T))
which.max <- which(output.constr[1, ] == max(output.constr[1, ], na.rm = T))

which.min.var <- which(output.constr[2, ] == min(output.constr[2, ], na.rm = T))
which.max.var <- which(output.constr[2, ] == max(output.constr[2, ], na.rm = T))


# California Income Distribution
plot(density(income.sample, from = 0 , to = 5e5), 
     main = "California Income Distribution",
     lwd = 2,
     ylim = c(0, 2e-5),
     xlab = "Income")
# Income Distribution of the tract with the lowest mean
lines(x, 
      dlnorm(x, output.constr[1, which.min], output.constr[2, which.min]), 
      col = "lightblue", 
      lwd = 2)
# Income Distribution of the tract with the highes mean
lines(x, 
      dlnorm(x, output.constr[1, which.max], output.constr[2, which.max]), 
      col = "orange", 
      lwd = 2)
lines(x, 
      dlnorm(x, output.constr[1, which.min.var], output.constr[2, which.min.var]), 
      col = "blue", 
      lwd = 2)
lines(x, 
      dlnorm(x, output.constr[1, which.max.var], output.constr[2, which.max.var]), 
      col = "red", 
      lwd = 2)
legend("topright", 
       legend = c("Total", "Min.mean", "Max.mean", "Min.var", "Max.var"), 
       col = c("black", "lightblue", "orange", "blue", "red"),
       lwd = 2)