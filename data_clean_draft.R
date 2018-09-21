# This file uses "ACS_16_5YR_B19080_with_ann.csv", fit quintiles in census tract 
# a log normal distribution 
# Date: Sep 5, 2018
# Author: Shuang Wang
# Institution: Department of Economics, Boston University



rm(list = ls()) # clear worksapce
cat("\014") #clear the console


###########################    Import Data     #################################

setwd("~/OneDrive - Boston University/RA_Marc")

data <- read.csv(file = "ACS_16_5YR_B19080_with_ann.csv", header = TRUE, sep = ",")

#delete the description row
data <- data[-1, ] 

colnames(data) <- c("GEO_id", "GEO_id2", "Geography", "Quintile1", "Quintile1_MoE", "Quintile2", "Quintile2_MoE", "Quintile3", "Quintile3_MoE", "Quintile4", "Quintile4_MoE", "Top5", "Top5_MoE")

keep <- c("Quintile1", "Quintile2", "Quintile3", "Quintile4", "Top5")
data <- data[ , keep]

# delete tracts without observed quantiles
data <- data[apply(data, 1, FUN = function(x) sum(x %in% c("-", "**")) < length(x)), ]

row.names(data) <- 1 : nrow(data)

##########      Fit each row a log normal distribution     ####################


# functions
weights.fn <- function(params, p){
        # This function calculates weights for each quantile 
        # 
        # args: (1)params: meanlog & sdlog of a log normal
        #       (2)p: cut values for quantiles
        #       
        # output: weights for each quantile in optimization
        
        mu <- params[1]
        sigma <- params[2]
        dlnorm(qlnorm(p, mu, sigma), mu, sigma)/p * (1 - p)
}

obj.fn <- function(params, q, weights){
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
        
        weights <- weights[!is.na(q)]
        p <- p[!is.na(q)]
        q <- q[!is.na(q)]
        
        diff <- q - qlnorm(p, mu, sigma)
        
        #weights <- dlnorm(qlnorm(p))^2/(p * (1 - p))
        
        obj.value <- sum(diff^2 * weights)
        
        return(obj.value)
}

optim.fit.constr <- function(x, weights){
        
        # This function distinguishes two cases:
        # (1) 2 or less quantiles are observed, bad empirical fits, return NA's
        # (2) more than 2 quantiles are observed, estimate with auglag(), which 
        # allows inequality constraints.
        # 
        # args: (1)x: each row of data
        #       (2)weights: weight for each quantile
        #       
        # output: optimal logmean and sdmeand of a log normal distribution
        
        lb <- max(which(x == "2,500-"))
        ub <- min(which(x == "250,000+"))
        
        x <- as.numeric(x)
        
        if (lb >= 3 | ub <= 3 | sum(!is.na(x)) < 2)
                return(c(NA, NA))
        else{
                
                hin <- function(params, q, weights){
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
                              fn = obj.fn, hin = hin, q = x, weights = rep(1, 5),
                              control.outer = list(trace = F))$par)
                
        }
}

# cut values for quantiles
p <- c(seq(from = 0.2, to = 0.8, by = 0.2), 0.95)

output.constr.stage1 <- apply(data, 1, 
                              FUN = optim.fit.constr, weights = rep(1, 5))

weights <- apply(output.constr.stage1, 2, weights.fn, p = p)

data.stage2 <- as.matrix(data)

output.constr.stage2 <- mapply(x = split(data.stage2, row(data.stage2)), 
                               weights = split(weights, col(weights)), 
                               FUN = optim.fit.constr)


# output summary
apply(output.constr.stage2, 1, summary)


