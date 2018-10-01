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
data.quintile.mean <- read.csv(file = "ACS_16_5YR_B19081 mean income by quintile/ACS_16_5YR_B19081_with_ann.csv", header = TRUE, sep = ",")

#delete the description row
data.quintile <- data.quintile[-1, ] 
data.population <- data.population[-1, ] 
data.quintile.mean <- data.quintile.mean[-1, ] 

colnames(data.quintile) <- c("GEO_id", "GEO_id2", "Geography", 
                             "Quintile1", "Quintile1_MoE",
                             "Quintile2", "Quintile2_MoE",
                             "Quintile3", "Quintile3_MoE",
                             "Quintile4", "Quintile4_MoE",
                             "Top5", "Top5_MoE")

keep.population <- c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD01")
data.population <- data.population[, keep.population]
colnames(data.population) <- c("GEO_id", "GEO_id2", "Geography", "Population")



colnames(data.quintile.mean) <- c("GEO_id", "GEO_id2", "Geography",  
                                  "Quintile1_Mean", "Quintile1_Mean_MoE",
                                  "Quintile2_Mean", "Quintile2_Mean_MoE",
                                  "Quintile3_Mean", "Quintile3_Mean_MoE",
                                  "Quintile4_Mean", "Quintile4_Mean_MoE",
                                  "Quintile5_Mean", "Quintile5_Mean_MoE",
                                  "Top5_Mean", "Top5_Mean_MoE")


data.temp <- merge(data.quintile, data.quintile.mean, 
                   by = c("GEO_id", "GEO_id2", "Geography"), 
                   all.x = T)
data <- merge(data.temp, data.population, 
              by = c("GEO_id", "GEO_id2", "Geography"),
              all.x = T)

#delete tracts without observed quantiles (84 tracts deleted)
data<- data[apply(data, 1, FUN = function(x) sum(x %in% c("-", "**")) < 10), ] 

row.names(data) <- 1 : nrow(data)


##########      Summary     ####################

# find tracts with "2,500-" or "250,000+"

top.coded <- apply(as.matrix(data), 1, function(x){sum(x %in% c("2,500-", "250,000+")) })


##########      Fit each row a log normal distribution     ####################


# functions
# 

cond.ex <- function(params, lc = 0, uc = Inf){
        
        mu <- params[1]
        sigma <- params[2]
        
        ex <- exp(mu + sigma^2/2)
        
        ex * (pnorm(sigma - (log(lc) - mu)/sigma) - pnorm(sigma - (log(uc) - mu)/sigma))/
               (pnorm((log(uc) - mu)/sigma) - pnorm((log(lc) - mu)/sigma))
}

obj.fn <- function(params, obs){
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
        
        q.obs <- obs[1 : 5]
        mean.obs <- obs[6 : 10]

        lc <- qlnorm(c(0, p[1 : 4]), mu, sigma)
        uc <- qlnorm(c(p[1 : 4], 1), mu, sigma)
        mean.true <- mapply(cond.ex, lc = lc, uc = uc, MoreArgs = list(params = params))
        
        q.true <- qlnorm(p, mu, sigma)
        
        #weights <- sqrt(p * (1 - p) / dlnorm(q.true)^2)

        diff <- c(((q.obs - q.true)/q.obs)[!is.na(q.obs)], 
                  ((mean.obs - mean.true)/mean.obs)[!is.na(mean.obs)])

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
        
        q.obs <- x[1 : 5]
        
        lb <- max(which(q.obs == "2,500-"))
        ub <- min(which(q.obs == "250,000+"))
        
        x <- as.numeric(x)
        
        if (sum(!is.na(x)) < 2)
                return(c(NA, NA))
        else{
                hin <- function(params, obs){
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
                        
                return(auglag(par = c(0, 1), fn = obj.fn, hin = hin, obs = x, 
                              control.outer = list(trace = F))$par)
        }            
        
}


# cut values for quantiles
p <- c(seq(from = 0.2, to = 0.8, by = 0.2), 0.95)



output.constr <- apply(data[, 4 : 13], 1, FUN = optim.fit.constr)

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
output.constr <- output.constr[, complete.cases(t(output.constr))]
# calculate the income mean and variance:
# E[X] = exp(mu + sigma^2/2)
# SD[X] = exp(mu + sigma^2/2) * sqrt(exp(sigma^2) - 1)
mu <- output.constr[1, ]
sigma <- output.constr[2, ]
income.dist <- rbind(mean = exp(mu + sigma^2/2),
                     sd = exp(mu + sigma^2/2) * sqrt(exp(sigma^2) - 1))


which.min.mean <- which(income.dist["mean", ] == min(income.dist["mean", ]))
which.max.mean <- which(income.dist["mean", ] == max(income.dist["mean", ]))

which.min.var <- which(income.dist["sd", ] == min(income.dist["sd", ]))
which.max.var <- which(income.dist["sd", ] == max(income.dist["sd", ]))

income.sample <- rincome(100000)
x <- seq(0, 5e5, length=1000)

# California Income Distribution
plot(density(income.sample, from = 0 , to = 2.5e5), 
     main = "California Income Distribution",
     lwd = 2,
     ylim = c(0, 1e-4),
     xlab = "Income")
# Income Distribution of the tract with the lowest mean
lines(x, 
      dlnorm(x, output.constr[1, which.min.mean], output.constr[2, which.min.mean]), 
      col = "lightblue", 
      lwd = 2)
# Income Distribution of the tract with the highes mean
lines(x, 
      dlnorm(x, output.constr[1, which.max.mean], output.constr[2, which.max.mean]), 
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
       legend = c("Total", "Min.Mean", "Max.Mean", "Min.Var", "Max.Var"), 
       col = c("black", "lightblue", "orange", "blue", "red"),
       lwd = 2)