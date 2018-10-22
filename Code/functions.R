
GetData <- function(wd, year){
        
        data <- numeric(0)
        
        for(i in year){
                
                pwd <- paste0(wd,"DMV/MMMyr_Tract_", i, ".csv")
                
                data.add <- read.csv(file = pwd, header = TRUE, sep = ",")
                data.add <- data.add[complete.cases(data.add), ]
                data.add <- data.add[data.add$YEAR_MODEL >= data.add$DMV_YEAR, ]
                
                data <- rbind(data, data.add)
        }
        
        data[order(data$TRACT, data$DMV_YEAR), ]
}



SalesSum <- function(x, group.by, data, order.by = c("DMV_YEAR")){
        
        x <- paste0("cbind(", paste0(x, collapse = ", "), ")")
        group.by <- paste0(group.by, collapse = " + ")
        formula <- as.formula(paste(x, " ~ ", group.by))
        smr.df <- aggregate(formula, data = data, FUN = sum)
        smr.df
}

CumMKTShare <- function(data, order.by = c("DMV_YEAR")){
        
        data <- data %>% arrange_(.dots = order.by)
        data$mkt.share <-  ave(data$Transaction_count, data$DMV_YEAR, 
                                 FUN = function(x) x/sum(x) * 100)
        data$cum.mkt.share <- ave(data$Transaction_count, data$DMV_YEAR, 
                                    FUN = function(x) cumsum(x)/sum(x)*100)
        data
}

obj.fn <- function(params, x, data){
        
        utility <- params[1] * data$Trans_price_mean * data$income + 
                colSums(t(data[, colnames(data) %in% x]) * params[-1])
        
        utility.exp <- exp(utility)
        
        temp <- mclapply(1 : R, function(i){
                utility.exp[, i]/
                        ave(utility.exp[, i], data$TRACT, data$DMV_YEAR, FUN = sum)
        }, mc.cores = no_cores)
        
        prob <- do.call(cbind, temp)
        
        MKT_SHARE.hat <- rowMeans(prob)                      
        
        sum((MKT_SHARE - MKT_SHARE.hat)^2)
        
}