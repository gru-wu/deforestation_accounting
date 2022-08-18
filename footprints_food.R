################################
# Food footprint
################################

agg <- function(x) {  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x)}

library(Matrix)
library(tidyverse)

items <- read.csv("/mnt/nfs_fineprint/tmp/fabio/v2/items.csv")
regions <- read.csv("/mnt/nfs_fineprint/tmp/fabio/v2/regions.csv")

Y_all <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/Y.rds")
X_all <- as.matrix(readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/X.rds"))
E_all <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/E.rds")
index <- E_all[[1]][,1:5]
rm(E_all)

# FOOTPRINT FUNCTION ------------------------------------------------------------------
footprint <- function(year = integer(), amort = integer(), luc = character(), var = integer()){
  e <- as.vector(E[E$year==year & E$amort==amort & E$luc==luc, var])
  e <- as.numeric(e / X[index$area=="Brazil" & index$item=="Soyabeans"])
  
  # calculate multipliers
  MP <- e * L[index$area=="Brazil" & index$item=="Soyabeans",]
  # calculate footprints
  FP <- MP * Y
  
  FP <- t(agg(t(FP)))
  FP <- as.matrix(FP)
  FP <- reshape2::melt(FP)
  colnames(FP) <- c("com_code", "iso", "value")
  FP <- FP[FP$value!=0,]
  
  results <- FP %>%
    mutate(year = year, amort = amort, luc = luc, var = var, type = "food") %>% 
    filter(value != 0)
  
  return(results)
}



# LOOP OVER YEARS ------------------------------------------------------------------------
results <- data.frame()
year=2013
years <- 2000:2013
for(year in years){
  print(year)
  
  L <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v2/",year,"_L_value.rds"))
  Y <- Y_all[[as.character(year)]]
  X <- X_all[, as.character(year)]
  
  E <- readRDS("input/defor.rds")
  
  # prepare final demand
  Y_codes <- data.frame(iso = rep(regions$iso3c, each=6),
                        type = rep(c("balancing","food","losses","other","stock_addition","unspecified"), 192))
  colnames(Y) <- Y_codes$iso
  Y <- Y[, Y_codes$type != "other"]
  Y <- agg(Y)
  
  
  # calculate footprints for dir/indir + 1/5/10yrs + ------------------------------------------------------------------------
  
  var = 1
  for(var in 1:3){
    results <- rbind(results, footprint(year = year, amort = 1, luc = "dir", var = var))
    results <- rbind(results, footprint(year = year, amort = 1, luc = "indir", var = var))
    results <- rbind(results, footprint(year = year, amort = 5, luc = "dir", var = var))
    results <- rbind(results, footprint(year = year, amort = 5, luc = "indir", var = var))
    results <- rbind(results, footprint(year = year, amort = 10, luc = "dir", var = var))
    results <- rbind(results, footprint(year = year, amort = 10, luc = "indir", var = var))
  }
  
}

saveRDS(results, paste0("output/footprint_food.rds"))




