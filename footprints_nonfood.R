################################
# Non-food footprint
################################

agg <- function(x) {  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x)}

library(Matrix)
library(tidyverse)

items <- read.csv("/mnt/nfs_fineprint/tmp/fabio/v2/items.csv")
regions <- read.csv("/mnt/nfs_fineprint/tmp/fabio/v2/regions.csv")
regions_exio_fao <- read.csv2("input/Regions_FAO-EXIO.csv", stringsAsFactors = FALSE)
regions_exio_fao$iso3c <- regions$iso3c[match(regions_exio_fao$Country.Code, regions$code)]
regions_exio_fao$iso3c[grepl("RoW", regions_exio_fao$EXIOregion)] <- NA
regions_exio <- unique(regions_exio_fao[,3:6])
regions_exio$EXIOcode <- as.numeric(regions_exio$EXIOcode)
regions_exio <- regions_exio[order(regions_exio$EXIOcode)[1:49],]
regions_exio$iso3c <- as.character(regions_exio$iso3c)
regions_exio$iso3c[45:49] <- c("$AP", "$AM", "$EU", "$AF", "$ME")
load("/mnt/nfs_fineprint/tmp/exiobase/Y.codes.RData")
load("/mnt/nfs_fineprint/tmp/exiobase/pxp/IO.codes.RData")
X_all <- as.matrix(readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/X.rds"))
E_all <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/E.rds")
index <- E_all[[1]][,1:5]

# FOOTPRINT FUNCTION ------------------------------------------------------------------
footprint <- function(year = integer(), amort = integer(), luc = character(), var = integer()){
  e <- as.vector(E[E$year==year & E$amort==amort & E$luc==luc, var])
  e <- as.numeric(e / X[index$area=="Brazil" & index$item=="Soyabeans"])
  
  # calculate multipliers
  MP <- e * L[index$area=="Brazil" & index$item=="Soyabeans",]
  # calculate footprints
  FP <- MP * Y
  
  rownames(FP) <- IO.codes$Product.Name
  FP <- t(agg(t(FP)))
  FP <- as.matrix(FP)
  FP <- reshape2::melt(FP)
  colnames(FP) <- c("com_code", "iso", "value")
  FP <- FP[FP$value!=0,]
  
  results <- FP %>%
    mutate(year = year, amort = amort, luc = luc, var = var, type = "nonfood") %>% 
    filter(value != 0)
  
  return(results)
}



# LOOP OVER YEARS ------------------------------------------------------------------------
results <- data.frame()
year=2013
years <- 1995:2013
for(year in years){
  print(year)
  
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
  L <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/",year,"_B_inv_value.rds"))
  X <- X_all[, as.character(year)]
  
  E <- readRDS("input/defor.rds")
  
  # prepare final demand
  colnames(Y) <- Y.codes$`Region Name`
  Y <- agg(Y)
  colnames(Y) <- regions_exio$iso3c
  
  
  # calculate footprints for dir/indir + 1/5/10yrs + ------------------------------------------------------------------------
  
  var = 1
  for(var in 1:3){
    print(colnames(E)[var])
    results <- rbind(results, footprint(year = year, amort = 1, luc = "dir", var = var))
    results <- rbind(results, footprint(year = year, amort = 1, luc = "indir", var = var))
    results <- rbind(results, footprint(year = year, amort = 5, luc = "dir", var = var))
    results <- rbind(results, footprint(year = year, amort = 5, luc = "indir", var = var))
    results <- rbind(results, footprint(year = year, amort = 10, luc = "dir", var = var))
    results <- rbind(results, footprint(year = year, amort = 10, luc = "indir", var = var))
  }
  
}

saveRDS(results, paste0("output/footprint_nonfood.rds"))



