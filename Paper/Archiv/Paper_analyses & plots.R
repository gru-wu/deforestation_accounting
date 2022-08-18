# Analyses & plots

library(readxl)
library(tidyverse)
library(ggplot2)
library(magrittr) #shows errors
library(labeling)
library(scales) 
library(dplyr)
library(areaplot)
library(networkD3)
library(htmlwidgets)
library(ggrepel)
library(treemap)
library(maps)


##
##    GLOSSARY
##

#   COMPLETE = food + nonfood
#   TOTAL = direct + indirect   (Opposite: INDIRECT / DIRECT)
#   OVERALL = all commodities summed (Opposite: - nothing specificated- )
#   PER CAPITA = divided by population (Opposite: - nothing specificated- )

# var 1 = 1year period var 2 = 5 year period, var3 = 10 year period

##
##    LOAD DATA
##

footprint_food <- readRDS("~/Deforestation-calculation/output/footprint_food.rds")
footprint_nonfood <- readRDS("~/Deforestation-calculation/output/footprint_nonfood.rds")
population_data_worldbank <- read_excel("~/Deforestation-calculation/input/population data_worldbank.xls")

##
##    PREPARE DATA
##


# -------------------------------- add population data  -----------------------------------


################# Add population data to footprint food

#compare country codes of footprint iso and worlbank data and 

footprint.code <- unique(footprint_food[2])
worldbank.code <- unique(population_data_worldbank[2])
x11 <- as.vector(footprint.code$iso)
x12 <- as.vector(worldbank.code$`Country Code`)
diff.footprint  <- setdiff(x11,x12) # iso codes in footprint but NOT in worldbankdata
diff.worldbank <- setdiff(x12,x11) # iso codes in worldbank but NOT in footprint data
rm(x11,x12)

pop_data <- filter(population_data_worldbank, !population_data_worldbank$`Country Code` %in% diff.worldbank) # take out iso codes that are only in worldbank

pop_data <- pop_data %>%
  select(-1,-3, -4, -("1960":"1999")) %>% # take out irrelevant columns an years
  t() # transpose data for loop

population <- c(NA) #create vector to add to dataframe

# add population data to footprint_food

defor_pop <- cbind(footprint_food, population) # add vector as column to dataframe = create column for outputs of loop


for(i in 1:nrow(defor_pop)){
  x <- as.character(defor_pop[i,2]) #find iso code
  test <- match(x,diff.footprint)   #check if iso code is NOT in worldbank data (NA = iso code is in worldbank AND in Footprint data)
  if(is.na(test)){
    x1 <- which(pop_data == x, arr.ind = TRUE) #gives position in pop_data of iso code defined in x 
    x1 <- as.numeric(x1[1,2]) 
    y <- as.character(defor_pop[i,4]) # finds year for which population data should be added
    y <- match(y, rownames(pop_data)) # finds row in pop_data containing population data for year defined in y
    endvalue <- as.character(pop_data[y,x1]) # prints value for iso defined in x1 and year defined in y
    endvalue <- as.numeric(endvalue)
    defor_pop[i,9] <- endvalue
  }
  if(!is.na(test)){
    defor_pop[i,9] <- NA
    next()
  }
}

saveRDS(defor_pop, paste0("Analysis_Footprints/footprint_food_pop_comodities.rds"))

rm(footprint.code,pop_data,worldbank.code,diff.footprint,diff.worldbank,endvalue,i,population,test,x,x1,y,defor_pop)


################# Add population data to footprint food


#compare country codes of footprint iso and worlbank data and 

footprint.code <- unique(footprint_nonfood[2])
worldbank.code <- unique(population_data_worldbank[2])
x11 <- as.vector(footprint.code$iso)
x12 <- as.vector(worldbank.code$`Country Code`)
diff.footprint  <- setdiff(x11,x12)
diff.worldbank <- setdiff(x12,x11)
rm(x11,x12,)

pop_data <- filter(population_data_worldbank, !population_data_worldbank$`Country Code`%in% diff.worldbank)

pop_data <- pop_data %>%
  select(-1,-3, -4, -("1960":"1999")) %>%
  t()

population <- c(NA)

# add data

defor_pop <- cbind(footprint_nonfood, population)


for(i in 1:nrow(defor_pop)){
  x <- as.character(defor_pop[i,2])
  test <- match(x,diff.footprint)
  if(is.na(test)){
    x1 <- which(pop_data == x, arr.ind = TRUE)
    x1 <- as.numeric(x1[1,2])
    y <- as.character(defor_pop[i,4])
    y <- match(y, rownames(pop_data))
    endvalue <- as.character(pop_data[y,x1])
    endvalue <- as.numeric(endvalue)
    defor_pop[i,9] <- endvalue
  }
  if(!is.na(test)){
    defor_pop[i,9] <- NA
    next()
  }
}

saveRDS(defor_pop, paste0("Analysis_Footprints/footprint_nonfood_pop_comodities.rds"))

rm(footprint.code,pop_data,worldbank.code,diff.footprint,diff.worldbank,endvalue,i,population,test,x,x1,y,defor_pop)


# -------------------------------- prepare dataset for further analysis: choose y10a5 and add per capita values  --------

# food
footprint_food_pop_comodities_y10a5 <- filter(footprint_food_pop_comodities,var == 3 & amort == 5) # filter for y10 and a5

footprint_food_analysis_data <- mutate(footprint_food_pop_comodities_y10a5, fp_per_capita = value / population) # calculate and add fp per capita

saveRDS(footprint_food_analysis_data, paste0("Analysis_Footprints/footprint_food_analysis_data.rds")) 

# non food
footprint_nonfood_pop_comodities_y10a5 <- filter(footprint_nonfood_pop_comodities,var == 3 & amort == 5) # filter for y10 and a5

footprint_nonfood_analysis_data <- mutate(footprint_nonfood_pop_comodities_y10a5, fp_per_capita = value / population)  # calculate and add fp per capita

saveRDS(footprint_nonfood_analysis_data, paste0("Analysis_Footprints/footprint_nonfood_analysis_data.rds")) 


# tidy Environment
rm(footprint_food_pop_comodities, footprint_food_pop_comodities_y10a5, footprint_nonfood_pop_comodities, footprint_nonfood_pop_comodities_y10a5)


# -------------------------------- prepare data for FOOD: calculate TOTAL/DIRECT/INDIRECT OVERALL FP -----------------------------------------

#### calculate TOTAL/DIRECT/INDIRECT OVERALL (=summed commodities) footprints for footprint_food  

# total overall footprint and per capita fp  FOOD (=sum up all commodities and sum indirect and direct )

food_analysis_tot_ov <- footprint_food_analysis_data %>%
  group_by(iso,var, amort,year, population) %>%
  summarise(
    fp = sum(value, na.rm = TRUE),
    fp_per_capita = sum(fp_per_capita, na.rm = TRUE)
    )


# indirect/direct overall footprint FOOD (=sum up all commodities)

food_analysis_ov <- footprint_food_analysis_data %>%
  group_by(iso,var, luc, amort,year, population) %>%
  summarise(
    fp = sum(value, na.rm = TRUE),
    fp_per_capita = sum(fp_per_capita, na.rm = TRUE)
  )


#food_analysis_ind_ov <- filter(x, luc == "indir")      #don't need this cause it is all in one now
#food_analysis_dir_ov <- filter(x, luc == "dir")



# -------------------------------- prepare data for NON FOOD & calulate COMPLETE: calculate TOTAL/DIRECT/INDIRECT OVERALL FP & add COMPLETE FP -----------------------------------------

#' info: there are way more countries in food than in nonfood, therefore the complete footprint can only be caluculated for all nonfood- countries
#       and is threrefore included in the nonfood-data 

#### calculate TOTAL/DIRECT/INDIRECT OVERALL (=summed commodities) footprints for footprint_nonfood  and add footprint per capita 

# total overall footprint and per capita fp  NONFOOD

nonfood_analysis_tot_ov <- footprint_nonfood_analysis_data %>% #(sum up all commodities and sum indirect and direct )
  group_by(iso,var,amort,year, population) %>%
  summarise(
    fp = sum(value, na.rm = TRUE),
    fp_per_capita = sum(fp_per_capita, na.rm = TRUE)
  )

# indirect/direct overall footprint FOOD

nonfood_analysis_ov <- footprint_nonfood_analysis_data %>%  #(sum up all commodities)
  group_by(iso,var,luc, amort,year, population) %>%
  summarise(
    fp = sum(value, na.rm = TRUE),
    fp_per_capita = sum(fp_per_capita, na.rm = TRUE)
  )

#nonfood_analysis_ind_ov <- filter(x, luc == "indir")       #don't need this cause it is all in one now
# direct overall footprint for FOOD
#nonfood_analysis_dir_ov <- filter(x, luc == "dir")


################# calculate and add COMPLETE footprint


food.code <- unique(food_analysis_tot_ov[1])
nonfood.code <- unique(nonfood_analysis_tot_ov[1])
x11 <- as.vector(food.code$iso)
x12 <- as.vector(nonfood.code$iso)
diff.food  <- setdiff(x11,x12)
diff.nonfood <- setdiff(x12,x11)
rm(x11,x12)

total_fp <- c(NA)

nonfood_analysis_tot_ov$complete_footprint <- total_fp

for(i in 1:nrow(nonfood_analysis_tot_ov)){
  x <- as.character(nonfood_analysis_tot_ov$iso[i]) 
  y <- nonfood_analysis_tot_ov$year[i] 
  #z <- as.character(nonfood_analysis_tot_ov$luc[i])  raus weil TOTAL (= direct + indirect)
  test <- match(x,diff.nonfood)
  if(is.na(test)){
    x1 <- which(food_analysis_tot_ov$iso == x & food_analysis_tot_ov$year == y) #& food_analysis_tot_ov$luc == z) raus weil TOTAL (= direct + indirect)
    x2 <- nonfood_analysis_tot_ov[i,6]
    endvalue <- food_analysis_tot_ov$fp[x1] + x2
    nonfood_analysis_tot_ov$complete_footprint[i] <- as.numeric(endvalue)
  }
  if(!is.na(test)){
    nonfood_analysis_tot_ov[i,8] <- NA
    next()
  }
}

rm(food.code, nonfood.code, total_fp, endvalue, i, test, x, x1, y, x2)


food.code <- unique(food_analysis_ov[1])
nonfood.code <- unique(nonfood_analysis_ov[1])
x11 <- as.vector(food.code$iso)
x12 <- as.vector(nonfood.code$iso)
diff.food  <- setdiff(x11,x12) # iso codes in Food but NOT in NONFood
diff.nonfood <- setdiff(x12,x11) # iso codes in NONFood but NOT in Food
rm(x11,x12)

total_fp <- c(NA)


nonfood_analysis_ov$complete_footprint <- total_fp

for(i in 1:nrow(nonfood_analysis_ov)){
  x <- as.character(nonfood_analysis_ov$iso[i]) 
  y <- nonfood_analysis_ov$year[i] 
  z <- as.character(nonfood_analysis_ov$luc[i])  
  test <- match(x,diff.nonfood)
  if(is.na(test)){
    x1 <- which(food_analysis_ov$iso == x & food_analysis_ov$year == y & food_analysis_ov$luc == z) #raus weil TOTAL (= direct + indirect)
    x2 <- nonfood_analysis_ov[i,7]
    endvalue <- food_analysis_ov$fp[x1] + x2
    nonfood_analysis_ov$complete_footprint[i] <- as.numeric(endvalue)
  }
  if(!is.na(test)){
    nonfood_analysis_ov[i,8] <- NA
    next()
  }
}

rm(food.code, nonfood.code, total_fp, endvalue, i, test, x, x1, y, x2, z)

saveRDS(diff.food, paste0("Analysis_Footprints/diff.food.rds"))
saveRDS(diff.nonfood, paste0("Analysis_Footprints/diff.nonfood.rds"))

saveRDS(food_analysis_ov, paste0("Analysis_Footprints/food_analysis_ov.rds"))
saveRDS(food_analysis_tot_ov, paste0("Analysis_Footprints/food_analysis_tot_ov.rds")) 

saveRDS(nonfood_analysis_ov, paste0("Analysis_Footprints/nonfood_analysis_ov.rds"))
saveRDS(nonfood_analysis_tot_ov, paste0("Analysis_Footprints/nonfood_analysis_tot_ov.rds")) 

rm(food_analysis_ov, food_analysis_tot_ov, nonfood_analysis_ov, nonfood_analysis_tot_ov, footprint_food_analysis_data, footprint_nonfood_analysis_data)

# -------------------------------- analysis 1. WHAT SHARE OF DEFORESTATION IS EXPORTED/ STAYS IN BRAZIL--------

###
### 1.2 calculate complete total overall  (indirect+ direct / food + nonfood) deforestation of all countries and substract total def for Brazil
###

# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")

# calucalte and add ROW complete footprint to nonfood_analysis_tot_ov (direct + indirect)


a <- nonfood_analysis_tot_ov %>%                # summarize ROW for NONfood
  filter(iso %in% diff.nonfood) %>%
  group_by(year) %>%
  summarise(ROW_nonfood = sum(fp, na.rm = TRUE)) 
  
b <- food_analysis_tot_ov %>%                   # summarize ROW for FOOD
  filter(iso %in% diff.food) %>%
  group_by(year) %>%
  summarise(ROW_food = sum(fp, na.rm = TRUE))

for (i in 1:nrow(a)) {                          # sum ROW FOOD and ROW NONfood to have ROW COMPLETE FP
  c <-  data.frame(iso = c("ROW"),
                   var = c(3),
                   amort = c(5),
                   year = a$year[i],
                   population = NA,
                   fp = NA, 
                   fp_per_capita = NA,
                   complete_footprint = sum(a[i,2], b[i,2])
  )
  nonfood_analysis_tot_ov <- rbind(as_tibble(nonfood_analysis_tot_ov),as_tibble(c)) 
}
  
rm(a,b,c)  

a <- nonfood_analysis_ov %>%                # summarize ROW for NONfood
  filter(iso %in% diff.nonfood) %>%
  group_by(luc, year) %>%
  summarise(ROW_nonfood = sum(fp, na.rm = TRUE)) 

b <- food_analysis_ov %>%                   # summarize ROW for FOOD
  filter(iso %in% diff.food) %>%
  group_by(luc, year) %>%
  summarise(ROW_food = sum(fp, na.rm = TRUE))

for (i in 1:nrow(a)) {                          # sum ROW FOOD and ROW NONfood to have ROW COMPLETE FP
  c <-  data.frame(iso = c("ROW"),
                   var = c(3),
                   amort = c(5),
                   luc = a$luc[i],
                   year = a$year[i],
                   population = NA,
                   fp = NA, 
                   fp_per_capita = NA,
                   complete_footprint = sum(a[i,3], b[i,3])
  )
  nonfood_analysis_ov <- rbind(as_tibble(nonfood_analysis_ov),as_tibble(c)) 
}

rm(a,b,c)  


# calucalte and add ROW complete footprint to nonfood_analysis_ov (direct & indirect distinct)

# calculate world COMPLETE TOTAL OVERALL 

world <- nonfood_analysis_tot_ov %>%
  filter(!iso == "BRA") %>% # (NO DOUBLE COUNTING cause $-countries are NA in complete fp) 
  group_by(year) %>%
  summarise(
    world_withoutBRA = sum(complete_footprint, na.rm = TRUE)
  )

world1 <- nonfood_analysis_tot_ov %>%  # global fp including Brazil
  group_by(year) %>%
  summarise(world = sum(complete_footprint, na.rm = TRUE))

world$world <- world1$world

rm(world1)

# calculate Brazil COMPLETE TOTAL OVERALL

Brazil <- nonfood_analysis_tot_ov %>%
  filter(iso == "BRA") %>%
  group_by(year) %>%
  summarise(
    complete_fp = sum(complete_footprint, na.rm = TRUE)
  )
 

# combine 
 
world$Brazil <- Brazil$complete_fp

# calculate share
# table 1
share_Brazil <- mutate(world, share_B_in_W = Brazil / world)

rm(world, Brazil)


# outtakes plot -----------------------------------------------------------


#  WHAT DID I WANTED TO SHOW ???
ggplot(share_Brazil)+
  geom_col(mapping = aes(x = year, y = Brazil, fill = world_withoutBRA), position = "stack")
  
#Scatterplot
# plot 
# WHAT DID I WANTED TO SHOW ???
ggplot(share_Brazil, aes(x=Brazil, y=world_withoutBRA)) +
  geom_point(shape= 1, colour = "darkseagreen3", size = 3, stroke = 2) +    
  geom_smooth(method=lm, colour = "darkseagreen4")

# # -----------------------------------------------------------------------

####### share of Brazil  

#plot 1
# stacked are chart
x <- share_Brazil %>% 
  select(year, world_withoutBRA)
x$iso <- c("foreign consumption")
colnames(x)<- c("year", "value", "iso")

y<- share_Brazil %>%
  select(year, Brazil)
y$iso <- c("domestic consumption")
colnames(y)<- c("year", "value", "iso")

z <- share_Brazil %>%
  select(year, world)
z$iso <- c("global consumption")
colnames(z)<- c("year", "value", "iso")

dataforareaplot <- rbind(x,y,z)

# Thesis Figure 5
dataforareaplot_km2 <- dataforareaplot %>%
  mutate(value_km2 = value*0.01)

ggplot(filter(dataforareaplot_km2, iso !="global consumption"), aes(x=year, y=value_km2, fill=iso)) + 
  geom_area()+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  #ggtitle("Figure 1: Deforestation Footprints, by region (Importing countries, Brazil)")+
  labs(x= "year", y = "deforestation [km2] ")+
  theme(legend.title=element_blank())
rm(dataforareaplot_km2)

#plot 2 / Thesis Figure 6
#share
ggplot(data=share_Brazil, aes(x=year, y=share_B_in_W)) +
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  scale_y_continuous(labels = scales::percent_format(1))+
  geom_smooth(aes(x= year, y = share_B_in_W),colour = "darkseagreen3", size = 0.5)+
  theme_bw()+
  theme_light()+
  #ggtitle("Figure 2: Share of Brazil's Footprint in Global Footprint")+
  labs(x= "year", y = "share [%]")+
  theme(legend.title=element_blank())
# geom_line(colour = "darkseagreen3", size = 3)

#plot 3 / Thesis Figure 7 

dataforareaplot_km2 <- dataforareaplot %>%
  mutate(value_km2 = value*0.01)

ggplot(dataforareaplot_km2, aes(x= year, y= value_km2, group = iso, colour = iso))+
  geom_line(size= 1)+
  scale_color_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  #ggtitle("Figure 3: Deforestation Footprints, by region (global, importing countries, Brazil)")+
  labs(x= "year", y = "deforestation [km²] ")+
  theme(legend.title=element_blank())
rm(dataforareaplot_km2)

#calc analysis

#(sum(share_Brazil$world_withoutBRA)) / (sum(share_Brazil$world))
  


# ??????
#ggplot(share_Brazil, aes(y= share, x= year))+
 # geom_bar(stat = "identity", width = 1) 

#plot 6
# pie charts (planned to do for 2000,(2003), 2005, (2007), 2010, 2013)
c <- c(share_Brazil$Brazil[11],share_Brazil$world_withoutBRA[11]) # change row numbers for different years (one more than the year 2001 -> 2)

pie_labels <- paste0(round(100 * (c/ sum(c)),2),"%")

test <- paste0(round(100 * (c/ sum(c)),2),"%")

pie(c, labels = pie_labels, col = cols, border = "white", lty = 1, main = 2010) +# change title depending on year
legend("topleft", legend = c("Brazil", "RoW"), fill = cols)  

rm(c, i, pie_labels, x, z, cols, y, share_Brazil)

rm(diff.food, diff.nonfood, food_analysis_ov, food_analysis_tot_ov, nonfood_analysis_ov, nonfood_analysis_tot_ov, footprint_food_analysis_data, footprint_nonfood_analysis_data)



# -------------------------------- analysis 2: WHERE DOES SOY/DEFORESTATION GO GLOBALLY ? ---------------

# Sankey  -----------------------------------------------------------------


# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")


# Sankey diagram (see working_file: sankey  for attempt to add continent as node to sankey diagram)
# (only for countries that are inculded in EXIOBASE and ROW)

# calculate complete FP for ROW (ROW = sum of diff.food and diff.nonfood)

row_food <- food_analysis_tot_ov %>%
  filter(iso %in% diff.food) %>%
  group_by(year) %>%
  summarise(ROW_calc = sum(fp))

ROW_calc <- nonfood_analysis_tot_ov %>%
  filter(iso %in% diff.nonfood) %>%
  group_by(year) %>%
  summarise(ROW_nonfood = sum(fp))%>%
  mutate(ROW_food = as.numeric(paste(row_food$ROW_calc)))%>%
  mutate(ROW_f_nf = ROW_food + ROW_nonfood)

rm(row_food)

test <-  data.frame(iso= "ROW", var = 3, amort =5,  year = ROW_calc$year, population = NA, fp = NA, fp_per_capita= NA, complete_footprint =  ROW_calc$ROW_f_nf, continents = NA)



Sankeydata <- filter(nonfood_analysis_tot_ov, iso %in% c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "ESP", 
                                                         "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", 
                                                         "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SWE", "SVN", "SVK", 
                                                         "GBR", "CHE", "NOR", "USA", "CAN", "BRA", "MEX", "CHN", "KOR", 
                                                         "IND", "JPN", "TWN", "IDN", "RUS", "AUS") )
Europe <- filter(Sankeydata, iso %in% c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "ESP", 
                                        "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", 
                                        "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SWE", "SVN", "SVK", 
                                        "GBR", "CHE", "NOR")) 
Europe$continents <- c("EU")


NorthAmerika <- filter(Sankeydata, iso %in% c("USA", "CAN"))
NorthAmerika$continents <- c("NorthA")

Asia <- filter(Sankeydata, iso %in% c("CHN", "KOR", "IND", "JPN", "TWN", "IDN"))
Asia$continents <- c("Asia")

Mex <- filter(Sankeydata, iso %in% c("MEX"))
Mex$continents <- c("Mex")

BRA <- filter(Sankeydata, iso %in% c("BRA"))
BRA$continents <- c("Brazil")

AUS <- filter(Sankeydata, iso %in% c("AUS"))
AUS$continents <- c("Australia")

RUS <- filter(Sankeydata, iso %in% c("RUS"))
RUS$continents <- c("Russia")


Sankeydata <- rbind(Asia, AUS, BRA, Europe, NorthAmerika, Mex, RUS)

Sankeydata <- rbind(data.frame(Sankeydata), test)

Sankeydata <- Sankeydata %>%         
  group_by(iso, continents)%>%
  summarise(complete_footprint = sum(complete_footprint, na.rm = TRUE)) %>% # calculate values for 13 year-period
  arrange(continents)

rm(Asia, AUS, BRA, Europe, Mex, NorthAmerika, RUS, test)

links <- filter(Sankeydata, iso !="BRA") %>% #year == 2000) %>%                  # change for different years !
  mutate(Brazil = c("BRA"))

links <- as.data.frame(links)

nodes <- data.frame(
  names = c(as.character(links$iso),
            as.character(links$Brazil)) %>% unique())

links$IDiso <- match(links$iso, nodes$names)-1
links$IDBrazil <- match(links$Brazil, nodes$names)-1

#plot 7
p <- sankeyNetwork(Links = links, Nodes = nodes, 
                   Source =  "IDBrazil", Target = "IDiso", 
                   Value = "complete_footprint", NodeID = "names", 
                   fontSize = 15, nodeWidth = 100, 
                   sinksRight = FALSE
                   )

saveWidget(p, paste0("Analysis_Footprints/Sankey.html"))    # change name for different footprint

saveRDS(Sankeydata, paste0("Analysis_Footprints/Sankeydata.rds")) 

rm(list = ls())


# Map ---------------------------------------------------------------------

#map('world',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )


# Sankey per capita -------------------------------------------------------

# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")



# calculate complete FP for ROW (ROW = sum of diff.food and diff.nonfood)

row_food <- food_analysis_tot_ov %>%
  filter(iso %in% diff.food) %>%
  group_by(year) %>%
  summarise(ROW_calc = sum(fp))

ROW_calc <- nonfood_analysis_tot_ov %>%
  filter(iso %in% diff.nonfood) %>%
  group_by(year) %>%
  summarise(ROW_nonfood = sum(fp))%>%
  mutate(ROW_food = as.numeric(paste(row_food$ROW_calc)))%>%
  mutate(ROW_f_nf = ROW_food + ROW_nonfood)

rm(row_food)

test <-  data.frame(iso= "ROW", var = 3, amort =5,  year = ROW_calc$year, population = NA, fp = NA, fp_per_capita= NA, complete_footprint =  ROW_calc$ROW_f_nf, continents = NA)



Sankeydata <- filter(nonfood_analysis_tot_ov, iso %in% c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "ESP", 
                                                         "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", 
                                                         "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SWE", "SVN", "SVK", 
                                                         "GBR", "CHE", "NOR", "USA", "CAN", "BRA", "MEX", "CHN", "KOR", 
                                                         "IND", "JPN", "TWN", "IDN", "RUS", "AUS") )
Europe <- filter(Sankeydata, iso %in% c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "ESP", 
                                        "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", 
                                        "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SWE", "SVN", "SVK", 
                                        "GBR", "CHE", "NOR")) 
Europe$continents <- c("EU")


NorthAmerika <- filter(Sankeydata, iso %in% c("USA", "CAN"))
NorthAmerika$continents <- c("NorthA")

Asia <- filter(Sankeydata, iso %in% c("CHN", "KOR", "IND", "JPN", "TWN", "IDN"))
Asia$continents <- c("Asia")

Mex <- filter(Sankeydata, iso %in% c("MEX"))
Mex$continents <- c("Mex")

BRA <- filter(Sankeydata, iso %in% c("BRA"))
BRA$continents <- c("Brazil")

AUS <- filter(Sankeydata, iso %in% c("AUS"))
AUS$continents <- c("Australia")

RUS <- filter(Sankeydata, iso %in% c("RUS"))
RUS$continents <- c("Russia")


Sankeydata <- rbind(Asia, AUS, BRA, Europe, NorthAmerika, Mex, RUS)

Sankeydata <- rbind(data.frame(Sankeydata), test)

### add population to taiwan 
twn_pop <- c(22276672 , 22405568 , 22520776 , 22604550 , 22689122 , 22770383 , 22876527 , 22958360 , 23037031 , 23119772 , 23162123 , 23224912 , 23315822 , 23373517)
Sankeydata$population[57:70] <- twn_pop

Sankeydata$fp_per_capita[57:70] <- Sankeydata$fp[57:70] / Sankeydata$population[57:70]

### calculate and add population data to ROW

# population data got from the internet https://www.worldometers.info/world-population/world-population-by-year/
world_pop <- c(6143493823, 6222626606,	6301773188, 6381185114, 6461159389, 6541907027, 6623517833, 6705946610, 6789088686,	6872767093,	6956823603,	7041194301,	7125828059,	7210581976)

test <- Sankeydata %>%
      group_by(year) %>%                                 #calculate population of all coutnries w/o ROW
    summarise(pop = sum(population, na.rm = TRUE))
 

Sankeydata$population[589:602] <- world_pop - test$pop   # global population - sum of pop of all contries = population ROW

rm(twn_pop, world_pop, test)

#calculate complete fp per capita

Sankeydata <- Sankeydata %>%
  mutate(complete_per_capita = complete_footprint/population)   #calculate per capita values
  
# filter data for sankey graph
sankeydata_per_capita <- Sankeydata %>%
  filter(complete_per_capita != is.na(complete_per_capita)) #take out NA

# plot 8
dataplot8 <- sankeydata_per_capita %>%
  filter(continents == "EU")

ggplot(dataplot8, aes(x= year, y = complete_per_capita, colour = iso))+
         geom_line()+
  geom_text(data = filter(dataplot8, year %in% c(2013, 2000)), aes(x= year, y = complete_per_capita, colour = iso, label = iso), check_overlap = TRUE)


sankeydata_per_capita <- sankeydata_per_capita %>%
    group_by(iso, continents)%>%
  summarise(complete_per_capita = sum(complete_per_capita, na.rm = TRUE))  %>%       # calculate values for 13 year-period
  arrange(continents) #arrange to have countries per continents together = order of Sankey chart

#### convert data to SAnkey format (used some code from the Internet and it worked- idk how)

links <- filter(sankeydata_per_capita, iso !="BRA") %>%                 
  mutate(Brazil = c("BRA"))

links <- as.data.frame(links)

nodes <- data.frame(
  names = c(as.character(links$iso),
            as.character(links$Brazil)) %>% unique())

links$IDiso <- match(links$iso, nodes$names)-1
links$IDBrazil <- match(links$Brazil, nodes$names)-1

#plot 9
p <- sankeyNetwork(Links = links, Nodes = nodes, 
                   Source =  "IDBrazil", Target = "IDiso", 
                   Value = "complete_per_capita", NodeID = "names",  
                   fontSize = 15, nodeWidth = 100, # change for different footprint
                   sinksRight = FALSE
)

saveWidget(p, paste0("Analysis_Footprints/Sankey_per_capita.html"))  
saveRDS(sankeydata_per_capita, paste0("Analysis_Footprints/sankeydata_per_capita.rds")) 

rm(list = ls())


# new plots with Sankey data ---------------------------------------------------------------

Sankeydata <- readRDS("~/Deforestation-calculation/Analysis_Footprints/Sankeydata.rds")
sankeydata_per_capita <- readRDS("~/Deforestation-calculation/Analysis_Footprints/sankeydata_per_capita")

# new bar plot/ Thesis Figure 12
sankeydata_fig12 <- Sankeydata %>%
  filter(iso != "BRA")%>%
  mutate(complete_footprint_km2 = complete_footprint*0.01)

  ggplot(sankeydata_fig12, aes(x = reorder(iso, complete_footprint), y = complete_footprint_km2))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3", stat="identity")+
  #ggtitle("figure 6: deforestation footprints (2000-2013), absolute values")+
  labs(x= "importing countries", y = "deforestation [km²]" )+
  theme_bw()+
  theme_light()+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),
                    limits = c(0, 18000))
rm(sankeydata_fig12)
  
# % calc
Sankeydata_percentage <- Sankeydata %>%
  mutate(percentage = complete_footprint / sum(Sankeydata$complete_footprint))

# new bar plot figure 7 in word
Sankeydata[order(-Sankeydata$complete_footprint),] %>%
  filter(iso != "BRA" & iso != "ROW")%>%
  head(x,n = 10) %>%
  ggplot( aes(x = reorder(iso,complete_footprint), y= complete_footprint)) +
  geom_bar(fill = "darkseagreen3", stat="identity")+
  ggtitle("Figure 7: top 10 deforestation footprints (2000-2013), absolute values")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) absolute" )+
  theme_bw()+
  theme_light()

# Thesis Figure 11
sankeydata_per_capita_fig11 <- sankeydata_per_capita %>%
  filter(iso  != "BRA" & iso != "ROW") %>%
  mutate(complete_per_capita_m2= complete_per_capita*10000)%>%
  mutate_if(is.numeric,
            round,
            digits = 1)

ggplot(sankeydata_per_capita_fig11,aes(x = reorder(iso, complete_per_capita_m2), y = complete_per_capita_m2))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3", stat="identity")+
  #ggtitle("figure 6: deforestation footprints (2000-2013), per capita")+
  labs(x= "importing countries", y = "deforestation [m²]" )+
  theme_bw()+
  theme_light()+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),
                    limits = c(0, 80))
rm(sankeydata_per_capita_fig11)


sankeydata_per_capita[order(-sankeydata_per_capita$complete_per_capita),] %>%
  filter(iso != "BRA" & iso != "ROW")%>%
  head(x,n = 10) %>%
  ggplot( aes(x = reorder(iso,complete_per_capita), y= complete_per_capita)) +
  geom_bar(fill = "darkseagreen3", stat="identity")+
  ggtitle("Figure 8: top 10 deforestation footprints (2000-2013), per capita")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) per capita" )+
  theme_bw()+
  theme_light()

## table top 10 per capita
x <- sankeydata_per_capita[order(-sankeydata_per_capita$complete_per_capita),] %>%
  filter(iso != "BRA")%>%
  head(x,n = 10)

table_per_capita <- merge(x, Sankeydata, by = "iso")
table_per_capita <- table_per_capita[order(-table_per_capita$complete_per_capita),]%>%
  select(iso, complete_per_capita, complete_footprint)

# Thesis table 1
table_per_capita%>%
  mutate(complete_per_capita_m2= complete_per_capita*10000,
         complete_footprint_km2 = complete_footprint*0.01)%>%
  select(iso, complete_per_capita_m2, complete_footprint_km2)%>%
  mutate_if(is.numeric,
            round,
            digits = 2)%>%
  gt()%>%
  tab_header(title = "Deforestation Footprints, ranked by per capita values")%>%
  cols_label(iso = "iso", complete_per_capita_m2= "per capita values [m²]", complete_footprint_km2 = "absolute values [km²]")%>%
  tab_options(column_labels.font.weight =  "bold")
  #fmt_number(columns = complete_per_capita, decimals = 10
  #)%>%
  #fmt_number(columns = complete_footprint,
             #suffixing = TRUE)

#table ranked per absolute value
x <- Sankeydata[order(-Sankeydata$complete_footprint),] %>%
  filter(iso != "BRA" & iso != "ROW")%>%
  head(x,n = 10)

table_absolute <- merge(x, sankeydata_per_capita, by = "iso")
table_absolute <- table_absolute[order(-table_absolute$complete_footprint),]%>%
  select(iso, complete_footprint, complete_per_capita)

# Thesis tabel 2
table_absolute%>%
  mutate(complete_per_capita_m2= complete_per_capita*10000,
         complete_footprint_km2 = complete_footprint*0.01)%>%
  select(iso, complete_per_capita_m2, complete_footprint_km2)%>%
  mutate_if(is.numeric,
            round,
            digits = 2)%>%
  gt()%>%
  tab_header(title = "Deforestation Footprints, ranked by absolute value")%>%
  cols_label(iso = "iso", complete_per_capita_m2 = "per capita values [m²]", complete_footprint_km2 = "absolute values [km²]")%>%
  tab_options(column_labels.font.weight =  "bold")#%>%
 

  
  
rm(list = ls())


# -------------------------------- analysis 3: HOW MUCH DEFORESTATION DO THE MAJOR EXPORTING DESTINATION IMPORT ? --------

# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")


# calculate complete fp per capita

nonfood_analysis_tot_ov <- nonfood_analysis_tot_ov %>%
  mutate(complete_fp_per_capita = complete_footprint/population)

#calculate total per capita 2000-2013
top_10_per_capita_0013 <- nonfood_analysis_tot_ov %>%
  group_by(iso) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA")%>%
  top_n(10,complete_fp_per_capita)

top_10_iso_per_capita <- top_10_per_capita_0013$iso

#calculate total absolute values 2000-2013
top_10_0013 <- nonfood_analysis_tot_ov %>%
  group_by(iso) %>%
  summarise(complete_fp = sum(complete_footprint))%>%
  filter(iso != "BRA")%>%
  top_n(10,complete_fp)

top_10_iso <- top_10_0013$iso


#plot 10 
ggplot(top_10_per_capita_0013, aes(x = iso, y = complete_fp_per_capita))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  ggtitle("figure 10: deforestation footprints (2000-2013)")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) per capita" )
 

#plot 10 
#ggplot(data = filter(top_10_per_capita, year == 2013), aes(x = iso, y = complete_fp_per_capita))+         # per capita w BRA
#  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())
  # from 2010 on Brazil is incredibly high -> because of the a5, y10 (?). This means that in contrast to
  # other states, Brazil's soy use exploded in the period 2000-2020. which can also be seen in the area share-graph

#plot 11
ggplot(data = filter(top_10_0013), aes(x = iso, y = complete_fp))+                       # w bra
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  ggtitle("figure 11: deforestation footprints (2000-2013)")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) absolute values" )


## belongs to analysis 1 --------------------------------------------------
# plot 12
ggplot(data = filter(top_10_per_capita, iso == "BRA"), aes(x = year, y = complete_fp_per_capita))+       # BRA 
  geom_smooth(color = "darkseagreen3")


data_plot <- filter(nonfood_analysis_tot_ov, iso %in% unique(top_10_per_capita$iso))  #change to top_10

#plot 13
ggplot(data_plot , aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  geom_text(data = filter(data_plot, year == 2013), aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso),check_overlap = TRUE) 


# # -----------------------------------------------------------------------

#### Per capita analysis top 10 development 2000 - 2013
test <- top_10_per_capita_0013[order(top_10_per_capita_0013$complete_fp_per_capita, decreasing = TRUE), ] 
test <- head(test, 10)

CHN <- filter(nonfood_analysis_tot_ov, iso == "CHN")
JPN <- filter(nonfood_analysis_tot_ov, iso == "JPN")
KOR <- filter(nonfood_analysis_tot_ov, iso == "KOR")
data_plot <- nonfood_analysis_tot_ov %>%
  filter(iso %in% test$iso)  #change to top_10

data_plot <- rbind(data_plot, CHN, JPN, KOR)
 
rm(test, CHN, JPN, KOR)
#plot 13 /Thesis Figure 13
data_plot_fig13 <- data_plot %>%
  mutate(complete_fp_per_capita_m2 = complete_fp_per_capita*10000)#%>%
  #mutate_if(is.numeric,
   #         round,
    #        digits = 2)
ggplot(data_plot_fig13, aes(x = year, y = complete_fp_per_capita_m2, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  #ggtitle("figure 13: deforestation footprints development 2000-2013, per capita")+
  #labs(x= "years ", y = "deforestation embodied in soy imports, (ha) per capita" )+
  theme(legend.title=element_blank())+
  labs(x= "year", y = "deforestation [m²]" )+
  theme_bw()+
  theme_light()
rm(data_plot_fig13)
#geom_text(data = filter(data_plot, year == 2013), aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso),check_overlap = TRUE) 

#plot13a
ggplot(filter(nonfood_analysis_tot_ov, iso %in% c("CHN", "KOR", "JPN")), aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  ggtitle("figure 13a: deforestation footprints development 2000-2013, per capita")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) per capita" )+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()
#geom_text(data = filter(data_plot, year == 2013), aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso),check_overlap = TRUE) 

dataforplot13b <- nonfood_analysis_tot_ov %>%
  group_by(iso, year) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA" & year == 2013)%>%
 arrange(desc(complete_fp_per_capita))%>%
  head(10)
 

#plot13b
ggplot(dataforplot13b, aes(x = iso, y = complete_fp_per_capita))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  ggtitle("figure 13b: deforestation footprints (2013)")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) per capita" )+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light() 
rm(dataforplot13b)

#### country total top 10 development 2000 - 2013
test <- top_10_0013[order(top_10_0013$complete_fp, decreasing = TRUE), ] 
test <- head(test, 10)

data_plot <- nonfood_analysis_tot_ov %>%
  filter(iso %in% test$iso)
 #change to top_10

rm(test)
#plot 14 / Thesis Figure 14
data_plot_fig14 <- data_plot %>%
  mutate(complete_footprint_km2 = complete_footprint*0.01)
ggplot(data= filter(data_plot_fig14, iso != "CHN"), aes(x = year, y = complete_footprint_km2, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  #ggtitle("figure 14: deforestation footprints development 2000-2013, absolute values")+
  labs(x= "year", y = "deforestation [km²]" )+
  theme(legend.title=element_blank())+
  theme_bw()
rm(data_plot_fig14)

  #geom_text(data = filter(data_plot, year == 2013), aes(x = year, y = complete_footprint, group = iso, color= iso, label = iso),check_overlap = TRUE) 
#plot 14b / Thesis Figure 15 

data_plot_Figure15 <- data_plot %>%
  mutate(complete_footprint_km2 = complete_footprint*0.01)#%>%
  #mutate_if(is.numeric,
   #         round,
    #        digits = 1)
colnames(data_plot_Figure15)[colnames(data_plot_Figure15) %in% c("iso")] <- c("state")

ggplot(data= filter(data_plot_Figure15, state == "CHN"), aes(x = year, y = complete_footprint_km2, group = state, color= state, label = state))+  #change to complete fp
  geom_line()+
  #scale_colour_hue(name = "state", l =30)+
  #ggtitle("figure 14b: deforestation footprints development 2000-2013, absolute values")+
  labs(x= "year", y = "deforestation [km²]" )+
  theme(legend.title= element_blank())+
  theme_bw()

rm(data_plot_Figure15)
#14c Thesis Figure 17

data_plot_fig17 <- data_plot %>%
  mutate(complete_footprint_km2 = complete_footprint*0.01)

ggplot(data= filter(data_plot_fig17, year == 2013), aes(x = iso, y = complete_footprint_km2))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  #ggtitle("figure 14c: deforestation footprints (2013)")+
  labs(x= "country", y = "deforestation [km²]" )+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()
rm(data_plot_fig17)

#14d/ Thesis Figure 16

data_plot_fig16 <- data_plot %>%
  mutate(complete_footprint_km2 = complete_footprint*0.01) 
ggplot(data= filter(data_plot_fig16, year == 2000), aes(x = iso, y = complete_footprint_km2))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  #ggtitle("figure 14d: deforestation footprints (2000)")+
  labs(x= "country", y = "deforestation [km²]" )+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()
rm(data_plot_fig16)




rm(data_plot,top_10_per_capita, top_10, top_10_0013, top_10_per_capita_0013)

rm(diff.nonfood, diff.food, food_analysis_ov, food_analysis_tot_ov, nonfood_analysis_ov, nonfood_analysis_tot_ov)


# -------------------------------- analysis 4: HOW IMPORTANT IS INDIRECT DEFORESTATION in FOOTPRINT CALCULATION --------

# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")


# add complete fp per capita 
nonfood_analysis_ov <-  nonfood_analysis_ov %>%
  mutate(complete_fp_per_capita = complete_footprint/population)

data_graph <- nonfood_analysis_ov %>%
  group_by(luc, year) %>%
  summarise(world = sum(complete_footprint, na.rm= TRUE))

#plot15 / Thesis Figure 21
data_graph_fig21 <- data_graph %>%
  mutate(world_km2= world*0.01)

ggplot(data_graph_fig21, aes(x = year, y= world_km2, fill = luc))+   # shows indirect/direct share of all footprints including BRA  
  geom_area(data = data_graph_fig21, aes(x = year, y= world_km2 ))+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  #ggtitle("figure 15: global deforestation footprint")+
  labs(x= "year", y = "deforestation [km²]" )+
  theme(legend.title=element_blank())
  #no ROW but its reasonably low to be left out
rm(data_graph_fig21)

data_graph1 <- nonfood_analysis_ov %>%
  filter(iso != "BRA")%>%
  group_by(luc, year) %>%
  summarise(world = sum(complete_footprint, na.rm= TRUE))

#plot16 / Thesis Figure 20
data_graph1_fig20 <- data_graph1 %>%
  mutate(world_km2 = world*0.01)

ggplot(data_graph1_fig20, aes(x = year, y= world_km2, fill = luc))+    
  geom_area(data = data_graph1_fig20, aes(x = year, y= world_km2 ))+   # shows share (dir/indir) of footprints in importing countries (w/o BRA)
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  #ggtitle("figure 16: global deforestation footprint for imports")+
  labs(x= "year", y = "deforestation [km²]" )+
  theme(legend.title=element_blank())
rm(data_graph1_fig20)

#percentage calculations

#test <- data_graph1 %>%
  #group_by(luc)%>%
  #summarize(test= sum(world))
#rm(test)


rm(data_graph, data_graph1)


# choose top 10 fp per capita
isoplot17 <- nonfood_analysis_ov %>%      
  group_by(iso) %>% 
  summarize("fp0013" = sum(complete_fp_per_capita))%>%
  filter(iso!= "BRA")%>%
  top_n(10,fp0013)
isoplot17 <-  unique(isoplot17$iso)

top_10_per_capita <- nonfood_analysis_ov %>%      # mention in analysis: NO ROW included
  #filter(iso != "BRA") %>% # take BRA out to see main exporting destinations
  group_by(iso,luc) %>% 
  summarize("fp0013" = sum(complete_fp_per_capita))%>%
  filter(iso %in% isoplot17)

#plot17 Thesis Figure 23
top_10_per_capita_fig23 <- top_10_per_capita %>%
  mutate(fp0013_m2 = fp0013*1000)

ggplot(top_10_per_capita_fig23, aes(x = iso, y = fp0013_m2, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  #ggtitle("figure 17: deforestation footprint for imports (2000-2013)")+
  labs(x= "state", y = "deforestation [m²]")+ #per capita" )+
  theme(legend.title=element_blank())
rm(top_10_per_capita_fig23)

#choose top 10 fp on country level
isoplot18 <- nonfood_analysis_ov %>%      
  group_by(iso) %>% 
  summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso!= "BRA")%>%
  top_n(10,fp0013)
isoplot18 <-  unique(isoplot18$iso)

top_10 <- nonfood_analysis_ov %>%
  group_by(iso,luc)  %>%
  summarize("fp0013" = sum(complete_footprint))%>%
   filter(iso %in% isoplot18)

#plot18 Thesis Figure 22
top_10_fig22 <- top_10 %>%
  mutate(fp0013_km2 = fp0013*0.01)#%>%
  mutate_if(is.numeric,
            round,
            digits = 1)
ggplot(data = top_10_fig22, aes(x = iso, y = fp0013_km2, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  #ggtitle("figure 18: deforestation footprint for imports (2000-2013)")+
  labs(x= "state", y = "deforestation [km²]" )+
  theme(legend.title=element_blank())

rm(top_10_per_capita, top_10, isoplot17, isoplot18, top_10_fig22)

rm(diff.food, diff.nonfood, food_analysis_ov, food_analysis_tot_ov, nonfood_analysis_ov, nonfood_analysis_tot_ov)

# -------------------------------- analysis 5: WHICH ARE THE FOREST RISK COMMODITIES AND WHERE DO THEY GO ------------------------------------------

################# top (ten) Food commodities ( ! CHANGE 636 "VALUE" TO "FP_PER_CAPITA" TO HAVE THE SAME OUTPUS PER CAPITA ! )

# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")

footprint_food_analysis_data <- readRDS("~/Deforestation-calculation/Analysis_Footprints/footprint_food_analysis_data.rds")
footprint_nonfood_analysis_data <- readRDS("~/Deforestation-calculation/Analysis_Footprints/footprint_nonfood_analysis_data.rds")

FABIO_lables <- read_excel("Analysis_Footprints/FABIO_lables(thesis Corinna).xlsx")

### insert commoditiy names

colnames(FABIO_lables) <- c("com_code", "FAO_code", "FAO_name", "commodity_group")
footprint_food_analysis_data_names <- merge(footprint_food_analysis_data, FABIO_lables, by = "com_code")

### 5.5.1 take major countries from analysis above and show in which commodieties the embodied deforestation ends up

#calculate total absolute values 2000-2013
top_10_0013 <- nonfood_analysis_tot_ov %>%
  group_by(iso) %>%
  summarise(complete_fp = sum(complete_footprint))%>%
  filter(iso != "BRA")%>%
  top_n(10,complete_fp)

top_10_iso <- top_10_0013$iso


footprint_food_0013 <-  footprint_food_analysis_data_names %>%
  group_by(iso, commodity_group) %>%
  summarise(value = sum(value))%>%
  arrange(desc(value), .by_group = TRUE)%>%
  filter(iso %in% top_10_iso)

footprint_food_0013$value[footprint_food_0013$value < 0] <- 0
footprint_food_0013 <- footprint_food_0013 %>%
  filter(!commodity_group %in% c("Alcohol", "Animal fats", "Sugar, sweeteners"))

#plot19 / Thesis Figure 18
ggplot(footprint_food_0013, aes(fill= commodity_group, y=value, x=iso)) + 
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  coord_flip()+
  #ggtitle("figure 19: deforestation for soy embodied in Food commodities")+
  labs(x= "country", y = "share [%]" )+
  labs(fill = "Food commodity group")+
  theme_light()

footprint_food_0013 <-  footprint_food_analysis_data_names %>%
  group_by(iso, FAO_name, commodity_group) %>%
  summarise(value = sum(value))%>%
  arrange(desc(value), .by_group = TRUE)%>%
  filter(iso %in% top_10_iso)

footprint_food_0013$value[footprint_food_0013$value < 0] <- 0
footprint_food_0013 <- footprint_food_0013 %>%
  filter(!FAO_name %in% c("Beverages, Alcoholic", "Fats, Animals, Raw", "Soybean Cake", "Sweeteners, Other"))

#plot 20 / Thesis Figure 19 
ggplot(footprint_food_0013, aes(fill= FAO_name, y=value, x=iso)) + 
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  coord_flip()+
  #ggtitle("Figure 20: deforestation for soy embodied in food commodities")+
  labs(x= "country", y ="share [%]")+
  labs(fill = "Food commodity")+
  theme_light()

# what did i do with this?
calc <- footprint_food_0013 %>% group_by(iso) %>% summarise(test = sum(value))%>% filter(iso == "JPN") #change country

footprint_food_0013[40,3]/calc[1,2]


EU <- footprint_food_0013 %>% 
  filter(iso %in% c("CHN" , "JPN" , "KOR", "RUS") == FALSE)%>%
  group_by(commodity_group)%>%
  summarise(value = sum(value))
 
EU[5,2]/sum(EU$value)



# outtakes 5 --------------------------------------------------------------

############   5.1 find the commodities that is responsible for the most deforestation between 2000- 2013

# calculate total fp for each food commodity and rank by fp

datafortreemap <- footprint_food_analysis_data_names %>%
  group_by(com_code, FAO_name) %>%
  summarize(total_00_13 = sum(value, na.rm = TRUE))%>%
  arrange(desc(total_00_13)) %>%
  head( n = 10)


datafortreemap$total_00_13[datafortreemap$total_00_13 < 0] <- 0 # set negative values to 0
  

treemap(datafortreemap,
        index="FAO_name",
        vSize="total_00_13",
        type="index",
        palette = "BuGn",
        border.col = "white",
        title = "forest risk food commodities",
        fontsize.title = 12
       
)

rm(datafortreemap)

dataforplot <- footprint_food_analysis_data_names %>%
  group_by(com_code, FAO_name) %>%
  summarize(total_00_13 = sum(value, na.rm = TRUE))%>%
  arrange(desc(total_00_13))

dataforplot$total_00_13[dataforplot$total_00_13 < 0] <- 0 # set negative values to 0

ggplot(dataforplot, aes(x = FAO_name, y = total_00_13))+
  geom_bar(stat = "identity", fill = "darkseagreen")+
  ylab("global embodied deforestation for soy, per capita (2000-2013)")+
  xlab("FABIO products")+
  ggtitle("forest risk food commodities")+
  coord_flip()

rm(dataforplot)

###########  5.2 show development of fp of commodities over the time (2000- 2013)

  # all commodities

#create data for graph
forestrisk_food <- footprint_food_analysis_data_names %>%   # aggregate food fp per commodity and find top 10
  group_by(FAO_name, year) %>%
  summarize(com_total = sum(value, na.rm = TRUE)) #%>%
  #top_n(10, com_total)  # take out for a complete overview

#plot "forest risk commodities"
ggplot(forestrisk_food, aes( x = year, y = com_total, color = FAO_name))+
  geom_line()+
  labs(y = "footprint per commoditiy (food)")+
  scale_colour_hue(name= "state", l = 30) #+
  #geom_text(data = filter(forestrisk_food, year == 2013), aes(x = year, y = com_total, group = FAO_name, color = FAO_name, label = FAO_name), check_overlap = TRUE)
#to do:  change legend to actual product names
  # here we see which commodities have the bigges fp. !BRA included
rm(forestrisk_food)
 

########### 5.2.1 discuss development per importing/consuming country

 # seperate (more detailed) and show the importing/consuming countries

#create data for graph
    #forestrisk_food_com_code <- forestrisk_food %>%
    # arrange(desc(com_total))
    #forestrisk_food_com_code <- unique(top_10_food_com$com_code) 

dataforplotfood <- footprint_food_analysis_data %>%  
  #filter(com_code %in% forestrisk_food_com_code) %>%
  group_by(com_code, year, iso) %>%
  summarize(com_total = sum(value, na.rm = TRUE)) %>%
  group_by(year, com_code) %>%
  arrange(desc(com_total)) %>%
  slice(1:5)

#plot
# the order of the commodities is the order of the plot "forest risk commodities" line 723
ggplot(data = filter(dataforplotfood, com_code %in% c("c116", "c069", "c114", "c117")), aes( x= year, y = com_total, color = iso))+
  geom_line()+
  facet_wrap(~com_code, ncol = 2)

ggplot(data = filter(dataforplotfood, com_code %in% c("c110", "c021", "c112", "c118")), aes( x= year, y = com_total, color = iso))+
  geom_line()+
  facet_wrap(~com_code, ncol = 2)

ggplot(data = filter(dataforplotfood, com_code %in% c("c119", "c120", "c121", "c122")), aes( x= year, y = com_total, color = iso))+
  geom_line()+
  facet_wrap(~com_code, ncol = 2)
# here we have the commodity fp per top 5 countries. We see which countries are the most responible for the commodity's fp 

rm(dataforplotfood)


################# top (ten) NONFood commodities ( ! CHANGE 636 "VALUE" TO "FP_PER_CAPITA" TO HAVE THE SAME OUTPUS PER CAPITA ! )

# No idea what i am goin to do with these results? Maybe i will jus focus on the food- commodities and have the non-food only in the complete fp
#

#create data for graph
top_10_commodities_nonfood <- footprint_nonfood_analysis_data %>%   # aggregate food fp per commodity and find top 10
  group_by(com_code, year) %>%
  summarize(com_total = sum(value, na.rm = TRUE)) %>%
  arrange(desc(com_total)) %>%
  group_by(year) %>%
  slice(1:5)

#ggplot(top_10_commodities_nonfood, aes( x = year, y = com_total, color = com_code))+
# geom_line()+
#labs(y = "footprint per commoditiy (nonfood)")+
#scale_colour_hue(name= "state", l = 30) +
#  geom_text(data = filter(top_10_commodities_nonfood, year == 2013), aes(x = year, y = com_total, group = com_code, color = com_code, label = com_code), check_overlap = TRUE)+
#  theme(legend.position = "none")
# change legend to actual product names 


ggplot(data = filter(top_10_commodities_nonfood, year %in% c(2000, 2005, 2008, 2013)), aes( x = year, y = com_total, fill= com_code))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_brewer(palette= "Paired")+
  scale_x_continuous(breaks= c(2000, 2005, 2008, 2013))+
  #geom_text(aes(label = com_code))+
  #facet_wrap(~year)
  coord_flip()
# wooow wtf am i going to do with these results !???

#test <- footprint_nonfood_analysis_data %>%
  #filter(iso != "BRA")%>%              # to have only exports 
 # group_by(com_code,iso,year)  %>% 
 # summarize(value= sum(value, na.rm = TRUE)) %>%
#  group_by(year, iso) %>%
#  arrange(desc(value)) %>%
#  slice(1:5)

#create data for graph

#top_10_nonfood_com <- unique(top_10_commodities_nonfood$com_code)

dataforplotnonfood <- footprint_nonfood_analysis_data %>% 
  #filter(com_code %in% top_10_nonfood_com) %>%
  group_by(com_code, year, iso) %>%
  summarize(com_total = sum(value, na.rm = TRUE)) %>%
  group_by(year, com_code) %>%
  arrange(desc(com_total)) %>%
  slice(1:5)

ggplot(dataforplotnonfood, aes( x= year, y = com_total, color = iso))+
  geom_line()+
  facet_wrap(~com_code, ncol = 2)

ggplot(data = filter(dataforplotnonfood, com_code %in% c("Products of forestry, logging and related services",	"Additives/Blending Components", "Health and social work services", "Construction work")), aes( x= year, y = com_total, color = iso))+
  geom_line()+
  facet_wrap(~com_code, ncol = 2)+
  geom_text(data = filter(dataforplotnonfood, year == 2013 & com_code %in% c("Products of forestry, logging and related services",	"Additives/Blending Components", "Health and social work services", "Construction work") ), aes( x= year, y = com_total, color = iso, label = iso), check_overlap = TRUE)

ggplot(data = filter(dataforplot, com_code %in% c("Leather and leather products", "Tobacco products", "Chemicals nec", "Motor vehicles, trailers and semi-trailers", "Furniture; other manufactured goods n.e.c.", "Radio, television and communication equipment and apparatus")), aes( x= year, y = com_total, color = iso))+
  geom_line()+
  facet_wrap(~com_code, ncol = 2)+
  geom_text(data = filter(dataforplotnonfood, year == 2013 & com_code %in% c("Leather and leather products", "Tobacco products", "Chemicals nec", "Motor vehicles, trailers and semi-trailers", "Furniture; other manufactured goods n.e.c.", "Radio, television and communication equipment and apparatus")), aes( x= year, y = com_total, color = iso, label = iso), check_overlap = TRUE)

rm(top_10_commodities_nonfood, dataforplotnonfood)




########### 5.3 which are the major consuming/importing countries for the forest risk food commodities?



#major export destinations for top 5 food commodities per year

## per capita and per total values are completely different 

dataforplotexport <- footprint_food_analysis_data %>%  
  filter(iso != "BRA")%>%
  group_by(com_code, year, iso) %>%
  summarize(com_total = sum(fp_per_capita, na.rm = TRUE)) %>% #change to fp_per_capita / value for per capita values
  group_by(year, com_code) %>%
  arrange(desc(com_total, .by_group = TRUE)) %>%
  slice(1:5)

head(unique(dataforplotexport$com_code),10) #find top exporting countries
#c021 c069 c082 c110 c111 c112 c113 c114 c115 c116

dataforlollipop <- dataforplotexport %>%
  filter(year %in% c(2000, 2005, 2008, 2013), com_code == "c082") %>% #2005, 2008, 2013 change for whatever year prefered //
  group_by(year, com_code) %>%
  arrange(year, com_code, desc(com_total, .by_group = TRUE)) 


ggplot(dataforlollipop, aes( x= iso, y= com_total))+ 
         geom_segment(aes(x= iso, xend = iso, y= 0, yend = com_total), color = "#006633")+
         geom_point( color = "#006633", size = 4, alpha = 0.6)+
         theme_light()+
         coord_flip() +
        facet_wrap(~year, ncol = 2)+
         theme(
           panel.grid.major.y = element_blank(),
           panel.border = element_blank(),
           axis.ticks.y = element_blank()
         )
rm(dataforlollipop, dataforplotexport)


  
