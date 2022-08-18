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

#-------------------------!!!! check data!! prepare data for Footprints on national level ------------- 


#     !!!! check data and see if all deforestation is still included  !!!


## load data
footprint_food <- readRDS("~/Deforestation-calculation/output/footprint_food.rds")
footprint_nonfood <- readRDS("~/Deforestation-calculation/output/footprint_nonfood.rds")
##

## sum commodities

footprint_food <- footprint_food %>%
  group_by(iso, year, amort, var, luc) %>%
  summarise( value = sum(value))

footprint_nonfood <- footprint_nonfood %>%
  group_by(iso, year, amort, var, luc) %>%
  summarise( value = sum(value))

## combine food and nonfood

food.code <- unique(footprint_food[1])
nonfood.code <- unique(footprint_nonfood[1])
x11 <- as.vector(food.code$iso)
x12 <- as.vector(nonfood.code$iso)
diff.food  <- setdiff(x11,x12)
diff.nonfood <- setdiff(x12,x11)
rm(x11,x12)

total_fp <- c(NA)

footprint_nonfood$complete_footprint <- total_fp

for(i in 1:nrow(footprint_nonfood)){
  x <- as.character(footprint_nonfood$iso[i]) 
  y <- footprint_nonfood$year[i] 
  z <- as.character(footprint_nonfood$luc[i]) 
  test <- match(x,diff.nonfood)
  if(is.na(test)){
    x1 <- which(footprint_food$iso == x & footprint_food$year == y & footprint_food$luc == z) 
    x2 <- footprint_nonfood[i,6]
    endvalue <- footprint_food$value[x1] + x2
    footprint_nonfood$complete_footprint[i] <- as.numeric(endvalue)
  }
  if(!is.na(test)){
    footprint_nonfood[i,8] <- NA
    next()
  }
}

rm(food.code, nonfood.code, total_fp, endvalue, i, test, x, x1, y, x2, z)

footprint_nonfood <- footprint_nonfood[,1:7]
  

## add combine values of countries in diff.food and diff.nonfood into ROW 
            

a <- footprint_nonfood %>%                # summarize ROW for NONfood
  filter(iso %in% diff.nonfood) %>%
  group_by(year,amort,var,luc) %>%
  summarise(ROW_nonfood = sum(value, na.rm = TRUE)) #(value NOT compelte_fp cause ROW is our complete_FP)

b <- footprint_food %>%                   # summarize ROW for FOOD
  filter(iso %in% diff.food) %>%
  group_by(year,amort,var,luc) %>%
  summarise(ROW_food = sum(value, na.rm = TRUE))

for (i in 1:nrow(a)) {                         # sum ROW FOOD and ROW NONfood to have ROW COMPLETE FP
  c <-  data.frame(iso = c("ROW"),
                   year = a$year[i],
                   amort = a$amort[i],
                   var = a$var[i],
                   luc = a$luc[i],
                   value = NA,
                   complete_footprint = sum(a[i,5], b[i,5])
  )
  footprint_nonfood <- rbind(as_tibble(footprint_nonfood),as_tibble(c)) 
}

rm(a,b,c,i)  

data_plots_paper <- as.data.frame(footprint_nonfood)
data_plots_paper <- data_plots_paper %>%
  select(!"value")

data_plots_paper <- data_plots_paper %>%
  filter(!grepl("\\$",iso))

saveRDS(data_plots_paper, paste0("Paper/data_plots_paper.rds"))

rm(list = ls())

#--------------------------------------plot: EU & China ------------------------------------------------

#load data

data_plots_paper <- readRDS("~/Deforestation-calculation/Paper/data_plots_paper.rds")

# combine amort & intervall into one variable
data_plots_paper$amort_var <- paste(data_plots_paper$amort, data_plots_paper$var, sep="_")

data_plots_paper <- select(data_plots_paper, -(amort:var))
select[-var]


EU <- data_plots_paper %>%
  filter(!iso %in% c("CHN", "KOR", "IND", "JPN", "TWN", "IDN", "USA", "CAN", "MEX", "BRA", "AUS", "RUS", "ROW", "ZAF"))%>%
  group_by(iso, year, complete_footprint, amort_var) %>%
  summarise(complete_footprint = sum(complete_footprint))

EU_gesamt <-  EU %>%
  group_by(year, amort_var)%>%
  summarise(complete_footprint = sum(complete_footprint))


CHN <- data_plots_paper %>%
  filter(iso == "CHN") 

### Change Data for CHN and EU

ggplot(EU_gesamt, aes(x = year, y = complete_footprint, fill = amort_var)) + #change EU_gesamt / CHN 
  geom_bar(stat= "identity")+
  guides(fill=guide_legend(title="amortization_time intervals"))

#other graph option: line 

ggplot(EU_gesamt, aes(x = year, y = complete_footprint, colour = amort_var)) + #change EU_gesamt / CHN 
  geom_line(stat= "identity")+
  guides(fill=guide_legend(title="amortization_time intervals"))


rm(list = ls())


#-------------------------prepare data for comparison plots-----------------------


#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
data_plots_paper <- readRDS("~/Deforestation-calculation/Paper/data_plots_paper.rds")

footprint_food <- readRDS("~/Deforestation-calculation/output/footprint_food.rds")
footprint_nonfood <- readRDS("~/Deforestation-calculation/output/footprint_nonfood.rds")

# choose base year
footprint_food <- footprint_food %>%
  group_by(iso, year, amort, luc, var, type)%>%
  summarise( value = sum(value))%>%
  group_by(year, amort, luc, var, type)%>%
  summarise(value = sum(value))


footprint_nonfood <- footprint_nonfood %>%
  group_by(iso, year, amort, luc, var, type)%>%
  summarise( value = sum(value))%>%
  group_by(year, amort, luc, var, type)%>%
  summarise(value = sum(value))

#### crosscheck if trade- deforestation makes sense
#defor <- defor %>% 
  #filter(amort == 5)
#footprint_nonfood <-  footprint_nonfood %>%
 # filter(amort == 5 & var == 3)
#footprint_food <-  footprint_food %>%
  #filter(amort == 5 & var == 3)

#sum(sum(footprint_nonfood$value), sum(footprint_food$value)) / sum(defor$y10) 
# 99,56 % deforestation is embodied food or nonfood

# combine food and  nonfood footprints

footprint_complete <- rbind(footprint_food, footprint_nonfood)

#### crosscheck if trade- deforestation makes sense
#footprint_complete <-  footprint_complete %>%
 # filter(amort == 5, var == 3)

#sum(footprint_complete$value)/sum(defor$y10)
  # 99.56 % 

saveRDS(footprint_complete, paste0("Paper/footprint_complete.rds"))

#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
data_plots_paper <- readRDS("~/Deforestation-calculation/Paper/data_plots_paper.rds")

footprint_food <- readRDS("~/Deforestation-calculation/output/footprint_food.rds")
footprint_nonfood <- readRDS("~/Deforestation-calculation/output/footprint_nonfood.rds")


# take out BRA to have trade- footprint
footprint_food <- footprint_food %>%
  group_by(iso, year, amort, luc, var, type)%>%
  filter(!iso == "BRA")%>%
  summarise( value = sum(value))%>%
  group_by(year, amort, luc, var, type)%>%
  summarise(value = sum(value))


footprint_nonfood <- footprint_nonfood %>%
  group_by(iso, year, amort, luc, var, type)%>%
  filter(!iso == "BRA")%>%
  summarise( value = sum(value))%>%
  group_by(year, amort, luc, var, type)%>%
  summarise(value = sum(value))


# crosscheck if trade- deforestation makes sense
#defor <- defor %>% 
  #filter(amort == 5)
#footprint_food <- footprint_food %>%
  #filter(amort == 5 & var == 3)
#footprint_nonfood <- footprint_nonfood %>%
  #filter(amort == 5 & var == 3)

#sum(sum(footprint_nonfood$value), sum(footprint_food$value)) / sum(defor$y10)
# 72,54 % deforestation is embodied in trade (makes sense)

# combine food and  nonfood footprints

footprint_complete_trade <- rbind(footprint_food, footprint_nonfood)

### crosscheck if trade- deforestation makes sense
#defor <- defor %>% 
#filter(amort == 5)
#footprint_complete_trade <- footprint_complete_trade %>%
#filter(amort == 5 & var == 3)

#sum(sum(footprint_complete_trade$value)) / sum(defor$y10)
  # 72,54 % deforestation is embodied in trade (makes sense)

saveRDS(footprint_complete_trade, paste0("Paper/footprint_complete_trade.rds"))

rm(list = ls())
#-------------------------------------plot: Facet Wrap deforestation (=complete =global) -----------------------------------

#load data
footprint_complete <- readRDS("~/Deforestation-calculation/Paper/footprint_complete")

# combine amort & intervall into one variable
footprint_complete$amort_var <- paste(footprint_complete$amort, footprint_complete$var, sep="_")

footprint_complete <- footprint_complete %>%
  group_by(year, amort, luc, var, amort_var) %>%
  summarise(value = sum(value))

ggplot(footprint_complete, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  geom_area()+
  facet_wrap(vars(amort_var))+
  guides(fill=guide_legend(title="luc"))

rm(list = ls())

#-------------------------------------plot: Facet Wrap (trade) -----------------------------------

#load data
footprint_complete_trade <- readRDS("~/Deforestation-calculation/Paper/footprint_complete_trade")

# combine amort & intervall into one variable
footprint_complete_trade$amort_var <- paste(footprint_complete_trade$amort, footprint_complete_trade$var, sep="_")

footprint_complete_trade <- footprint_complete_trade %>%
  group_by(year, amort, luc, var, amort_var) %>%
  summarise(value = sum(value))

ggplot(footprint_complete_trade, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  geom_area()+
  facet_wrap(vars(amort_var))+
  guides(fill=guide_legend(title="luc"))

rm(list = ls())

#-------------------------------------plot: deviation deforestation all combinations--------

#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")

# summarized direct+indirect
sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")


fun <- function(x){
  (x/sens_table[,9])-1
}

plot_data <- data.frame(apply(sens_table,2, fun))

rm(fun)

colnames(plot_data) <- colnames(sens_table)
plot_data$year <- sens_table$year


y1a1 <- plot_data[,c(1,2)]
colnames(y1a1) <- c("year", "complete_footprint")
y1a1$amort_var <- c("y1a1")

y1a5 <- plot_data[,c(1,3)]
colnames(y1a5) <- c("year", "complete_footprint")
y1a5$amort_var <- c("y1a5")

y1a10 <- plot_data[,c(1,4)]
colnames(y1a10) <- c("year", "complete_footprint")
y1a10$amort_var <- c("y1a10")

y5a1 <- plot_data[,c(1,5)]
colnames(y5a1) <- c("year", "complete_footprint")
y5a1$amort_var <- c("y5a1")

y5a5 <- plot_data[,c(1,6)]
colnames(y5a5) <- c("year", "complete_footprint")
y5a5$amort_var <- c("y5a5")

y5a10 <- plot_data[,c(1,7)]
colnames(y5a10) <- c("year", "complete_footprint")
y5a10$amort_var <- c("y5a10")

y10a1 <- plot_data[,c(1,8)]
colnames(y10a1) <- c("year", "complete_footprint")
y10a1$amort_var <- c("y10a1")

y10a5 <- plot_data[,c(1,9)]
colnames(y10a5) <- c("year", "complete_footprint")
y10a5$amort_var <- c("y10a5")

y10a10 <- plot_data[,c(1,10)]
colnames(y10a10) <- c("year", "complete_footprint")
y10a10$amort_var <- c("y10a10")

plot_data <- rbind(y1a1, y1a5, y1a10, y5a1, y5a5, y5a10, y10a1, y10a5, y10a10)

rm(y1a1, y1a5, y1a10, y5a1, y5a5, y5a10, y10a1, y10a5, y10a10)

#would be an area chart better ? 
ggplot(plot_data, aes(x= year, y= complete_footprint, colour = amort_var))+
  geom_line()+
  scale_colour_manual(values = c("#8B7355", "#CDAA7D","#FFD39B", "#528B8B", "#97FFFF", "#00CDCD", "#458B00", "#A2CD5A", "#CAFF70") )
  
rm(list = ls())  

#-------------------------------------calculations: deviation direct/indirect ----------

# to see how the share of indi and di varies with different amortization periods and luc intervals
# we better calculate this like we did in lines 389-395


#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")

defor_dev_luc <- defor %>%
  select(y10:luc)%>%
  filter(amort == 5)

# if we need a plot to show the share of direct and indirect deforestation
ggplot(defor_dev_luc, aes(x = year, y = y10, fill = luc))+
  geom_area()

defor_dev_luc <- defor_dev_luc %>%
  group_by(amort, luc)%>%
  summarise(y10 = sum(y10))

defor_dev_luc[2,3]/(defor_dev_luc[1,3] + defor_dev_luc[2,3])
# 0.6567289 is the share of indirect deforestation for base year
# -> deviation of around 65 % depending on including indirect deforestation or not

rm(list = ls())  

#-------------------------------------calculations: deviation amortisation ----------

#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")

defor_dev_amort <- defor %>%
  select(y10:luc)%>%
  group_by(year, amort)%>%
  summarise(y10 = sum(y10))%>%
  group_by(amort)%>%
  summarise(y10 = sum(y10))

defor_dev_amort$deviation <- defor_dev_amort$y10/7951346

# here we can see how much deforestation results varies with amortization periods 

defor_dev_amort_luc <- defor %>%
  select(y10:luc)%>%
  group_by(amort, luc)%>%
  summarise(y10 = sum(y10))

# here we can see how much the share of direct/indirect deforestation varies with amortization periods
defor_dev_amort_luc[2,3]/(defor_dev_amort_luc[1,3] + defor_dev_amort_luc[2,3])
# 0.6930431
defor_dev_amort_luc[4,3]/(defor_dev_amort_luc[3,3] + defor_dev_amort_luc[4,3])
# 0.6567289
defor_dev_amort_luc[6,3]/(defor_dev_amort_luc[5,3] + defor_dev_amort_luc[6,3])
# 0.6307413

rm(list = ls())  


#-------------------------------------calculations: deviation LUC intervals-----

#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")

defor_dev_var <- defor %>%
  filter(amort == 5) 

y1 <- defor_dev_var %>%
  select(y1,amort,luc)


