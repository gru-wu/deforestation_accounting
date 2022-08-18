# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")

# calucalte and add ROW complete footprint to nonfood_analysis_ov (direct + indirect)
### actually it would have been smarter to user nonfood_analysis_tot_ov to skip grouping in line 42

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

rm(a,b,c,i)  

sankey_data<-filter(nonfood_analysis_ov, !(nonfood_analysis_ov$iso %in% diff.nonfood))

sankey_data <- sankey_data %>%
  select(iso, year, complete_footprint)%>%
  group_by(iso, year)%>%
  summarise(sum_luc = sum(complete_footprint))


Sankeydata <- filter(sankey_data, iso %in% c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "ESP", 
                                             "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", 
                                             "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SWE", "SVN", "SVK", 
                                             "GBR", "CHE", "NOR", "USA", "CAN", "BRA", "MEX", "CHN", "KOR", 
                                             "IND", "JPN", "TWN", "IDN", "RUS", "AUS", "ROW") )
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

ROW <- filter(Sankeydata, iso %in% c("ROW"))
ROW$continents <- c("ROW")

rm(sankey_data)

Sankeydata_Continents <- rbind(Asia, AUS, BRA, Europe, NorthAmerika, Mex, RUS, ROW) 
Sankeydata_Continents <-   Sankeydata_Continents[ , c(4, 1, 2, 3)]  

Sankeydata_Continents <-  Sankeydata_Continents %>%
  group_by(continents, iso)%>%
  summarise("year00_13"= sum(sum_luc))

rm(Asia, AUS, BRA, Europe, Mex, NorthAmerika, RUS, ROW, Sankeydata)

# calculate flows from to Brazil to continents (1. level of Sankey)

continents <- Sankeydata_Continents %>%
  group_by(continents)%>%
  summarise("year00_13_con" = sum(year00_13))

continents <- cbind(continents, Brazil = c("BRA", "BRA", "BRA", "BRA","BRA", "BRA", "BRA", "BRA"))
continents <- continents[, c(3,1,2)] 
continents <- data.frame(
  source = continents$Brazil,
  target = continents$continents,
  value = continents$year00_13_con
)
Sankeydata_Continents <-  data.frame(
  source = Sankeydata_Continents$continents,
  target = Sankeydata_Continents$iso,
  value = Sankeydata_Continents$year00_13
)

# create Sankey 

# Library
library(networkD3)
library(dplyr)

# all data and all levels needs to be in one data frame 
Sankey_chart <- rbind(Sankeydata_Continents, continents)
Sankey_chart <- Sankey_chart[-c(8,46,42), ] # take out double Brazil

# A connection data frame is a list of flows with intensity for each flow

links <- data.frame(
  source = Sankey_chart$source,
  target = Sankey_chart$target,
  value = Sankey_chart$value
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
#change color for nodes
nodes$group <- as.factor(c("a","b","c","d","e","f","g",
                           "a","a","a","a","a",
                           "b",
                           "c","c","c","c","c","c","c","c","c","c",
                           "c","c","c","c","c","c","c","c","c","c",
                           "c","c","c","c","c","c","c","c","c","c",
                           "d",
                           "e","e",
                           "f",
                           "g",
                           "h"
                   ))

nodes$group <- as.factor(c("my_unique_group"))
my_color <- 'd3.scaleOrdinal() .domain(["a", "b", "c", "d", "e","f","g","h","my_unique_group"]) .range([
"#9ACD32","#6E8B3D", "#CAFF70", "#556B2F", "#9ACD32", "#6E8B3D","#CD6600", "#6E8B3D","grey"])'

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# change colors of flows
links$group <- as.factor(c("a","a","a","a","a","a",
                          "b",
                           "c","c","c","c","c","c","c","c","c","c",
                           "c","c","c","c","c","c","c","c","c","c",
                           "c","c","c","c","c","c","c","c","c","c",
                           "d",
                           "e","e",
                           "f",
                           "a",
                           "b",
                           "c",
                           "d",
                           "e",
                           "f",
                           "f"
                           
                           ))

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name",
                   colourScale=my_color, LinkGroup="group",NodeGroup="group",
                   sinksRight=FALSE)
p

