summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso!= "BRA")%>%
  top_n(10,fp0013)
isoplot17 <-  unique(isoplot17$iso)
isoplot18 <- nonfood_analysis_ov %>%
  group_by(iso) %>%
  summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso!= "BRA")%>%
  top_n(10,fp0013)
isoplot18 <- nonfood_analysis_ov %>%
  group_by(iso) %>%
  summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso!= "BRA")%>%
  top_n(10,fp0013)
View(isoplot18)
isoplot18 <-  unique(isoplot18$iso)
top_10 <- nonfood_analysis_ov %>%
  group_by(iso,luc)  %>%
  summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso != "BRA")
#plot18
ggplot(data = top_10, aes(x = iso, y = fp0013, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 18: deforestation footprint for imports (2000-2013)")+
  labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
  theme(legend.title=element_blank())
top_10 <- nonfood_analysis_ov %>%
  group_by(iso,luc)  %>%
  summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso != "BRA")%>%
  top_n(10,fp0013)
#plot18
ggplot(data = top_10, aes(x = iso, y = fp0013, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 18: deforestation footprint for imports (2000-2013)")+
  labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
  theme(legend.title=element_blank())
View(top_10)
top_10 <- nonfood_analysis_ov %>%
  group_by(iso,luc)  %>%
  summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso %in% isoplot17)
#plot18
ggplot(data = top_10, aes(x = iso, y = fp0013, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 18: deforestation footprint for imports (2000-2013)")+
  labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
  theme(legend.title=element_blank())
top_10 <- nonfood_analysis_ov %>%
  group_by(iso,luc)  %>%
  summarize("fp0013" = sum(complete_footprint))%>%
  filter(iso %in% isoplot18)
#plot18
ggplot(data = top_10, aes(x = iso, y = fp0013, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 18: deforestation footprint for imports (2000-2013)")+
  labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
  theme(legend.title=element_blank())
View(top_10_per_capita)
0.001844332+0.003222903
#### Per capita analysis top 10 development 2000 - 2013
test <- top_10_per_capita_0013[order(top_10_per_capita_0013$complete_fp_per_capita, decreasing = TRUE), ]
# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")
food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")
diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")
nonfood_analysis_tot_ov <- nonfood_analysis_tot_ov %>%
  mutate(complete_fp_per_capita = complete_footprint/population)
#calculate total per capita 2000-2013
top_10_per_capita_0013 <- nonfood_analysis_tot_ov %>%
  group_by(iso) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA")%>%
  top_n(10,complete_fp_per_capita)
#calculate total absolute values 2000-2013
top_10_0013 <- nonfood_analysis_tot_ov %>%
  group_by(iso) %>%
  summarise(complete_fp = sum(complete_footprint))%>%
  filter(iso != "BRA")%>%
  top_n(10,complete_fp)
#plot 10
ggplot(top_10_per_capita_0013, aes(x = iso, y = complete_fp_per_capita))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  ggtitle("figure 10: deforestation footprints (2000-2013)")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) per capita" )
#plot 11
ggplot(data = filter(top_10_0013), aes(x = iso, y = complete_fp))+                       # w bra
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  ggtitle("figure 11: deforestation footprints (2000-2013)")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) absolute values" )
## belongs to analysis 1 --------------------------------------------------
# plot 12
ggplot(data = filter(top_10_per_capita, iso == "BRA"), aes(x = year, y = complete_fp_per_capita))+       # BRA
  geom_smooth(color = "darkseagreen3")
#### Per capita analysis top 10 development 2000 - 2013
test <- top_10_per_capita_0013[order(top_10_per_capita_0013$complete_fp_per_capita, decreasing = TRUE), ]
test <- head(test, 10)
data_plot <- nonfood_analysis_tot_ov %>%
  filter(iso %in% test$iso)  #change to top_10
rm(test)
#plot 13
ggplot(data_plot, aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  ggtitle("figure 13: deforestation footprints development 2000-2013, per capita")+
  labs(x= "importing countries (top 10 per capita)", y = "deforestation embodied in soy imports, (ha) per capita" )#+
#plot13a
ggplot(filter(nonfood_analysis_tot_ov, iso %in% c("CHN", "KOR", "TWN","JPN")), aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  ggtitle("figure 13a: deforestation footprints development 2000-2013, per capita")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) per capita" )#+
#plot13b
ggplot(data= filter(top_10_per_capita, year == 2013 & iso != "BRA"), aes(x = iso, y = complete_fp_per_capita))+         # per capita w BRA
  geom_bar(fill = "darkseagreen3",stat="identity", position=position_dodge())+
  ggtitle("figure 13b: deforestation footprints (2013)")+
  labs(x= "importing countries (top 10 in 2013)", y = "deforestation embodied in soy imports, (ha) per capita" )
dataforplot13b <- nonfood_analysis_tot_ov %>%
  group_by(iso, year) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA" & year == 2013)%>%
  top_n(10, complete_fp_per_capita)
View(data_plot)
View(dataforplot13b)
dataforplot13b <- nonfood_analysis_tot_ov %>%
  group_by(iso, year) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA" & year == 2013)%>%
  top_n(10, complete_fp_per_capita)
dataforplot13b <- nonfood_analysis_tot_ov %>%
  group_by(iso, year) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA" & year == 2013)%>%
  top_n(10, complete_fp_per_capita)
View(top_10_per_capita_0013)
top_n(10, dataforplot13b$complete_fp_per_capita)
dataforplot13b <- nonfood_analysis_tot_ov %>%
  group_by(iso, year) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA" & year == 2013)%>%
  order(complete_fp_per_capita, descending = TRUE)
dataforplot13b <- nonfood_analysis_tot_ov %>%
  group_by(iso, year) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA" & year == 2013)%>%
  top_n(10, complete_fp_per_capita)
dataforplot13b <- nonfood_analysis_tot_ov %>%
  group_by(iso, year) %>%
  summarise(complete_fp_per_capita = sum(complete_fp_per_capita))%>%
  filter(iso != "BRA" & year == 2013)%>%
  arrange(desc(complete_fp_per_capita))
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
  labs(x= "importing countries (top 10 in 2013)", y = "deforestation embodied in soy imports, (ha) per capita" )
#### country total top 10 development 2000 - 2013
test <- top_10_0013[order(top_10_0013$complete_fp, decreasing = TRUE), ]
test <- head(test, 10)
data_plot <- nonfood_analysis_tot_ov %>%
  filter(iso %in% test$iso)  #change to top_10
rm(test)
rm(dataforplot13b)
#### country total top 10 development 2000 - 2013
test <- top_10_0013[order(top_10_0013$complete_fp, decreasing = TRUE), ]
test <- head(test, 10)
data_plot <- nonfood_analysis_tot_ov %>%
  filter(iso %in% test$iso)  #change to top_10
rm(test)
#plot 14
ggplot(data_plot, aes(x = year, y = complete_footprint, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+ ggtitle("figure 14: deforestation footprints development 2000-2013")+
  labs(x= "importing countries (top 10)", y = "deforestation embodied in soy imports, (ha) absolute values" )#+
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
  labs(x= "importing countries (top 10 in 2013)", y = "deforestation embodied in soy imports, (ha) per capita" )
g
#plot 13
ggplot(data_plot, aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  ggtitle("figure 13: deforestation footprints development 2000-2013, per capita")+
  labs(x= "importing countries (top 10 per capita)", y = "deforestation embodied in soy imports, (ha) per capita" )+
  theme(legend.title=element_blank())
l
#plot13a
ggplot(filter(nonfood_analysis_tot_ov, iso %in% c("CHN", "KOR", "TWN","JPN")), aes(x = year, y = complete_fp_per_capita, group = iso, color= iso, label = iso))+  #change to complete fp
  geom_line()+
  scale_colour_hue(name = "state", l =30)+
  ggtitle("figure 13a: deforestation footprints development 2000-2013, per capita")+
  labs(x= "importing countries", y = "deforestation embodied in soy imports, (ha) per capita" )+
  theme(legend.title=element_blank())
rm(data_plot,top_10_per_capita, top_10, top_10_0013, top_10_per_capita_0013)
rm(diff.nonfood, diff.food, food_analysis_ov, food_analysis_tot_ov, nonfood_analysis_ov, nonfood_analysis_tot_ov)
rm(dataforplot13b)
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
#plot15
ggplot(data_graph, aes(x = year, y= world, fill = luc))+   # shows indirect/direct share of all footprints including BRA
  geom_area(data = data_graph, aes(x = year, y= world ))+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 15: global deforestation footprint")+
  labs(x= "year", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
  theme(legend.title=element_blank())
data_graph1 <- nonfood_analysis_ov %>%
  filter(iso != "BRA")%>%
  group_by(luc, year) %>%
  summarise(world = sum(complete_footprint, na.rm= TRUE))
#plot16
ggplot(data_graph1, aes(x = year, y= world, fill = luc))+
  geom_area(data = data_graph1, aes(x = year, y= world ))+   # shows share (dir/indir) of footprints in importing countries (w/o BRA)
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 16: global deforestation footprint for imports")+
  labs(x= "year", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
  theme(legend.title=element_blank())
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
#plot17
ggplot(top_10_per_capita, aes(x = iso, y = fp0013, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 17: deforestation footprint for imports (2000-2013)")+
  labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) per capita" )+
  theme(legend.title=element_blank())
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
#plot18
ggplot(data = top_10, aes(x = iso, y = fp0013, fill = luc))+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme_light()+
  ggtitle("figure 18: deforestation footprint for imports (2000-2013)")+
  labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
  theme(legend.title=element_blank())
538157.05 + 1160475.75
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
#calculate total absolute values 2000-2013
top_10_0013 <- nonfood_analysis_tot_ov %>%
  group_by(iso) %>%
  summarise(complete_fp = sum(complete_footprint))%>%
  filter(iso != "BRA")%>%
  top_n(10,complete_fp)
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
  labs(x=
         View(top_10_0013)
       View(top_10_0013)
       View(top_10_0013)
       1160475.75 - 538157.05
       (1160475.75 - 538157.05)/(1160475.75 + 538157.05)
       #plot16
       ggplot(data_graph1, aes(x = year, y= world, fill = luc))+
         geom_area(data = data_graph1, aes(x = year, y= world ))+   # shows share (dir/indir) of footprints in importing countries (w/o BRA)
         scale_fill_brewer(palette="Greens")+
         theme_bw()+
         theme_light()+
         ggtitle("figure 16: global deforestation footprint for imports")+
         labs(x= "year", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
         theme(legend.title=element_blank())
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
       #plot15
       ggplot(data_graph, aes(x = year, y= world, fill = luc))+   # shows indirect/direct share of all footprints including BRA
         geom_area(data = data_graph, aes(x = year, y= world ))+
         scale_fill_brewer(palette="Greens")+
         theme_bw()+
         theme_light()+
         ggtitle("figure 15: global deforestation footprint")+
         labs(x= "year", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
         theme(legend.title=element_blank())
       data_graph1 <- nonfood_analysis_ov %>%
         filter(iso != "BRA")%>%
         group_by(luc, year) %>%
         summarise(world = sum(complete_footprint, na.rm= TRUE))
       #plot16
       ggplot(data_graph1, aes(x = year, y= world, fill = luc))+
         geom_area(data = data_graph1, aes(x = year, y= world ))+   # shows share (dir/indir) of footprints in importing countries (w/o BRA)
         scale_fill_brewer(palette="Greens")+
         theme_bw()+
         theme_light()+
         ggtitle("figure 16: global deforestation footprint for imports")+
         labs(x= "year", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
         theme(legend.title=element_blank())
       View(data_graph1)
       test <- data_graph1 %>%
         group_by(luc)%>%
         summarize(test= sum(world))
       View(test)
       3109024/(1622674 + 3109024)
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
       #plot15
       ggplot(data_graph, aes(x = year, y= world, fill = luc))+   # shows indirect/direct share of all footprints including BRA
         geom_area(data = data_graph, aes(x = year, y= world ))+
         scale_fill_brewer(palette="Greens")+
         theme_bw()+
         theme_light()+
         ggtitle("figure 15: global deforestation footprint")+
         labs(x= "year", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
         theme(legend.title=element_blank())
       #no ROW but its raesonably low to be left out
       data_graph1 <- nonfood_analysis_ov %>%
         filter(iso != "BRA")%>%
         group_by(luc, year) %>%
         summarise(world = sum(complete_footprint, na.rm= TRUE))
       #plot16
       ggplot(data_graph1, aes(x = year, y= world, fill = luc))+
         geom_area(data = data_graph1, aes(x = year, y= world ))+   # shows share (dir/indir) of footprints in importing countries (w/o BRA)
         scale_fill_brewer(palette="Greens")+
         theme_bw()+
         theme_light()+
         ggtitle("figure 16: global deforestation footprint for imports")+
         labs(x= "year", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
         theme(legend.title=element_blank())
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
       #plot17
       ggplot(top_10_per_capita, aes(x = iso, y = fp0013, fill = luc))+
         geom_bar(stat="identity", position="stack")+
         scale_fill_brewer(palette="Greens")+
         theme_bw()+
         theme_light()+
         ggtitle("figure 17: deforestation footprint for imports (2000-2013)")+
         labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) per capita" )+
         theme(legend.title=element_blank())
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
       #plot18
       ggplot(data = top_10, aes(x = iso, y = fp0013, fill = luc))+
         geom_bar(stat="identity", position="stack")+
         scale_fill_brewer(palette="Greens")+
         theme_bw()+
         theme_light()+
         ggtitle("figure 18: deforestation footprint for imports (2000-2013)")+
         labs(x= "importing country", y = "deforestation embodied in soy consumption, (ha) absolute values" )+
         theme(legend.title=element_blank())
       View(top_10)
       1160475.75/(1160475.75 + 538157.05)
       (1160475.75 - 538157.05)
       622318.7 /(1160475.75 + 538157.05)
       rm(top_10_per_capita, top_10)
       rm(diff.food, diff.nonfood, food_analysis_ov, food_analysis_tot_ov, nonfood_analysis_ov, nonfood_analysis_tot_ov)
       rm(top_10_per_capita, top_10, isoplot17, isoplot18)
       # load data
       nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
       nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")
       food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
       food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")
       diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
       diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")
       footprint_food_analysis_data <- readRDS("~/Deforestation-calculation/Analysis_Footprints/footprint_food_analysis_data.rds")
       footprint_nonfood_analysis_data <- readRDS("~/Deforestation-calculation/Analysis_Footprints/footprint_nonfood_analysis_data.rds")
       datafortreemap <- footprint_food_analysis_data %>%
         group_by(com_code) %>%
         summarize(total_00_13 = sum(value, na.rm = TRUE))%>%
         arrange(desc(total_00_13))
       datafortreemap$total_00_13[datafortreemap$total_00_13 < 0] <- 0 # set negative values to 0
       treemap(datafortreemap,
               index="com_code",
               vSize="total_00_13",
               type="index",
               palette = "BuGn",
               border.col = "white",
               title = "forest risk commodities: FOOD"
       )
       library(treemap)
       install.packages("treemap")
       library(treemap)
       install.packages("rlang")
       install.packages("rlang")
       