
# load data
nonfood_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_tot_ov.rds")
nonfood_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/nonfood_analysis_ov.rds")

food_analysis_tot_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_tot_ov.rds")
food_analysis_ov <- readRDS("~/Deforestation-calculation/Analysis_Footprints/food_analysis_ov.rds")

diff.nonfood <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.nonfood.rds")
diff.food <- readRDS("~/Deforestation-calculation/Analysis_Footprints/diff.food.rds")

# NO IDEA - JUST RUN THE CODE -----------------------------------------------------------


#  WHAT DID I WANTED TO SHOW ???
ggplot(share_Brazil)+
  geom_col(mapping = aes(x = year, y = Brazil, fill = world_withoutBRA), position = "stack")

#Scatterplot
# plot 
# WHAT DID I WANTED TO SHOW ???
ggplot(share_Brazil, aes(x=Brazil, y=world_withoutBRA)) +
  geom_point(shape= 1, colour = "darkseagreen3", size = 3, stroke = 2) +    
  geom_smooth(method=lm, colour = "darkseagreen4")

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

ggplot(filter(dataforareaplot_km2, iso !="global consumption"), aes(x=year, y=value_km2, fill = factor(iso, levels = c("foreign consumption", "domestic consumption")) )) + 
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

#plot 3 / Thesis Figure 7 / paper Figure 3

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

#