#---------------------------------plot: facet wrap----

#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")

facet_data <- defor %>%
   gather(key= y, value = "value",1:3)%>%
   mutate(value = value / 100)
facet_data$y <- substring(facet_data$y, 2)
 
# combine amort & intervall into one variable
a <- "a"
b <- "_y"
facet_data$amort_var <- paste0(a, sprintf("%02d",as.numeric(facet_data$amort)), 
                               b, sprintf("%02d",as.numeric(facet_data$y)))
rm(a,b)

ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
 geom_area()+
 facet_wrap(vars(amort_var))+
 ylab("deforestation [km²]")+
 labs(fill = "LUC")+
 scale_fill_manual(values = c( "#31688e", "#addc30"))

rm(list = ls())

#---------------------------------plot: line graph all combinations----

#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
 
# summarized direct+indirect
sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
 
 
 fun <- function(x){
   round(((x/sens_table[,6])-1) * 100, 0)
 }
 
plot_data <- data.frame(apply(sens_table,2, fun))
 
colnames(plot_data) <- colnames(sens_table)
plot_data$year <- sens_table$year
 
plot_data <- plot_data %>% 
   gather("key","value",-year) %>% 
   mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>% 
   mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>% 
   select(-key)
 
ggplot(plot_data, aes(x= year, y= value, colour = amort))+
   geom_line(aes(linetype=intervall, color = amort))+
   ylab("deviation [%]")+
   #scale_colour_viridis_d("amortization", direction = -1)+
   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
   #labs(colour = "system definitions")
   scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )

rm(list = ls())

#---------------------------------plot: EU/CHN Footprints----

  
footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
 
# aggregate EU countries to EU total
plot_EU <- footprint_EU %>%
   group_by(year, amort, var) %>%
   summarise(complete_footprint = sum(complete_footprint)) %>% 
   ungroup() %>% 
   mutate(year = as.character(year))

avg <- plot_EU %>% 
   group_by(amort, var) %>% 
   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>% 
   ungroup() %>% 
   mutate(year = "average")
 
plot_EU <- bind_rows(plot_EU, avg)%>%
   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
   mutate(complete_footprint = complete_footprint / 100)

rm(avg)

 
#plot EU
ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
   geom_boxplot()+ 
   scale_y_continuous("Footprint [km²]", limits = c(0, 3200)) +
   theme(legend.position="none")+
   xlab("years")+
   ggtitle("EU") +
   scale_fill_manual(values=c("#31688e", "#35b779"))


#prepare plot data for CHINA
plot_CHN <-  footprint_CHN %>%
   group_by(year, amort, var) %>%
   summarise(complete_footprint = sum(complete_footprint)) %>% 
   ungroup() %>% 
   mutate(year = as.character(year))
 
avg <- plot_CHN %>% 
   group_by(amort, var) %>% 
   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>% 
   ungroup() %>% 
   mutate(year = "average")

plot_CHN <- bind_rows(plot_CHN, avg)%>%
   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
   mutate(complete_footprint = complete_footprint / 100)

# plot China
ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
   geom_boxplot()+ 
   theme(legend.position="none")+
   xlab("years")+
   scale_y_continuous("Footprint [km²]", limits = c(0, 3500))+
   ggtitle("CHN") +
   scale_fill_manual(values=c("#31688e", "#35b779"))

rm(list=ls())

#---------------------------------plot: EU/CHN direct vs. dir+indir----

footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")

table_CHN_dir <- footprint_CHN %>%
     filter(luc == "dir")%>%
     ungroup()%>%
     select(-iso, -nonfood, -food, -luc)%>%
     mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
     select(-amort,-var)%>%
     spread(amort_var, complete_footprint)

table_CHN_indirect <- footprint_CHN %>%
    filter(luc == "indir")%>%
    ungroup()%>%
    select(-iso, -nonfood, -food, -luc)%>%
    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
    select(-amort,-var)%>%
    spread(amort_var, complete_footprint)
 
### waruuuuuum geht das niiiiiiiiicht ????!!!!!

table_EU_direct <- footprint_EU %>%
  filter(luc == "dir")%>%
  ungroup()%>%
  select(-iso, -nonfood, -food, -luc)%>%
  mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
  select(-amort,-var)%>%
  spread(amort_var, complete_footprint)

table_EU_indirect <- footprint_EU %>%
  filter(luc == "indir")%>%
  ungroup()%>%
  select(-iso, -nonfood, -food, -luc)%>%
  mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
  select(-amort,-var)%>%
  spread(amort_var, complete_footprint)

